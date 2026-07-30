#!/usr/bin/env Rscript

Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1"
)

suppressPackageStartupMessages({
  library(data.table)
  library(nnet)
  library(parallel)
})
options(encoding = "UTF-8")

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/",
                mustWork = TRUE)
} else {
  normalizePath("10_CHARLS_equity_interaction_bootstrap_2000.R",
                winslash = "/", mustWork = TRUE)
}
out_dir <- dirname(script_path)
source(file.path(out_dir, "01_unified_health_equity_analysis.R"),
       local = .GlobalEnv, encoding = "UTF-8")

n_boot <- as.integer(Sys.getenv("MARKOV_EQUITY_BOOTSTRAP_N", "2000"))
base_seed <- as.integer(Sys.getenv("MARKOV_EQUITY_BOOTSTRAP_SEED", "20260731"))
detected_cores <- parallel::detectCores(logical = TRUE)
n_workers <- as.integer(Sys.getenv(
  "MARKOV_EQUITY_BOOTSTRAP_WORKERS",
  as.character(min(24L, max(1L, detected_cores - 1L)))
))
n_workers <- max(1L, min(n_workers, detected_cores))
max_attempts <- as.integer(Sys.getenv(
  "MARKOV_EQUITY_BOOTSTRAP_MAX_ATTEMPTS",
  as.character(max(4000L, n_boot * 2L))
))

period_levels <- c("Pre-expansion", "Expansion")
dimension_specs <- list(
  Age = list(
    variable = "age_group",
    exposed = "75+",
    reference = "65-74",
    formula = to_state_f ~ from_state_f * policy_period * age_group +
      interval_years + rural_group + education_group + sex_group
  ),
  Residence = list(
    variable = "rural_group",
    exposed = "Rural",
    reference = "Urban/town",
    formula = to_state_f ~ from_state_f * policy_period * rural_group +
      interval_years + age_group + education_group + sex_group
  ),
  Education = list(
    variable = "education_group",
    exposed = "Primary or less",
    reference = "Middle school or higher",
    formula = to_state_f ~ from_state_f * policy_period * education_group +
      interval_years + age_group + rural_group + sex_group
  )
)

weighted_quantile <- function(x, w, probs = c(0.01, 0.99)) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (!length(x) || sum(w) <= 0) return(rep(NA_real_, length(probs)))
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

predict_full <- function(model, newdata) {
  pp <- predict(model, newdata = newdata, type = "probs")
  if (is.null(dim(pp))) pp <- matrix(pp, nrow = 1)
  if (is.null(colnames(pp))) colnames(pp) <- model$lev
  full <- matrix(0, nrow = nrow(pp), ncol = 4,
                 dimnames = list(NULL, as.character(1:4)))
  full[, colnames(pp)] <- pp
  full
}

one_group_metrics <- function(model, target, target_weights, variable, level) {
  nd <- copy(target)
  nd[, policy_period := factor("Expansion", levels = period_levels)]
  nd[, interval_years := 2]
  nd[[variable]] <- factor(level, levels = levels(target[[variable]]))

  entry_i <- which(as.integer(as.character(nd$from_state_f)) %in% 1:2)
  recovery_i <- which(as.integer(as.character(nd$from_state_f)) %in% 2:3)
  death_i <- which(as.integer(as.character(nd$from_state_f)) %in% 1:3)
  if (!length(entry_i) || !length(recovery_i) || !length(death_i)) {
    stop("missing_eligible_transition")
  }

  p_entry <- predict_full(model, nd[entry_i])
  p_recovery <- predict_full(model, nd[recovery_i])
  p_death <- predict_full(model, nd[death_i])
  recovery_from <- as.integer(as.character(nd$from_state_f[recovery_i]))
  recovery_probability <- ifelse(
    recovery_from == 2L,
    p_recovery[, "1"],
    p_recovery[, "1"] + p_recovery[, "2"]
  )

  data.table(
    metric = c(
      "Entry to high deficit",
      "Recovery from intermediate/high deficit",
      "Death"
    ),
    eligible_definition = c(
      "Current low or intermediate deficit",
      "Current intermediate or high deficit",
      "Any living current state"
    ),
    estimate = c(
      weighted.mean(p_entry[, "3"], target_weights[entry_i]),
      weighted.mean(recovery_probability, target_weights[recovery_i]),
      weighted.mean(p_death[, "4"], target_weights[death_i])
    ),
    eligible_n_weighted = c(
      sum(target_weights[entry_i]),
      sum(target_weights[recovery_i]),
      sum(target_weights[death_i])
    )
  )
}

all_equity_metrics <- function(models, target, target_weights) {
  ans <- list()
  k <- 0L
  for (dimension in names(dimension_specs)) {
    ss <- dimension_specs[[dimension]]
    for (level in c(ss$reference, ss$exposed)) {
      k <- k + 1L
      z <- one_group_metrics(
        models[[dimension]], target, target_weights,
        ss$variable, level
      )
      z[, `:=`(
        dimension = dimension,
        level = level,
        period = "Expansion",
        horizon_years = 2L
      )]
      ans[[k]] <- z
    }
  }
  rbindlist(ans, use.names = TRUE)
}

make_gaps <- function(metrics) {
  ans <- list()
  k <- 0L
  for (dim_name in names(dimension_specs)) {
    ss <- dimension_specs[[dim_name]]
    ex <- metrics[dimension == dim_name & level == ss$exposed]
    ref <- metrics[dimension == dim_name & level == ss$reference]
    z <- merge(
      ex, ref,
      by = c("dimension", "period", "horizon_years", "metric",
             "eligible_definition"),
      suffixes = c("_exposed", "_reference")
    )
    k <- k + 1L
    ans[[k]] <- z[, .(
      dimension, period, horizon_years, metric, eligible_definition,
      exposed = level_exposed,
      reference = level_reference,
      contrast = paste0(level_exposed, " minus ", level_reference),
      estimate = estimate_exposed - estimate_reference,
      eligible_n_exposed = eligible_n_weighted_exposed,
      eligible_n_reference = eligible_n_weighted_reference
    )]
  }
  rbindlist(ans)
}

fit_models <- function(fit_dat, weight) {
  fit_dat <- copy(fit_dat)
  fit_dat[, .fit_weight := weight]
  lapply(dimension_specs, function(ss) {
    mod <- multinom(
      ss$formula, data = fit_dat, weights = .fit_weight,
      trace = FALSE, maxit = 1000
    )
    if (!is.null(mod$convergence) && mod$convergence != 0) {
      stop("multinom_nonconvergence")
    }
    mod
  })
}

fit_one_sample <- function(sample_frequency) {
  boot <- merge(ipcw_dat, sample_frequency, by = "ID", all = FALSE)
  if (!nrow(boot)) stop("empty_bootstrap_sample")

  den <- glm(
    resolved ~ factor(interval) + from_state_f + age_group + rural_group +
      education_group + sex_group,
    family = binomial(), data = boot, weights = boot_frequency
  )
  num <- glm(
    resolved ~ factor(interval),
    family = binomial(), data = boot, weights = boot_frequency
  )
  boot[, p_den_boot := predict(den, newdata = boot, type = "response")]
  boot[, p_num_boot := predict(num, newdata = boot, type = "response")]
  if (any(!is.finite(boot$p_den_boot)) ||
      any(!is.finite(boot$p_num_boot)) ||
      any(boot$p_den_boot <= 0) || any(boot$p_num_boot <= 0)) {
    stop("invalid_ipcw_probability")
  }
  boot[, ipcw_raw_boot := p_num_boot / p_den_boot]
  trim_boot <- weighted_quantile(
    boot[resolved == 1, ipcw_raw_boot],
    boot[resolved == 1, boot_frequency],
    c(0.01, 0.99)
  )
  if (any(!is.finite(trim_boot)) || trim_boot[1] <= 0) {
    stop("invalid_ipcw_trim")
  }
  boot[, ipcw_boot := pmin(pmax(ipcw_raw_boot, trim_boot[1]), trim_boot[2])]
  if (any(!is.finite(boot$ipcw_boot)) || any(boot$ipcw_boot <= 0)) {
    stop("invalid_ipcw_weight")
  }

  fit_dat <- boot[resolved == 1 & !is.na(to_state)]
  fit_dat[, to_state_f := factor(to_state, levels = 1:4)]
  if (uniqueN(fit_dat$to_state) < 4L) stop("missing_outcome_state")
  models <- fit_models(fit_dat, fit_dat$boot_frequency * fit_dat$ipcw_boot)
  metrics <- all_equity_metrics(models, boot, boot$boot_frequency)
  if (any(!is.finite(metrics$estimate)) ||
      any(metrics$estimate < 0) || any(metrics$estimate > 1)) {
    stop("invalid_standardized_probability")
  }
  list(
    metrics = metrics,
    gaps = make_gaps(metrics),
    diagnostics = data.table(
      ipcw_trim_low = trim_boot[1],
      ipcw_trim_high = trim_boot[2],
      ipcw_min = min(fit_dat$ipcw_boot),
      ipcw_max = max(fit_dat$ipcw_boot),
      ipcw_mean = weighted.mean(
        fit_dat$ipcw_boot, fit_dat$boot_frequency
      ),
      n_ids_selected = nrow(sample_frequency),
      n_deaths_weighted = sum(
        fit_dat$boot_frequency[fit_dat$to_state == 4]
      )
    )
  )
}

bootstrap_one <- function(attempt_id, seed) {
  set.seed(seed)
  sampled <- sample.int(length(bootstrap_ids), length(bootstrap_ids),
                        replace = TRUE)
  freq <- tabulate(sampled, nbins = length(bootstrap_ids))
  sample_frequency <- data.table(
    ID = bootstrap_ids[freq > 0],
    boot_frequency = freq[freq > 0]
  )
  started <- proc.time()[3]
  result <- tryCatch(
    fit_one_sample(sample_frequency),
    error = function(e) structure(
      list(error = conditionMessage(e)), class = "bootstrap_failure"
    )
  )
  elapsed <- proc.time()[3] - started
  if (inherits(result, "bootstrap_failure")) {
    return(list(
      success = FALSE,
      diagnostics = data.table(
        attempt_id, seed, success = FALSE,
        failure_reason = result$error, elapsed_seconds = elapsed
      )
    ))
  }
  for (nm in c("metrics", "gaps")) result[[nm]][, attempt_id := attempt_id]
  result$diagnostics[, `:=`(
    attempt_id = attempt_id, seed = seed, success = TRUE,
    failure_reason = NA_character_, elapsed_seconds = elapsed
  )]
  c(list(success = TRUE), result)
}

summarize_bootstrap <- function(point, boot, keys) {
  stats <- boot[, .(
    n_boot = uniqueN(rep),
    bootstrap_mean = mean(estimate),
    bootstrap_se = sd(estimate),
    conf_low = as.numeric(quantile(estimate, 0.025, names = FALSE)),
    conf_high = as.numeric(quantile(estimate, 0.975, names = FALSE))
  ), by = keys]
  ans <- merge(point, stats, by = keys, all.x = TRUE)
  ans[, bias := bootstrap_mean - estimate]
  ans
}

bootstrap_ids <- sort(unique(ipcw_dat$ID))
point_fit_dat <- ipcw_dat[resolved == 1 & !is.na(to_state)]
point_fit_dat[, to_state_f := factor(to_state, levels = 1:4)]
point_models <- fit_models(point_fit_dat, point_fit_dat$ipcw)
point_metrics <- all_equity_metrics(
  point_models, ipcw_dat, rep(1, nrow(ipcw_dat))
)
point_gaps <- make_gaps(point_metrics)

set.seed(base_seed)
attempt_seeds <- sample.int(.Machine$integer.max, max_attempts)
cl <- makeCluster(n_workers, type = "PSOCK")
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterEvalQ(cl, {
  Sys.setenv(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1"
  )
  suppressPackageStartupMessages({ library(data.table); library(nnet) })
  NULL
})
clusterExport(
  cl,
  c(
    "ipcw_dat", "bootstrap_ids", "attempt_seeds", "period_levels",
    "dimension_specs", "weighted_quantile", "predict_full",
    "one_group_metrics", "all_equity_metrics", "make_gaps",
    "fit_models", "fit_one_sample", "bootstrap_one"
  ),
  envir = environment()
)

results <- list()
diagnostics <- list()
n_success <- 0L
n_attempted <- 0L
batch_size <- min(100L, n_boot)
started_at <- Sys.time()
while (n_success < n_boot && n_attempted < max_attempts) {
  needed <- n_boot - n_success
  this_batch <- min(batch_size, needed, max_attempts - n_attempted)
  attempt_ids <- n_attempted + seq_len(this_batch)
  batch <- parLapplyLB(
    cl, attempt_ids,
    function(i) bootstrap_one(i, attempt_seeds[i])
  )
  diagnostics <- c(diagnostics, lapply(batch, `[[`, "diagnostics"))
  good <- batch[vapply(batch, function(x) isTRUE(x$success), logical(1))]
  results <- c(results, good)
  n_success <- length(results)
  n_attempted <- n_attempted + this_batch
  cat(sprintf(
    "Equity bootstrap progress: %d/%d successful after %d attempts (%s)\n",
    n_success, n_boot, n_attempted, format(Sys.time(), "%H:%M:%S")
  ))
}
stopCluster(cl)
if (n_success < n_boot) {
  stop(sprintf(
    "Only %d successful bootstrap replications after %d attempts",
    n_success, n_attempted
  ))
}

for (i in seq_along(results)) {
  for (nm in c("metrics", "gaps")) {
    tmp <- as.data.table(results[[i]][[nm]])
    setalloccol(tmp)
    set(tmp, j = "rep", value = i)
    results[[i]][[nm]] <- tmp
  }
  tmp <- as.data.table(results[[i]]$diagnostics)
  setalloccol(tmp)
  set(tmp, j = "rep", value = i)
  results[[i]]$diagnostics <- tmp
}
boot_metrics <- rbindlist(lapply(results, `[[`, "metrics"))
boot_gaps <- rbindlist(lapply(results, `[[`, "gaps"))
attempt_diagnostics <- rbindlist(diagnostics, fill = TRUE)
success_diagnostics <- rbindlist(
  lapply(results, `[[`, "diagnostics"), fill = TRUE
)

metric_keys <- c(
  "dimension", "level", "period", "horizon_years",
  "metric", "eligible_definition"
)
gap_keys <- c(
  "dimension", "period", "horizon_years", "metric",
  "eligible_definition", "exposed", "reference", "contrast"
)
metric_summary <- summarize_bootstrap(
  point_metrics[, !c("eligible_n_weighted")],
  boot_metrics, metric_keys
)
gap_summary <- summarize_bootstrap(
  point_gaps[, !c("eligible_n_exposed", "eligible_n_reference")],
  boot_gaps, gap_keys
)

for (x in list(metric_summary, gap_summary)) {
  x[, `:=`(
    estimate_per_1000 = 1000 * estimate,
    bootstrap_mean_per_1000 = 1000 * bootstrap_mean,
    bootstrap_se_per_1000 = 1000 * bootstrap_se,
    conf_low_per_1000 = 1000 * conf_low,
    conf_high_per_1000 = 1000 * conf_high,
    bias_per_1000 = 1000 * bias
  )]
}

qa <- rbindlist(list(
  data.table(
    check = "successful_bootstrap_replicates",
    value = uniqueN(boot_gaps$rep),
    expected = n_boot,
    pass = uniqueN(boot_gaps$rep) == n_boot
  ),
  data.table(
    check = "metrics_with_2000_replicates",
    value = min(metric_summary$n_boot),
    expected = n_boot,
    pass = all(metric_summary$n_boot == n_boot)
  ),
  data.table(
    check = "gaps_with_2000_replicates",
    value = min(gap_summary$n_boot),
    expected = n_boot,
    pass = all(gap_summary$n_boot == n_boot)
  ),
  data.table(
    check = "finite_positive_ipcw",
    value = min(success_diagnostics$ipcw_min),
    expected = "> 0",
    pass = all(is.finite(success_diagnostics$ipcw_min)) &&
      all(success_diagnostics$ipcw_min > 0)
  ),
  data.table(
    check = "absolute_probabilities_in_0_1",
    value = paste(range(boot_metrics$estimate), collapse = " to "),
    expected = "0 to 1",
    pass = all(boot_metrics$estimate >= 0 & boot_metrics$estimate <= 1)
  )
), fill = TRUE)

fwrite(metric_summary, file.path(
  out_dir, "36_CHARLS_equity_interaction_metrics_absolute.csv"
))
fwrite(gap_summary, file.path(
  out_dir, "37_CHARLS_equity_interaction_gaps_per_1000.csv"
))
fwrite(boot_metrics, file.path(
  out_dir, "38_CHARLS_equity_interaction_metrics_distribution.csv.gz"
))
fwrite(boot_gaps, file.path(
  out_dir, "39_CHARLS_equity_interaction_gaps_distribution.csv.gz"
))
fwrite(attempt_diagnostics, file.path(
  out_dir, "40_CHARLS_equity_interaction_attempt_diagnostics.csv"
))
fwrite(success_diagnostics, file.path(
  out_dir, "41_CHARLS_equity_interaction_success_diagnostics.csv"
))
fwrite(qa, file.path(
  out_dir, "42_CHARLS_equity_interaction_QA.csv"
))

finished_at <- Sys.time()
log_lines <- c(
  "CHARLS pre-specified health-equity interaction bootstrap",
  sprintf("Started: %s", started_at),
  sprintf("Finished: %s", finished_at),
  sprintf("Elapsed minutes: %.2f",
          as.numeric(difftime(finished_at, started_at, units = "mins"))),
  sprintf("Requested successful replicates: %d", n_boot),
  sprintf("Successful replicates: %d", n_success),
  sprintf("Attempts: %d", n_attempted),
  sprintf("Failures: %d", n_attempted - n_success),
  sprintf("Workers: %d", n_workers),
  sprintf("Participants in sampling frame: %d", length(bootstrap_ids)),
  "Sampling unit: participant ID; all transitions retained",
  "IPCW: denominator and numerator re-estimated in every replicate",
  "IPCW trimming: replicate-specific weighted 1st/99th percentiles",
  "Models: separate current state x period x age/residence/education",
  "Period reported: expansion; prediction horizon standardized to 2 years",
  "Uncertainty: percentile 2.5th-97.5th bootstrap interval"
)
writeLines(log_lines, file.path(
  out_dir, "43_CHARLS_equity_interaction_log.txt"
))

if (!all(qa$pass)) stop("One or more equity bootstrap QA checks failed")
cat("Equity interaction bootstrap completed successfully.\n")
