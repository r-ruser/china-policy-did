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
  normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("04_CHARLS_four_state_cluster_bootstrap_2000.R",
                winslash = "/", mustWork = TRUE)
}
out_dir <- dirname(script_path)

# Build the adjudicated transition data and the original IPCW model using the
# audited main analysis. This deliberately refreshes point-estimate inputs
# before the bootstrap begins.
source(file.path(out_dir, "01_unified_health_equity_analysis.R"),
       local = .GlobalEnv, encoding = "UTF-8")

n_boot <- as.integer(Sys.getenv("MARKOV_BOOTSTRAP_N", "2000"))
base_seed <- as.integer(Sys.getenv("MARKOV_BOOTSTRAP_SEED", "20260730"))
detected_cores <- parallel::detectCores(logical = TRUE)
default_workers <- min(24L, max(1L, detected_cores - 1L))
n_workers <- as.integer(Sys.getenv(
  "MARKOV_BOOTSTRAP_WORKERS", as.character(default_workers)
))
n_workers <- max(1L, min(n_workers, detected_cores))
max_attempts <- as.integer(Sys.getenv(
  "MARKOV_BOOTSTRAP_MAX_ATTEMPTS",
  as.character(max(4000L, 2L * n_boot))
))
stopifnot(n_boot > 0L, n_workers > 0L, max_attempts >= n_boot)

state_labels <- c("Low deficit", "Intermediate deficit",
                  "High deficit", "Death")
period_levels <- c("Pre-expansion", "Expansion")
dimension_specs <- data.table(
  dimension = c("Age", "Age", "Residence", "Residence",
                "Education", "Education", "Sex", "Sex"),
  level = c("65-74", "75+", "Urban/town", "Rural",
            "Middle school or higher", "Primary or less",
            "Male", "Female"),
  variable = c("age_group", "age_group", "rural_group", "rural_group",
               "education_group", "education_group", "sex_group", "sex_group")
)
contrast_specs <- data.table(
  dimension = c("Age", "Residence", "Education", "Sex"),
  exposed = c("75+", "Rural", "Primary or less", "Female"),
  reference = c("65-74", "Urban/town", "Middle school or higher", "Male")
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

weighted_col_means <- function(x, w) {
  as.numeric(crossprod(w, x) / sum(w))
}

predict_four_state <- function(model, target, target_weights, period_value,
                               from_value, dimension = "Overall",
                               level = "Overall", variable = NA_character_) {
  nd <- copy(target)
  nd[, policy_period := factor(period_value, levels = period_levels)]
  nd[, from_state_f := factor(from_value, levels = 1:3)]
  nd[, interval_years := 2]
  if (!is.na(variable)) {
    nd[[variable]] <- factor(level, levels = levels(target[[variable]]))
  }
  pp <- predict(model, newdata = nd, type = "probs")
  if (is.null(dim(pp))) pp <- matrix(pp, nrow = 1)
  if (is.null(colnames(pp))) colnames(pp) <- model$lev
  full <- matrix(0, nrow = nrow(pp), ncol = 4,
                 dimnames = list(NULL, as.character(1:4)))
  full[, colnames(pp)] <- pp
  p <- weighted_col_means(full, target_weights)
  data.table(
    period = period_value,
    horizon_years = 2L,
    dimension,
    level,
    from_state = from_value,
    to_state = 1:4,
    estimate = p
  )
}

all_standardized_probabilities <- function(model, target, target_weights) {
  ans <- vector("list", 2L * 3L * (1L + nrow(dimension_specs)))
  k <- 0L
  for (per in period_levels) {
    for (from in 1:3) {
      k <- k + 1L
      ans[[k]] <- predict_four_state(
        model, target, target_weights, per, from
      )
      for (j in seq_len(nrow(dimension_specs))) {
        ss <- dimension_specs[j]
        k <- k + 1L
        ans[[k]] <- predict_four_state(
          model, target, target_weights, per, from,
          ss$dimension, ss$level, ss$variable
        )
      }
    }
  }
  rbindlist(ans)
}

make_period_differences <- function(abs_dt) {
  pre <- abs_dt[period == "Pre-expansion"]
  exp <- abs_dt[period == "Expansion"]
  z <- merge(
    exp, pre,
    by = c("horizon_years", "dimension", "level",
           "from_state", "to_state"),
    suffixes = c("_expansion", "_pre")
  )
  z[, .(
    horizon_years, dimension, level, from_state, to_state,
    estimate = estimate_expansion - estimate_pre
  )]
}

make_equity_gaps <- function(abs_dt) {
  ans <- vector("list", nrow(contrast_specs))
  for (j in seq_len(nrow(contrast_specs))) {
    cc <- contrast_specs[j]
    exposed <- abs_dt[
      dimension == cc$dimension & level == cc$exposed
    ]
    reference <- abs_dt[
      dimension == cc$dimension & level == cc$reference
    ]
    z <- merge(
      exposed, reference,
      by = c("period", "horizon_years", "dimension",
             "from_state", "to_state"),
      suffixes = c("_exposed", "_reference")
    )
    ans[[j]] <- z[, .(
      period, horizon_years, dimension,
      contrast = paste0(level_exposed, " minus ", level_reference),
      from_state, to_state,
      estimate = estimate_exposed - estimate_reference
    )]
  }
  rbindlist(ans)
}

make_recovery_summary <- function(abs_dt) {
  long <- abs_dt[, .(
    recovery = sum(estimate[to_state < from_state]),
    deterioration = sum(estimate[to_state > from_state & to_state <= 3]),
    high_deficit = sum(estimate[to_state == 3]),
    death = sum(estimate[to_state == 4])
  ), by = .(period, horizon_years, dimension, level, from_state)]
  melt(
    long,
    id.vars = c("period", "horizon_years", "dimension",
                "level", "from_state"),
    variable.name = "metric",
    value.name = "estimate"
  )
}

adjust_matrix <- function(P, prevention = 0, recovery = 0) {
  Q <- P
  for (i in 1:3) {
    worse <- unique(c(which(seq_len(4) > i & seq_len(4) <= 3), 4))
    removed <- sum(Q[i, worse] * prevention)
    Q[i, worse] <- Q[i, worse] * (1 - prevention)
    Q[i, i] <- Q[i, i] + removed

    better <- which(seq_len(4) < i)
    if (length(better) && recovery > 0) {
      desired <- sum(Q[i, better] * recovery)
      actual <- min(desired, Q[i, i])
      if (sum(Q[i, better]) > 0) {
        Q[i, better] <- Q[i, better] +
          actual * Q[i, better] / sum(Q[i, better])
      }
      Q[i, i] <- Q[i, i] - actual
    }
    Q[i, ] <- Q[i, ] / sum(Q[i, ])
  }
  Q[4, ] <- c(0, 0, 0, 1)
  Q
}

matrix_power <- function(M, n) {
  ans <- diag(nrow(M))
  if (n == 0L) return(ans)
  for (i in seq_len(n)) ans <- ans %*% M
  ans
}

make_scenarios <- function(abs_dt, initial) {
  expansion <- abs_dt[
    period == "Expansion" & dimension == "Overall" & level == "Overall"
  ]
  P <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:3) {
    P[i, ] <- expansion[from_state == i][order(to_state)]$estimate
  }
  P[4, ] <- c(0, 0, 0, 1)
  P <- P / rowSums(P)

  defs <- list(
    Observed = c(prevention = 0, recovery = 0),
    `Prevention 20%` = c(prevention = 0.20, recovery = 0),
    `Recovery 20%` = c(prevention = 0, recovery = 0.20),
    `Combined 20%` = c(prevention = 0.20, recovery = 0.20)
  )
  ans <- list()
  k <- 0L
  for (sc in names(defs)) {
    pars <- defs[[sc]]
    Q <- adjust_matrix(P, pars["prevention"], pars["recovery"])
    if (any(abs(Q[4, ] - c(0, 0, 0, 1)) > 0)) {
      stop("death_state_is_not_absorbing")
    }
    for (cycles in c(1L, 3L, 5L)) {
      dist <- drop(initial %*% matrix_power(Q, cycles))
      k <- k + 1L
      ans[[k]] <- data.table(
        horizon_years = 2L * cycles,
        scenario = sc,
        low_deficit = dist[1],
        intermediate_deficit = dist[2],
        high_deficit = dist[3],
        death = dist[4],
        living = sum(dist[1:3]),
        high_need_among_living = ifelse(
          sum(dist[1:3]) > 0, dist[3] / sum(dist[1:3]), NA_real_
        )
      )
    }
  }
  wide <- rbindlist(ans)
  observed <- wide[scenario == "Observed", .(
    horizon_years,
    observed_high_deficit = high_deficit,
    observed_death = death
  )]
  wide <- merge(wide, observed, by = "horizon_years", all.x = TRUE)
  wide[, `:=`(
    high_deficit_difference_vs_observed =
      high_deficit - observed_high_deficit,
    death_difference_vs_observed = death - observed_death
  )]
  melt(
    wide[, !c("observed_high_deficit", "observed_death")],
    id.vars = c("horizon_years", "scenario"),
    variable.name = "metric",
    value.name = "estimate"
  )
}

initial_distribution <- function(id_frequency) {
  z <- merge(
    fi_2015_base,
    id_frequency,
    by = "ID",
    all = FALSE
  )
  counts <- z[, .(n = sum(boot_frequency)), by = state]
  v <- numeric(3)
  v[counts$state] <- counts$n
  if (!is.finite(sum(v)) || sum(v) <= 0) return(rep(NA_real_, 4))
  c(v / sum(v), 0)
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
      any(boot$p_den_boot <= 0) ||
      any(boot$p_num_boot <= 0)) {
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
  mod <- multinom(
    to_state_f ~ from_state_f * policy_period + interval_years +
      age_group + rural_group + education_group + sex_group,
    weights = boot_frequency * ipcw_boot,
    data = fit_dat, trace = FALSE, maxit = 1000
  )
  if (!is.null(mod$convergence) && mod$convergence != 0) {
    stop("multinom_nonconvergence")
  }

  abs_dt <- all_standardized_probabilities(
    mod, boot, boot$boot_frequency
  )
  row_check <- abs_dt[, .(total = sum(estimate)),
                      by = .(period, dimension, level, from_state)]
  if (any(!is.finite(abs_dt$estimate)) ||
      any(abs_dt$estimate < -1e-10) ||
      any(abs_dt$estimate > 1 + 1e-10) ||
      any(abs(row_check$total - 1) > 1e-8)) {
    stop("invalid_standardized_probability")
  }

  initial <- initial_distribution(sample_frequency)
  if (any(!is.finite(initial)) || abs(sum(initial) - 1) > 1e-8) {
    stop("invalid_initial_distribution")
  }

  list(
    absolute = abs_dt,
    period = make_period_differences(abs_dt),
    equity = make_equity_gaps(abs_dt),
    recovery = make_recovery_summary(abs_dt),
    scenario = make_scenarios(abs_dt, initial),
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
        failure_reason = result$error,
        elapsed_seconds = elapsed
      )
    ))
  }
  aid <- attempt_id
  seed_value <- seed
  for (nm in c("absolute", "period", "equity", "recovery", "scenario")) {
    result[[nm]][, attempt_id := aid]
  }
  result$diagnostics[, `:=`(
    attempt_id = aid,
    seed = seed_value,
    success = TRUE,
    failure_reason = NA_character_,
    elapsed_seconds = elapsed
  )]
  c(list(success = TRUE), result)
}

summarize_bootstrap <- function(point, boot, keys) {
  stats <- boot[, .(
    n_boot = uniqueN(rep),
    bootstrap_mean = mean(estimate),
    bootstrap_median = median(estimate),
    bootstrap_se = sd(estimate),
    conf_low = as.numeric(quantile(estimate, 0.025, names = FALSE)),
    conf_high = as.numeric(quantile(estimate, 0.975, names = FALSE))
  ), by = keys]
  ans <- merge(point, stats, by = keys, all.x = TRUE)
  ans[, bias := bootstrap_mean - estimate]
  ans[]
}

attach_labels <- function(x) {
  x[, `:=`(
    database = "CHARLS",
    from_state_label = state_labels[from_state],
    to_state_label = state_labels[to_state]
  )]
  x
}

# Master participant bootstrap frame and initial distribution source.
bootstrap_ids <- sort(unique(ipcw_dat$ID))
fi_2015_base <- fi[
  wave == 2015 & age_at_wave >= 65 & fi_valid_80 == TRUE,
  .(ID = as.character(ID), state)
]

# Point estimates under the same full-target standardization used in each
# bootstrap sample.
point_absolute <- all_standardized_probabilities(
  multistate_model, ipcw_dat, rep(1, nrow(ipcw_dat))
)
point_period <- make_period_differences(point_absolute)
point_equity <- make_equity_gaps(point_absolute)
point_recovery <- make_recovery_summary(point_absolute)
point_initial <- initial_distribution(data.table(
  ID = bootstrap_ids, boot_frequency = 1L
))
point_scenario <- make_scenarios(point_absolute, point_initial)

set.seed(base_seed)
attempt_seeds <- sample.int(.Machine$integer.max, max_attempts)

cl <- makeCluster(n_workers, type = "PSOCK")
on.exit(stopCluster(cl), add = TRUE)
clusterEvalQ(cl, {
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1"
  )
  suppressPackageStartupMessages({
    library(data.table)
    library(nnet)
  })
  NULL
})
clusterExport(
  cl,
  c(
    "ipcw_dat", "fi_2015_base", "bootstrap_ids", "attempt_seeds",
    "period_levels",
    "dimension_specs", "contrast_specs", "weighted_quantile",
    "weighted_col_means", "predict_four_state",
    "all_standardized_probabilities", "make_period_differences",
    "make_equity_gaps", "make_recovery_summary", "adjust_matrix",
    "matrix_power", "make_scenarios", "initial_distribution",
    "fit_one_sample", "bootstrap_one"
  ),
  envir = environment()
)

results <- list()
diagnostics <- list()
n_success <- 0L
n_attempted <- 0L
batch_size <- min(250L, n_boot)
started_at <- Sys.time()

while (n_success < n_boot && n_attempted < max_attempts) {
  needed <- n_boot - n_success
  this_batch <- min(batch_size, needed, max_attempts - n_attempted)
  attempt_ids <- n_attempted + seq_len(this_batch)
  batch <- parLapplyLB(
    cl,
    attempt_ids,
    function(i) bootstrap_one(i, attempt_seeds[i])
  )
  diagnostics <- c(diagnostics, lapply(batch, `[[`, "diagnostics"))
  good <- batch[vapply(batch, function(x) isTRUE(x$success), logical(1))]
  results <- c(results, good)
  n_success <- length(results)
  n_attempted <- n_attempted + this_batch
  cat(sprintf(
    "Bootstrap progress: %d/%d successful after %d attempts (%s)\n",
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
  for (nm in c("absolute", "period", "equity", "recovery", "scenario")) {
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

boot_absolute <- rbindlist(lapply(results, `[[`, "absolute"))
boot_period <- rbindlist(lapply(results, `[[`, "period"))
boot_equity <- rbindlist(lapply(results, `[[`, "equity"))
boot_recovery <- rbindlist(lapply(results, `[[`, "recovery"))
boot_scenario <- rbindlist(lapply(results, `[[`, "scenario"))
attempt_diagnostics <- rbindlist(diagnostics, fill = TRUE)
success_diagnostics <- rbindlist(lapply(results, `[[`, "diagnostics"), fill = TRUE)

absolute_summary <- summarize_bootstrap(
  point_absolute, boot_absolute,
  c("period", "horizon_years", "dimension", "level",
    "from_state", "to_state")
)
period_summary <- summarize_bootstrap(
  point_period, boot_period,
  c("horizon_years", "dimension", "level", "from_state", "to_state")
)
equity_summary <- summarize_bootstrap(
  point_equity, boot_equity,
  c("period", "horizon_years", "dimension", "contrast",
    "from_state", "to_state")
)
recovery_summary <- summarize_bootstrap(
  point_recovery, boot_recovery,
  c("period", "horizon_years", "dimension", "level",
    "from_state", "metric")
)
scenario_summary <- summarize_bootstrap(
  point_scenario, boot_scenario,
  c("horizon_years", "scenario", "metric")
)

# Reader-facing main outputs, now with 2,000-replication percentile intervals.
absolute_out <- attach_labels(copy(absolute_summary))
absolute_out[, `:=`(
  probability = estimate,
  events_per_1000 = 1000 * estimate,
  conf_low_per_1000 = 1000 * conf_low,
  conf_high_per_1000 = 1000 * conf_high
)]
setcolorder(absolute_out, c(
  "database", "period", "horizon_years", "dimension", "level",
  "from_state", "from_state_label", "to_state", "to_state_label",
  "probability", "bootstrap_mean", "bootstrap_median", "bootstrap_se",
  "bias", "conf_low", "conf_high", "events_per_1000",
  "conf_low_per_1000", "conf_high_per_1000", "n_boot"
))
fwrite(
  absolute_out,
  file.path(out_dir, "06_CHARLS_IPCW_absolute_transition_probabilities.csv")
)

equity_out <- attach_labels(copy(equity_summary))
equity_out[, `:=`(
  probability_difference = estimate,
  percentage_point_difference = 100 * estimate,
  conf_low_pp = 100 * conf_low,
  conf_high_pp = 100 * conf_high,
  events_per_1000_difference = 1000 * estimate,
  conf_low_per_1000 = 1000 * conf_low,
  conf_high_per_1000 = 1000 * conf_high,
  interval_excludes_zero = conf_low > 0 | conf_high < 0
)]
setcolorder(equity_out, c(
  "database", "period", "horizon_years", "dimension", "contrast",
  "from_state", "from_state_label", "to_state", "to_state_label",
  "probability_difference", "percentage_point_difference",
  "conf_low_pp", "conf_high_pp", "events_per_1000_difference",
  "conf_low_per_1000", "conf_high_per_1000",
  "interval_excludes_zero", "bootstrap_mean", "bootstrap_median",
  "bootstrap_se", "bias", "n_boot"
))
fwrite(
  equity_out,
  file.path(out_dir, "07_CHARLS_health_equity_transition_gaps.csv")
)

recovery_out <- copy(recovery_summary)
recovery_out[, `:=`(
  database = "CHARLS",
  from_state_label = state_labels[from_state],
  probability = estimate,
  events_per_1000 = 1000 * estimate,
  conf_low_per_1000 = 1000 * conf_low,
  conf_high_per_1000 = 1000 * conf_high
)]
setcolorder(recovery_out, c(
  "database", "period", "horizon_years", "dimension", "level",
  "from_state", "from_state_label", "metric", "probability",
  "bootstrap_mean", "bootstrap_median", "bootstrap_se", "bias",
  "conf_low", "conf_high", "events_per_1000",
  "conf_low_per_1000", "conf_high_per_1000", "n_boot"
))
fwrite(
  recovery_out,
  file.path(out_dir, "08_CHARLS_recovery_and_care_need_per_1000.csv")
)

scenario_out <- copy(scenario_summary)
scenario_out[, `:=`(
  probability = estimate,
  per_1000 = 1000 * estimate,
  conf_low_per_1000 = 1000 * conf_low,
  conf_high_per_1000 = 1000 * conf_high,
  interval_excludes_zero = fifelse(
    grepl("_difference_", metric),
    conf_low > 0 | conf_high < 0,
    NA
  )
)]
fwrite(
  scenario_out,
  file.path(out_dir, "09_care_need_scenario_simulation_per_1000.csv")
)

period_out <- attach_labels(copy(period_summary))
period_out[, `:=`(
  contrast = "Expansion minus pre-expansion",
  probability_difference = estimate,
  percentage_point_difference = 100 * estimate,
  conf_low_pp = 100 * conf_low,
  conf_high_pp = 100 * conf_high,
  events_per_1000_difference = 1000 * estimate,
  conf_low_per_1000 = 1000 * conf_low,
  conf_high_per_1000 = 1000 * conf_high,
  interval_excludes_zero = conf_low > 0 | conf_high < 0
)]
fwrite(
  period_out,
  file.path(out_dir, "25_CHARLS_period_transition_differences_bootstrap_2000.csv")
)

significance_summary <- rbindlist(list(
  period_out[, .(
    contrast_family = "Expansion minus pre-expansion",
    period = NA_character_,
    scenario = NA_character_,
    horizon_years = 2L,
    n_contrasts = .N,
    n_interval_excludes_zero = sum(interval_excludes_zero),
    share_interval_excludes_zero = mean(interval_excludes_zero)
  ), by = dimension],
  equity_out[, .(
    contrast_family = "Health-equity group difference",
    scenario = NA_character_,
    n_contrasts = .N,
    n_interval_excludes_zero = sum(interval_excludes_zero),
    share_interval_excludes_zero = mean(interval_excludes_zero)
  ), by = .(period, horizon_years, dimension)],
  scenario_out[
    grepl("_difference_", metric) & scenario != "Observed",
    .(
      contrast_family = "Scenario minus observed",
      period = NA_character_,
      n_contrasts = .N,
      n_interval_excludes_zero = sum(
        interval_excludes_zero, na.rm = TRUE
      ),
      share_interval_excludes_zero = mean(
        interval_excludes_zero, na.rm = TRUE
      )
    ),
    by = .(horizon_years, scenario, dimension = metric)
  ]
), fill = TRUE)
fwrite(
  significance_summary,
  file.path(out_dir, "33_CHARLS_bootstrap_2000_interval_summary.csv")
)

# Full distributions are compressed but remain directly auditable with fread().
fwrite(
  boot_absolute,
  file.path(out_dir, "26_CHARLS_bootstrap_2000_absolute_distribution.csv.gz"),
  compress = "gzip"
)
fwrite(
  rbindlist(list(
    boot_period[, contrast_type := "Period"],
    boot_equity[, contrast_type := "Equity"]
  ), fill = TRUE),
  file.path(out_dir, "27_CHARLS_bootstrap_2000_contrast_distribution.csv.gz"),
  compress = "gzip"
)
fwrite(
  boot_recovery,
  file.path(out_dir, "28_CHARLS_bootstrap_2000_recovery_distribution.csv.gz"),
  compress = "gzip"
)
fwrite(
  boot_scenario,
  file.path(out_dir, "29_CHARLS_bootstrap_2000_scenario_distribution.csv.gz"),
  compress = "gzip"
)
fwrite(
  attempt_diagnostics[order(attempt_id)],
  file.path(out_dir, "30_CHARLS_bootstrap_2000_attempt_diagnostics.csv")
)

# QA checks intentionally separate absolute-probability bounds from
# contrast-interval exclusion of zero.
abs_row_sums <- boot_absolute[, .(total = sum(estimate)),
                              by = .(rep, period, dimension, level, from_state)]
scenario_sums <- dcast(
  boot_scenario[
    metric %in% c("low_deficit", "intermediate_deficit",
                  "high_deficit", "death")
  ],
  rep + horizon_years + scenario ~ metric,
  value.var = "estimate"
)
scenario_sums[, total := low_deficit + intermediate_deficit +
                high_deficit + death]
qa <- data.table(
  check = c(
    "Exactly requested successful replications",
    "Every four-state probability row sums to one",
    "Death is an absorbing state in every scenario matrix",
    "All absolute transition probabilities are bounded",
    "All absolute confidence limits are bounded",
    "All scenario state vectors sum to one",
    "All successful IPCW values are finite and positive",
    "Every summary cell uses all successful replications",
    "Point estimates are finite",
    "Contrast significance is evaluated only against zero"
  ),
  passed = c(
    length(results) == n_boot,
    all(abs(abs_row_sums$total - 1) < 1e-8),
    TRUE,
    all(boot_absolute$estimate >= 0 & boot_absolute$estimate <= 1),
    all(absolute_summary$conf_low >= 0 & absolute_summary$conf_high <= 1),
    all(abs(scenario_sums$total - 1) < 1e-8),
    all(is.finite(success_diagnostics$ipcw_min) &
          success_diagnostics$ipcw_min > 0 &
          is.finite(success_diagnostics$ipcw_max)),
    all(c(
      absolute_summary$n_boot, period_summary$n_boot,
      equity_summary$n_boot, recovery_summary$n_boot,
      scenario_summary$n_boot
    ) == n_boot),
    all(is.finite(c(
      point_absolute$estimate, point_period$estimate,
      point_equity$estimate, point_recovery$estimate,
      point_scenario$estimate
    ))),
    all(is.logical(equity_out$interval_excludes_zero)) &&
      all(is.logical(period_out$interval_excludes_zero))
  ),
  detail = c(
    sprintf("%d successful after %d attempts", n_boot, n_attempted),
    sprintf("Maximum deviation %.3g", max(abs(abs_row_sums$total - 1))),
    "Every bootstrap scenario call stops unless row 4 equals (0,0,0,1)",
    sprintf("Range %.6f to %.6f",
            min(boot_absolute$estimate), max(boot_absolute$estimate)),
    sprintf("CI range %.6f to %.6f",
            min(absolute_summary$conf_low), max(absolute_summary$conf_high)),
    sprintf("Maximum deviation %.3g", max(abs(scenario_sums$total - 1))),
    sprintf("IPCW range %.6f to %.6f",
            min(success_diagnostics$ipcw_min),
            max(success_diagnostics$ipcw_max)),
    sprintf("All cells use n_boot=%d", n_boot),
    "Absolute, contrast, recovery, and scenario point estimates checked",
    "Absolute probabilities report bounds; only differences receive excludes-zero flags"
  )
)
fwrite(qa, file.path(out_dir, "31_CHARLS_bootstrap_2000_QA.csv"))
stopifnot(all(qa$passed))

failure_table <- attempt_diagnostics[success == FALSE, .N, by = failure_reason]
elapsed_minutes <- as.numeric(difftime(Sys.time(), started_at,
                                       units = "mins"))
log_lines <- c(
  "CHARLS four-state IPCW cluster bootstrap",
  paste("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("Requested successful replications:", n_boot),
  paste("Attempts:", n_attempted),
  paste("Successful replications:", n_success),
  paste("Failed attempts:", n_attempted - n_success),
  paste("Participants in sampling frame:", length(bootstrap_ids)),
  paste("Workers:", n_workers),
  paste("Base seed:", base_seed),
  paste("Elapsed minutes:", round(elapsed_minutes, 2)),
  paste("IPCW re-estimated in every replication:", TRUE),
  paste("IPCW trimming:", "replicate-specific weighted 1st/99th percentiles"),
  paste("Standardization target:", "all eligible baseline transition records in each bootstrap sample"),
  paste(
    "Confidence interval:",
    sprintf(
      "2.5th and 97.5th percentile of %s successful replications",
      format(n_boot, big.mark = ",", scientific = FALSE)
    )
  ),
  if (nrow(failure_table)) {
    paste("Failures:", paste(
      paste0(failure_table$failure_reason, "=", failure_table$N),
      collapse = "; "
    ))
  } else {
    "Failures: none"
  }
)
writeLines(
  log_lines,
  file.path(out_dir, "32_CHARLS_bootstrap_2000_log.txt"),
  useBytes = TRUE
)

cat(paste(log_lines, collapse = "\n"), "\n")
