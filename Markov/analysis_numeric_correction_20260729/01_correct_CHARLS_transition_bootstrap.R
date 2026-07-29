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
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
} else {
  normalizePath(getwd(), winslash = "/")
}
project_root <- normalizePath(
  Sys.getenv("MARKOV_PROJECT_ROOT", unset = file.path(script_dir, "..")),
  winslash = "/",
  mustWork = TRUE
)
out_dir <- file.path(project_root, "analysis_numeric_correction_20260729")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fi <- fread(file.path(project_root, "data", "CHARLS_wave_specific_continuous_FI.csv"))

flow <- data.table(
  step = c(
    "Source wave records",
    "Age >=65",
    "Valid FI (>=80% items)",
    "At least two valid waves",
    "Observed transitions"
  ),
  records = c(
    nrow(fi),
    nrow(fi[age_at_wave >= 65]),
    nrow(fi[age_at_wave >= 65 & fi_valid_80 == TRUE]),
    NA_integer_,
    NA_integer_
  ),
  participants = c(
    uniqueN(fi$ID),
    uniqueN(fi[age_at_wave >= 65]$ID),
    uniqueN(fi[age_at_wave >= 65 & fi_valid_80 == TRUE]$ID),
    NA_integer_,
    NA_integer_
  )
)

dt <- fi[
  age_at_wave >= 65 & fi_valid_80 == TRUE,
  .(
    ID = as.character(ID),
    wave = as.integer(wave),
    age_at_wave = as.numeric(age_at_wave),
    female = as.numeric(female),
    fi = as.numeric(fi_primary)
  )
]
dt[, state := fifelse(fi < 0.10, 1L, fifelse(fi < 0.25, 2L, 3L))]
setorder(dt, ID, wave)

eligible_ids <- dt[, .N, by = ID][N >= 2L, ID]
flow[step == "At least two valid waves", `:=`(
  records = nrow(dt[ID %in% eligible_ids]),
  participants = length(eligible_ids)
)]

# data.table's 1:(.N-1) is unsafe for .N == 1 because 1:0 yields c(1, 0).
# Restrict to participants with at least two valid waves before pairing records.
trans <- dt[ID %in% eligible_ids, {
  stopifnot(.N >= 2L)
  .(
    wave_from = wave[-.N],
    wave_to = wave[-1L],
    state_from = state[-.N],
    state_to = state[-1L],
    age_from = age_at_wave[-.N],
    female = female[-.N]
  )
}, by = ID]

trans[, interval_years := as.numeric(wave_to - wave_from)]
stopifnot(
  nrow(trans) == 11972L,
  uniqueN(trans$ID) == 5936L,
  all(trans$interval_years > 0)
)

trans[, period := fifelse(wave_to <= 2015L, 0L, 1L)]
trans[, age_c := (age_from - 70) / 5]
trans[, state_from_f := factor(state_from, levels = 1:3)]
trans[, state_to_f := factor(state_to, levels = 1:3)]

flow[step == "Observed transitions", `:=`(
  records = nrow(trans),
  participants = uniqueN(trans$ID)
)]
fwrite(flow, file.path(out_dir, "CHARLS_corrected_sample_flow.csv"))
saveRDS(trans, file.path(out_dir, "CHARLS_corrected_transition_dataset.rds"))

new_2yr <- expand.grid(
  state_from_f = factor(1:3, levels = 1:3),
  period = c(0L, 1L),
  interval_years = 2,
  age_c = 0,
  female = 0.5
)

model_formula <- state_to_f ~ state_from_f * period + interval_years + age_c + female
fit <- multinom(model_formula, data = trans, trace = FALSE, maxit = 300)
pred <- predict(fit, newdata = new_2yr, type = "probs")

point_rows <- list()
for (from_s in 1:3) {
  for (to_s in 1:3) {
    point_rows[[length(point_rows) + 1L]] <- data.table(
      from = from_s,
      to = to_s,
      pre = pred[from_s, to_s],
      expansion = pred[from_s + 3L, to_s],
      difference = pred[from_s + 3L, to_s] - pred[from_s, to_s]
    )
  }
}
point <- rbindlist(point_rows)
fwrite(point, file.path(out_dir, "CHARLS_corrected_point_estimates.csv"))

bootstrap_one <- function(rep_id, seed, transition_data, target, formula) {
  set.seed(seed)
  ids <- unique(transition_data$ID)
  sampled_index <- sample.int(length(ids), length(ids), replace = TRUE)
  cluster_frequency <- tabulate(sampled_index, nbins = length(ids))
  boot <- copy(transition_data)
  boot[, bootstrap_weight := cluster_frequency[match(ID, ids)]]
  boot <- boot[bootstrap_weight > 0]
  mod <- tryCatch(
    multinom(
      formula,
      data = boot,
      weights = bootstrap_weight,
      trace = FALSE,
      maxit = 300
    ),
    error = function(e) NULL
  )
  if (is.null(mod)) {
    return(data.table(rep = rep_id, converged = FALSE))
  }
  p <- tryCatch(predict(mod, newdata = target, type = "probs"), error = function(e) NULL)
  if (is.null(p) || any(!is.finite(p)) || any(p < 0) || any(p > 1)) {
    return(data.table(rep = rep_id, converged = FALSE))
  }
  ans <- list()
  for (from_s in 1:3) {
    for (to_s in 1:3) {
      ans[[length(ans) + 1L]] <- data.table(
        rep = rep_id,
        from = from_s,
        to = to_s,
        pre = p[from_s, to_s],
        expansion = p[from_s + 3L, to_s],
        difference = p[from_s + 3L, to_s] - p[from_s, to_s],
        converged = TRUE
      )
    }
  }
  rbindlist(ans)
}

n_boot <- 2000L
n_workers <- max(1L, detectCores(logical = TRUE))
set.seed(20260729)
seeds <- sample.int(.Machine$integer.max, n_boot)

cl <- makeCluster(n_workers, type = "PSOCK")
on.exit(stopCluster(cl), add = TRUE)
clusterEvalQ(cl, {
  Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
  library(data.table)
  library(nnet)
  NULL
})
clusterExport(
  cl,
  c("trans", "new_2yr", "model_formula", "bootstrap_one", "seeds"),
  envir = environment()
)

boot_list <- parLapplyLB(
  cl,
  seq_len(n_boot),
  function(i) bootstrap_one(i, seeds[i], trans, new_2yr, model_formula)
)
boot <- rbindlist(boot_list, fill = TRUE)
ok_reps <- unique(boot[converged == TRUE, rep])
stopifnot(length(ok_reps) == n_boot)

fwrite(
  boot[converged == TRUE],
  file.path(out_dir, "CHARLS_corrected_bootstrap_2000_distribution.csv")
)

summary <- boot[converged == TRUE, .(
  n_boot = uniqueN(rep),
  bootstrap_mean = mean(difference),
  bootstrap_median = median(difference),
  bootstrap_se = sd(difference),
  ci_low = quantile(difference, 0.025),
  ci_high = quantile(difference, 0.975),
  bias = mean(difference)
), by = .(from, to)]
summary <- merge(point, summary, by = c("from", "to"))
summary[, bias := bootstrap_mean - difference]
fwrite(summary, file.path(out_dir, "CHARLS_corrected_bootstrap_2000_summary.csv"))

prevalence_2018 <- dt[wave == 2018L, .(
  n = .N,
  low = sum(state == 1L),
  intermediate = sum(state == 2L),
  high = sum(state == 3L),
  low_pct = 100 * mean(state == 1L),
  intermediate_pct = 100 * mean(state == 2L),
  high_pct = 100 * mean(state == 3L)
)]
fwrite(prevalence_2018, file.path(out_dir, "CHARLS_2018_state_prevalence.csv"))

metadata <- c(
  paste("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("R version:", R.version.string),
  paste("Workers:", n_workers),
  paste("Bootstrap seed:", 20260729),
  paste("Successful replications:", length(ok_reps)),
  "Formula: next state ~ current state * period + interval years + age_c + female",
  "Standardization: 2 years, age 70 (age_c=0), female=0.5",
  paste("Transitions:", nrow(trans)),
  paste("Participants:", uniqueN(trans$ID))
)
writeLines(metadata, file.path(out_dir, "CHARLS_corrected_analysis_metadata.txt"))

print(flow)
print(summary)
