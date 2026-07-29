#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

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
analysis_dir <- file.path(project_root, "analysis_numeric_correction_20260729")
input_dir <- file.path(analysis_dir, "figure_input")
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

unaffected <- c(
  "r_corrected_main_results.csv",
  "r_cfps_event_study.csv",
  "CHARLS_state_ADL_validation_corrected.csv",
  "CLASS_concurrent_FI_ADL_PR.csv"
)
ok <- file.copy(
  file.path(project_root, "data", unaffected),
  file.path(input_dir, unaffected),
  overwrite = TRUE
)
stopifnot(all(ok))

point <- fread(file.path(analysis_dir, "CHARLS_corrected_point_estimates.csv"))
prob_long <- rbind(
  point[, .(period = "pre-expansion", from, to, probability = pre)],
  point[, .(period = "expansion", from, to, probability = expansion)]
)
prob_long[, destination := c("p_low", "p_mid", "p_high")[to]]
prob <- dcast(
  prob_long,
  period + from ~ destination,
  value.var = "probability"
)
setcolorder(prob, c("period", "from", "p_low", "p_mid", "p_high"))
fwrite(
  prob,
  file.path(input_dir, "CHARLS_common_2year_transition_probabilities.csv")
)

boot <- fread(file.path(analysis_dir, "CHARLS_corrected_bootstrap_2000_summary.csv"))
boot[, `:=`(
  original_diff = difference,
  ci_low = ci_low,
  ci_high = ci_high,
  bias_pct = fifelse(abs(difference) > 0, 100 * bias / abs(difference), NA_real_),
  in_ci = difference >= ci_low & difference <= ci_high
)]
fwrite(
  boot[, .(
    from,
    to,
    original_diff,
    bootstrap_mean,
    bootstrap_median,
    bootstrap_se,
    ci_low,
    ci_high,
    bias,
    bias_pct,
    in_ci,
    n_boot
  )],
  file.path(input_dir, "CHARLS_cluster_bootstrap_2000_corrected_results.csv")
)

cat("Prepared corrected figure inputs in", input_dir, "\n")
