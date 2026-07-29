#!/usr/bin/env Rscript
# Aggregate bootstrap results from all workers
suppressPackageStartupMessages({library(data.table)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("=== Aggregating Bootstrap Results ===\n")

# Find all worker result files
output_dir <- file.path(root, "07_results", "bootstrap_checkpoints")
worker_files <- list.files(output_dir, pattern = "worker_.*_results\\.rds$", full.names = TRUE)
cat("Worker result files found:", length(worker_files), "\n")

if (length(worker_files) == 0) {
  cat("ERROR: No worker result files found\n")
  stop("No results to aggregate")
}

# Load and combine all results
all_results <- list()
for (f in worker_files) {
  worker_results <- readRDS(f)
  all_results[[length(all_results) + 1]] <- worker_results
  cat("  Loaded:", basename(f), "- ", nrow(worker_results), "rows\n")
}

all_boot <- rbindlist(all_results)
n_total <- uniqueN(all_boot[converged == TRUE, rep])
cat("\nTotal successful replications:", n_total, "\n")

if (n_total < 500) {
  cat("WARNING: Less than 500 successful replications\n")
  cat("  May need supplementary batches\n")
}

# Compute summary statistics
boot_ok <- unique(all_boot[converged == TRUE], by = "rep")
boot_summary <- boot_ok[, .(
  n_boot = .N,
  bootstrap_mean = round(mean(diff), 4),
  bootstrap_median = round(median(diff), 4),
  bootstrap_se = round(sd(diff), 4),
  ci_pct_low = round(quantile(diff, 0.025), 4),
  ci_pct_high = round(quantile(diff, 0.975), 4),
  ci_basic_low = round(quantile(diff, 0.025), 4),
  ci_basic_high = round(quantile(diff, 0.975), 4),
  empirical_p_low = round(mean(diff < 0), 4),
  empirical_p_high = round(mean(diff > 0), 4)
), by = .(from, to)]

# Add original point estimates
fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)
trans <- dt[, .(wave_from=wave[1:(.N-1)], wave_to=wave[2:.N],
  state_from=state[1:(.N-1)], state_to=state[2:.N]), by=ID]
trans <- trans[!is.na(state_to)]
trans[, period := fifelse(wave_to <= 2015, 0L, 1L)]
trans[, interval_years := as.numeric(wave_to - wave_from)]
trans[, state_from_f := factor(state_from, levels = 1:3)]
trans[, state_to_f := factor(state_to, levels = 1:3)]

suppressPackageStartupMessages({library(nnet)})
m_orig <- multinom(state_to_f ~ state_from_f * period + interval_years, data = trans, trace = FALSE)
new_2yr <- expand.grid(state_from_f = factor(1:3, levels = 1:3), period = c(0, 1), interval_years = 2)
pred_orig <- predict(m_orig, newdata = new_2yr, type = "probs")

orig_diffs <- data.table()
for (from_s in 1:3) for (to_s in 1:3) {
  orig_diffs <- rbind(orig_diffs, data.table(
    from = from_s, to = to_s,
    original_diff = round(pred_orig[from_s + 3, to_s] - pred_orig[from_s, to_s], 4)))
}

final_summary <- merge(orig_diffs, boot_summary, by = c("from", "to"))
fwrite(final_summary, file.path(root, "CHARLS_cluster_bootstrap_500_results.csv"))
cat("\nFinal 500-rep bootstrap results:\n")
print(final_summary)

# Save full distribution
fwrite(boot_ok, file.path(root, "CHARLS_cluster_bootstrap_500_probability_distributions.csv"))

# Worker summary
worker_summaries <- list()
for (f in worker_files) {
  worker_id <- sub(".*worker_(\\d+)_.*", "\\1", basename(f))
  wr <- readRDS(f)
  n_ok <- uniqueN(wr[converged == TRUE, rep])
  n_fail <- sum(!wr$converged)
  worker_summaries[[length(worker_summaries) + 1]] <- data.table(
    worker = worker_id, n_ok = n_ok, n_fail = n_fail)
}
fwrite(rbindlist(worker_summaries), file.path(root, "CHARLS_cluster_bootstrap_500_worker_summary.csv"))

cat("\nAggregation complete.\n")
cat("Total successful:", n_total, "\n")
