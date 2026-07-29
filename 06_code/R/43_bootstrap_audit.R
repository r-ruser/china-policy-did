#!/usr/bin/env Rscript
# V5.2: Bootstrap estimation consistency audit
suppressPackageStartupMessages({library(haven); library(data.table); library(nnet)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
cat("=== V5.2 Bootstrap Consistency Audit ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. Estimand specification
# ============================================================
cat("[1] Estimand specification...\n")

fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, age_at_wave, female, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)

trans <- dt[, .(wave_from=wave[1:(.N-1)], wave_to=wave[2:.N],
  state_from=state[1:(.N-1)], state_to=state[2:.N],
  age_from=age_at_wave[1:(.N-1)], female=female[1:(.N-1)]), by=ID]
trans <- trans[!is.na(state_to)]
trans[, period := fifelse(wave_to <= 2015, 0L, 1L)]
trans[, interval_years := as.numeric(wave_to - wave_from)]
trans[, state_from_f := factor(state_from, levels = 1:3)]
trans[, state_to_f := factor(state_to, levels = 1:3)]

new_2yr <- expand.grid(state_from_f = factor(1:3, levels = 1:3),
                        period = c(0L, 1L), interval_years = 2)

cat("Analysis data: CHARLS_wave_specific_continuous_FI.csv\n")
cat("N records:", nrow(trans), "\n")
cat("N subjects:", uniqueN(trans$ID), "\n")
cat("Start state 1:", sum(trans$state_from == 1), "\n")
cat("Start state 2:", sum(trans$state_from == 2), "\n")
cat("Start state 3:", sum(trans$state_from == 3), "\n")
cat("Model: state_to ~ state_from * period + interval_years\n")
cat("Period: 0=pre (2011-2015), 1=expansion (2015-2018)\n")
cat("interval_years: 2 for both scenarios\n")
cat("Standardisation target: age_c=0, female=0.5\n")

# ============================================================
# 2. Reference point estimates (unique model)
# ============================================================
cat("\n[2] Reference point estimates...\n")

fit_and_predict <- function(data, target) {
  m <- multinom(state_to_f ~ state_from_f * period + interval_years,
                data = data, trace = FALSE, maxit = 500)
  pred <- predict(m, newdata = target, type = "probs")
  list(model = m, pred = pred)
}

result_ref <- fit_and_predict(trans, new_2yr)
pred_ref <- result_ref$pred

ref_estimates <- data.table()
for (from_s in 1:3) {
  for (to_s in 1:3) {
    pre <- pred_ref[from_s, to_s]
    exp <- pred_ref[from_s + 3, to_s]
    ref_estimates <- rbind(ref_estimates, data.table(
      from = from_s, to = to_s,
      pre = round(pre, 6), expansion = round(exp, 6),
      diff = round(exp - pre, 6)))
  }
}
fwrite(ref_estimates, file.path(root, "CHARLS_bootstrap_reference_point_estimates.csv"))
cat("Reference estimates:\n"); print(ref_estimates)

# ============================================================
# 3. Identity test (no resampling)
# ============================================================
cat("\n[3] Identity test (no resampling)...\n")

# Create "bootstrap sample" with no resampling (each participant once)
set.seed(20260728)
unique_ids <- unique(trans$ID)
identity_ids <- sample(unique_ids)  # Random order, no replacement
identity_data <- rbindlist(lapply(identity_ids, function(id) trans[ID == id]))

result_identity <- fit_and_predict(identity_data, new_2yr)
pred_identity <- result_identity$pred

identity_test <- data.table()
for (from_s in 1:3) {
  for (to_s in 1:3) {
    ref_diff <- ref_estimates[from == from_s & to == to_s]$diff
    id_diff <- pred_identity[from_s + 3, to_s] - pred_identity[from_s, to_s]
    identity_test <- rbind(identity_test, data.table(
      from = from_s, to = to_s,
      ref_diff = round(ref_diff, 6),
      identity_diff = round(id_diff, 6),
      abs_diff = round(abs(ref_diff - id_diff), 10),
      pass = abs(ref_diff - id_diff) < 1e-6))
  }
}
fwrite(identity_test, file.path(root, "CHARLS_bootstrap_identity_test.csv"))
cat("Identity test:\n"); print(identity_test)
cat("All pass:", all(identity_test$pass), "\n")

# ============================================================
# 4. Test a single bootstrap replication
# ============================================================
cat("\n[4] Single bootstrap replication test...\n")

# Use the same function as bootstrap worker
set.seed(42)
sampled_ids <- sample(unique_ids, size = length(unique_ids), replace = TRUE)
boot_list <- list()
for (i in seq_along(sampled_ids)) {
  sub <- trans[ID == sampled_ids[i]]
  boot_list[[i]] <- sub
}
boot_data <- rbindlist(boot_list)

result_boot <- fit_and_predict(boot_data, new_2yr)
pred_boot <- result_boot$pred

boot_single <- data.table()
for (from_s in 1:3) {
  for (to_s in 1:3) {
    ref_diff <- ref_estimates[from == from_s & to == to_s]$diff
    boot_diff <- pred_boot[from_s + 3, to_s] - pred_boot[from_s, to_s]
    boot_single <- rbind(boot_single, data.table(
      from = from_s, to = to_s,
      ref_diff = round(ref_diff, 6),
      boot_diff = round(boot_diff, 6),
      diff_from_ref = round(boot_diff - ref_diff, 6)))
  }
}
cat("Single bootstrap vs reference:\n"); print(boot_single)

# ============================================================
# 5. Check bootstrap results for bias
# ============================================================
cat("\n[5] Bootstrap bias diagnostics...\n")

boot_results <- fread(file.path(root, "CHARLS_cluster_bootstrap_500_probability_distributions.csv"))

# For each transition, compute bootstrap stats
bias_dt <- data.table()
for (from_s in 1:3) {
  for (to_s in 1:3) {
    # Get pre and expansion probabilities for this transition
    pre_boot <- boot_results[start == from_s & period == 0][[paste0("p_", c("low","mid","high")[to_s])]]
    exp_boot <- boot_results[start == from_s & period == 1][[paste0("p_", c("low","mid","high")[to_s])]]
    diff_boot <- exp_boot - pre_boot

    ref_diff <- ref_estimates[from == from_s & to == to_s]$diff

    bias_dt <- rbind(bias_dt, data.table(
      from = from_s, to = to_s,
      original = round(ref_diff, 4),
      boot_mean = round(mean(diff_boot), 4),
      boot_median = round(median(diff_boot), 4),
      bias = round(mean(diff_boot) - ref_diff, 4),
      bias_pct = round(100 * (mean(diff_boot) - ref_diff) / abs(ref_diff), 1),
      p25 = round(quantile(diff_boot, 0.25), 4),
      p975 = round(quantile(diff_boot, 0.975), 4),
      in_ci = ref_diff >= quantile(diff_boot, 0.025) & ref_diff <= quantile(diff_boot, 0.975)
    ))
  }
}
fwrite(bias_dt, file.path(root, "CHARLS_bootstrap_bias_diagnostics.csv"))
cat("Bias diagnostics:\n"); print(bias_dt)

# ============================================================
# 6. Check probability constraints
# ============================================================
cat("\n[6] Probability constraint audit...\n")

constraint_dt <- data.table()
for (from_s in 1:3) {
  for (period_val in 0:1) {
    probs <- boot_results[start == from_s & period == period_val]
    prob_sum <- probs$p_low + probs$p_mid + probs$p_high
    constraint_dt <- rbind(constraint_dt, data.table(
      start = from_s, period = period_val,
      mean_sum = round(mean(prob_sum), 6),
      max_deviation = round(max(abs(prob_sum - 1)), 10),
      all_valid = all(prob_sum > 0.999 & prob_sum < 1.001)
    ))
  }
}
fwrite(constraint_dt, file.path(root, "CHARLS_bootstrap_probability_constraint_audit.csv"))
cat("Probability constraints:\n"); print(constraint_dt)

# Check difference constraints
cat("\nDifference constraints (diff sum = 0):\n")
for (from_s in 1:3) {
  diffs <- boot_results[start == from_s & period == 1]
  diff_sum <- diffs$p_low - boot_results[start == from_s & period == 0]$p_low +
              diffs$p_mid - boot_results[start == from_s & period == 0]$p_mid +
              diffs$p_high - boot_results[start == from_s & period == 0]$p_high
  cat("  Start", from_s, ": mean diff sum =", round(mean(diff_sum), 10), "\n")
}

cat("\nCompleted:", format(Sys.time()), "\n")
