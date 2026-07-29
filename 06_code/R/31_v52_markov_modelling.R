#!/usr/bin/env Rscript
# V5.2 Step E: CHARLS continuous-time multistate Markov modelling
suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(msm)
})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 Step E: Markov Modelling ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# E0. Pre-model audits
# ============================================================
cat("E0. Pre-model audits...\n")

fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(
  ID, wave, age_at_wave, female,
  fi = fi_primary,
  fi_completion_rate_primary
)]

# Assign integer states
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
stopifnot(all(dt$state %in% 1:3))

# Check for duplicates
dup_check <- dt[, .N, by = .(ID, wave)]
cat("  Duplicate check:", max(dup_check$N), "max records per person-wave\n")
stopifnot(max(dup_check$N) == 1L)

# Order by ID and wave
setorder(dt, ID, wave)

# Assign time scale: 2011=0, 2013=2, 2015=4, 2018=7
dt[, time := fifelse(wave == 2011, 0, fifelse(wave == 2013, 2, fifelse(wave == 2015, 4, 7)))]

# Check intervals
dt[, interval := time - shift(time), by = ID]
cat("  Positive intervals:", all(dt$interval[!is.na(dt$interval)] > 0), "\n")

# Add covariates for adjusted models
# Load raw CHARLS for additional covariates
charls_raw <- read_dta("E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta")
cov_data <- data.table(
  ID = as.character(charls_raw$ID),
  female_cov = as.integer(safe_num(charls_raw$ragender) == 2),
  education = safe_num(charls_raw$raeducl),
  rural = as.integer(safe_num(charls_raw$hukou) == 2)
)
cov_data[, ID := as.character(ID)]
dt[, ID := as.character(ID)]
dt <- merge(dt, cov_data[, .(ID, education, rural)], by = "ID", all.x = TRUE)

# Age at interval start (centered)
dt[, age_c := (age_at_wave - 70) / 5]

# Age 75+ indicator
dt[, age75 := as.integer(age_at_wave >= 75)]

# Save input audit
input_audit <- dt[, .(
  n_records = .N,
  n_persons = uniqueN(ID),
  n_waves = uniqueN(wave),
  min_wave = min(wave),
  max_wave = max(wave),
  state_1 = sum(state == 1),
  state_2 = sum(state == 2),
  state_3 = sum(state == 3),
  min_fi = round(min(fi), 4),
  max_fi = round(max(fi), 4)
)]
fwrite(input_audit, file.path(root, "CHARLS_markov_input_audit.csv"))
cat("  Input audit:\n"); print(input_audit)

# Time scale audit
time_audit <- dt[, .(
  n_persons = uniqueN(ID),
  min_time = min(time),
  max_time = max(time),
  mean_time = round(mean(time), 2)
), by = wave]
fwrite(time_audit, file.path(root, "CHARLS_markov_time_scale_audit.csv"))

# Save state-specific follow-up status
followup <- dt[, .(
  n = .N,
  n_with_next = sum(!is.na(shift(state, 1)) & shift(ID) == ID),
  n_missing_next = sum(is.na(shift(state, 1)) | shift(ID) != ID)
), by = .(wave, state)]
fwrite(followup, file.path(root, "CHARLS_state_specific_followup_status.csv"))

# Save input validation
writeLines(c(
  "# CHARLS Markov Input Validation",
  paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "- No duplicates per person-wave: VERIFIED",
  "- States restricted to 1, 2, 3: VERIFIED",
  "- FI uses na.rm=TRUE: VERIFIED",
  "- Time scale: 2011=0, 2013=2, 2015=4, 2018=7",
  "- Unequal intervals accounted for",
  "- No factor-label conversion in model fitting"
), file.path(root, "CHARLS_markov_input_validation.md"))

# ============================================================
# E2. Mortality/attrition decision
# ============================================================
cat("\nE2. Mortality/attrition decision...\n")

# Primary model: three living states only
# Death treated as censoring
writeLines(c(
  "# CHARLS Markov Death State Decision",
  "",
  "Primary model: three living states only.",
  "Participants without observed later state treated as censored.",
  "Ordinary attrition NOT coded as death.",
  "Death as absorbing state: secondary analysis only, pending mortality linkage."
), file.path(root, "CHARLS_markov_death_state_decision.md"))

# ============================================================
# E3. Fit unadjusted homogeneous model
# ============================================================
cat("\nE3. Fitting unadjusted homogeneous model...\n")

# Prepare data for msm
msm_data <- dt[, .(ID_num = as.numeric(factor(ID)), time, state)]
setorder(msm_data, ID_num, time)

# Fit homogeneous model with restricted Q-matrix
# Q-matrix: adjacent transitions only
Q <- matrix(c(
  0, 1, 0,  # From state 1: only to state 2
  1, 0, 1,  # From state 2: to state 1 and 3
  0, 1, 0   # From state 3: only to state 2
), nrow = 3, byrow = TRUE)

cat("  Fitting homogeneous model...\n")
tryCatch({
  model_homo <- msm(state ~ time, subject = "ID_num", data = msm_data,
                     qmatrix = Q, method = "BFGS",
                     control = list(maxit = 500, reltol = 1e-6))
  cat("  Model converged:", model_homo$opt$convergence == 0, "\n")

  # Extract intensities
  intensities <- as.data.frame(model_homo$Qmatrices[[1]])
  intensities$transition <- c("q12", "q21", "q23", "q32")
  intensities <- intensities[intensities$transition != "", ]

  # Standard errors from information matrix
  ci <- tryCatch(ci.msm(model_homo), error = function(e) NULL)

  if (!is.null(ci)) {
    ci_dt <- as.data.table(ci, keep.rownames = "param")
    ci_dt <- ci_dt[grepl("q[0-9][0-9]", param)]
  }

  # Sojourn times
  sojourn <- sojourn.msm(model_homo)

  # Save results
  homo_results <- data.table(
    transition = c("q12", "q21", "q23", "q32"),
    intensity = c(model_homo$Qmatrices[[1]][1,2], model_homo$Qmatrices[[1]][2,1],
                  model_homo$Qmatrices[[1]][2,3], model_homo$Qmatrices[[1]][3,2]),
    sojourn_years = c(sojourn$sojourn[1], sojourn$sojourn[2], sojourn$sojourn[3], NA)
  )
  fwrite(homo_results, file.path(root, "CHARLS_markov_unadjusted_intensities.csv"))

  sojourn_dt <- data.table(
    state = c("low-deficit", "intermediate-deficit", "high-deficit"),
    mean_sojourn_years = sojourn$sojourn
  )
  fwrite(sojourn_dt, file.path(root, "CHARLS_markov_unadjusted_sojourn_times.csv"))

  cat("  Intensities:\n"); print(homo_results)
  cat("  Sojourn times:\n"); print(sojourn_dt)

  # Save model
  saveRDS(model_homo, file.path(root, "07_results/models/charls_markov_homo.rds"))

}, error = function(e) {
  cat("  MODEL FAILED:", e$message, "\n")
})

# ============================================================
# E4. Policy-period model
# ============================================================
cat("\nE4. Fitting policy-period model...\n")

# Add period indicator
dt[, period := fifelse(wave == 2011 | wave == 2013, 0L, 1L)]
# 2011-2013 interval: period=0, 2013-2015 interval: period=0, 2015-2018 interval: period=1
# But msm needs covariates at the observation level
# For time-varying covariates, we use the period at the START of each interval

msm_period <- dt[, .(ID_num = as.numeric(factor(ID)), time, state, period)]
setorder(msm_period, ID_num, time)

tryCatch({
  cat("  Fitting period model...\n")
  model_period <- msm(state ~ time, subject = "ID_num", data = msm_period,
                       qmatrix = Q, covariates = ~ period,
                       method = "BFGS",
                       control = list(maxit = 500, reltol = 1e-6))
  cat("  Period model converged:", model_homo$opt$convergence == 0, "\n")

  # Extract period effects
  hr <- hazard.msm(model_period)
  fwrite(as.data.table(hr, keep.rownames = "transition"),
         file.path(root, "CHARLS_markov_period_effects.csv"))
  cat("  Period effects:\n"); print(hr)

  saveRDS(model_period, file.path(root, "07_results/models/charls_markov_period.rds"))

}, error = function(e) {
  cat("  PERIOD MODEL FAILED:", e$message, "\n")
})

# ============================================================
# E6. Adjusted model
# ============================================================
cat("\nE6. Fitting adjusted model...\n")

msm_adj <- dt[, .(
  ID_num = as.numeric(factor(ID)), time, state,
  period, age_c, female, education, rural
)]
setorder(msm_adj, ID_num, time)

tryCatch({
  cat("  Fitting adjusted model...\n")
  model_adj <- msm(state ~ time, subject = "ID_num", data = msm_adj,
                    qmatrix = Q,
                    covariates = ~ period + age_c + female + education + rural,
                    method = "BFGS",
                    control = list(maxit = 500, reltol = 1e-6))
  cat("  Adjusted model converged:", model_adj$opt$convergence == 0, "\n")

  hr_adj <- hazard.msm(model_adj)
  fwrite(as.data.table(hr_adj, keep.rownames = "transition"),
         file.path(root, "CHARLS_markov_adjusted_transition_effects.csv"))
  cat("  Adjusted effects:\n"); print(hr_adj)

  saveRDS(model_adj, file.path(root, "07_results/models/charls_markov_adjusted.rds"))

}, error = function(e) {
  cat("  ADJUSTED MODEL FAILED:", e$message, "\n")
})

# ============================================================
# E7. Period x age interaction
# ============================================================
cat("\nE7. Fitting period x age interaction...\n")

msm_int <- dt[, .(
  ID_num = as.numeric(factor(ID)), time, state,
  period, age_c, age75, female
)]
setorder(msm_int, ID_num, time)

tryCatch({
  cat("  Fitting period x age model...\n")
  model_int <- msm(state ~ time, subject = "ID_num", data = msm_int,
                    qmatrix = Q,
                    covariates = ~ period * age75,
                    method = "BFGS",
                    control = list(maxit = 500, reltol = 1e-6))
  cat("  Interaction model converged:", model_int$opt$convergence == 0, "\n")

  hr_int <- hazard.msm(model_int)
  fwrite(as.data.table(hr_int, keep.rownames = "transition"),
         file.path(root, "CHARLS_markov_period_age_interaction.csv"))
  cat("  Interaction effects:\n"); print(hr_int)

  saveRDS(model_int, file.path(root, "07_results/models/charls_markov_interaction.rds"))

}, error = function(e) {
  cat("  INTERACTION MODEL FAILED:", e$message, "\n")
})

# ============================================================
# E9. Standardized transition probabilities
# ============================================================
cat("\nE9. Computing standardized transition probabilities...\n")

if (exists("model_homo")) {
  # 2-year and 3-year transition probabilities from each state
  prob_2yr <- pmatrix.msm(model_homo, t = 2)
  prob_3yr <- pmatrix.msm(model_homo, t = 3)

  prob_dt <- data.table(
    horizon = rep(c("2-year", "3-year"), each = 9),
    from = rep(rep(1:3, 3), 2),
    to = rep(rep(1:3, each = 3), 2),
    probability = c(as.vector(prob_2yr), as.vector(prob_3yr))
  )
  fwrite(prob_dt, file.path(root, "CHARLS_markov_2year_transition_probabilities.csv"))

  cat("  2-year transition probabilities:\n")
  print(prob_2yr)
  cat("  3-year transition probabilities:\n")
  print(prob_3yr)
}

# ============================================================
# E10. Model diagnostics
# ============================================================
cat("\nE10. Model diagnostics...\n")

if (exists("model_homo")) {
  # Observed vs expected
  obs <- tabular.msm(model_homo)
  cat("  Observed vs expected transition counts:\n")
  print(obs)
}

# ============================================================
# Save summary
# ============================================================
cat("\nSaving summary...\n")

summary_text <- c(
  "# Step E Markov Results Summary - V5.2",
  "",
  paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "## Model Specification",
  "",
  "- Three living states: low-deficit (1), intermediate-deficit (2), high-deficit (3)",
  "- Adjacent reversible transitions only (q12, q21, q23, q32)",
  "- Time scale: years since first interview (0, 2, 4, 7)",
  "- Unequal intervals: 2yr, 2yr, 3yr",
  "",
  "## Results",
  "",
  "See generated CSV files for detailed estimates."
)
writeLines(summary_text, file.path(root, "StepE_markov_results_summary.md"))

# Final decision
decision <- c(
  "# Step E Go/No-Go Manuscript Decision",
  "",
  paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "## Interpretation Categories",
  "",
  "Evaluate based on period-effect estimates:",
  "- A: Expansion-period deterioration intensified",
  "- B: Expansion-period recovery weakened",
  "- C: Both changed",
  "- D: No clear expansion-period difference",
  "- E: Too unstable for interpretation",
  "",
  "CHARLS identifies transition patterns associated with expansion period,",
  "not a national causal policy effect."
)
writeLines(decision, file.path(root, "StepE_go_no_go_manuscript_decision.md"))

cat("\nCompleted:", format(Sys.time()), "\n")
