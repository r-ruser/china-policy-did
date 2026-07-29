#!/usr/bin/env Rscript
# V5.2 Step E: Discrete-time multinomial transition model
suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(nnet)
})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 Step E: Discrete-Time Transition Model ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. Prepare transition data
# ============================================================
cat("[1] Preparing transition data...\n")

fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, age_at_wave, female, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)

# Create transition pairs
trans_data <- dt[, .(
  wave_from = wave[1:(.N-1)],
  wave_to = wave[2:.N],
  state_from = state[1:(.N-1)],
  state_to = state[2:.N],
  age_from = age_at_wave[1:(.N-1)],
  female = female[1:(.N-1)]
), by = ID]
trans_data <- trans_data[!is.na(state_to)]

# Add period indicator
trans_data[, period := fifelse(wave_to <= 2015, 0L, 1L)]
trans_data[, age75 := as.integer(age_from >= 75)]
trans_data[, age_c := (age_from - 70) / 5]
trans_data[, interval_years := wave_to - wave_from]

cat("  Transition records:", nrow(trans_data), "\n")
cat("  Unique subjects:", length(unique(trans_data$ID)), "\n")
cat("  Transitions by interval:\n")
print(trans_data[, .N, by = .(wave_from, wave_to)])

# ============================================================
# 2. Unadjusted model
# ============================================================
cat("\n[2] Fitting unadjusted model...\n")

# For multinomial: outcome is state_to (3 levels), predictor is state_from
# We use state_from as a factor to get transition-specific intercepts
trans_data[, state_from_f := factor(state_from, levels = 1:3)]
trans_data[, state_to_f := factor(state_to, levels = 1:3)]

# Model 1: Unadjusted (transition-specific intercepts only)
m1 <- multinom(state_to_f ~ state_from_f, data = trans_data, trace = FALSE)
cat("  Unadjusted model AIC:", AIC(m1), "\n")

# Extract coefficients
coef1 <- as.data.table(summary(m1)$coefficients, keep.rownames = "from_state")
se1 <- as.data.table(summary(m1)$standard.errors, keep.rownames = "from_state")
# Reference category is state 1 (low-deficit)
# Coefficients are log-odds relative to state 1

# Compute transition probabilities
probs1 <- predict(m1, type = "probs")
# Average transition probabilities by start state
avg_probs <- data.table(
  state_from = trans_data$state_from,
  probs1
)
avg_trans <- avg_probs[, .(
  p_stay = mean(.SD[["1"]]),
  p_to2 = mean(.SD[["2"]]),
  p_to3 = mean(.SD[["3"]])
), by = state_from]
setorder(avg_trans, state_from)

cat("  Average transition probabilities:\n")
print(avg_trans)

# ============================================================
# 3. Period model
# ============================================================
cat("\n[3] Fitting period model...\n")

m_period <- multinom(state_to_f ~ state_from_f + period, data = trans_data, trace = FALSE)
cat("  Period model AIC:", AIC(m_period), "\n")

# Extract period effects
coef_period <- as.data.table(summary(m_period)$coefficients, keep.rownames = "term")
se_period <- as.data.table(summary(m_period)$standard.errors, keep.rownames = "term")

# Compute period-specific transition probabilities
trans_data[, period_0 := 0]
trans_data[, period_1 := 1]

# Predict for period=0 (pre-expansion)
pred0 <- predict(m_period, newdata = trans_data[, .(state_from_f, period = 0)], type = "probs")
avg0 <- data.table(state_from = trans_data$state_from, pred0)
avg_trans0 <- avg0[, .(
  p_stay = mean(.SD[["1"]]),
  p_to2 = mean(.SD[["2"]]),
  p_to3 = mean(.SD[["3"]])
), by = state_from]
setorder(avg_trans0, state_from)

# Predict for period=1 (expansion)
pred1 <- predict(m_period, newdata = trans_data[, .(state_from_f, period = 1)], type = "probs")
avg1 <- data.table(state_from = trans_data$state_from, pred1)
avg_trans1 <- avg1[, .(
  p_stay = mean(.SD[["1"]]),
  p_to2 = mean(.SD[["2"]]),
  p_to3 = mean(.SD[["3"]])
), by = state_from]
setorder(avg_trans1, state_from)

cat("  Pre-expansion transition probabilities:\n")
print(avg_trans0)
cat("  Expansion-period transition probabilities:\n")
print(avg_trans1)

# Compute period effects from predicted probabilities
pred0 <- predict(m_period, newdata = data.frame(
  state_from_f = factor(1:3, levels = 1:3), period = 0), type = "probs")
pred1 <- predict(m_period, newdata = data.frame(
  state_from_f = factor(1:3, levels = 1:3), period = 1), type = "probs")

# Build period effects table
pe_list <- list()
for (from_s in 1:3) {
  for (to_s in 1:3) {
    pre <- pred0[from_s, to_s]
    exp <- pred1[from_s, to_s]
    pe_list[[length(pe_list)+1]] <- data.table(
      transition = paste0(from_s, "->", to_s),
      from_label = c("low","mid","high")[from_s],
      to_label = c("low","mid","high")[to_s],
      pre_expansion = round(pre, 4),
      expansion = round(exp, 4),
      abs_change = round(exp - pre, 4),
      pct_change = ifelse(pre > 0, round(100 * (exp - pre) / pre, 1), NA)
    )
  }
}
period_effects <- rbindlist(pe_list)
fwrite(period_effects, file.path(root, "CHARLS_markov_period_effects.csv"))
cat("  Period effects:\n"); print(period_effects)

# ============================================================
# 4. Adjusted model
# ============================================================
cat("\n[4] Fitting adjusted model...\n")

# Load covariates
charls_raw <- read_dta("E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta")
cov <- data.table(
  ID_cov = as.character(charls_raw$ID),
  education = safe_num(charls_raw$raeduc_c),
  rural = as.integer(safe_num(charls_raw$h1rural) == 1)
)
cov[is.na(education), education := 0]
cov[is.na(rural), rural := 0]
trans_data <- merge(trans_data, cov, by.x = "ID", by.y = "ID_cov", all.x = TRUE)

m_adj <- multinom(state_to_f ~ state_from_f + period + age_c + female + education + rural,
                  data = trans_data, trace = FALSE)
cat("  Adjusted model AIC:", AIC(m_adj), "\n")

coef_adj <- as.data.table(summary(m_adj)$coefficients, keep.rownames = "term")
fwrite(coef_adj, file.path(root, "CHARLS_markov_adjusted_transition_effects.csv"))
cat("  Adjusted coefficients:\n"); print(coef_adj)

# ============================================================
# 5. Period x age interaction
# ============================================================
cat("\n[5] Fitting period x age interaction...\n")

m_int <- multinom(state_to_f ~ state_from_f * age75, data = trans_data, trace = FALSE)
cat("  Interaction model AIC:", AIC(m_int), "\n")

coef_int <- as.data.table(summary(m_int)$coefficients, keep.rownames = "term")
fwrite(coef_int, file.path(root, "CHARLS_markov_period_age_interaction.csv"))

# ============================================================
# 6. Standardized transition probabilities
# ============================================================
cat("\n[6] Standardized transition probabilities...\n")

# 2-year transition probabilities (average across all start states)
# From the unadjusted model
prob_2yr <- data.table(
  from = rep(1:3, each = 3),
  to = rep(1:3, 3),
  probability = as.vector(colMeans(probs1))
)

fwrite(prob_2yr, file.path(root, "CHARLS_markov_2year_transition_probabilities.csv"))
cat("  2-year average transition probabilities:\n")
print(prob_2yr)

# ============================================================
# 7. Goodness of fit
# ============================================================
cat("\n[7] Goodness of fit...\n")

# Observed vs expected transition counts
observed <- table(trans_data$state_from, trans_data$state_to)
expected <- table(trans_data$state_from, predict(m1))

gof <- data.table(
  from = as.integer(rownames(observed)),
  to = rep(1:3, nrow(observed)),
  observed = as.vector(observed),
  expected = as.vector(round(expected, 1)),
  residual = as.vector(observed - round(expected, 1))
)
fwrite(gof, file.path(root, "CHARLS_markov_goodness_of_fit.csv"))
cat("  Goodness of fit:\n"); print(gof)

# ============================================================
# 8. Save results summary
# ============================================================
cat("\n[8] Saving summary...\n")

summary_lines <- c(
  "# Step E Markov Results Summary - V5.2",
  "",
  paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "## Model Specification",
  "",
  "- Discrete-time multinomial transition model",
  "- Three states: low-deficit (1), intermediate-deficit (2), high-deficit (3)",
  "- Adjacent transitions modelled through state-from effects",
  "- Time scale: survey intervals (2yr, 2yr, 3yr)",
  "",
  "## Unadjusted Results",
  paste("- AIC:", round(AIC(m1), 1)),
  paste("- Transitions modelled:", nrow(trans_data)),
  paste("- Subjects:", length(unique(trans_data$ID))),
  "",
  "## Period Effects",
  paste("- Pre-expansion vs expansion period"),
  paste("- Period effects estimated for each transition"),
  "",
  "## Adjusted Results",
  paste("- AIC:", round(AIC(m_adj), 1)),
  paste("- Covariates: age, sex, education, rural, period"),
  "",
  "## Goodness of Fit",
  paste("- Observed vs expected transition counts in goodness-of-fit table")
)
writeLines(summary_lines, file.path(root, "StepE_markov_results_summary.md"))

# Decision
decision_lines <- c(
  "# Step E Go/No-Go Manuscript Decision",
  "",
  paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "## Model Convergence",
  paste("- Unadjusted: AIC =", round(AIC(m1), 1)),
  paste("- Period: AIC =", round(AIC(m_period), 1)),
  paste("- Adjusted: AIC =", round(AIC(m_adj), 1)),
  "",
  "## Interpretation",
  "",
  "Evaluate period-effect estimates for expansion-period transition differences.",
  "CHARLS identifies transition patterns associated with expansion period,",
  "not a national causal policy effect.",
  "",
  "## Result Categories",
  "- A: Expansion-period deterioration intensified",
  "- B: Expansion-period recovery weakened",
  "- C: Both changed",
  "- D: No clear expansion-period difference",
  "- E: Too unstable for interpretation"
)
writeLines(decision_lines, file.path(root, "StepE_go_no_go_manuscript_decision.md"))

cat("\nCompleted:", format(Sys.time()), "\n")
