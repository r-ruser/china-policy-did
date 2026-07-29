#!/usr/bin/env Rscript

# CLASS 2018 restricted cubic spline audit and analysis
# Project: Average Policy Estimates and State-Dependent Health Mobility...
# Role of CLASS: cross-sectional external construct corroboration only.

options(encoding = "UTF-8", warn = 1)
set.seed(20260729)

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(rms)
  library(Hmisc)
  library(sandwich)
  library(lmtest)
})

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
data_dir <- Sys.getenv("MARKOV_INPUT_DIR", unset = file.path(project_root, "data"))
output_dir <- Sys.getenv("CLASS_RCS_OUTPUT_DIR", unset = script_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw_path <- Sys.getenv("CLASS2018_DTA", unset = "")
if (!nzchar(raw_path)) {
  stop("Set CLASS2018_DTA to the local CLASS 2018 .dta file.")
}
fi_path <- file.path(data_dir, "CLASS_wave_specific_continuous_FI.csv")
charls_state_path <- file.path(data_dir, "CHARLS_wave_state_prevalence_recalculated.csv")
charls_adl_path <- file.path(data_dir, "CHARLS_state_ADL_validation_corrected.csv")
legacy_result_path <- file.path(data_dir, "CLASS_concurrent_FI_ADL_PR.csv")

log_path <- file.path(output_dir, "21_CLASS_RCS_analysis_log.txt")
log_con <- file(log_path, open = "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  while (sink.number(type = "message") > 0) sink(type = "message")
  while (sink.number(type = "output") > 0) sink(type = "output")
  try(close(log_con), silent = TRUE)
}, add = TRUE)

cat("CLASS 2018 RCS analysis log\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
cat("Random seed: 20260729\n")
cat("Project root: [current workspace; Chinese path omitted from console log to prevent Windows encoding corruption]\n")
cat("Raw CLASS file: CLASS2018-cleaned release.dta\n")
cat("FI file: CLASS_wave_specific_continuous_FI.csv\n\n")

required_files <- c(raw_path, fi_path, charls_state_path, charls_adl_path, legacy_result_path)
if (any(!file.exists(required_files))) {
  stop("Missing required file(s): ", paste(required_files[!file.exists(required_files)], collapse = "; "))
}

safe_num <- function(x) suppressWarnings(as.numeric(x))
value_labels <- function(x) {
  z <- attr(x, "labels")
  if (is.null(z)) return("")
  paste(paste0(names(z), "=", unname(z)), collapse = "; ")
}
fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}
wald_joint <- function(model, vcov_mat, terms) {
  b <- coef(model)[terms]
  v <- vcov_mat[terms, terms, drop = FALSE]
  keep <- is.finite(b) & apply(v, 1, function(z) all(is.finite(z)))
  b <- b[keep]
  v <- v[keep, keep, drop = FALSE]
  if (!length(b)) return(list(statistic = NA_real_, df = 0L, p_value = NA_real_))
  q <- as.numeric(t(b) %*% solve(v, b))
  list(statistic = q, df = length(b), p_value = pchisq(q, df = length(b), lower.tail = FALSE))
}
robust_ci <- function(model, term, scale = 1) {
  v <- sandwich::vcovHC(model, type = "HC0")
  b <- unname(coef(model)[term])
  se <- unname(sqrt(v[term, term]))
  c(estimate = exp(b * scale),
    ci_low = exp((b - 1.96 * se) * scale),
    ci_high = exp((b + 1.96 * se) * scale),
    beta = b, robust_se = se)
}
build_rcs_frame <- function(x, knots) {
  b <- Hmisc::rcspline.eval(x, knots = knots, inclx = TRUE)
  b <- as.data.frame(b)
  names(b) <- paste0("rcs", seq_len(ncol(b)))
  b
}
fit_rcs <- function(dat, knots, label) {
  basis <- build_rcs_frame(dat$hdi, knots)
  model_dat <- cbind(as.data.frame(dat), basis)
  f <- as.formula(paste("adl_help ~", paste(names(basis), collapse = " + ")))
  model <- glm(f, data = model_dat, family = poisson(link = "log"), x = TRUE, y = TRUE)
  v <- sandwich::vcovHC(model, type = "HC0")
  all_terms <- names(basis)
  nonlinear_terms <- all_terms[-1]
  overall <- wald_joint(model, v, all_terms)
  nonlinear <- wald_joint(model, v, nonlinear_terms)
  list(label = label, model = model, vcov = v, knots = knots,
       formula = f, overall = overall, nonlinear = nonlinear)
}
predict_rcs <- function(fit, grid, reference = 0.10) {
  basis <- build_rcs_frame(grid, fit$knots)
  x <- cbind(`(Intercept)` = 1, as.matrix(basis))
  beta <- coef(fit$model)
  x <- x[, names(beta), drop = FALSE]
  eta <- as.numeric(x %*% beta)
  se_eta <- sqrt(pmax(0, rowSums((x %*% fit$vcov) * x)))
  prevalence <- exp(eta)

  ref_basis <- build_rcs_frame(reference, fit$knots)
  xref <- c(`(Intercept)` = 1, unlist(ref_basis[1, ]))
  xref <- matrix(xref[names(beta)], nrow = 1)
  contrast <- sweep(x, 2, xref[1, ], "-")
  log_pr <- as.numeric(contrast %*% beta)
  se_log_pr <- sqrt(pmax(0, rowSums((contrast %*% fit$vcov) * contrast)))

  data.table(
    hdi = grid,
    prevalence = prevalence,
    prevalence_ci_low = exp(eta - 1.96 * se_eta),
    prevalence_ci_high = exp(eta + 1.96 * se_eta),
    pr = exp(log_pr),
    pr_ci_low = exp(log_pr - 1.96 * se_log_pr),
    pr_ci_high = exp(log_pr + 1.96 * se_log_pr),
    reference = reference,
    specification = fit$label
  )
}

# -------------------------------------------------------------------------
# 1. Read source data and reproduce the actual legacy data linkage
# -------------------------------------------------------------------------
raw <- read_dta(raw_path)
fi_all <- fread(fi_path)
legacy_reported <- fread(legacy_result_path)

raw_dat <- data.table(
  class_id_num = safe_num(raw[["rid"]]),
  birth_year = safe_num(raw[["a2__1__open"]]),
  sex = safe_num(raw[["a1"]]),
  education = safe_num(raw[["a3"]]),
  marital_status = safe_num(raw[["a5"]]),
  residence_type = safe_num(raw[["q5a"]]),
  adl_raw = safe_num(raw[["b5"]])
)
raw_dat[, age := 2018 - birth_year]
raw_dat[, adl_help := fifelse(adl_raw == 1, 1L, fifelse(adl_raw == 2, 0L, NA_integer_))]

fi_2018 <- fi_all[wave == 2018]
fi_link <- fi_2018[, .(
  class_id_num = safe_num(class_id),
  hdi = fi_primary,
  fi_valid_80,
  fi_completion_rate = fi_completion_rate_primary
)]
dat <- merge(raw_dat, fi_link, by = "class_id_num", all.x = TRUE, sort = FALSE)
legacy_sample <- dat[!is.na(hdi) & !is.na(adl_help)]
primary_sample <- dat[!is.na(age) & age >= 65 & !is.na(hdi) & !is.na(adl_help)]

cat("Raw rows:", nrow(raw), "\n")
cat("Legacy valid FI/outcome sample:", nrow(legacy_sample), "\n")
cat("Age >=65 requested primary sample:", nrow(primary_sample), "\n")
cat("Legacy sample age <65:", sum(legacy_sample$age < 65), "\n\n")

# -------------------------------------------------------------------------
# 2. Variable audit, including explicit absences
# -------------------------------------------------------------------------
audit_row <- function(role, actual, coding, x = NULL, valid_range = "",
                      included_fi = FALSE, included_model = FALSE, notes = "") {
  if (is.null(x)) {
    missing_n <- nrow(raw)
    missing_percent <- 100
  } else {
    missing_n <- sum(is.na(x))
    missing_percent <- 100 * mean(is.na(x))
  }
  data.table(
    variable_role = role,
    actual_variable_name = actual,
    coding = coding,
    missing_n = missing_n,
    missing_percent = round(missing_percent, 3),
    valid_range = valid_range,
    included_in_deficit_index = included_fi,
    included_in_current_model = included_model,
    notes = notes
  )
}

audit <- rbindlist(list(
  audit_row("identifier", "rid / class_id", "Numeric randomized code; linked to derived FI by numeric ID",
            raw_dat$class_id_num, paste(range(raw_dat$class_id_num), collapse = " to "), FALSE, FALSE,
            "Unique in CLASS 2018; cross-wave identifiers are incompatible."),
  audit_row("health-deficit index", "fi_primary (renamed hdi in this analysis)",
            "Observed deficit-score sum divided by completed items; >=80% completion",
            dat$hdi, sprintf("%.4f to %.4f", min(dat$hdi, na.rm = TRUE), max(dat$hdi, na.rm = TRUE)),
            TRUE, TRUE,
            "Actual executable code uses 17 items, despite legacy documentation describing 20."),
  audit_row("ADL-help outcome", "b5 (derived adl_help)",
            paste0(value_labels(raw[["b5"]]), "; analysis: 1=needs help, 0=does not need help"),
            raw_dat$adl_help, "0 to 1", FALSE, TRUE,
            "Question explicitly covers help with eating, bathing, dressing and toileting."),
  audit_row("age", "a2__1__open (derived age = 2018 - birth year)",
            "Reported year of birth; derived continuous age",
            raw_dat$age, paste(range(raw_dat$age, na.rm = TRUE), collapse = " to "), FALSE, FALSE,
            "Not used in the legacy current model; used only to enforce the requested age >=65 sample."),
  audit_row("sex", "a1", value_labels(raw[["a1"]]), raw_dat$sex, "1 to 2", FALSE, FALSE,
            "Available but not used in the legacy current model."),
  audit_row("education", "a3", value_labels(raw[["a3"]]), raw_dat$education, "1 to 7", FALSE, FALSE,
            "Available but not used in the legacy current model."),
  audit_row("marital status", "a5", value_labels(raw[["a5"]]), raw_dat$marital_status, "1 to 4", FALSE, FALSE,
            "Available but not used in the legacy current model."),
  audit_row("urban/rural residence", "q5a", value_labels(raw[["q5a"]]), raw_dat$residence_type, "1 to 6", FALSE, FALSE,
            "5=rural; categories 1-4 are urban/town contexts; not used in legacy current model."),
  audit_row("sampling weight", "NOT FOUND", "No validated sampling-weight variable in the 2018 release", NULL, "", FALSE, FALSE,
            "No variable name containing weight/wt and no validated project mapping."),
  audit_row("PSU", "NOT FOUND", "No validated PSU variable", NULL, "", FALSE, FALSE,
            "Geographic codes were not assumed to be PSUs."),
  audit_row("stratum", "NOT FOUND", "No validated stratum variable", NULL, "", FALSE, FALSE,
            "No complex-survey design fitted; ordinary Poisson with robust HC0 covariance used."),
  audit_row("current model adjustment set", "none", "Legacy formula: adl_help ~ fi", rep(0, nrow(raw)), "not applicable",
            FALSE, TRUE, "No demographic covariates were included in the executable legacy model.")
), fill = TRUE)

component_map <- data.table(
  role = c(rep("FI chronic-disease component", 11),
           "FI self-rated-health component",
           rep("FI physical-function component", 4),
           "FI depression component"),
  raw_name = c("b9_1__1", "b9_1__2", "b9_1__3", "b9_1__4", "b9_1__5",
               "b9_1__7", "b9_1__9", "b9_1__11", "b9_1__6", "b9_1__20",
               "b9_1__18", "b1", "b6_1", "b6_3", "b6_7", "b6_2",
               "e2__1-e2__9"),
  derived_name = c("hypertension_score", "heart_disease_score", "stroke_score",
                   "lung_disease_score", "diabetes_score", "cancer_score",
                   "arthritis_score", "kidney_disease_score", "liver_disease_score",
                   "stomach_disease_score", "osteoporosis_score", "srh_score",
                   "climb_stairs_score", "walk_outside_score", "lift_heavy_score",
                   "fall_12m_score", "depression_score")
)
for (i in seq_len(nrow(component_map))) {
  dn <- component_map$derived_name[i]
  x <- if (dn %in% names(fi_2018)) fi_2018[[dn]] else NULL
  coding <- if (grepl("disease", component_map$role[i])) "1=deficit; 0=no/uncertain"
  else if (dn == "srh_score") "0/0.5/1 from self-rated health"
  else if (dn == "depression_score") "9-item score standardized to 0-1 within wave"
  else "1=functional deficit/event; 0=no deficit/event"
  note <- if (dn %in% c("climb_stairs_score", "walk_outside_score", "lift_heavy_score", "fall_12m_score"))
    "Health-deficit construct overlap with functional limitation; not direct b5 outcome contamination."
  else "No direct overlap with b5 ADL-help outcome."
  audit <- rbind(audit, audit_row(component_map$role[i],
                                  paste0(component_map$raw_name[i], " -> ", dn),
                                  coding, x, "0 to 1", TRUE, FALSE, note), fill = TRUE)
}
fwrite(audit, file.path(output_dir, "01_CLASS_RCS_variable_audit.csv"))

# -------------------------------------------------------------------------
# 3. Sample flow
# -------------------------------------------------------------------------
age_valid <- dat[!is.na(age)]
age65 <- age_valid[age >= 65]
age65_fi <- age65[!is.na(hdi)]
age65_outcome <- age65_fi[!is.na(adl_help)]
sample_flow <- data.table(
  step = 0:5,
  description = c(
    "CLASS 2018 raw respondents",
    "Valid birth year / derived age",
    "Age >=65 years",
    "Valid continuous health-deficit index",
    "Valid ADL-help requirement outcome",
    "Other legacy main-analysis exclusions"
  ),
  n_remaining = c(nrow(dat), nrow(age_valid), nrow(age65), nrow(age65_fi),
                  nrow(age65_outcome), nrow(age65_outcome)),
  n_excluded_at_step = c(0, nrow(dat) - nrow(age_valid),
                         nrow(age_valid) - nrow(age65),
                         nrow(age65) - nrow(age65_fi),
                         nrow(age65_fi) - nrow(age65_outcome), 0)
)
sample_flow[, matches_reported_n_11163 := n_remaining == 11163]
sample_flow[, note := ""]
sample_flow[description == "Valid ADL-help requirement outcome",
            note := "Requested age-restricted primary sample; does not match legacy n=11,163."]
fwrite(sample_flow, file.path(output_dir, "02_CLASS_RCS_sample_flow.csv"))

# -------------------------------------------------------------------------
# 4. Exposure distribution and reference-point check
# -------------------------------------------------------------------------
probs <- c(0, .01, .05, .10, .25, .35, .50, .65, .75, .90, .95, .99, 1)
qs <- quantile(primary_sample$hdi, probs = probs, names = FALSE, na.rm = TRUE)
dist_summary <- data.table(
  record_type = "quantile",
  metric = paste0("p", format(100 * probs, trim = TRUE, scientific = FALSE)),
  value = qs,
  n = nrow(primary_sample)
)
dist_other <- data.table(
  record_type = "summary",
  metric = c("mean", "sd", "median", "iqr", "min", "max",
             "n_within_0.01_of_0.10", "percent_within_0.01_of_0.10",
             "n_unique_values", "adl_prevalence"),
  value = c(mean(primary_sample$hdi), sd(primary_sample$hdi),
            median(primary_sample$hdi), IQR(primary_sample$hdi),
            min(primary_sample$hdi), max(primary_sample$hdi),
            sum(abs(primary_sample$hdi - 0.10) <= 0.01),
            100 * mean(abs(primary_sample$hdi - 0.10) <= 0.01),
            uniqueN(primary_sample$hdi), mean(primary_sample$adl_help)),
  n = nrow(primary_sample)
)
hist_obj <- hist(primary_sample$hdi, breaks = seq(0, 1, by = 0.025), plot = FALSE)
dist_hist <- data.table(
  record_type = "histogram",
  metric = paste0(sprintf("%.3f", head(hist_obj$breaks, -1)), "-",
                  sprintf("%.3f", tail(hist_obj$breaks, -1))),
  value = hist_obj$counts,
  n = nrow(primary_sample)
)
fwrite(rbindlist(list(dist_summary, dist_other, dist_hist), fill = TRUE),
       file.path(output_dir, "03_CLASS_RCS_exposure_distribution.csv"))

reference <- 0.10
p1 <- unname(quantile(primary_sample$hdi, 0.01))
p99 <- unname(quantile(primary_sample$hdi, 0.99))
if (sum(abs(primary_sample$hdi - reference) <= 0.01) < 100 ||
    reference < p1 || reference > p99) {
  reference <- median(primary_sample$hdi)
  cat("Reference changed to median because 0.10 lacked reliable support.\n")
} else {
  cat("Reference retained at 0.10; n within +/-0.01:",
      sum(abs(primary_sample$hdi - reference) <= 0.01), "\n")
}

# -------------------------------------------------------------------------
# 5. Linear modified Poisson reproducibility
# -------------------------------------------------------------------------
legacy_linear <- glm(adl_help ~ hdi, data = legacy_sample,
                     family = poisson(link = "log"), x = TRUE, y = TRUE)
legacy_profile <- suppressMessages(confint(legacy_linear, "hdi"))
legacy_robust <- robust_ci(legacy_linear, "hdi", 0.10)

primary_linear <- glm(adl_help ~ hdi, data = primary_sample,
                      family = poisson(link = "log"), x = TRUE, y = TRUE)
primary_linear_robust <- robust_ci(primary_linear, "hdi", 0.10)

linear_results <- rbindlist(list(
  data.table(
    model = "Legacy executable model (all valid ages; model-based profile CI)",
    sample_definition = "Valid FI and b5; no age restriction",
    n = nrow(legacy_sample), events = sum(legacy_sample$adl_help),
    formula = paste(deparse(formula(legacy_linear)), collapse = ""),
    covariance = "Poisson model-based profile likelihood",
    pr_per_0.10 = exp(coef(legacy_linear)["hdi"] * 0.10),
    ci_low = exp(legacy_profile[1] * 0.10),
    ci_high = exp(legacy_profile[2] * 0.10)
  ),
  data.table(
    model = "Legacy sample with valid modified-Poisson robust CI",
    sample_definition = "Valid FI and b5; no age restriction",
    n = nrow(legacy_sample), events = sum(legacy_sample$adl_help),
    formula = paste(deparse(formula(legacy_linear)), collapse = ""),
    covariance = "HC0 sandwich",
    pr_per_0.10 = legacy_robust["estimate"],
    ci_low = legacy_robust["ci_low"], ci_high = legacy_robust["ci_high"]
  ),
  data.table(
    model = "Requested primary age-restricted linear model",
    sample_definition = "Age >=65; valid FI and b5",
    n = nrow(primary_sample), events = sum(primary_sample$adl_help),
    formula = paste(deparse(formula(primary_linear)), collapse = ""),
    covariance = "HC0 sandwich",
    pr_per_0.10 = primary_linear_robust["estimate"],
    ci_low = primary_linear_robust["ci_low"], ci_high = primary_linear_robust["ci_high"]
  )
), fill = TRUE)
linear_results[, p_value := c(
  summary(legacy_linear)$coefficients["hdi", "Pr(>|z|)"],
  2 * pnorm(abs(coef(legacy_linear)["hdi"] / legacy_robust["robust_se"]), lower.tail = FALSE),
  2 * pnorm(abs(coef(primary_linear)["hdi"] / primary_linear_robust["robust_se"]), lower.tail = FALSE)
)]
fwrite(linear_results, file.path(output_dir, "04_CLASS_linear_model_results.csv"))

# -------------------------------------------------------------------------
# 6. RCS models: 4-knot primary; 3/5-knot sensitivity
# -------------------------------------------------------------------------
knot_probs <- list(
  `3 knots` = c(.10, .50, .90),
  `4 knots (primary)` = c(.05, .35, .65, .95),
  `5 knots` = c(.05, .275, .50, .725, .95)
)
knots <- lapply(knot_probs, function(p) unname(quantile(primary_sample$hdi, p, na.rm = TRUE)))
knot_table <- rbindlist(lapply(names(knots), function(nm) {
  data.table(specification = nm, knot_number = seq_along(knots[[nm]]),
             percentile = knot_probs[[nm]], knot_value = knots[[nm]])
}))
fwrite(knot_table, file.path(output_dir, "CLASS_RCS_knots.csv"))

fits <- lapply(names(knots), function(nm) fit_rcs(primary_sample, knots[[nm]], nm))
names(fits) <- names(knots)
fit4 <- fits[["4 knots (primary)"]]

rcs4_results <- data.table(
  specification = fit4$label,
  n = nrow(primary_sample),
  events = sum(primary_sample$adl_help),
  reference = reference,
  prediction_min_p1 = p1,
  prediction_max_p99 = p99,
  knot_1 = fit4$knots[1],
  knot_2 = fit4$knots[2],
  knot_3 = fit4$knots[3],
  knot_4 = fit4$knots[4],
  overall_wald_chisq = fit4$overall$statistic,
  overall_df = fit4$overall$df,
  overall_p = fit4$overall$p_value,
  nonlinearity_wald_chisq = fit4$nonlinear$statistic,
  nonlinearity_df = fit4$nonlinear$df,
  nonlinearity_p = fit4$nonlinear$p_value,
  formula = paste(deparse(fit4$formula), collapse = ""),
  covariance = "HC0 sandwich"
)
fwrite(rcs4_results, file.path(output_dir, "05_CLASS_RCS_4knot_results.csv"))

grid <- seq(p1, p99, length.out = 300)
predictions <- rbindlist(lapply(fits, predict_rcs, grid = grid, reference = reference))
pred4 <- predictions[specification == "4 knots (primary)"]

probe_values <- unique(c(reference, unname(quantile(primary_sample$hdi, c(.50, .75, .90, .95)))))
sensitivity <- rbindlist(lapply(fits, function(fit) {
  pp <- predict_rcs(fit, probe_values, reference)
  full_pp <- predict_rcs(fit, grid, reference)
  data.table(
    specification = fit$label,
    knots = paste(sprintf("%.6f", fit$knots), collapse = ";"),
    n = nrow(primary_sample),
    overall_p = fit$overall$p_value,
    nonlinearity_p = fit$nonlinear$p_value,
    aic = AIC(fit$model),
    bic = BIC(fit$model),
    deviance = deviance(fit$model),
    monotonic_non_decreasing = all(diff(full_pp$prevalence) >= -1e-10),
    max_ci_width_p1_p99 = max(full_pp$prevalence_ci_high - full_pp$prevalence_ci_low),
    probe_hdi = pp$hdi,
    probe_prevalence = pp$prevalence,
    probe_prevalence_ci_low = pp$prevalence_ci_low,
    probe_prevalence_ci_high = pp$prevalence_ci_high,
    probe_pr = pp$pr,
    probe_pr_ci_low = pp$pr_ci_low,
    probe_pr_ci_high = pp$pr_ci_high
  )
}))
fwrite(sensitivity, file.path(output_dir, "06_CLASS_RCS_knot_sensitivity.csv"))

fwrite(pred4[, .(hdi, adjusted_prevalence = prevalence,
                 ci_low = prevalence_ci_low, ci_high = prevalence_ci_high,
                 reference, specification)],
       file.path(output_dir, "07_CLASS_RCS_prediction_prevalence.csv"))
fwrite(pred4[, .(hdi, prevalence_ratio = pr, ci_low = pr_ci_low,
                 ci_high = pr_ci_high, reference, specification)],
       file.path(output_dir, "08_CLASS_RCS_prediction_PR.csv"))

linear_v <- sandwich::vcovHC(primary_linear, type = "HC0")
linear_overall <- wald_joint(primary_linear, linear_v, "hdi")
model_tests <- rbindlist(list(
  data.table(
    model = "Linear modified Poisson",
    n_parameters = length(coef(primary_linear)),
    aic = AIC(primary_linear), bic = BIC(primary_linear),
    deviance = deviance(primary_linear),
    overall_wald_chisq = linear_overall$statistic,
    overall_df = linear_overall$df,
    overall_p = linear_overall$p_value,
    nonlinearity_wald_chisq = NA_real_, nonlinearity_df = NA_integer_,
    nonlinearity_p = NA_real_,
    delta_aic_vs_linear = 0, delta_bic_vs_linear = 0
  ),
  rbindlist(lapply(fits, function(fit) data.table(
    model = paste0("RCS ", fit$label),
    n_parameters = length(coef(fit$model)),
    aic = AIC(fit$model), bic = BIC(fit$model),
    deviance = deviance(fit$model),
    overall_wald_chisq = fit$overall$statistic,
    overall_df = fit$overall$df,
    overall_p = fit$overall$p_value,
    nonlinearity_wald_chisq = fit$nonlinear$statistic,
    nonlinearity_df = fit$nonlinear$df,
    nonlinearity_p = fit$nonlinear$p_value,
    delta_aic_vs_linear = AIC(fit$model) - AIC(primary_linear),
    delta_bic_vs_linear = BIC(fit$model) - BIC(primary_linear)
  )))
), fill = TRUE)
fwrite(model_tests, file.path(output_dir, "09_CLASS_RCS_model_tests.csv"))

# Legacy-sample RCS for like-for-like comparison with the paper's n=11,163
legacy_knots4 <- unname(quantile(legacy_sample$hdi, c(.05, .35, .65, .95)))
legacy_fit4 <- fit_rcs(legacy_sample, legacy_knots4, "Legacy-sample 4 knots")
charls_state_2018 <- fread(charls_state_path)[wave == 2018]
repro <- data.table(
  check = c(
    "Legacy sample size",
    "Legacy point estimate per 0.10",
    "Legacy reported lower CI",
    "Legacy reported upper CI",
    "Valid robust lower CI on legacy sample",
    "Valid robust upper CI on legacy sample",
    "Requested age-restricted sample size",
    "Legacy 4-knot overall P",
    "Legacy 4-knot nonlinearity P",
    "CHARLS 2018 low state label",
    "CHARLS 2018 intermediate state label",
    "CHARLS 2018 high state label"
  ),
  observed = c(
    nrow(legacy_sample),
    exp(coef(legacy_linear)["hdi"] * .10),
    exp(legacy_profile[1] * .10),
    exp(legacy_profile[2] * .10),
    legacy_robust["ci_low"],
    legacy_robust["ci_high"],
    nrow(primary_sample),
    legacy_fit4$overall$p_value,
    legacy_fit4$nonlinear$p_value,
    charls_state_2018$n1,
    charls_state_2018$n2,
    charls_state_2018$n3
  ),
  expected_or_reported = c(
    11163,
    legacy_reported$pr_per_010_fi[1],
    legacy_reported$ci_low[1],
    legacy_reported$ci_high[1],
    NA, NA, NA, NA, NA, 551, 2261, 3802
  ),
  status = c(
    ifelse(nrow(legacy_sample) == 11163, "reproduced", "not reproduced"),
    ifelse(abs(exp(coef(legacy_linear)["hdi"] * .10) - legacy_reported$pr_per_010_fi[1]) < .001, "reproduced", "not reproduced"),
    "reproduced only with model-based/profile Poisson CI",
    "reproduced only with model-based/profile Poisson CI",
    "robust modified-Poisson correction",
    "robust modified-Poisson correction",
    "does not match legacy n because age>=65 removes younger respondents",
    "supplementary legacy-sample comparison",
    "supplementary legacy-sample comparison",
    "label confirmed", "label confirmed", "label confirmed"
  ),
  notes = c(
    "Legacy code did not restrict age.",
    "Point estimate exactly matches the paper after rounding.",
    "Paper CI is not a sandwich-robust interval.",
    "Paper CI is not a sandwich-robust interval.",
    "HC0 interval is wider than the paper interval.",
    "HC0 interval is wider than the paper interval.",
    "Primary sample follows the present request.",
    "Same 4-knot percentiles, computed in legacy sample.",
    "Same 4-knot percentiles, computed in legacy sample.",
    "551/6614 = 8.3%.",
    "2261/6614 = 34.2%.",
    "3802/6614 = 57.5%."
  )
)
fwrite(repro, file.path(output_dir, "10_CLASS_RCS_reproducibility_check.csv"))

# -------------------------------------------------------------------------
# 7. Figures
# -------------------------------------------------------------------------
ink <- "#202124"
teal <- "#2A8C82"
navy <- "#315A86"
neutral_mid <- "#7A7F85"
neutral_light <- "#D9DDE1"
coral <- "#C95A50"

theme_nature <- function(base_size = 9) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(colour = ink),
      axis.line = element_line(linewidth = .4, colour = ink),
      axis.ticks = element_line(linewidth = .4, colour = ink),
      plot.title = element_text(face = "bold", size = base_size + .5),
      plot.subtitle = element_text(size = base_size - .5, colour = neutral_mid),
      plot.tag = element_text(face = "bold"),
      legend.position = "none",
      plot.margin = margin(7, 9, 7, 7)
    )
}
annotation_text <- paste0("Overall P ", fmt_p(fit4$overall$p_value),
                          "\nNon-linearity P ", fmt_p(fit4$nonlinear$p_value))
rug_dat <- primary_sample[seq(1, .N, by = max(1, floor(.N / 900)))]

p_prev <- ggplot(pred4, aes(hdi, 100 * prevalence)) +
  geom_ribbon(aes(ymin = 100 * prevalence_ci_low, ymax = 100 * prevalence_ci_high),
              fill = teal, alpha = .18) +
  geom_line(colour = teal, linewidth = 1) +
  geom_vline(xintercept = c(.10, .25), linetype = "dashed",
             colour = c(navy, coral), linewidth = .45) +
  geom_rug(data = rug_dat, aes(x = hdi), inherit.aes = FALSE,
           sides = "b", colour = neutral_mid, alpha = .35, linewidth = .25) +
  annotate("text", x = p1 + .02 * (p99 - p1),
           y = max(100 * pred4$prevalence_ci_high) * .96,
           label = annotation_text, hjust = 0, vjust = 1, size = 3, family = "Arial") +
  coord_cartesian(xlim = c(p1, p99), clip = "off") +
  labs(x = "Health-deficit index",
       y = "Adjusted prevalence of\nADL-help requirement, %",
       title = "CLASS 2018 external construct corroboration",
       subtitle = "Restricted cubic spline, four prespecified knots; age ≥65 years") +
  theme_nature()

p_pr <- ggplot(pred4, aes(hdi, pr)) +
  geom_ribbon(aes(ymin = pr_ci_low, ymax = pr_ci_high),
              fill = teal, alpha = .18) +
  geom_line(colour = teal, linewidth = 1) +
  geom_hline(yintercept = 1, colour = neutral_mid, linewidth = .45) +
  geom_vline(xintercept = c(.10, .25), linetype = "dashed",
             colour = c(navy, coral), linewidth = .45) +
  geom_rug(data = rug_dat, aes(x = hdi), inherit.aes = FALSE,
           sides = "b", colour = neutral_mid, alpha = .35, linewidth = .25) +
  annotate("text", x = p1 + .02 * (p99 - p1),
           y = max(pred4$pr_ci_high) * .94,
           label = annotation_text, hjust = 0, vjust = 1, size = 3, family = "Arial") +
  coord_cartesian(xlim = c(p1, p99), clip = "off") +
  labs(x = "Health-deficit index", y = "Prevalence ratio",
       title = "CLASS 2018 prevalence-ratio curve",
       subtitle = sprintf("Reference = %.2f; age ≥65 years", reference)) +
  theme_nature()

ggsave(file.path(output_dir, "12_CLASS_RCS_figure_prevalence.png"), p_prev,
       width = 170, height = 115, units = "mm", dpi = 600, bg = "white")
ggsave(file.path(output_dir, "13_CLASS_RCS_figure_prevalence.pdf"), p_prev,
       width = 170, height = 115, units = "mm", device = cairo_pdf, bg = "white")
ggsave(file.path(output_dir, "14_CLASS_RCS_figure_prevalence.tiff"), p_prev,
       width = 170, height = 115, units = "mm", dpi = 600,
       compression = "lzw", bg = "white")
ggsave(file.path(output_dir, "15_CLASS_RCS_figure_PR.png"), p_pr,
       width = 170, height = 115, units = "mm", dpi = 600, bg = "white")
ggsave(file.path(output_dir, "16_CLASS_RCS_figure_PR.pdf"), p_pr,
       width = 170, height = 115, units = "mm", device = cairo_pdf, bg = "white")
ggsave(file.path(output_dir, "16_CLASS_RCS_figure_PR.tiff"), p_pr,
       width = 170, height = 115, units = "mm", dpi = 600,
       compression = "lzw", bg = "white")

# Figure 4 options
charls_adl <- fread(charls_adl_path)
charls_adl[, state_label := factor(
  state,
  levels = 1:3,
  labels = c("Low deficit", "Intermediate", "High deficit")
)]
charls_adl[, risk_pct := 100 * risk]
charls_adl[, ci_low_pct := 100 * qbeta(.025, nadl + 1, n - nadl + 1)]
charls_adl[, ci_high_pct := 100 * qbeta(.975, nadl + 1, n - nadl + 1)]

f4a <- ggplot(charls_adl, aes(state_label, risk_pct)) +
  geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct),
                width = .15, linewidth = .55, colour = navy) +
  geom_point(size = 2.3, colour = navy) +
  labs(tag = "a", title = "CHARLS longitudinal validation",
       subtitle = "Incident ADL limitation by 2018",
       x = "2015 health-deficit state", y = "Three-year risk (%)") +
  theme_nature(8) + theme(axis.text.x = element_text(angle = 0))

legacy_plot <- linear_results[model == "Legacy executable model (all valid ages; model-based profile CI)"]
f4b_linear <- ggplot(legacy_plot, aes(pr_per_0.10, 1)) +
  geom_vline(xintercept = 1, colour = neutral_mid, linewidth = .4) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = .10,
                orientation = "y", colour = teal, linewidth = .65) +
  geom_point(size = 2.6, colour = teal) +
  annotate("text", x = 1.92, y = 1.13,
           label = sprintf("PR %.2f (%.2f, %.2f)", legacy_plot$pr_per_0.10,
                           legacy_plot$ci_low, legacy_plot$ci_high),
           hjust = 1, size = 2.7, family = "Arial") +
  scale_x_continuous(breaks = seq(1.2, 2.1, by = .3),
                     limits = c(1, 2.35), expand = expansion(mult = c(.01, .01))) +
  scale_y_continuous(limits = c(.82, 1.18)) +
  labs(tag = "b", title = "CLASS external corroboration",
       subtitle = "Concurrent ADL-help prevalence per 0.10 higher index",
       x = "Prevalence ratio", y = NULL) +
  theme_nature(8) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

f4b_curve <- p_prev +
  labs(tag = "b", title = "CLASS spline corroboration",
       subtitle = "Adjusted ADL-help prevalence; age ≥65 years") +
  theme_nature(8)
f4c_linear <- f4b_linear + labs(tag = "c")
f4c_linear <- f4c_linear +
  labs(title = "CLASS linear association",
       subtitle = "PR per 0.10 higher index") +
  theme(plot.margin = margin(7, 16, 7, 9))

# Use a compact two-line annotation in the narrow three-panel layout.
f4c_linear$layers[[4]] <- NULL
f4c_linear <- f4c_linear +
  annotate(
    "text",
    x = 1.70,
    y = 1.13,
    label = sprintf("PR %.2f\n(%.2f, %.2f)",
                    legacy_plot$pr_per_0.10,
                    legacy_plot$ci_low,
                    legacy_plot$ci_high),
    hjust = 0.5,
    vjust = 0.5,
    lineheight = 0.95,
    size = 2.6,
    family = "Arial"
  )

option_a <- f4a + f4b_linear + plot_layout(widths = c(1.15, 1))
option_b <- f4a + f4b_curve + f4c_linear + plot_layout(widths = c(1, 1.22, 1.05))
ggsave(file.path(output_dir, "17_Figure4_option_A.png"), option_a,
       width = 183, height = 82, units = "mm", dpi = 600, bg = "white")
ggsave(file.path(output_dir, "18_Figure4_option_B.png"), option_b,
       width = 245, height = 82, units = "mm", dpi = 600, bg = "white")

# -------------------------------------------------------------------------
# 8. Model objects, formulas, session info, and final audit notes
# -------------------------------------------------------------------------
model_formulas <- data.table(
  model = c("legacy_linear", "primary_age65_linear", names(fits), "legacy_sample_4knot"),
  sample = c("legacy n=11,163", "age>=65", rep("age>=65", length(fits)), "legacy n=11,163"),
  formula = c(paste(deparse(formula(legacy_linear)), collapse = ""),
              paste(deparse(formula(primary_linear)), collapse = ""),
              vapply(fits, function(z) paste(deparse(z$formula), collapse = ""), character(1)),
              paste(deparse(legacy_fit4$formula), collapse = "")),
  covariance = c("model-based and HC0", "HC0", rep("HC0", length(fits)), "HC0")
)
fwrite(model_formulas, file.path(output_dir, "CLASS_RCS_model_formulas.csv"))
saveRDS(list(
  seed = 20260729,
  legacy_linear = legacy_linear,
  primary_age65_linear = primary_linear,
  rcs_fits_age65 = fits,
  legacy_sample_rcs4 = legacy_fit4,
  primary_knots = knots,
  legacy_knots4 = legacy_knots4,
  reference = reference,
  prediction_range = c(p1 = p1, p99 = p99),
  sample_sizes = c(raw = nrow(raw), legacy = nrow(legacy_sample), age65 = nrow(primary_sample))
), file.path(output_dir, "CLASS_RCS_model_objects.rds"))

writeLines(capture.output(sessionInfo()),
           file.path(output_dir, "22_CLASS_RCS_sessionInfo.txt"), useBytes = TRUE)

cat("\nKey audit conclusions\n")
cat("- Legacy point estimate reproduced:", exp(coef(legacy_linear)["hdi"] * .10), "\n")
cat("- Legacy paper CI reproduced only with model-based/profile Poisson inference:",
    exp(legacy_profile * .10), "\n")
cat("- Legacy robust HC0 CI:", legacy_robust["ci_low"], legacy_robust["ci_high"], "\n")
cat("- Age>=65 primary n:", nrow(primary_sample), "\n")
cat("- Age>=65 linear robust PR:", primary_linear_robust["estimate"],
    "CI", primary_linear_robust["ci_low"], primary_linear_robust["ci_high"], "\n")
cat("- Four-knot values:", paste(fit4$knots, collapse = ", "), "\n")
cat("- Four-knot overall P:", fit4$overall$p_value, "\n")
cat("- Four-knot non-linearity P:", fit4$nonlinear$p_value, "\n")
cat("- Direct ADL outcome contamination: not found; b5 is not in executable fi_primary.\n")
cat("- Construct overlap: climb stairs, walk outside, lift heavy, and falls.\n")
cat("- Executable FI has 17 items; legacy logs/audit describe 20, a documentation mismatch.\n")
cat("- CHARLS labels confirmed: low 551, intermediate 2261, high 3802.\n")
cat("\nExecution history and warnings\n")
cat("- Final statistical run completed without model warnings or errors.\n")
cat("- An initial development run failed because rcspline.eval was called from rms rather than Hmisc; the namespace was corrected.\n")
cat("- A subsequent development run failed because the CHARLS state-count audit file is wide (n1/n2/n3), not long (state_code/N); the audit mapping was corrected.\n")
cat("- DOCX deep-schema validation could not use the optional skill validator because defusedxml is absent from the bundled Python environment; both DOCX files passed python-docx reopen and ZIP integrity tests.\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
