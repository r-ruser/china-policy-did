#!/usr/bin/env Rscript

# Epidemiology-corrected DID/DDD pipeline.
# Run from the project root. All models and statistical outputs are produced in R.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(fixest)
})

options(encoding = "UTF-8")
set.seed(20260724)
n_cores <- parallel::detectCores(logical = TRUE)
if (is.na(n_cores) || n_cores < 1L) n_cores <- 1L
setFixest_nthreads(n_cores)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
path_tables <- file.path(project_root, "07_results", "tables")
path_models <- file.path(project_root, "07_results", "models")
path_diag <- file.path(project_root, "07_results", "diagnostics")
path_logs <- file.path(project_root, "10_logs")
invisible(lapply(c(path_tables, path_models, path_diag, path_logs), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

log_file <- file.path(path_logs, "r_epi_corrected_analysis.log")
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("Epidemiology-corrected DID/DDD analysis\n")
cat("Started:", format(Sys.time()), "\n")
cat("Project:", project_root, "\n\n")
cat("Logical CPU cores used by fixest:", n_cores, "\n\n")

charls_path <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
stopifnot(file.exists(charls_path))

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

binary01 <- function(x) {
  z <- safe_numeric(x)
  ifelse(is.na(z), NA_real_, ifelse(z == 1, 1, ifelse(z == 0, 0, NA_real_)))
}

complete_two_wave <- function(df, outcome, years = c(2015, 2018), id = "ID") {
  df |>
    filter(.data$year %in% years, !is.na(.data[[outcome]])) |>
    group_by(.data[[id]]) |>
    filter(n_distinct(.data$year) == length(years)) |>
    ungroup()
}

tidy_term <- function(model, pattern, analysis, outcome, estimand,
                      evidence_grade, notes = "") {
  cf <- coef(model)
  idx <- grep(pattern, names(cf))
  if (length(idx) != 1L) {
    stop(sprintf("Expected one term for '%s'; got: %s",
                 pattern, paste(names(cf)[idx], collapse = ", ")))
  }
  term <- names(cf)[idx]
  ci <- confint(model, parm = term)
  data.frame(
    analysis = analysis,
    outcome = outcome,
    estimand = estimand,
    term = term,
    estimate = unname(cf[[term]]),
    std_error = unname(se(model)[[term]]),
    conf_low = unname(ci[1]),
    conf_high = unname(ci[2]),
    p_value = unname(pvalue(model)[[term]]),
    n_obs = nobs(model),
    n_id = if (length(fixef(model))) length(fixef(model)[[1]]) else NA_integer_,
    evidence_grade = evidence_grade,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

event_terms <- function(model, analysis, outcome, interaction_label,
                        evidence_grade, reference_year) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  names(ct)[1:4] <- c("estimate", "std_error", "t_value", "p_value")
  ct <- ct[grepl("::", ct$term, fixed = TRUE), , drop = FALSE]
  ct$year <- as.integer(sub(".*::([0-9]{4}).*", "\\1", ct$term))
  ct$conf_low <- ct$estimate - 1.96 * ct$std_error
  ct$conf_high <- ct$estimate + 1.96 * ct$std_error
  out <- ct[, c("year", "term", "estimate", "std_error",
                "conf_low", "conf_high", "p_value")]
  out$analysis <- analysis
  out$outcome <- outcome
  out$interaction <- interaction_label
  out$evidence_grade <- evidence_grade
  ref <- data.frame(
    year = reference_year,
    term = "Reference",
    estimate = 0,
    std_error = NA_real_,
    conf_low = 0,
    conf_high = 0,
    p_value = NA_real_,
    analysis = analysis,
    outcome = outcome,
    interaction = interaction_label,
    evidence_grade = evidence_grade
  )
  bind_rows(out, ref) |> arrange(year)
}

joint_pretrend <- function(model, years, label) {
  keep_pattern <- paste(sprintf("::%s", years), collapse = "|")
  w <- tryCatch(fixest::wald(model, keep = keep_pattern, print = FALSE),
                error = function(e) NULL)
  if (is.null(w)) {
    return(data.frame(test = label, statistic = NA_real_, p_value = NA_real_))
  }
  data.frame(
    test = label,
    statistic = unname(w$stat),
    p_value = unname(w$p)
  )
}

make_ipcw <- function(baseline, retained, predictors) {
  dat <- baseline
  dat$retained <- retained
  used <- character()
  for (v in predictors) {
    x <- safe_numeric(dat[[v]])
    miss <- is.na(x)
    if (any(miss)) {
      med <- median(x, na.rm = TRUE)
      if (!is.finite(med)) med <- 0
      x[miss] <- med
      miss_name <- paste0(v, "_missing")
      dat[[miss_name]] <- as.integer(miss)
      used <- c(used, miss_name)
    }
    dat[[v]] <- x
    used <- c(used, v)
  }
  f <- reformulate(used, response = "retained")
  fit <- glm(f, data = dat, family = binomial())
  pr <- pmin(pmax(predict(fit, type = "response"), 0.01), 0.99)
  sw <- mean(dat$retained) / pr
  q <- quantile(sw[dat$retained == 1], c(0.01, 0.99), na.rm = TRUE)
  sw <- pmin(pmax(sw, q[[1]]), q[[2]])
  list(
    weights = sw,
    model = fit,
    diagnostics = data.frame(
      retained_n = sum(dat$retained == 1),
      baseline_n = nrow(dat),
      retained_pct = mean(dat$retained),
      probability_min = min(pr),
      probability_p01 = quantile(pr, 0.01),
      probability_median = median(pr),
      probability_p99 = quantile(pr, 0.99),
      probability_max = max(pr),
      weight_p01 = quantile(sw[dat$retained == 1], 0.01),
      weight_median = median(sw[dat$retained == 1]),
      weight_p99 = quantile(sw[dat$retained == 1], 0.99),
      weight_max = max(sw[dat$retained == 1])
    )
  )
}

# -------------------------------------------------------------------------
# CHARLS: nationwide target-group differential change
# -------------------------------------------------------------------------

cat("[1] Reading CHARLS\n")
charls_wide <- read_dta(charls_path)
charls_wide <- charls_wide |>
  mutate(
    age_2015 = 2015 - safe_numeric(rabyear),
    older_2015 = as.integer(age_2015 >= 75),
    female = as.integer(safe_numeric(ragender) == 2)
  ) |>
  filter(age_2015 >= 65, safe_numeric(inw3) == 1)

wave_year <- c(`1` = 2011, `2` = 2013, `3` = 2015, `4` = 2018)
charls_panel <- bind_rows(lapply(names(wave_year), function(w) {
  yr <- unname(wave_year[[w]])
  adl <- safe_numeric(charls_wide[[paste0("r", w, "adla_c")]])
  cesd <- safe_numeric(charls_wide[[paste0("r", w, "cesd10")]])
  orient <- safe_numeric(charls_wide[[paste0("r", w, "orient")]])
  imrc <- safe_numeric(charls_wide[[paste0("r", w, "imrc")]])
  ser7 <- safe_numeric(charls_wide[[paste0("r", w, "ser7")]])
  srh_alt <- safe_numeric(charls_wide[[paste0("r", w, "shlta")]])
  cog_complete <- !is.na(orient) & !is.na(imrc) & !is.na(ser7)
  data.frame(
    ID = as.character(charls_wide$ID),
    year = yr,
    in_wave = safe_numeric(charls_wide[[paste0("inw", w)]]) == 1,
    age_2015 = charls_wide$age_2015,
    older_2015 = charls_wide$older_2015,
    female = charls_wide$female,
    any_adl = ifelse(is.na(adl), NA_real_, as.numeric(adl >= 1)),
    cesd10 = cesd,
    depression = ifelse(is.na(cesd), NA_real_, as.numeric(cesd >= 10)),
    cognition = ifelse(cog_complete, orient + imrc + ser7, NA_real_),
    poor_srh = ifelse(srh_alt %in% 1:5,
                      as.numeric(srh_alt >= 4), NA_real_),
    smoke = binary01(charls_wide[[paste0("r", w, "smokev")]]),
    drink = binary01(charls_wide[[paste0("r", w, "drinkev")]]),
    hypertension = binary01(charls_wide[[paste0("r", w, "hibpe")]]),
    heart_disease = binary01(charls_wide[[paste0("r", w, "hearte")]]),
    stroke = binary01(charls_wide[[paste0("r", w, "stroke")]]),
    lung_disease = binary01(charls_wide[[paste0("r", w, "lunge")]]),
    diabetes = binary01(charls_wide[[paste0("r", w, "diabe")]])
  )
})) |>
  filter(in_wave)

charls_bl <- charls_panel |>
  filter(year == 2015) |>
  transmute(
    ID,
    baseline_adl = any_adl,
    baseline_cesd10 = cesd10,
    baseline_depression = depression,
    baseline_cognition = cognition,
    baseline_poor_srh = poor_srh,
    baseline_smoke = smoke,
    baseline_drink = drink,
    baseline_hypertension = hypertension,
    baseline_heart_disease = heart_disease,
    baseline_stroke = stroke,
    baseline_lung_disease = lung_disease,
    baseline_diabetes = diabetes
  )

charls_panel <- left_join(charls_panel, charls_bl, by = "ID") |>
  mutate(
    post = as.integer(year >= 2018),
    baseline_cvd = ifelse(
      is.na(baseline_hypertension) & is.na(baseline_heart_disease) &
        is.na(baseline_stroke),
      NA_real_,
      as.numeric(coalesce(baseline_hypertension, 0) == 1 |
                   coalesce(baseline_heart_disease, 0) == 1 |
                   coalesce(baseline_stroke, 0) == 1)
    )
  )

charls_flow <- charls_panel |>
  group_by(year, older_2015) |>
  summarise(
    n_observed = n_distinct(ID),
    adl_observed = sum(!is.na(any_adl)),
    poor_srh_observed = sum(!is.na(poor_srh)),
    cesd_observed = sum(!is.na(cesd10)),
    cognition_observed = sum(!is.na(cognition)),
    .groups = "drop"
  )
fwrite(charls_flow, file.path(path_diag, "charls_sample_flow.csv"))

charls_results <- list()
charls_models <- list()

incident_specs <- list(
  list(outcome = "any_adl", baseline = "baseline_adl", eligible = 0,
       label = "Incident ADL", unit = "risk difference"),
  list(outcome = "depression", baseline = "baseline_depression", eligible = 0,
       label = "Incident depression", unit = "risk difference")
)

for (sp in incident_specs) {
  dat <- charls_panel |>
    filter(.data[[sp$baseline]] == sp$eligible) |>
    complete_two_wave(sp$outcome)
  model <- feols(
    as.formula(sprintf("%s ~ older_2015:post | ID + year", sp$outcome)),
    data = dat,
    cluster = ~ID
  )
  charls_models[[paste0("charls_", sp$outcome)]] <- model
  charls_results[[length(charls_results) + 1L]] <- tidy_term(
    model, "older_2015:post|post:older_2015",
    "CHARLS target-group period change", sp$label,
    paste0("Difference-in-differences on ", sp$unit),
    "Descriptive/associational",
    "Individual and year fixed effects; causal attribution is not supported because national policy exposure has no untreated geographic units and pre-trends are assessed separately."
  )
}

repeated_specs <- list(
  list(outcome = "poor_srh", label = "Poor self-rated health",
       estimand = "Difference-in-differences risk difference"),
  list(outcome = "cesd10", label = "CESD-10 score",
       estimand = "Difference-in-differences in mean change"),
  list(outcome = "cognition", label = "Cognitive score (0-19)",
       estimand = "Difference-in-differences in mean change")
)
for (sp in repeated_specs) {
  dat <- complete_two_wave(charls_panel, sp$outcome)
  model <- feols(
    as.formula(sprintf("%s ~ older_2015:post | ID + year", sp$outcome)),
    data = dat,
    cluster = ~ID
  )
  charls_models[[paste0("charls_", sp$outcome)]] <- model
  charls_results[[length(charls_results) + 1L]] <- tidy_term(
    model, "older_2015:post|post:older_2015",
    "CHARLS target-group period change", sp$label,
    sp$estimand,
    "Descriptive/associational",
    "Individual and year fixed effects; coefficient is an age-group differential period change, not a policy causal effect."
  )
}

# Event-study diagnostics use repeated prevalence/score, not future baseline-free
# selection. This avoids conditioning pre-trend diagnostics on a 2015 outcome.
charls_events <- list()
charls_pretests <- list()
for (sp in list(
  list(outcome = "any_adl", label = "ADL prevalence"),
  list(outcome = "poor_srh", label = "Poor self-rated health"),
  list(outcome = "depression", label = "Depression prevalence"),
  list(outcome = "cesd10", label = "CESD-10 score"),
  list(outcome = "cognition", label = "Cognitive score (0-19)")
)) {
  dat <- charls_panel |> filter(!is.na(.data[[sp$outcome]]))
  model <- feols(
    as.formula(sprintf("%s ~ i(year, older_2015, ref = 2015) | ID + year",
                       sp$outcome)),
    data = dat,
    cluster = ~ID
  )
  charls_models[[paste0("charls_event_", sp$outcome)]] <- model
  charls_events[[length(charls_events) + 1L]] <- event_terms(
    model, "CHARLS event-study diagnostic", sp$label,
    "Age 75+ vs 65-74 years in 2015 x survey year",
    "Diagnostic only", 2015
  )
  charls_pretests[[length(charls_pretests) + 1L]] <- joint_pretrend(
    model, c(2011, 2013), paste0("CHARLS ", sp$label, " joint pre-trend")
  )
}

# DDD: effect modification of the age-group period contrast. Baseline chronic
# conditions are not randomized, so these are heterogeneity estimates only.
ddd_conditions <- c(
  baseline_diabetes = "Diabetes",
  baseline_cvd = "Any cardiovascular disease",
  baseline_heart_disease = "Heart disease",
  baseline_hypertension = "Hypertension",
  baseline_lung_disease = "Lung disease"
)
charls_ddd <- list()
for (v in names(ddd_conditions)) {
  dat <- charls_panel |>
    filter(baseline_adl == 0, !is.na(.data[[v]])) |>
    complete_two_wave("any_adl") |>
    mutate(condition = .data[[v]])
  model <- feols(
    any_adl ~ older_2015:post + condition:post +
      older_2015:condition:post | ID + year,
    data = dat,
    cluster = ~ID
  )
  charls_models[[paste0("charls_ddd_", v)]] <- model
  res <- tidy_term(
    model, "older_2015:condition:post|older_2015:post:condition|condition:older_2015:post",
    "CHARLS DDD heterogeneity", "Incident ADL",
    paste0("Triple difference by baseline ", ddd_conditions[[v]]),
    "Exploratory effect modification",
    "Baseline condition is observational; estimate does not identify a condition-specific policy effect."
  )
  res$modifier <- ddd_conditions[[v]]
  charls_ddd[[length(charls_ddd) + 1L]] <- res
}

# Attrition IPCW sensitivity for incident ADL.
charls_baseline_ipcw <- charls_panel |>
  filter(year == 2015, baseline_adl == 0) |>
  distinct(ID, .keep_all = TRUE)
retained_ids <- charls_panel |>
  filter(year == 2018, !is.na(any_adl)) |>
  pull(ID)
retained <- as.integer(charls_baseline_ipcw$ID %in% retained_ids)
ipcw_obj <- make_ipcw(
  charls_baseline_ipcw, retained,
  c("age_2015", "female", "baseline_cesd10", "baseline_cognition",
    "baseline_smoke", "baseline_drink", "baseline_hypertension",
    "baseline_heart_disease", "baseline_stroke", "baseline_lung_disease",
    "baseline_diabetes")
)
charls_baseline_ipcw$ipcw <- ipcw_obj$weights
ipcw_dat <- charls_panel |>
  filter(baseline_adl == 0) |>
  complete_two_wave("any_adl") |>
  left_join(charls_baseline_ipcw[, c("ID", "ipcw")], by = "ID")
ipcw_model <- feols(
  any_adl ~ older_2015:post | ID + year,
  data = ipcw_dat,
  weights = ~ipcw,
  cluster = ~ID
)
charls_models$charls_incident_adl_ipcw <- ipcw_model
charls_results[[length(charls_results) + 1L]] <- tidy_term(
  ipcw_model, "older_2015:post|post:older_2015",
  "CHARLS target-group period change", "Incident ADL (IPCW)",
  "IPCW difference-in-differences risk difference",
  "Sensitivity analysis",
  "Stabilized inverse-probability-of-observation weights, trimmed at the 1st and 99th percentiles."
)
fwrite(ipcw_obj$diagnostics, file.path(path_diag, "charls_ipcw_diagnostics.csv"))

# -------------------------------------------------------------------------
# CFPS: pilot-area DID and DDD
# -------------------------------------------------------------------------

cat("[2] Reading CFPS derived cohorts\n")
cfps_elderly_path <- file.path(project_root, "05_analysis_data",
                               "cfps_elderly_cohort.csv.gz")
stopifnot(file.exists(cfps_elderly_path))

cfps_elderly <- fread(cfps_elderly_path, encoding = "UTF-8") |>
  mutate(
    pid = as.character(pid),
    wave = as.integer(wave),
    city_cluster = floor(admin_code / 100) * 100,
    poor_srh_corrected = ifelse(is.na(srh), NA_real_, as.numeric(srh <= 2)),
    activity_limitation_corrected = ifelse(
      is.na(dw_clean), NA_real_, as.numeric(dw_clean <= 2)
    ),
    high_need_age75 = as.integer(baseline_age >= 75),
    high_need_combined = ifelse(
      baseline_age >= 75, 1,
      ifelse(is.na(baseline_dw), NA_real_, as.numeric(baseline_dw <= 2))
    ),
    treat_high_age75 = treat * high_need_age75,
    treat_high_combined = treat * high_need_combined
  ) |>
  filter(baseline_age >= 65)

cfps_elderly_main <- cfps_elderly |>
  filter(wave %in% c(2012, 2014, 2018))

cfps_flow <- cfps_elderly |>
  group_by(wave, treat) |>
  summarise(
    n_id = n_distinct(pid),
    srh_observed = sum(!is.na(poor_srh_corrected)),
    limitation_observed = sum(!is.na(activity_limitation_corrected)),
    .groups = "drop"
  )
fwrite(cfps_flow, file.path(path_diag, "cfps_sample_flow.csv"))

cfps_models <- list()
cfps_results <- list()
cfps_events <- list()
cfps_pretests <- list()

cfps_health_event <- feols(
  poor_srh_corrected ~ i(wave, treat, ref = 2014) | pid + wave,
  data = cfps_elderly_main,
  cluster = ~city_cluster
)
cfps_models$cfps_health_event <- cfps_health_event
cfps_events[[1]] <- event_terms(
  cfps_health_event, "CFPS pilot-area event study", "Poor self-rated health",
  "Pilot area x survey year", "Diagnostic/associational", 2014
)
cfps_pretests[[1]] <- joint_pretrend(
  cfps_health_event, 2012, "CFPS poor self-rated health pre-trend"
)

cfps_health_did_dat <- cfps_elderly |>
  filter(wave %in% c(2014, 2018)) |>
  mutate(post = as.integer(wave == 2018))
cfps_health_did <- feols(
  poor_srh_corrected ~ treat:post | pid + wave,
  data = cfps_health_did_dat,
  cluster = ~city_cluster
)
cfps_models$cfps_health_did <- cfps_health_did
cfps_results[[1]] <- tidy_term(
  cfps_health_did, "treat:post|post:treat",
  "CFPS pilot-area DID", "Poor self-rated health",
  "Pilot-area difference-in-differences risk difference",
  "Exploratory/associational",
  "Individual and year fixed effects; city-clustered SE. Causal interpretation depends on parallel trends, stable composition, no spillover, and correct pilot mapping."
)

for (spec in list(
  list(high = "high_need_age75", th = "treat_high_age75",
       label = "Age 75+ at baseline"),
  list(high = "high_need_combined", th = "treat_high_combined",
       label = "Age 75+ or baseline activity limitation")
)) {
  dat <- cfps_elderly_main |> filter(!is.na(.data[[spec$high]]))
  form <- as.formula(sprintf(
    "poor_srh_corrected ~ i(wave, treat, ref=2014) + i(wave, %s, ref=2014) + i(wave, %s, ref=2014) | pid + wave",
    spec$high, spec$th
  ))
  model <- feols(form, data = dat, cluster = ~city_cluster)
  cfps_models[[paste0("cfps_health_ddd_", spec$high)]] <- model
  ev <- event_terms(
    model, "CFPS pilot-area DDD event study", "Poor self-rated health",
    paste0("Pilot area x ", spec$label, " x survey year"),
    "Exploratory triple difference", 2014
  )
  # event_terms also sees the two lower-order i() families. Retain only the
  # treat-high interaction family for the triple-difference series.
  ev <- ev[ev$term == "Reference" | grepl(spec$th, ev$term, fixed = TRUE), ]
  cfps_events[[length(cfps_events) + 1L]] <- ev
  cfps_pretests[[length(cfps_pretests) + 1L]] <- joint_pretrend(
    model, 2012, paste0("CFPS DDD pre-trend: ", spec$label)
  )
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  target <- ct[grepl("::2018", ct$term) &
                 grepl(spec$th, ct$term, fixed = TRUE), , drop = FALSE]
  if (nrow(target) == 1L) {
    est <- target[[1]][1]
    se_ <- target[[2]][1]
    cfps_results[[length(cfps_results) + 1L]] <- data.frame(
      analysis = "CFPS pilot-area DDD",
      outcome = "Poor self-rated health",
      estimand = paste0("2018 triple difference: ", spec$label),
      term = target$term,
      estimate = est,
      std_error = se_,
      conf_low = est - 1.96 * se_,
      conf_high = est + 1.96 * se_,
      p_value = target[[4]][1],
      n_obs = nobs(model),
      n_id = n_distinct(dat$pid),
      evidence_grade = "Exploratory triple difference",
      notes = "One independent pre-policy DDD contrast only; multiplicity and measurement limitations apply.",
      stringsAsFactors = FALSE
    )
  }
}

# -------------------------------------------------------------------------
# Descriptive trends and database provenance
# -------------------------------------------------------------------------

charls_trends <- charls_panel |>
  group_by(year, older_2015) |>
  summarise(
    n = n_distinct(ID),
    adl_prevalence = mean(any_adl, na.rm = TRUE),
    poor_srh_prevalence = mean(poor_srh, na.rm = TRUE),
    depression_prevalence = mean(depression, na.rm = TRUE),
    cesd10_mean = mean(cesd10, na.rm = TRUE),
    cognition_mean = mean(cognition, na.rm = TRUE),
    .groups = "drop"
  )

cfps_health_trends <- cfps_elderly_main |>
  group_by(wave, treat) |>
  summarise(
    n = n_distinct(pid),
    poor_srh = mean(poor_srh_corrected, na.rm = TRUE),
    activity_limitation = mean(activity_limitation_corrected, na.rm = TRUE),
    .groups = "drop"
  )

database_inventory <- data.frame(
  database = c("CHARLS", "CFPS", "CLDS", "CLASS", "CHFS", "Provincial GDP"),
  configured_or_referenced = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  data_accessible = c(
    file.exists(charls_path),
    dir.exists("E:/公共数据库/中国数据库/CFPS"),
    dir.exists("E:/公共数据库/中国数据库/CLDS"),
    dir.exists("E:/公共数据库/中国数据库/CLASS数据全"),
    dir.exists("E:/公共数据库/中国数据库/CHFS"),
    dir.exists("E:/公共数据库/中国数据库/31省名义、实际GDP及GDP平减指数数据（2000-2024年）")
  ),
  role_in_corrected_pipeline = c(
    "Nationwide target-group differential-change analysis; event-study diagnostics; DDD heterogeneity; trajectory input",
    "Pilot-area DID and DDD for poor self-rated health",
    "Existing code performs availability/geography audit only; no defensible outcome model in current workspace",
    "Primary post-policy longitudinal validation (2018-2023) and depressive-symptom trajectory classification",
    "Configured in legacy R settings but not used in any verified model",
    "Configured in legacy R settings but not linked to a verified model"
  ),
  causal_status = c(
    "No national untreated geographic control; age-group estimates are period differences, not identified policy effects",
    "Potential quasi-experiment, but interpretation is conditional on pre-trends, mapping, no spillover, and outcome comparability",
    "Not analyzed",
    "Primary longitudinal validation; all analyzed waves are post-policy, so no policy DID is identified",
    "Not analyzed",
    "Not analyzed"
  ),
  stringsAsFactors = FALSE
)

main_results <- bind_rows(charls_results, cfps_results) |>
  mutate(
    fdr_family = case_when(
      analysis == "CFPS pilot-area DDD" &
        outcome == "Poor self-rated health" ~
        "CFPS poor-self-rated-health high-need DDD",
      TRUE ~ NA_character_
    ),
    p_fdr = NA_real_
  )
for (family_name in na.omit(unique(main_results$fdr_family))) {
  idx <- which(main_results$fdr_family == family_name)
  main_results$p_fdr[idx] <- p.adjust(main_results$p_value[idx],
                                      method = "BH")
}
charls_event_results <- bind_rows(charls_events)
cfps_event_results <- bind_rows(cfps_events)
pretrend_results <- bind_rows(charls_pretests, cfps_pretests)
charls_ddd_results <- bind_rows(charls_ddd) |>
  mutate(
    fdr_family = "CHARLS incident-ADL chronic-condition modifiers",
    p_fdr = p.adjust(p_value, method = "BH"),
    fdr_significant = p_fdr < 0.05
  )

fwrite(main_results, file.path(path_tables, "r_corrected_main_results.csv"))
fwrite(charls_event_results, file.path(path_tables, "r_charls_event_study.csv"))
fwrite(cfps_event_results, file.path(path_tables, "r_cfps_event_study.csv"))
fwrite(pretrend_results, file.path(path_diag, "r_parallel_trend_tests.csv"))
fwrite(charls_ddd_results, file.path(path_tables, "r_charls_ddd_results.csv"))
fwrite(charls_trends, file.path(path_tables, "r_charls_raw_trends.csv"))
fwrite(cfps_health_trends, file.path(path_tables, "r_cfps_health_raw_trends.csv"))
fwrite(database_inventory, file.path(path_tables, "r_database_inventory.csv"))

saveRDS(
  list(
    charls = charls_models,
    cfps = cfps_models,
    session = sessionInfo()
  ),
  file.path(path_models, "r_corrected_models.rds")
)

writeLines(capture.output(sessionInfo()),
           file.path(path_logs, "r_session_info.txt"), useBytes = TRUE)

cat("\nMain results\n")
print(main_results)
cat("\nPre-trend tests\n")
print(pretrend_results)
cat("\nCompleted:", format(Sys.time()), "\n")
