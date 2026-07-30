#!/usr/bin/env Rscript

# Unified health-equity, recovery-opportunity, and high-need analysis
# Databases: CFPS, CHARLS, CLASS
# Primary common scale: standardized absolute probability, percentage-point
# difference, and events per 1,000 persons.

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(nnet)
})

options(encoding = "UTF-8", stringsAsFactors = FALSE)
set.seed(20260730)

project_root <- Sys.getenv("CHINA_POLICY_DID_ROOT")
if (!nzchar(project_root)) {
  stop("Set CHINA_POLICY_DID_ROOT to the local project root.")
}
markov_root <- file.path(project_root, "Markov")
out_dir <- Sys.getenv(
  "V7_OUTPUT_DIR",
  unset = file.path(markov_root, "analysis_v7")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(out_dir, "00_analysis_log.txt")
log_con <- file(log_path, open = "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("Unified health-equity analysis\n")
cat("Started:", format(Sys.time()), "\n")
cat("Output:", out_dir, "\n\n")

safe_num <- function(x) suppressWarnings(as.numeric(x))
clip01 <- function(x) pmin(pmax(x, 0), 1)

prob_ci <- function(p, se) {
  data.table(
    probability = p,
    conf_low = clip01(p - 1.96 * se),
    conf_high = clip01(p + 1.96 * se)
  )
}

std_binomial <- function(model, newdata, averaging_weight = NULL) {
  tt <- delete.response(terms(model))
  x <- model.matrix(tt, newdata)
  b <- coef(model)
  p <- plogis(drop(x %*% b))
  if (is.null(averaging_weight)) averaging_weight <- rep(1, length(p))
  averaging_weight[!is.finite(averaging_weight) | averaging_weight <= 0] <- 0
  averaging_weight <- averaging_weight / sum(averaging_weight)
  est <- sum(averaging_weight * p)
  grad <- colSums(x * (averaging_weight * p * (1 - p)))
  se <- sqrt(drop(t(grad) %*% vcov(model) %*% grad))
  c(probability = est, std_error = se,
    conf_low = clip01(est - 1.96 * se),
    conf_high = clip01(est + 1.96 * se))
}

binary_margin_table <- function(model, target, time_var, time_values,
                                focal_specs, database, outcome,
                                design_role, averaging_weight_var = NULL,
                                extra_set = list()) {
  rows <- list()
  k <- 0L
  aw <- if (is.null(averaging_weight_var)) NULL else target[[averaging_weight_var]]

  add_row <- function(time_value, dimension, level, focal_set) {
    nd <- copy(target)
    nd[[time_var]] <- factor(time_value, levels = levels(model.frame(model)[[time_var]]))
    for (nm in names(extra_set)) nd[[nm]] <- extra_set[[nm]]
    for (nm in names(focal_set)) nd[[nm]] <- focal_set[[nm]]
    z <- std_binomial(model, nd, aw)
    k <<- k + 1L
    rows[[k]] <<- data.table(
      database = database,
      design_role = design_role,
      outcome = outcome,
      time = as.character(time_value),
      dimension = dimension,
      level = level,
      probability = unname(z["probability"]),
      std_error = unname(z["std_error"]),
      conf_low = unname(z["conf_low"]),
      conf_high = unname(z["conf_high"])
    )
  }

  for (tv in time_values) {
    add_row(tv, "Overall", "Overall", list())
    for (dimension in names(focal_specs)) {
      for (level in names(focal_specs[[dimension]])) {
        add_row(tv, dimension, level, focal_specs[[dimension]][[level]])
      }
    }
  }
  ans <- rbindlist(rows, fill = TRUE)
  ans[, `:=`(
    percentage = 100 * probability,
    percentage_points = 100 * probability,
    events_per_1000 = 1000 * probability,
    conf_low_per_1000 = 1000 * conf_low,
    conf_high_per_1000 = 1000 * conf_high
  )]
  ans[]
}

equity_gaps_binary <- function(tab) {
  contrasts <- data.table(
    dimension = c("Age", "Residence", "Education", "Sex", "Baseline high need"),
    exposed = c("75+", "Rural", "Primary or less", "Female", "High need"),
    reference = c("65-74", "Urban/town", "Middle school or higher", "Male", "Not high need")
  )
  out <- list()
  k <- 0L
  for (i in seq_len(nrow(contrasts))) {
    cc <- contrasts[i]
    a <- tab[dimension == cc$dimension & level == cc$exposed]
    b <- tab[dimension == cc$dimension & level == cc$reference]
    if (!nrow(a) || !nrow(b)) next
    merge_keys <- c("database", "design_role", "outcome", "time", "dimension")
    if ("exposure_group" %in% names(tab)) merge_keys <- c(merge_keys, "exposure_group")
    m <- merge(
      a, b,
      by = merge_keys,
      suffixes = c("_exposed", "_reference")
    )
    if (!nrow(m)) next
    k <- k + 1L
    out[[k]] <- m[, .(
      database, design_role, outcome, time, dimension,
      exposure_group = if ("exposure_group" %in% names(m)) exposure_group else NA_character_,
      contrast = paste0(level_exposed, " minus ", level_reference),
      probability_exposed, probability_reference,
      percentage_point_difference = 100 * (probability_exposed - probability_reference),
      events_per_1000_difference = 1000 * (probability_exposed - probability_reference)
    )]
  }
  if (!length(out)) return(data.table())
  rbindlist(out, fill = TRUE)
}

# -------------------------------------------------------------------------
# 1. Harmonized estimand dictionary
# -------------------------------------------------------------------------

dictionary <- data.table(
  database = c("CFPS", "CHARLS", "CLASS"),
  native_primary_outcome = c(
    "Poor self-rated health",
    "Low/intermediate/high health-deficit state plus death",
    "Current ADL-help requirement"
  ),
  common_probability_estimand = c(
    "Model-standardized probability of poor self-rated health",
    "IPCW model-standardized absolute transition probability",
    "Model-standardized probability of ADL-help requirement"
  ),
  baseline_high_need_rule = c(
    "Age >=75 years (common cross-database core definition)",
    "Age >=75 years (common cross-database core definition)",
    "Not longitudinally identifiable; current ADL-help requirement is the need indicator"
  ),
  age_groups = "65-74; 75+",
  residence_groups = "Urban/town; Rural",
  education_groups = "Primary or less; Middle school or higher",
  sex_groups = "Male; Female",
  common_output_units = "Probability; percentage points; events per 1,000",
  comparability_boundary = c(
    "Health-need probability; not a direct care-use measure",
    "Health-state mobility, recovery opportunity, high-deficit care need, and mortality",
    "Direct current care-help need; repeated cross-section because IDs are not linkable"
  )
)
fwrite(dictionary, file.path(out_dir, "01_harmonized_estimand_dictionary.csv"))

# -------------------------------------------------------------------------
# 2. CFPS: standardized absolute poor-SRH probabilities
# -------------------------------------------------------------------------

cat("[1] CFPS standardized probabilities\n")
cfps_path <- file.path(project_root, "05_analysis_data", "cfps_elderly_cohort.csv.gz")
stopifnot(file.exists(cfps_path))
cfps <- fread(cfps_path)
cfps <- cfps[baseline_age >= 65 & wave %in% c(2014, 2018)]
cfps[, poor_srh_u := fifelse(is.na(srh), NA_real_, as.numeric(srh <= 2))]

# The derived 'female' field is invalid (all zero) because source gen is 0/1.
# In the source files gen=0 denotes female and gen=1 denotes male.
cfps[, female_u := fifelse(gen %in% c(0, 1), as.integer(gen == 0), NA_integer_)]
cfps[, age_group := factor(
  fifelse(baseline_age >= 75, "75+", "65-74"),
  levels = c("65-74", "75+")
)]
cfps[, rural_group := factor(
  fifelse(rural == 1, "Rural", fifelse(rural == 0, "Urban/town", NA_character_)),
  levels = c("Urban/town", "Rural")
)]
cfps[, education_group := factor(
  fifelse(educ <= 6, "Primary or less",
          fifelse(educ > 6, "Middle school or higher", NA_character_)),
  levels = c("Middle school or higher", "Primary or less")
)]
cfps[, sex_group := factor(
  fifelse(female_u == 1, "Female", fifelse(female_u == 0, "Male", NA_character_)),
  levels = c("Male", "Female")
)]
cfps[, wave_f := factor(wave, levels = c(2014, 2018))]
cfps[, treat_f := factor(treat, levels = c(0, 1), labels = c("Control", "Pilot"))]

cfps_model_dat <- cfps[complete.cases(
  poor_srh_u, wave_f, treat_f, age_group, rural_group,
  education_group, sex_group
)]
cfps_model <- glm(
  poor_srh_u ~ wave_f * treat_f + age_group + rural_group +
    education_group + sex_group,
  family = binomial(), data = cfps_model_dat
)

cfps_specs <- list(
  Age = list(
    `65-74` = list(age_group = factor("65-74", levels = levels(cfps_model_dat$age_group))),
    `75+` = list(age_group = factor("75+", levels = levels(cfps_model_dat$age_group)))
  ),
  Residence = list(
    `Urban/town` = list(rural_group = factor("Urban/town", levels = levels(cfps_model_dat$rural_group))),
    Rural = list(rural_group = factor("Rural", levels = levels(cfps_model_dat$rural_group)))
  ),
  Education = list(
    `Middle school or higher` = list(education_group = factor("Middle school or higher", levels = levels(cfps_model_dat$education_group))),
    `Primary or less` = list(education_group = factor("Primary or less", levels = levels(cfps_model_dat$education_group)))
  ),
  Sex = list(
    Male = list(sex_group = factor("Male", levels = levels(cfps_model_dat$sex_group))),
    Female = list(sex_group = factor("Female", levels = levels(cfps_model_dat$sex_group)))
  )
)

cfps_abs <- rbindlist(lapply(c("Control", "Pilot"), function(tr) {
  binary_margin_table(
    cfps_model, cfps_model_dat, "wave_f", c("2014", "2018"),
    cfps_specs, "CFPS", "Poor self-rated health",
    "Pilot-area panel; standardized absolute probability",
    extra_set = list(
      treat_f = factor(tr, levels = levels(cfps_model_dat$treat_f))
    )
  )[, exposure_group := tr]
}), fill = TRUE)
fwrite(cfps_abs, file.path(out_dir, "02_CFPS_standardized_absolute_probabilities.csv"))

cfps_gaps <- equity_gaps_binary(cfps_abs)

# Preserve the previously estimated policy contrasts, but translate them.
existing_main <- fread(file.path(markov_root, "data", "r_corrected_main_results.csv"))
cfps_effects <- existing_main[
  grepl("^CFPS", analysis) &
    !grepl("activity limitation|high_combined", paste(estimand, term), ignore.case = TRUE),
  .(
  database = "CFPS",
  analysis, outcome, estimand, term,
  probability_difference = estimate,
  percentage_point_difference = 100 * estimate,
  conf_low_pp = 100 * conf_low,
  conf_high_pp = 100 * conf_high,
  events_per_1000_difference = 1000 * estimate,
  conf_low_per_1000 = 1000 * conf_low,
  conf_high_per_1000 = 1000 * conf_high,
  p_value, evidence_grade, notes
)]
fwrite(cfps_effects, file.path(out_dir, "03_CFPS_policy_differences_per_1000.csv"))

# -------------------------------------------------------------------------
# 3. CLASS: all four waves, repeated cross-sectional ADL-help probabilities
# -------------------------------------------------------------------------

cat("[2] CLASS four-wave standardized ADL-help probabilities\n")
class_root <- Sys.getenv("CLASS_DATA_ROOT")
if (!nzchar(class_root)) {
  stop("Set CLASS_DATA_ROOT to the directory containing CLASS Stata files.")
}
class_paths <- c(
  `2016` = file.path(class_root, "2016class-individual-发布版.dta"),
  `2018` = file.path(class_root, "CLASS2018-cleaned release.dta"),
  `2020` = file.path(class_root, "individual -2020  cleaned for user.dta"),
  `2023` = file.path(class_root, "2023_individual_release_weighted .dta")
)
stopifnot(all(file.exists(class_paths)))

extract_class_wave <- function(year, path) {
  d <- read_dta(path)
  if (year == 2016) {
    sex <- safe_num(d[["a1"]])
    birth <- safe_num(d[["a2_1_open"]])
    edu <- safe_num(d[["a4"]])
    residence <- safe_num(d[["q5a"]])
    adl <- safe_num(d[["b5"]])
    wt <- rep(1, nrow(d))
  } else if (year == 2018) {
    sex <- safe_num(d[["a1"]])
    birth <- safe_num(d[["a2__1__open"]])
    edu <- safe_num(d[["a3"]])
    residence <- safe_num(d[["q5a"]])
    adl <- safe_num(d[["b5"]])
    wt <- rep(1, nrow(d))
  } else {
    sex <- safe_num(d[["A1"]])
    birth <- safe_num(d[["A2_1_open"]])
    edu <- safe_num(d[["A3"]])
    residence <- safe_num(d[["Q5a"]])
    adl <- safe_num(d[["B5"]])
    wt <- if (year == 2023) safe_num(d[["weight"]]) else rep(1, nrow(d))
  }
  ans <- data.table(
    year = year,
    age = year - birth,
    female_u = fifelse(sex == 2, 1L, fifelse(sex == 1, 0L, NA_integer_)),
    rural_u = fifelse(residence == 5, 1L,
                      fifelse(residence %in% 1:4, 0L, NA_integer_)),
    low_education = fifelse(edu %in% 1:3, 1L,
                            fifelse(edu %in% 4:7, 0L, NA_integer_)),
    adl_help = fifelse(adl == 1, 1, fifelse(adl == 2, 0, NA_real_)),
    analysis_weight = wt
  )
  ans[!is.finite(analysis_weight) | analysis_weight <= 0, analysis_weight := NA_real_]
  ans[, analysis_weight := analysis_weight / mean(analysis_weight, na.rm = TRUE)]
  ans
}

class_dt <- rbindlist(lapply(names(class_paths), function(y) {
  extract_class_wave(as.integer(y), class_paths[[y]])
}), fill = TRUE)
class_dt <- class_dt[age >= 65 & age <= 110]
class_dt[, age_group := factor(
  fifelse(age >= 75, "75+", "65-74"), levels = c("65-74", "75+")
)]
class_dt[, rural_group := factor(
  fifelse(rural_u == 1, "Rural", fifelse(rural_u == 0, "Urban/town", NA_character_)),
  levels = c("Urban/town", "Rural")
)]
class_dt[, education_group := factor(
  fifelse(low_education == 1, "Primary or less",
          fifelse(low_education == 0, "Middle school or higher", NA_character_)),
  levels = c("Middle school or higher", "Primary or less")
)]
class_dt[, sex_group := factor(
  fifelse(female_u == 1, "Female", fifelse(female_u == 0, "Male", NA_character_)),
  levels = c("Male", "Female")
)]
class_dt[, year_f := factor(year, levels = c(2016, 2018, 2020, 2023))]

class_model_dat <- class_dt[complete.cases(
  adl_help, year_f, age_group, rural_group, education_group, sex_group,
  analysis_weight
)]
class_model <- glm(
  adl_help ~ year_f + age_group + rural_group + education_group + sex_group,
  family = binomial(), data = class_model_dat, weights = analysis_weight
)

class_specs <- cfps_specs[c("Age", "Residence", "Education", "Sex")]
class_specs$Age$`65-74`$age_group <- factor("65-74", levels = levels(class_model_dat$age_group))
class_specs$Age$`75+`$age_group <- factor("75+", levels = levels(class_model_dat$age_group))
class_specs$Residence$`Urban/town`$rural_group <- factor("Urban/town", levels = levels(class_model_dat$rural_group))
class_specs$Residence$Rural$rural_group <- factor("Rural", levels = levels(class_model_dat$rural_group))
class_specs$Education$`Middle school or higher`$education_group <- factor("Middle school or higher", levels = levels(class_model_dat$education_group))
class_specs$Education$`Primary or less`$education_group <- factor("Primary or less", levels = levels(class_model_dat$education_group))
class_specs$Sex$Male$sex_group <- factor("Male", levels = levels(class_model_dat$sex_group))
class_specs$Sex$Female$sex_group <- factor("Female", levels = levels(class_model_dat$sex_group))

class_abs <- binary_margin_table(
  class_model, class_model_dat, "year_f", c("2016", "2018", "2020", "2023"),
  class_specs, "CLASS", "Current ADL-help requirement",
  "Repeated cross-section; standardized absolute care-need probability",
  averaging_weight_var = "analysis_weight"
)
fwrite(class_abs, file.path(out_dir, "04_CLASS_four_wave_standardized_ADL_probabilities.csv"))
class_gaps <- equity_gaps_binary(class_abs)

# -------------------------------------------------------------------------
# 4. CHARLS transition file, death state, and IPCW
# -------------------------------------------------------------------------

cat("[3] CHARLS transition construction, death state, and IPCW\n")
fi_path <- file.path(project_root, "CHARLS_wave_specific_continuous_FI.csv")
charls_path <- Sys.getenv("CHARLS_HARMONIZED_DTA")
if (!nzchar(charls_path)) {
  stop("Set CHARLS_HARMONIZED_DTA to the harmonized CHARLS Stata file.")
}
stopifnot(file.exists(fi_path), file.exists(charls_path))

fi <- fread(fi_path)
fi <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE & !is.na(fi_primary)]
fi[, ID := as.character(ID)]
fi[, state := fifelse(fi_primary < 0.10, 1L,
                      fifelse(fi_primary < 0.25, 2L, 3L))]

raw <- read_dta(charls_path, col_select = c(
  ID, ragender, raeduc_c,
  h1rural, h2rural, h3rural,
  r1adla_c, r2adla_c, r3adla_c,
  r2iwstat, r3iwstat, r4iwstat
))
cov <- data.table(
  ID = as.character(raw[["ID"]]),
  female_u = fifelse(safe_num(raw[["ragender"]]) == 2, 1L,
                     fifelse(safe_num(raw[["ragender"]]) == 1, 0L, NA_integer_)),
  education_code = safe_num(raw[["raeduc_c"]]),
  rural_2011 = safe_num(raw[["h1rural"]]),
  rural_2013 = safe_num(raw[["h2rural"]]),
  rural_2015 = safe_num(raw[["h3rural"]]),
  adl_2011 = safe_num(raw[["r1adla_c"]]),
  adl_2013 = safe_num(raw[["r2adla_c"]]),
  adl_2015 = safe_num(raw[["r3adla_c"]]),
  status_2013 = safe_num(raw[["r2iwstat"]]),
  status_2015 = safe_num(raw[["r3iwstat"]]),
  status_2018 = safe_num(raw[["r4iwstat"]])
)

intervals <- data.table(
  from_wave = c(2011L, 2013L, 2015L),
  to_wave = c(2013L, 2015L, 2018L),
  interval = c("2011-2013", "2013-2015", "2015-2018"),
  interval_years = c(2, 2, 3),
  policy_period = c("Pre-expansion", "Pre-expansion", "Expansion"),
  rural_var = c("rural_2011", "rural_2013", "rural_2015"),
  adl_var = c("adl_2011", "adl_2013", "adl_2015"),
  status_var = c("status_2013", "status_2015", "status_2018")
)

transition_rows <- list()
for (i in seq_len(nrow(intervals))) {
  ii <- intervals[i]
  from <- fi[wave == ii$from_wave, .(
    ID, age_from = age_at_wave, female_fi = female, fi_from = fi_primary,
    from_state = state
  )]
  to <- fi[wave == ii$to_wave, .(ID, to_state_living = state)]
  dd <- merge(from, cov, by = "ID", all.x = TRUE)
  dd <- merge(dd, to, by = "ID", all.x = TRUE)
  dd[, `:=`(
    from_wave = ii$from_wave,
    to_wave = ii$to_wave,
    interval = ii$interval,
    interval_years = ii$interval_years,
    policy_period = ii$policy_period,
    rural_u = get(ii$rural_var),
    baseline_adl = get(ii$adl_var),
    next_status = get(ii$status_var)
  )]
  dd[, death := as.integer(next_status == 5)]
  dd[, resolved := as.integer(!is.na(to_state_living) | death == 1)]
  dd[, to_state := fifelse(!is.na(to_state_living), to_state_living,
                           fifelse(death == 1, 4L, NA_integer_))]
  transition_rows[[i]] <- dd
}
trans <- rbindlist(transition_rows, fill = TRUE)
trans[, female_u := fcoalesce(female_u, female_fi)]
trans[, low_education := fifelse(education_code %in% 1:4, 1L,
                                 fifelse(education_code %in% 5:10, 0L, NA_integer_))]
trans[, age_group := factor(
  fifelse(age_from >= 75, "75+", "65-74"), levels = c("65-74", "75+")
)]
trans[, rural_group := factor(
  fifelse(rural_u == 1, "Rural", fifelse(rural_u == 0, "Urban/town", NA_character_)),
  levels = c("Urban/town", "Rural")
)]
trans[, education_group := factor(
  fifelse(low_education == 1, "Primary or less",
          fifelse(low_education == 0, "Middle school or higher", NA_character_)),
  levels = c("Middle school or higher", "Primary or less")
)]
trans[, sex_group := factor(
  fifelse(female_u == 1, "Female", fifelse(female_u == 0, "Male", NA_character_)),
  levels = c("Male", "Female")
)]
trans[, baseline_high_need := factor(
  fifelse(age_from >= 75,
          "High need", "Not high need"),
  levels = c("Not high need", "High need")
)]
trans[, policy_period := factor(policy_period, levels = c("Pre-expansion", "Expansion"))]
trans[, from_state_f := factor(from_state, levels = 1:3)]

ipcw_dat <- trans[complete.cases(
  resolved, interval, from_state_f, age_from, age_group, rural_group,
  education_group, sex_group
)]
ipcw_den <- glm(
  resolved ~ factor(interval) + from_state_f + age_group + rural_group +
    education_group + sex_group,
  family = binomial(), data = ipcw_dat
)
ipcw_num <- glm(
  resolved ~ factor(interval),
  family = binomial(), data = ipcw_dat
)
ipcw_dat[, p_den := predict(ipcw_den, type = "response")]
ipcw_dat[, p_num := predict(ipcw_num, type = "response")]
ipcw_dat[, ipcw_raw := p_num / p_den]
trim <- quantile(ipcw_dat[resolved == 1, ipcw_raw], c(0.01, 0.99), na.rm = TRUE)
ipcw_dat[, ipcw := pmin(pmax(ipcw_raw, trim[1]), trim[2])]

ipcw_diag <- ipcw_dat[, .(
  n_baseline = .N,
  n_resolved = sum(resolved),
  n_living_state = sum(!is.na(to_state) & to_state %in% 1:3),
  n_death = sum(to_state == 4, na.rm = TRUE),
  n_unresolved = sum(resolved == 0),
  resolved_probability = mean(resolved),
  ipcw_mean_resolved = mean(ipcw[resolved == 1]),
  ipcw_min_resolved = min(ipcw[resolved == 1]),
  ipcw_max_resolved = max(ipcw[resolved == 1])
), by = .(interval, policy_period)]
ipcw_diag[, `:=`(trim_1pct = trim[1], trim_99pct = trim[2])]
fwrite(ipcw_diag, file.path(out_dir, "05_CHARLS_IPCW_and_death_diagnostics.csv"))

model_dat <- ipcw_dat[resolved == 1 & !is.na(to_state)]
model_dat[, to_state_f := factor(to_state, levels = 1:4)]
multistate_model <- multinom(
  to_state_f ~ from_state_f * policy_period + interval_years +
    age_group + rural_group + education_group + sex_group,
  weights = ipcw, data = model_dat, trace = FALSE, maxit = 1000
)
saveRDS(multistate_model, file.path(out_dir, "CHARLS_IPCW_four_state_multinomial_model.rds"))

std_multistate <- function(model, target, period_value, from_value,
                           dimension = "Overall", level = "Overall",
                           focal_set = list()) {
  nd <- copy(target)
  nd[, policy_period := factor(period_value, levels = levels(target$policy_period))]
  nd[, from_state_f := factor(from_value, levels = levels(target$from_state_f))]
  nd[, interval_years := 2]
  for (nm in names(focal_set)) nd[[nm]] <- focal_set[[nm]]
  pp <- predict(model, newdata = nd, type = "probs")
  if (is.null(dim(pp))) pp <- matrix(pp, nrow = 1)
  if (ncol(pp) < 4) {
    full <- matrix(0, nrow = nrow(pp), ncol = 4)
    colnames(full) <- as.character(1:4)
    full[, colnames(pp)] <- pp
    pp <- full
  }
  p <- colMeans(pp)
  data.table(
    period = period_value,
    horizon_years = 2,
    from_state = from_value,
    to_state = 1:4,
    dimension = dimension,
    level = level,
    probability = as.numeric(p),
    events_per_1000 = 1000 * as.numeric(p)
  )
}

charls_specs <- list(
  Age = list(
    `65-74` = list(age_group = factor("65-74", levels = levels(model_dat$age_group))),
    `75+` = list(age_group = factor("75+", levels = levels(model_dat$age_group)))
  ),
  Residence = list(
    `Urban/town` = list(rural_group = factor("Urban/town", levels = levels(model_dat$rural_group))),
    Rural = list(rural_group = factor("Rural", levels = levels(model_dat$rural_group)))
  ),
  Education = list(
    `Middle school or higher` = list(education_group = factor("Middle school or higher", levels = levels(model_dat$education_group))),
    `Primary or less` = list(education_group = factor("Primary or less", levels = levels(model_dat$education_group)))
  ),
  Sex = list(
    Male = list(sex_group = factor("Male", levels = levels(model_dat$sex_group))),
    Female = list(sex_group = factor("Female", levels = levels(model_dat$sex_group)))
  )
)

charls_prob_rows <- list()
k <- 0L
for (per in levels(model_dat$policy_period)) {
  for (from in 1:3) {
    k <- k + 1L
    charls_prob_rows[[k]] <- std_multistate(
      multistate_model, model_dat, per, from
    )
    for (dimension in names(charls_specs)) {
      for (level in names(charls_specs[[dimension]])) {
        k <- k + 1L
        charls_prob_rows[[k]] <- std_multistate(
          multistate_model, model_dat, per, from,
          dimension, level, charls_specs[[dimension]][[level]]
        )
      }
    }
  }
}
charls_probs <- rbindlist(charls_prob_rows)
state_labels <- c("Low deficit", "Intermediate deficit", "High deficit", "Death")
charls_probs[, `:=`(
  database = "CHARLS",
  from_state_label = state_labels[from_state],
  to_state_label = state_labels[to_state]
)]
setcolorder(charls_probs, c(
  "database", "period", "horizon_years", "dimension", "level",
  "from_state", "from_state_label", "to_state", "to_state_label",
  "probability", "events_per_1000"
))
fwrite(charls_probs, file.path(out_dir, "06_CHARLS_IPCW_absolute_transition_probabilities.csv"))

charls_contrasts <- data.table(
  dimension = c("Age", "Residence", "Education", "Sex"),
  exposed = c("75+", "Rural", "Primary or less", "Female"),
  reference = c("65-74", "Urban/town", "Middle school or higher", "Male")
)
transition_gaps <- list()
for (i in seq_len(nrow(charls_contrasts))) {
  cc <- charls_contrasts[i]
  a <- charls_probs[dimension == cc$dimension & level == cc$exposed]
  b <- charls_probs[dimension == cc$dimension & level == cc$reference]
  m <- merge(
    a, b,
    by = c("database", "period", "horizon_years", "dimension",
           "from_state", "from_state_label", "to_state", "to_state_label"),
    suffixes = c("_exposed", "_reference")
  )
  transition_gaps[[i]] <- m[, .(
    database, period, horizon_years, dimension,
    contrast = paste0(level_exposed, " minus ", level_reference),
    from_state, from_state_label, to_state, to_state_label,
    probability_exposed, probability_reference,
    percentage_point_difference = 100 * (probability_exposed - probability_reference),
    events_per_1000_difference = 1000 * (probability_exposed - probability_reference)
  )]
}
transition_gaps <- rbindlist(transition_gaps)
fwrite(transition_gaps, file.path(out_dir, "07_CHARLS_health_equity_transition_gaps.csv"))

# Recovery opportunity and deterioration/care-need summaries.
charls_summary <- charls_probs[, .(
  recovery_probability = sum(probability[to_state < from_state]),
  deterioration_probability = sum(probability[to_state > from_state & to_state <= 3]),
  high_deficit_probability = sum(probability[to_state == 3]),
  death_probability = sum(probability[to_state == 4])
), by = .(
  database, period, horizon_years, dimension, level,
  from_state, from_state_label
)]
charls_summary[, `:=`(
  recovery_per_1000 = 1000 * recovery_probability,
  deterioration_per_1000 = 1000 * deterioration_probability,
  high_deficit_per_1000 = 1000 * high_deficit_probability,
  deaths_per_1000 = 1000 * death_probability
)]
fwrite(charls_summary, file.path(out_dir, "08_CHARLS_recovery_and_care_need_per_1000.csv"))

# -------------------------------------------------------------------------
# 5. Care-demand scenario simulation per 1,000
# -------------------------------------------------------------------------

cat("[4] Scenario simulation\n")
exp_overall <- charls_probs[
  period == "Expansion" & dimension == "Overall" & level == "Overall"
]
P <- matrix(0, nrow = 4, ncol = 4)
for (i in 1:3) {
  P[i, ] <- exp_overall[from_state == i][order(to_state)]$probability
}
P[4, 4] <- 1
P <- P / rowSums(P)

adjust_matrix <- function(P, prevention = 0, recovery = 0) {
  Q <- P
  for (i in 1:3) {
    worse <- c(which(seq_len(4) > i & seq_len(4) <= 3), 4)
    worse <- unique(worse)
    removed <- sum(Q[i, worse] * prevention)
    Q[i, worse] <- Q[i, worse] * (1 - prevention)
    Q[i, i] <- Q[i, i] + removed

    better <- which(seq_len(4) < i)
    if (length(better) && recovery > 0) {
      desired <- sum(Q[i, better] * recovery)
      available <- Q[i, i]
      actual <- min(desired, available)
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

mat_power <- function(M, n) {
  ans <- diag(nrow(M))
  if (n == 0) return(ans)
  for (i in seq_len(n)) ans <- ans %*% M
  ans
}

fi_2015 <- fi[wave == 2015 & age_at_wave >= 65]
initial <- as.numeric(table(factor(fi_2015$state, levels = 1:3)))
initial <- c(initial / sum(initial), 0)

scenario_defs <- list(
  Observed = list(prevention = 0, recovery = 0),
  `Prevention 20%` = list(prevention = 0.20, recovery = 0),
  `Recovery 20%` = list(prevention = 0, recovery = 0.20),
  `Combined 20%` = list(prevention = 0.20, recovery = 0.20)
)
scenario_rows <- list()
k <- 0L
for (sc in names(scenario_defs)) {
  pars <- scenario_defs[[sc]]
  Q <- adjust_matrix(P, pars$prevention, pars$recovery)
  for (cycles in c(0, 1, 3, 5)) {
    dist <- drop(initial %*% mat_power(Q, cycles))
    k <- k + 1L
    scenario_rows[[k]] <- data.table(
      scenario = sc,
      horizon_years = 2 * cycles,
      low_deficit_per_1000 = 1000 * dist[1],
      intermediate_deficit_per_1000 = 1000 * dist[2],
      high_deficit_care_need_per_1000 = 1000 * dist[3],
      deaths_per_1000 = 1000 * dist[4],
      living_per_1000 = 1000 * sum(dist[1:3]),
      high_need_among_living_per_1000 = ifelse(
        sum(dist[1:3]) > 0, 1000 * dist[3] / sum(dist[1:3]), NA_real_
      )
    )
  }
}
scenarios <- rbindlist(scenario_rows)
observed_ref <- scenarios[scenario == "Observed", .(
  horizon_years,
  observed_high_need = high_deficit_care_need_per_1000,
  observed_deaths = deaths_per_1000
)]
scenarios <- merge(scenarios, observed_ref, by = "horizon_years", all.x = TRUE)
scenarios[, `:=`(
  high_need_difference_vs_observed_per_1000 =
    high_deficit_care_need_per_1000 - observed_high_need,
  death_difference_vs_observed_per_1000 =
    deaths_per_1000 - observed_deaths
)]
fwrite(scenarios, file.path(out_dir, "09_care_need_scenario_simulation_per_1000.csv"))

# -------------------------------------------------------------------------
# 6. Unified output and QA
# -------------------------------------------------------------------------

cat("[5] Unified tables and QA\n")
unified_abs <- rbindlist(list(
  cfps_abs[, .(
    database, design_role, outcome, time, exposure_group,
    dimension, level, probability, conf_low, conf_high,
    events_per_1000, conf_low_per_1000, conf_high_per_1000
  )],
  class_abs[, .(
    database, design_role, outcome, time,
    exposure_group = "Population",
    dimension, level, probability, conf_low, conf_high,
    events_per_1000, conf_low_per_1000, conf_high_per_1000
  )]
), fill = TRUE)
fwrite(unified_abs, file.path(out_dir, "10_unified_standardized_absolute_probabilities.csv"))

unified_gaps <- rbindlist(list(cfps_gaps, class_gaps), fill = TRUE)
fwrite(unified_gaps, file.path(out_dir, "11_unified_health_equity_probability_gaps.csv"))

qa <- data.table(
  check = c(
    "CFPS female variation restored",
    "CFPS standardized probability bounds",
    "CLASS contains four requested waves",
    "CLASS standardized probability bounds",
    "CHARLS transition rows sum to one",
    "CHARLS death state present",
    "CHARLS IPCW finite and positive",
    "Scenario rows sum to 1,000",
    "No CLASS longitudinal transition claim"
  ),
  passed = c(
    uniqueN(cfps_model_dat$sex_group) == 2,
    all(cfps_abs$probability >= 0 & cfps_abs$probability <= 1),
    identical(sort(unique(class_model_dat$year)), c(2016L, 2018L, 2020L, 2023L)),
    all(class_abs$probability >= 0 & class_abs$probability <= 1),
    all(abs(charls_probs[, sum(probability), by = .(
      period, dimension, level, from_state
    )]$V1 - 1) < 1e-8),
    any(charls_probs$to_state == 4 & charls_probs$probability > 0),
    all(is.finite(model_dat$ipcw) & model_dat$ipcw > 0),
    all(abs(
      scenarios$low_deficit_per_1000 +
        scenarios$intermediate_deficit_per_1000 +
        scenarios$high_deficit_care_need_per_1000 +
        scenarios$deaths_per_1000 - 1000
    ) < 1e-6),
    TRUE
  ),
  detail = c(
    paste("Female share:", round(mean(cfps_model_dat$sex_group == "Female"), 4)),
    paste("Range:", paste(round(range(cfps_abs$probability), 4), collapse = " to ")),
    paste(sort(unique(class_model_dat$year)), collapse = ", "),
    paste("Range:", paste(round(range(class_abs$probability), 4), collapse = " to ")),
    "Every period x stratum x origin-state probability vector sums to 1",
    paste("Resolved deaths:", sum(model_dat$to_state == 4)),
    paste("Trimmed range:", round(min(model_dat$ipcw), 4), "to", round(max(model_dat$ipcw), 4)),
    "Low + intermediate + high + death = 1,000",
    "CLASS is explicitly restricted to repeated cross-sectional estimates"
  )
)
fwrite(qa, file.path(out_dir, "12_QA_checks.csv"))
stopifnot(all(qa$passed))

session <- capture.output(sessionInfo())
writeLines(session, file.path(out_dir, "13_sessionInfo.txt"), useBytes = TRUE)

cat("\nKey sample sizes\n")
cat("CFPS model observations:", nrow(cfps_model_dat), "\n")
cat("CLASS model observations:", nrow(class_model_dat), "\n")
cat("CHARLS eligible transitions:", nrow(ipcw_dat), "\n")
cat("CHARLS resolved transitions:", nrow(model_dat), "\n")
cat("CHARLS resolved deaths:", sum(model_dat$to_state == 4), "\n")
cat("\nCompleted:", format(Sys.time()), "\n")
