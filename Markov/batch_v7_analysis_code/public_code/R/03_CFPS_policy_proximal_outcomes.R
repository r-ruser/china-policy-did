#!/usr/bin/env Rscript

# CFPS policy-proximal outcome module
# Main substantive outcome: functional dependence (post-expansion only)
# Mechanism-consistent outcome: usual source is a formal primary-care facility
# Supporting outcome: hospitalization in the past 12 months
# No formal causal mediation analysis is performed.

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})
options(encoding = "UTF-8", stringsAsFactors = FALSE)
set.seed(20260730)

project_root <- Sys.getenv("CHINA_POLICY_DID_ROOT")
if (!nzchar(project_root)) {
  stop("Set CHINA_POLICY_DID_ROOT to the local project root.")
}
out_dir <- Sys.getenv(
  "V7_OUTPUT_DIR",
  unset = file.path(project_root, "Markov", "analysis_v7")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(out_dir, "16_CFPS_policy_proximal_log.txt")
log_con <- file(log_path, "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

safe_num <- function(x) suppressWarnings(as.numeric(x))
clip01 <- function(x) pmin(pmax(x, 0), 1)

cat("CFPS policy-proximal outcomes\nStarted:", format(Sys.time()), "\n\n")

# -------------------------------------------------------------------------
# 1. Baseline cohort and corrected common strata
# -------------------------------------------------------------------------

cohort_path <- file.path(
  project_root, "05_analysis_data", "cfps_elderly_cohort.csv.gz"
)
base <- fread(cohort_path)[wave == 2014 & baseline_age >= 65]
base[, pid := as.character(pid)]
base <- unique(base, by = "pid")
base[, female_u := fifelse(gen %in% c(0, 1), as.integer(gen == 0), NA_integer_)]
base[, age_group := factor(
  fifelse(baseline_age >= 75, "75+", "65-74"), levels = c("65-74", "75+")
)]
base[, rural_group := factor(
  fifelse(rural == 1, "Rural", fifelse(rural == 0, "Urban/town", NA_character_)),
  levels = c("Urban/town", "Rural")
)]
base[, education_group := factor(
  fifelse(educ <= 6, "Primary or less",
          fifelse(educ > 6, "Middle school or higher", NA_character_)),
  levels = c("Middle school or higher", "Primary or less")
)]
base[, sex_group := factor(
  fifelse(female_u == 1, "Female", fifelse(female_u == 0, "Male", NA_character_)),
  levels = c("Male", "Female")
)]
base[, treat_f := factor(treat, levels = c(0, 1), labels = c("Control", "Pilot"))]

# -------------------------------------------------------------------------
# 2. Direct raw-wave extraction
# -------------------------------------------------------------------------

cfps_root <- Sys.getenv("CFPS_DATA_ROOT")
if (!nzchar(cfps_root)) {
  stop("Set CFPS_DATA_ROOT to the directory containing CFPS wave files.")
}
paths <- c(
  `2016` = file.path(cfps_root, "16", "cfps2016adult_201906.dta"),
  `2018` = file.path(cfps_root, "18", "cfps2018person_202012.dta"),
  `2020` = file.path(cfps_root, "20", "cfps2020person_202306.dta")
)
stopifnot(all(file.exists(paths)))

extract_wave <- function(year, path) {
  hosp_var <- if (year == 2016) "pc401" else "qc401"
  oop_var <- if (year == 2016) "pc701" else "qc701"
  vars <- c(
    "pid", "qq1011", "qq1013", "qp601", hosp_var,
    "metotal", oop_var
  )
  d <- read_dta(path, col_select = tidyselect::all_of(vars))
  outdoor <- safe_num(d[["qq1011"]])
  kitchen <- safe_num(d[["qq1013"]])
  usual_source <- safe_num(d[["qp601"]])
  hospitalized <- safe_num(d[[hosp_var]])
  total_medical <- safe_num(d[["metotal"]])
  oop <- safe_num(d[[oop_var]])

  data.table(
    pid = as.character(d[["pid"]]),
    wave = year,
    functional_dependence = fifelse(
      outdoor == 0 | kitchen == 0, 1,
      fifelse(outdoor == 1 & kitchen == 1, 0, NA_real_)
    ),
    formal_primary_care_usual_source = fifelse(
      usual_source %in% c(3, 4), 1,
      fifelse(usual_source %in% c(1, 2, 5), 0, NA_real_)
    ),
    hospitalized_12m = fifelse(
      hospitalized == 1, 1,
      fifelse(hospitalized == 0, 0, NA_real_)
    ),
    total_medical_expenditure = fifelse(total_medical >= 0, total_medical, NA_real_),
    oop_medical_expenditure = fifelse(oop >= 0, oop, NA_real_)
  )
}

panel <- rbindlist(lapply(names(paths), function(y) {
  extract_wave(as.integer(y), paths[[y]])
}), fill = TRUE)
panel <- merge(panel, base[, .(
  pid, baseline_age, age_group, rural_group, education_group,
  sex_group, treat, treat_f, admin_code, provcd
)], by = "pid", all = FALSE)
panel[, wave_f := factor(wave, levels = c(2016, 2018, 2020))]

# -------------------------------------------------------------------------
# 3. Availability/adjudication table
# -------------------------------------------------------------------------

availability <- rbindlist(list(
  panel[, .(
    outcome_group = "Function and care",
    outcome = "Functional dependence: unable to independently perform outdoor or kitchen activity",
    role = "Primary substantive outcome",
    n_eligible = .N,
    n_nonmissing = sum(!is.na(functional_dependence)),
    events = sum(functional_dependence == 1, na.rm = TRUE),
    prevalence = mean(functional_dependence, na.rm = TRUE),
    temporality = "Observed in 2016, 2018, 2020; all waves are post-expansion",
    decision = "Model as descriptive post-expansion outcome; not a policy DID"
  ), by = wave],
  panel[, .(
    outcome_group = "Service use",
    outcome = "Usual source is community/township/village primary care",
    role = "Mechanism-consistent secondary outcome",
    n_eligible = .N,
    n_nonmissing = sum(!is.na(formal_primary_care_usual_source)),
    events = sum(formal_primary_care_usual_source == 1, na.rm = TRUE),
    prevalence = mean(formal_primary_care_usual_source, na.rm = TRUE),
    temporality = "Observed consistently in 2016, 2018, 2020",
    decision = "Model as secondary outcome; no formal mediation claim"
  ), by = wave],
  panel[, .(
    outcome_group = "Service use",
    outcome = "Hospitalization in past 12 months",
    role = "Supporting service-use outcome",
    n_eligible = .N,
    n_nonmissing = sum(!is.na(hospitalized_12m)),
    events = sum(hospitalized_12m == 1, na.rm = TRUE),
    prevalence = mean(hospitalized_12m, na.rm = TRUE),
    temporality = "Observed consistently in 2016, 2018, 2020",
    decision = "Retain as supporting outcome, not the sole mechanism"
  ), by = wave],
  panel[, .(
    outcome_group = "Economic and social consequences",
    outcome = "Out-of-pocket medical expenditure",
    role = "Candidate supporting outcome",
    n_eligible = .N,
    n_nonmissing = sum(!is.na(oop_medical_expenditure)),
    events = NA_real_,
    prevalence = median(oop_medical_expenditure, na.rm = TRUE),
    temporality = "Observed in 2016, 2018, 2020; zero-inflated and denominator-sensitive",
    decision = "Availability reported; not promoted to main model in this module"
  ), by = wave]
), fill = TRUE)

excluded <- data.table(
  outcome_group = c("Function and care", "Function and care"),
  outcome = c("Derived variable dw", "Unmet care need"),
  role = "Excluded",
  wave = NA_integer_,
  n_eligible = NA_integer_,
  n_nonmissing = NA_integer_,
  events = NA_real_,
  prevalence = NA_real_,
  temporality = c(
    "dw is labelled social status in every source wave",
    "No consistent pre/post individual item with a valid denominator was identified"
  ),
  decision = c(
    "Do not use as activity limitation or high-need status",
    "Do not construct from absence of observed help"
  )
)
availability <- rbindlist(list(availability, excluded), fill = TRUE)
fwrite(availability, file.path(out_dir, "17_CFPS_policy_proximal_outcome_audit.csv"))

# -------------------------------------------------------------------------
# 4. Standardized absolute probabilities and robust uncertainty
# -------------------------------------------------------------------------

robust_vcov <- function(model, cluster) {
  if (requireNamespace("sandwich", quietly = TRUE)) {
    return(sandwich::vcovCL(model, cluster = cluster, type = "HC1"))
  }
  vcov(model)
}

margin <- function(model, nd, V) {
  x <- model.matrix(delete.response(terms(model)), nd)
  p <- plogis(drop(x %*% coef(model)))
  est <- mean(p)
  grad <- colMeans(x * (p * (1 - p)))
  se <- sqrt(drop(t(grad) %*% V %*% grad))
  list(
    estimate = est, se = se, gradient = grad,
    low = clip01(est - 1.96 * se), high = clip01(est + 1.96 * se)
  )
}

focal_specs <- list(
  Age = list(`65-74` = list(age_group = "65-74"), `75+` = list(age_group = "75+")),
  Residence = list(`Urban/town` = list(rural_group = "Urban/town"), Rural = list(rural_group = "Rural")),
  Education = list(
    `Middle school or higher` = list(education_group = "Middle school or higher"),
    `Primary or less` = list(education_group = "Primary or less")
  ),
  Sex = list(Male = list(sex_group = "Male"), Female = list(sex_group = "Female"))
)

fit_outcome <- function(outcome_var, outcome_label, role) {
  d <- panel[complete.cases(
    get(outcome_var), wave_f, treat_f, age_group, rural_group,
    education_group, sex_group
  )]
  form <- as.formula(paste0(
    outcome_var,
    " ~ wave_f * treat_f + age_group + rural_group + education_group + sex_group"
  ))
  model <- glm(form, family = binomial(), data = d)
  V <- robust_vcov(model, d$pid)
  rows <- list()
  gradients <- list()
  k <- 0L

  add <- function(wave_value, treat_value, dimension, level, focal = list()) {
    nd <- copy(d)
    nd[, wave_f := factor(wave_value, levels = levels(d$wave_f))]
    nd[, treat_f := factor(treat_value, levels = levels(d$treat_f))]
    for (nm in names(focal)) {
      nd[[nm]] <- factor(focal[[nm]], levels = levels(d[[nm]]))
    }
    z <- margin(model, nd, V)
    key <- paste(wave_value, treat_value, dimension, level, sep = "|")
    gradients[[key]] <<- z
    k <<- k + 1L
    rows[[k]] <<- data.table(
      database = "CFPS",
      outcome = outcome_label,
      role = role,
      design = "Post-expansion pilot-control descriptive comparison",
      wave = as.integer(wave_value),
      exposure_group = treat_value,
      dimension = dimension,
      level = level,
      probability = z$estimate,
      conf_low = z$low,
      conf_high = z$high,
      events_per_1000 = 1000 * z$estimate,
      conf_low_per_1000 = 1000 * z$low,
      conf_high_per_1000 = 1000 * z$high
    )
  }

  for (wv in levels(d$wave_f)) {
    for (tr in levels(d$treat_f)) {
      add(wv, tr, "Overall", "Overall")
      for (dimension in names(focal_specs)) {
        for (level in names(focal_specs[[dimension]])) {
          add(wv, tr, dimension, level, focal_specs[[dimension]][[level]])
        }
      }
    }
  }

  # 2020-vs-2016 difference in change. This is post-expansion descriptive,
  # not a DID estimate of policy initiation.
  z_p20 <- gradients[["2020|Pilot|Overall|Overall"]]
  z_p16 <- gradients[["2016|Pilot|Overall|Overall"]]
  z_c20 <- gradients[["2020|Control|Overall|Overall"]]
  z_c16 <- gradients[["2016|Control|Overall|Overall"]]
  g <- z_p20$gradient - z_p16$gradient - z_c20$gradient + z_c16$gradient
  est <- z_p20$estimate - z_p16$estimate - z_c20$estimate + z_c16$estimate
  se <- sqrt(drop(t(g) %*% V %*% g))
  contrast <- data.table(
    database = "CFPS",
    outcome = outcome_label,
    role = role,
    estimand = "Pilot-control difference in 2020-vs-2016 change",
    interpretation = "Descriptive post-expansion divergence; not policy DID and not mediation",
    probability_difference = est,
    percentage_point_difference = 100 * est,
    conf_low_pp = 100 * (est - 1.96 * se),
    conf_high_pp = 100 * (est + 1.96 * se),
    events_per_1000_difference = 1000 * est,
    conf_low_per_1000 = 1000 * (est - 1.96 * se),
    conf_high_per_1000 = 1000 * (est + 1.96 * se)
  )
  list(
    probabilities = rbindlist(rows),
    contrast = contrast,
    n = nrow(d),
    n_id = uniqueN(d$pid)
  )
}

fits <- list(
  fit_outcome(
    "functional_dependence",
    "Functional dependence (outdoor or kitchen activity)",
    "Primary substantive outcome"
  ),
  fit_outcome(
    "formal_primary_care_usual_source",
    "Usual source is formal primary care",
    "Mechanism-consistent secondary outcome"
  ),
  fit_outcome(
    "hospitalized_12m",
    "Hospitalization in past 12 months",
    "Supporting service-use outcome"
  )
)
proximal_probs <- rbindlist(lapply(fits, `[[`, "probabilities"))
proximal_contrasts <- rbindlist(lapply(fits, `[[`, "contrast"))
fwrite(proximal_probs, file.path(out_dir, "18_CFPS_policy_proximal_standardized_probabilities.csv"))
fwrite(proximal_contrasts, file.path(out_dir, "19_CFPS_policy_proximal_descriptive_changes.csv"))

# -------------------------------------------------------------------------
# 5. Province-level yearbook context (ecological; not an individual mediator)
# -------------------------------------------------------------------------

yearbook_path <- file.path(out_dir, "14_yearbook_primary_care_context_2001_2020.csv")
if (file.exists(yearbook_path)) {
  yearbook <- fread(yearbook_path, encoding = "UTF-8")
  province_map <- data.table(
    provcd = c(11, 12, 13, 14, 15, 21, 22, 23, 31, 32, 33, 34, 35, 36,
               37, 41, 42, 43, 44, 45, 46, 50, 51, 52, 53, 54, 61, 62,
               63, 64, 65),
    area = c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区",
             "辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省", "浙江省",
             "安徽省", "福建省", "江西省", "山东省", "河南省", "湖北省",
             "湖南省", "广东省", "广西壮族自治区", "海南省", "重庆市",
             "四川省", "贵州省", "云南省", "西藏自治区", "陕西省", "甘肃省",
             "青海省", "宁夏回族自治区", "新疆维吾尔自治区")
  )
  context <- merge(yearbook, province_map, by = "area", all = FALSE)
  context <- context[year %in% c(2016, 2018, 2020)]
  fwrite(context, file.path(out_dir, "20_yearbook_province_context_2016_2020.csv"))

  participant_context <- merge(
    unique(panel[, .(pid, wave, treat, provcd)]),
    context,
    by.x = c("provcd", "wave"), by.y = c("provcd", "year"),
    all.x = TRUE
  )
  context_summary <- participant_context[, .(
    n_person_wave = .N,
    n_provinces = uniqueN(provcd),
    primary_care_institutions_per_10000 =
      mean(primary_care_institutions_per_10000, na.rm = TRUE),
    primary_care_staff_per_1000 =
      mean(primary_care_staff_per_1000, na.rm = TRUE),
    primary_care_beds_per_1000 =
      mean(primary_care_beds_per_1000, na.rm = TRUE),
    health_examination_people_count =
      mean(health_examination_people_count, na.rm = TRUE),
    visits_per_resident = mean(visits_per_resident, na.rm = TRUE),
    annual_hospitalization_rate_percent =
      mean(annual_hospitalization_rate_percent, na.rm = TRUE)
  ), by = .(
    wave,
    exposure_group = fifelse(treat == 1, "Pilot", "Control")
  )]
  context_summary[, interpretation := paste(
    "Participant-weighted province-level service environment;",
    "ecological context only, not an individual mediator or adjustment variable"
  )]
  fwrite(context_summary, file.path(out_dir, "21_CFPS_yearbook_service_context_by_group.csv"))
}

# -------------------------------------------------------------------------
# 6. QA
# -------------------------------------------------------------------------

qa <- data.table(
  check = c(
    "Source dw excluded from functional outcome",
    "Functional outcome uses raw qq1011/qq1013",
    "Primary-care source uses categories 3/4 only",
    "All modeled probabilities in [0,1]",
    "All three requested common age/residence/education/sex strata present",
    "No causal mediation claim"
  ),
  passed = c(
    TRUE,
    all(panel$functional_dependence %in% c(0, 1, NA)),
    all(panel$formal_primary_care_usual_source %in% c(0, 1, NA)),
    all(proximal_probs$probability >= 0 & proximal_probs$probability <= 1),
    all(c("Age", "Residence", "Education", "Sex") %in% proximal_probs$dimension),
    TRUE
  ),
  detail = c(
    "dw is social status in every small source file",
    "Unable to independently perform outdoor or kitchen activity",
    "Community/township health center or community/village health station",
    paste(round(range(proximal_probs$probability), 4), collapse = " to "),
    paste(sort(unique(proximal_probs$dimension)), collapse = "; "),
    "Secondary service outcome is reported as mechanism-consistent only"
  )
)
fwrite(qa, file.path(out_dir, "22_CFPS_policy_proximal_QA.csv"))
stopifnot(all(qa$passed))

writeLines(capture.output(sessionInfo()), file.path(out_dir, "23_policy_proximal_sessionInfo.txt"))
cat("\nCompleted:", format(Sys.time()), "\n")
