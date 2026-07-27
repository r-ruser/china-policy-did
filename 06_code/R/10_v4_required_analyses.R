#!/usr/bin/env Rscript

# V4 policy-equity analyses.
# The estimands are age-group differential period changes and age gradients.
# CHARLS and CLASS do not identify a national causal policy effect.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(fixest)
  library(mice)
  library(splines)
  library(sandwich)
})

options(encoding = "UTF-8")
set.seed(20260727)
setFixest_nthreads(max(1L, parallel::detectCores(logical = TRUE)))

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
tab_dir <- file.path(root, "07_results", "tables")
diag_dir <- file.path(root, "07_results", "diagnostics")
model_dir <- file.path(root, "07_results", "models")
log_dir <- file.path(root, "10_logs")
invisible(lapply(c(tab_dir, diag_dir, model_dir, log_dir), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

log_con <- file(file.path(log_dir, "r_v4_required_analyses.log"),
                open = "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("V4 required policy-equity analyses\nStarted:", format(Sys.time()), "\n\n")

num <- function(x) suppressWarnings(as.numeric(x))

coef_row <- function(model, pattern, cohort, wave, analysis, specification,
                     scale = 1) {
  nm <- names(coef(model))
  hit <- grep(pattern, nm)
  if (length(hit) != 1L) {
    stop("Expected one coefficient for ", pattern, "; found ",
         paste(nm[hit], collapse = ", "))
  }
  term <- nm[hit]
  ci <- confint(model, parm = term)
  ci_values <- as.numeric(unlist(ci))
  if (length(ci_values) < 2L) {
    stop("Could not extract confidence interval for ", term)
  }
  data.frame(
    cohort = cohort,
    wave = wave,
    analysis = analysis,
    specification = specification,
    term = term,
    estimate = unname(coef(model)[term]) * scale,
    std_error = unname(se(model)[term]) * scale,
    conf_low = ci_values[1] * scale,
    conf_high = ci_values[2] * scale,
    p_value = unname(pvalue(model)[term]),
    n_obs = nobs(model),
    stringsAsFactors = FALSE
  )
}

robust_lm_row <- function(model, term, cluster, cohort, wave, analysis,
                          specification, scale = 1) {
  vv <- sandwich::vcovCL(model, cluster = cluster, type = "HC1")
  est <- coef(model)[term]
  se_ <- sqrt(diag(vv))[term]
  data.frame(
    cohort, wave, analysis, specification, term,
    estimate = unname(est) * scale,
    std_error = unname(se_) * scale,
    conf_low = unname(est - 1.96 * se_) * scale,
    conf_high = unname(est + 1.96 * se_) * scale,
    p_value = 2 * pnorm(-abs(est / se_)),
    n_obs = nobs(model),
    stringsAsFactors = FALSE
  )
}

prepare_predictors <- function(dat, vars) {
  out <- dat
  used <- character()
  for (v in vars) {
    x <- num(out[[v]])
    miss <- is.na(x)
    if (all(miss)) next
    med <- median(x, na.rm = TRUE)
    x[miss] <- med
    out[[v]] <- x
    used <- c(used, v)
    if (any(miss)) {
      mv <- paste0(v, "_missing")
      out[[mv]] <- as.integer(miss)
      used <- c(used, mv)
    }
  }
  list(data = out, variables = used)
}

ipcw_weights <- function(base, retained, predictors, trims) {
  pp <- prepare_predictors(base, predictors)
  dat <- pp$data
  dat$retained <- retained
  fit <- glm(reformulate(pp$variables, response = "retained"),
             data = dat, family = binomial())
  pr <- pmin(pmax(predict(fit, type = "response"), 0.01), 0.99)
  raw <- mean(retained) / pr
  out_w <- list()
  out_d <- list()
  for (tr in trims) {
    qs <- quantile(raw[retained == 1], c(tr, 1 - tr), na.rm = TRUE)
    w <- pmin(pmax(raw, qs[1]), qs[2])
    label <- paste0(100 * tr, "/", 100 * (1 - tr))
    out_w[[label]] <- w
    wr <- w[retained == 1]
    out_d[[label]] <- data.frame(
      trim = label,
      baseline_n = nrow(dat),
      retained_n = sum(retained),
      retained_pct = mean(retained),
      probability_min = min(pr),
      probability_p01 = quantile(pr, 0.01),
      probability_median = median(pr),
      probability_p99 = quantile(pr, 0.99),
      probability_max = max(pr),
      weight_min = min(wr),
      weight_p01 = quantile(wr, 0.01),
      weight_median = median(wr),
      weight_p99 = quantile(wr, 0.99),
      weight_max = max(wr),
      effective_sample_size = sum(wr)^2 / sum(wr^2)
    )
  }
  list(weights = out_w, diagnostics = bind_rows(out_d), model = fit)
}

smd_one <- function(x, retained) {
  x <- num(x)
  a <- x[retained == 1]
  b <- x[retained == 0]
  (mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) /
    sqrt((var(a, na.rm = TRUE) + var(b, na.rm = TRUE)) / 2)
}

attrition_table <- function(base, retained, cohort, wave, age, predictors) {
  bind_rows(lapply(c(age, predictors), function(v) {
    x <- num(base[[v]])
    data.frame(
      cohort, wave, variable = v,
      retained_mean = mean(x[retained == 1], na.rm = TRUE),
      lost_mean = mean(x[retained == 0], na.rm = TRUE),
      standardized_difference = smd_one(x, retained)
    )
  }))
}

pair_fe <- function(panel, id, year_var, outcome, baseline_year, followup_year,
                    age_var, cutpoint, weights = NULL, label = "Complete case") {
  dat <- panel |>
    filter(.data[[year_var]] %in% c(baseline_year, followup_year),
           !is.na(.data[[outcome]])) |>
    group_by(.data[[id]]) |>
    filter(n_distinct(.data[[year_var]]) == 2) |>
    ungroup() |>
    mutate(
      high_age = as.integer(.data[[age_var]] >= cutpoint),
      post_pair = as.integer(.data[[year_var]] == followup_year)
    )
  if (!is.null(weights)) {
    dat <- left_join(dat, weights, by = id)
    model <- feols(
      as.formula(paste0(outcome, " ~ high_age:post_pair | ", id,
                        " + ", year_var)),
      data = dat, weights = ~analysis_weight,
      cluster = as.formula(paste0("~", id))
    )
  } else {
    model <- feols(
      as.formula(paste0(outcome, " ~ high_age:post_pair | ", id,
                        " + ", year_var)),
      data = dat, cluster = as.formula(paste0("~", id))
    )
  }
  list(
    model = model, data = dat,
    row = coef_row(
      model, "high_age:post_pair|post_pair:high_age",
      NA_character_, followup_year, "Age-cutpoint risk difference",
      paste0(label, "; cutpoint ", cutpoint, " years"), scale = 100
    )
  )
}

continuous_fe <- function(panel, id, year_var, outcome, baseline_year,
                          followup_year, age_var) {
  dat <- panel |>
    filter(.data[[year_var]] %in% c(baseline_year, followup_year),
           !is.na(.data[[outcome]])) |>
    group_by(.data[[id]]) |>
    filter(n_distinct(.data[[year_var]]) == 2) |>
    ungroup() |>
    mutate(age5 = (.data[[age_var]] - 70) / 5,
           post_pair = as.integer(.data[[year_var]] == followup_year))
  model <- feols(
    as.formula(paste0(outcome, " ~ age5:post_pair | ", id,
                      " + ", year_var)),
    data = dat, cluster = as.formula(paste0("~", id))
  )
  list(model = model, data = dat,
       row = coef_row(model, "age5:post_pair|post_pair:age5",
                      NA_character_, followup_year,
                      "Continuous-age gradient",
                      "Risk-difference change per 5 years of baseline age",
                      scale = 100))
}

raw_risks <- function(dat, cohort, wave, age_var, outcome) {
  dat |>
    filter(!is.na(.data[[outcome]])) |>
    mutate(age_group = ifelse(.data[[age_var]] >= 75, "75+", "65-74")) |>
    group_by(age_group) |>
    summarise(
      n = n(), cases = sum(.data[[outcome]]),
      risk = mean(.data[[outcome]]),
      se = sqrt(risk * (1 - risk) / n),
      conf_low = pmax(0, risk - 1.96 * se),
      conf_high = pmin(1, risk + 1.96 * se),
      .groups = "drop"
    ) |>
    mutate(cohort = cohort, wave = wave)
}

standardized_spline <- function(base_follow, cohort, wave, age_var, outcome,
                                covariates) {
  pp <- prepare_predictors(base_follow, covariates)
  dat <- pp$data |> filter(!is.na(.data[[outcome]]), !is.na(.data[[age_var]]))
  age <- dat[[age_var]]
  boundary <- as.numeric(quantile(age, c(0.05, 0.95), na.rm = TRUE))
  knots <- as.numeric(quantile(age, c(0.35, 0.65), na.rm = TRUE))
  rhs <- paste(
    sprintf("ns(%s, knots=c(%f,%f), Boundary.knots=c(%f,%f))",
            age_var, knots[1], knots[2], boundary[1], boundary[2]),
    if (length(pp$variables)) paste(pp$variables, collapse = " + ") else NULL,
    sep = if (length(pp$variables)) " + " else ""
  )
  fit <- glm(as.formula(paste(outcome, "~", rhs)), data = dat,
             family = binomial())
  grid <- seq(ceiling(boundary[1]), floor(boundary[2]), by = 1)
  keep_coef <- !is.na(coef(fit))
  beta <- coef(fit)[keep_coef]
  vv <- vcov(fit)[keep_coef, keep_coef, drop = FALSE]
  ans <- lapply(grid, function(a) {
    nd <- dat
    nd[[age_var]] <- a
    mm <- model.matrix(delete.response(terms(fit)), nd)
    mm <- mm[, names(beta), drop = FALSE]
    xm <- colMeans(mm)
    eta <- sum(xm * beta)
    se_eta <- sqrt(drop(t(xm) %*% vv %*% xm))
    data.frame(
      cohort, wave, baseline_age = a,
      standardized_risk = plogis(eta),
      conf_low = plogis(eta - 1.96 * se_eta),
      conf_high = plogis(eta + 1.96 * se_eta)
    )
  })
  out <- bind_rows(ans)
  monotonic <- data.frame(
    cohort, wave,
    spearman_rho = cor(out$baseline_age, out$standardized_risk,
                       method = "spearman"),
    proportion_non_decreasing =
      mean(diff(out$standardized_risk) >= -1e-8),
    risk_change_10_years =
      approx(out$baseline_age, out$standardized_risk,
             xout = min(out$baseline_age) + 10)$y -
      out$standardized_risk[1]
  )
  list(fit = fit, curve = out, monotonic = monotonic,
       knots = data.frame(cohort, wave, knot = c(boundary[1], knots,
                                                 boundary[2]),
                          knot_type = c("boundary", "internal", "internal",
                                        "boundary")))
}

pool_scalar <- function(est, variance) {
  m <- length(est)
  q <- mean(est)
  u <- mean(variance)
  b <- var(est)
  total <- u + (1 + 1 / m) * b
  data.frame(estimate = q, std_error = sqrt(total),
             conf_low = q - 1.96 * sqrt(total),
             conf_high = q + 1.96 * sqrt(total),
             p_value = 2 * pnorm(-abs(q / sqrt(total))))
}

mi_risk_difference <- function(dat, outcomes, age_var, predictors,
                               cohort, waves, m = 20) {
  pp <- prepare_predictors(dat, predictors)
  imp_dat <- pp$data[, unique(c(outcomes, age_var, pp$variables)), drop = FALSE]
  imp_dat$age75 <- as.integer(imp_dat[[age_var]] >= 75)
  method <- mice::make.method(imp_dat)
  method[] <- ""
  # Predictive mean matching preserves the observed 0/1 support here and avoids
  # coercion of numeric outcomes to factors inside the pooled LPM.
  method[outcomes] <- "pmm"
  pred <- mice::make.predictorMatrix(imp_dat)
  pred[,] <- 0
  for (o in outcomes) {
    pred[o, setdiff(names(imp_dat), o)] <- 1
  }
  where <- is.na(imp_dat)
  where[, setdiff(names(imp_dat), outcomes)] <- FALSE
  imp <- mice(imp_dat, m = m, maxit = 10, method = method,
              predictorMatrix = pred, where = where,
              printFlag = FALSE, seed = 20260727)
  res <- lapply(seq_along(outcomes), function(j) {
    o <- outcomes[j]
    fits <- lapply(seq_len(m), function(i) {
      dd <- complete(imp, i)
      fit <- lm(reformulate("age75", response = o), data = dd)
      c(est = unname(coef(fit)["age75"]),
        var = unname(vcov(fit)["age75", "age75"]))
    })
    mat <- do.call(rbind, fits)
    pool_scalar(mat[, "est"], mat[, "var"]) |>
      mutate(cohort = cohort, wave = waves[j],
             analysis = "Multiple imputation",
             specification = paste0(m, " imputations; Rubin pooling"))
  })
  list(result = bind_rows(res), mids = imp)
}

extreme_scenarios <- function(dat, outcome, age_var, cohort, wave) {
  high <- dat[[age_var]] >= 75
  observed <- dat[[outcome]]
  scenarios <- list(
    "All missing=no event" = ifelse(is.na(observed), 0, observed),
    "All missing=event" = ifelse(is.na(observed), 1, observed),
    "Worst inequality" = ifelse(is.na(observed), as.integer(high), observed),
    "Best inequality" = ifelse(is.na(observed), as.integer(!high), observed)
  )
  bind_rows(lapply(names(scenarios), function(s) {
    y <- scenarios[[s]]
    data.frame(
      cohort, wave, scenario = s,
      risk_age_75plus = mean(y[high]),
      risk_age_65_74 = mean(y[!high]),
      risk_difference = mean(y[high]) - mean(y[!high])
    )
  }))
}

# -------------------------------------------------------------------------
# CHARLS
# -------------------------------------------------------------------------

cat("[1] CHARLS\n")
charls_path <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
stopifnot(file.exists(charls_path))
cw <- read_dta(charls_path)
cw <- cw |>
  mutate(
    ID = as.character(ID),
    age_2015 = 2015 - num(rabyear),
    female = as.integer(num(ragender) == 2),
    education = num(raeduc_c)
  ) |>
  filter(num(inw3) == 1, age_2015 >= 65)

wave_map <- c(`1` = 2011, `2` = 2013, `3` = 2015, `4` = 2018)
cp <- bind_rows(lapply(names(wave_map), function(w) {
  yr <- wave_map[[w]]
  adl <- num(cw[[paste0("r", w, "adla_c")]])
  cesd <- num(cw[[paste0("r", w, "cesd10")]])
  orient <- num(cw[[paste0("r", w, "orient")]])
  imrc <- num(cw[[paste0("r", w, "imrc")]])
  ser7 <- num(cw[[paste0("r", w, "ser7")]])
  srh <- num(cw[[paste0("r", w, "shlta")]])
  data.frame(
    ID = cw$ID, year = yr,
    in_wave = num(cw[[paste0("inw", w)]]) == 1,
    age_2015 = cw$age_2015, female = cw$female,
    education = cw$education,
    any_adl = ifelse(is.na(adl), NA_real_, as.numeric(adl >= 1)),
    cesd10 = cesd,
    cognition = ifelse(!is.na(orient) & !is.na(imrc) & !is.na(ser7),
                       orient + imrc + ser7, NA_real_),
    poor_srh = ifelse(srh %in% 1:5, as.numeric(srh >= 4), NA_real_)
  )
})) |>
  filter(in_wave)

cb <- cp |>
  filter(year == 2015) |>
  transmute(
    ID, age_2015, female, education,
    baseline_adl = any_adl, baseline_cesd10 = cesd10,
    baseline_cognition = cognition, baseline_poor_srh = poor_srh
  ) |>
  distinct(ID, .keep_all = TRUE)
cp <- left_join(cp, cb |> select(ID, starts_with("baseline_")), by = "ID")
cp_inc <- cp |> filter(baseline_adl == 0)

charls_cut <- list()
charls_cont <- list()
for (cut in c(70, 75, 80)) {
  z <- pair_fe(cp_inc, "ID", "year", "any_adl", 2015, 2018,
               "age_2015", cut)
  z$row$cohort <- "CHARLS"
  charls_cut[[length(charls_cut) + 1]] <- z$row
}
zc <- continuous_fe(cp_inc, "ID", "year", "any_adl", 2015, 2018,
                    "age_2015")
zc$row$cohort <- "CHARLS"
charls_cont[[1]] <- zc$row

charls_follow <- cb |>
  left_join(cp |> filter(year == 2018) |>
              select(ID, adl_2018 = any_adl), by = "ID") |>
  filter(baseline_adl == 0)
retained_charls <- as.integer(!is.na(charls_follow$adl_2018))
charls_ipcw <- ipcw_weights(
  charls_follow, retained_charls,
  c("age_2015", "female", "education", "baseline_cesd10",
    "baseline_cognition", "baseline_poor_srh"),
  c(0.01, 0.025, 0.05)
)
charls_ipcw_rows <- list()
for (nm in names(charls_ipcw$weights)) {
  ww <- data.frame(
    ID = charls_follow$ID,
    analysis_weight = charls_ipcw$weights[[nm]]
  )
  z <- pair_fe(cp_inc, "ID", "year", "any_adl", 2015, 2018,
               "age_2015", 75, weights = ww,
               label = paste0("IPCW trim ", nm))
  z$row$cohort <- "CHARLS"
  charls_ipcw_rows[[nm]] <- z$row
}
charls_attr <- attrition_table(
  charls_follow, retained_charls, "CHARLS", 2018, "age_2015",
  c("female", "education", "baseline_cesd10", "baseline_cognition",
    "baseline_poor_srh")
)
charls_attr_rate <- charls_follow |>
  mutate(age_group = ifelse(age_2015 >= 75, "75+", "65-74"),
         retained = retained_charls) |>
  group_by(age_group) |>
  summarise(
    baseline_n = n(), retained_n = sum(retained),
    lost_n = baseline_n - retained_n,
    attrition_rate = lost_n / baseline_n, .groups = "drop"
  ) |>
  mutate(cohort = "CHARLS", wave = 2018)
charls_weight_diag <- charls_ipcw$diagnostics |>
  mutate(cohort = "CHARLS", wave = 2018)
charls_raw <- raw_risks(
  charls_follow, "CHARLS", 2018, "age_2015", "adl_2018"
)
charls_spline <- standardized_spline(
  charls_follow, "CHARLS", 2018, "age_2015", "adl_2018",
  c("female", "education", "baseline_cesd10",
    "baseline_cognition", "baseline_poor_srh")
)
charls_mi <- mi_risk_difference(
  charls_follow, "adl_2018", "age_2015",
  c("female", "education", "baseline_cesd10",
    "baseline_cognition", "baseline_poor_srh"),
  "CHARLS", 2018, m = 20
)
charls_extreme <- extreme_scenarios(
  charls_follow, "adl_2018", "age_2015", "CHARLS", 2018
)

# Placebo periods use repeated ADL prevalence and do not condition on the
# future 2015 ADL value.
placebo_rows <- list()
for (pr in list(c(2011, 2013), c(2013, 2015), c(2015, 2018))) {
  dd <- cp |>
    filter(year %in% pr, !is.na(any_adl)) |>
    group_by(ID) |>
    filter(n_distinct(year) == 2) |>
    ungroup() |>
    mutate(age75 = as.integer(age_2015 >= 75),
           period = as.integer(year == pr[2]))
  fit <- feols(any_adl ~ age75:period | ID + year,
               data = dd, cluster = ~ID)
  rr <- coef_row(
    fit, "age75:period|period:age75", "CHARLS", pr[2],
    ifelse(pr[2] <= 2015, "Placebo period", "Observed 2015-2018 period"),
    paste0(pr[1], "-", pr[2], " age-group differential change"),
    scale = 100
  )
  placebo_rows[[paste(pr, collapse = "_")]] <- rr
}
placebo_tbl <- bind_rows(placebo_rows)

trend_dat <- cp |> filter(!is.na(any_adl)) |>
  mutate(age75 = as.integer(age_2015 >= 75),
         time = year - 2015, post2018 = as.integer(year == 2018))
trend_fit <- feols(
  any_adl ~ age75:time + age75:post2018 | ID + year,
  data = trend_dat, cluster = ~ID
)
trend_adjusted <- coef_row(
  trend_fit, "age75:post2018|post2018:age75", "CHARLS", 2018,
  "Differential-trend-adjusted deviation",
  "2018 deviation after allowing a linear age-group differential trend",
  scale = 100
)

pre1 <- placebo_tbl$estimate[placebo_tbl$wave == 2013]
pre2 <- placebo_tbl$estimate[placebo_tbl$wave == 2015]
main_row <- charls_cut[[2]]
cat("Placebo rows before sensitivity bounds:\n")
print(placebo_tbl)
cat("Main cutpoint row before sensitivity bounds:\n")
print(main_row)
annual_pre <- max(abs(c(pre1, pre2))) / 2
parallel_bounds <- bind_rows(lapply(c(0, 0.5, 1, 1.5, 2), function(M) {
  bias <- M * annual_pre * 3
  data.frame(
    M = M, assumed_max_bias_pp = bias,
    estimate_pp = main_row$estimate,
    sensitivity_low = main_row$conf_low - bias,
    sensitivity_high = main_row$conf_high + bias
  )
}))

# Continuous-age pre-period trend extrapolation to a 2018 counterfactual.
pre_dat <- cp |> filter(year <= 2015, !is.na(any_adl)) |>
  mutate(time_pre = year - 2011)
age_bounds <- quantile(pre_dat$age_2015, c(0.05, 0.95), na.rm = TRUE)
age_knots <- quantile(pre_dat$age_2015, c(0.35, 0.65), na.rm = TRUE)
pre_fit <- lm(
  any_adl ~ time_pre +
    ns(age_2015, knots = age_knots, Boundary.knots = age_bounds) +
    time_pre:ns(age_2015, knots = age_knots,
                Boundary.knots = age_bounds),
  data = pre_dat
)
obs2018 <- cp |> filter(year == 2018, !is.na(any_adl))
obs_fit <- glm(
  any_adl ~ ns(age_2015, knots = age_knots, Boundary.knots = age_bounds),
  data = obs2018, family = binomial()
)
age_grid <- seq(ceiling(age_bounds[1]), floor(age_bounds[2]), by = 1)
pre_extrap <- bind_rows(lapply(age_grid, function(a) {
  nd <- data.frame(age_2015 = a, time_pre = 7)
  cf <- predict(pre_fit, newdata = nd, se.fit = TRUE)
  ob <- predict(obs_fit, newdata = data.frame(age_2015 = a),
                type = "link", se.fit = TRUE)
  data.frame(
    baseline_age = a,
    counterfactual_2018 = pmin(pmax(cf$fit, 0), 1),
    counterfactual_low = pmin(pmax(cf$fit - 1.96 * cf$se.fit, 0), 1),
    counterfactual_high = pmin(pmax(cf$fit + 1.96 * cf$se.fit, 0), 1),
    observed_2018 = plogis(ob$fit),
    observed_low = plogis(ob$fit - 1.96 * ob$se.fit),
    observed_high = plogis(ob$fit + 1.96 * ob$se.fit)
  )
})) |>
  mutate(excess_over_counterfactual = observed_2018 - counterfactual_2018)

# -------------------------------------------------------------------------
# CLASS
# -------------------------------------------------------------------------

cat("[2] CLASS\n")
class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"
class_paths <- c(
  `2018` = file.path(class_root, "CLASS2018-cleaned release.dta"),
  `2020` = file.path(class_root, "individual -2020  cleaned for user.dta"),
  `2023` = file.path(class_root, "2023_individual_release_weighted .dta")
)
stopifnot(all(file.exists(class_paths)))

score_dep9 <- function(df, vars) {
  mat <- do.call(cbind, lapply(vars, function(v) {
    x <- num(df[[v]])
    x[!x %in% 1:3] <- NA_real_
    x
  }))
  scored <- mat - 1
  scored[, c(1, 4, 9)] <- 2 - scored[, c(1, 4, 9)]
  n <- rowSums(!is.na(scored))
  ifelse(n >= 7, rowSums(scored, na.rm = TRUE) * 9 / n, NA_real_)
}

extract_class <- function(year, path) {
  d <- read_dta(path)
  if (year == 2018) {
    id <- as.character(d[["q1__1__open"]])
    sex <- num(d[["a1"]]); birth <- num(d[["a2__1__open"]])
    edu <- num(d[["a3"]]); srh <- num(d[["b1"]])
    adl <- num(d[["b5"]]); depv <- paste0("e2__", 1:9)
  } else {
    id <- as.character(d[["Q1_1_open"]])
    sex <- num(d[["A1"]]); birth <- num(d[["A2_1_open"]])
    edu <- num(d[["A3"]]); srh <- num(d[["B1"]])
    adl <- num(d[["B5"]]); depv <- paste0("E2_", 1:9)
  }
  data.frame(
    class_id = id, year = year, source_row = seq_len(nrow(d)),
    female = ifelse(sex %in% 1:2, as.integer(sex == 2), NA_real_),
    birth_year = ifelse(birth >= 1900 & birth <= year, birth, NA_real_),
    education = ifelse(edu > 0 & edu < 20, edu, NA_real_),
    poor_srh = ifelse(srh %in% 1:5, as.integer(srh >= 4), NA_real_),
    adl_help = ifelse(adl %in% 1:2, as.integer(adl == 1), NA_real_),
    depression9 = score_dep9(d, depv)
  )
}

cl <- bind_rows(lapply(names(class_paths), function(y) {
  extract_class(as.integer(y), class_paths[[y]])
})) |>
  filter(!is.na(class_id), nzchar(class_id))

mode_num <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(names(which.max(table(x))))
}
stable <- cl |> group_by(class_id) |>
  summarise(sf = mode_num(female), sb = mode_num(birth_year),
            .groups = "drop")
cl <- cl |>
  left_join(stable, by = "class_id") |>
  mutate(match_score = (!is.na(female) & female == sf) +
           (!is.na(birth_year) & birth_year == sb)) |>
  group_by(class_id, year) |>
  arrange(desc(match_score), source_row, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(-sf, -sb, -match_score, -source_row)

class_base <- cl |> filter(year == 2018) |>
  mutate(age_2018 = 2018 - birth_year) |>
  filter(age_2018 >= 65) |>
  transmute(
    class_id, age_2018, female, education,
    baseline_adl = adl_help, baseline_poor_srh = poor_srh,
    baseline_depression9 = depression9
  ) |>
  distinct(class_id, .keep_all = TRUE)
cl <- semi_join(cl, class_base, by = "class_id") |>
  left_join(class_base |> select(class_id, age_2018, starts_with("baseline_")),
            by = "class_id")
cl_inc <- cl |> filter(baseline_adl == 0)

class_cut <- list()
class_cont <- list()
class_ipcw_rows <- list()
class_attr <- list()
class_attr_rate <- list()
class_weight_diag <- list()
class_raw <- list()
class_spline <- list()
class_extreme <- list()
class_follow_wide <- class_base |>
  left_join(cl |> filter(year == 2020) |>
              select(class_id, adl_2020 = adl_help), by = "class_id") |>
  left_join(cl |> filter(year == 2023) |>
              select(class_id, adl_2023 = adl_help), by = "class_id") |>
  filter(baseline_adl == 0)

for (fu in c(2020, 2023)) {
  for (cut in c(70, 75, 80)) {
    z <- pair_fe(cl_inc, "class_id", "year", "adl_help", 2018, fu,
                 "age_2018", cut)
    z$row$cohort <- "CLASS"
    class_cut[[paste(fu, cut)]] <- z$row
  }
  zc2 <- continuous_fe(cl_inc, "class_id", "year", "adl_help",
                       2018, fu, "age_2018")
  zc2$row$cohort <- "CLASS"
  class_cont[[as.character(fu)]] <- zc2$row

  yvar <- paste0("adl_", fu)
  retained <- as.integer(!is.na(class_follow_wide[[yvar]]))
  iw <- ipcw_weights(
    class_follow_wide, retained,
    c("age_2018", "female", "education", "baseline_poor_srh",
      "baseline_depression9"),
    c(0.01, 0.025, 0.05)
  )
  for (nm in names(iw$weights)) {
    ww <- data.frame(
      class_id = class_follow_wide$class_id,
      analysis_weight = iw$weights[[nm]]
    )
    z <- pair_fe(cl_inc, "class_id", "year", "adl_help", 2018, fu,
                 "age_2018", 75, weights = ww,
                 label = paste0("IPCW trim ", nm))
    z$row$cohort <- "CLASS"
    class_ipcw_rows[[paste(fu, nm)]] <- z$row
  }
  class_attr[[as.character(fu)]] <- attrition_table(
    class_follow_wide, retained, "CLASS", fu, "age_2018",
    c("female", "education", "baseline_poor_srh",
      "baseline_depression9")
  )
  class_attr_rate[[as.character(fu)]] <- class_follow_wide |>
    mutate(age_group = ifelse(age_2018 >= 75, "75+", "65-74"),
           retained = retained) |>
    group_by(age_group) |>
    summarise(
      baseline_n = n(), retained_n = sum(retained),
      lost_n = baseline_n - retained_n,
      attrition_rate = lost_n / baseline_n, .groups = "drop"
    ) |>
    mutate(cohort = "CLASS", wave = fu)
  class_weight_diag[[as.character(fu)]] <- iw$diagnostics |>
    mutate(cohort = "CLASS", wave = fu)
  ff <- class_follow_wide |>
    rename(adl_followup = all_of(yvar))
  class_raw[[as.character(fu)]] <- raw_risks(
    ff, "CLASS", fu, "age_2018", "adl_followup"
  )
  class_spline[[as.character(fu)]] <- standardized_spline(
    ff, "CLASS", fu, "age_2018", "adl_followup",
    c("female", "education", "baseline_poor_srh",
      "baseline_depression9")
  )
  class_extreme[[as.character(fu)]] <- extreme_scenarios(
    ff, "adl_followup", "age_2018", "CLASS", fu
  )
}

class_mi <- mi_risk_difference(
  class_follow_wide, c("adl_2020", "adl_2023"), "age_2018",
  c("female", "education", "baseline_poor_srh",
    "baseline_depression9"),
  "CLASS", c(2020, 2023), m = 20
)

# -------------------------------------------------------------------------
# CFPS supplementary quasi-experimental robustness
# -------------------------------------------------------------------------

cat("[3] CFPS\n")
cfps <- fread(file.path(root, "05_analysis_data",
                        "cfps_elderly_cohort.csv.gz"), encoding = "UTF-8") |>
  mutate(
    pid = as.character(pid), wave = as.integer(wave),
    poor = ifelse(is.na(srh), NA_real_, as.numeric(srh <= 2)),
    high75 = as.integer(baseline_age >= 75),
    highneed = ifelse(baseline_age >= 75, 1,
                      ifelse(is.na(baseline_dw), NA_real_,
                             as.integer(baseline_dw <= 2))),
    city = floor(admin_code / 100) * 100,
    province = ifelse(!is.na(provcd14), provcd14,
                      ifelse(!is.na(provcd), provcd,
                             floor(admin_code / 10000)))
  ) |>
  filter(baseline_age >= 65, wave %in% c(2014, 2018))

cfps_base <- cfps |>
  filter(wave == 2014) |>
  select(pid, treat, city, province, high75, highneed,
         baseline_age, baseline_female, edu, baseline_srh, baseline_dw,
         urban14, mar) |>
  distinct(pid, .keep_all = TRUE)
cfps_outcomes <- cfps |>
  select(pid, wave, poor) |>
  distinct(pid, wave, .keep_all = TRUE) |>
  pivot_wider(names_from = wave, values_from = poor,
              names_prefix = "poor_")
cfps_w <- left_join(cfps_base, cfps_outcomes, by = "pid") |>
  filter(!is.na(poor_2014), !is.na(poor_2018)) |>
  mutate(change = poor_2018 - poor_2014)

cluster_vcov <- function(fit, cluster) {
  sandwich::vcovCL(fit, cluster = cluster, type = "HC1")
}

wild_cluster_test <- function(formula_full, formula_restricted, term,
                              dat, cluster, B = 999) {
  full <- lm(formula_full, data = dat)
  rest <- lm(formula_restricted, data = dat)
  vv <- cluster_vcov(full, dat[[cluster]])
  tobs <- coef(full)[term] / sqrt(diag(vv))[term]
  fitted0 <- fitted(rest)
  resid0 <- resid(rest)
  g <- as.character(dat[[cluster]])
  groups <- unique(g)
  tb <- numeric(B)
  for (b in seq_len(B)) {
    signs <- sample(c(-1, 1), length(groups), replace = TRUE)
    names(signs) <- groups
    ystar <- fitted0 + resid0 * signs[g]
    dd <- dat
    dd$ystar <- ystar
    fstar <- update(formula_full, ystar ~ .)
    fm <- lm(fstar, data = dd)
    vb <- cluster_vcov(fm, dd[[cluster]])
    tb[b] <- coef(fm)[term] / sqrt(diag(vb))[term]
  }
  data.frame(
    term = term, estimate = coef(full)[term],
    std_error = sqrt(diag(vv))[term], t_statistic = tobs,
    wild_cluster_p = mean(abs(tb) >= abs(tobs)),
    clusters = length(groups), replications = B
  )
}

cfps_wild <- bind_rows(
  wild_cluster_test(change ~ treat, change ~ 1, "treat",
                    cfps_w, "city"),
  wild_cluster_test(change ~ treat * high75, change ~ treat + high75,
                    "treat:high75", cfps_w, "city"),
  wild_cluster_test(change ~ treat * highneed,
                    change ~ treat + highneed,
                    "treat:highneed", cfps_w |> filter(!is.na(highneed)),
                    "city")
) |>
  mutate(estimand = c("Poor SRH DID", "Age 75+ DDD", "High-need DDD"))

leave_one <- function(dat, unit, formula, term, label) {
  bind_rows(lapply(sort(unique(dat[[unit]])), function(u) {
    dd <- dat[dat[[unit]] != u, ]
    fit <- lm(formula, data = dd)
    vv <- cluster_vcov(fit, dd$city)
    data.frame(
      analysis = label, omitted_unit = as.character(u),
      estimate = coef(fit)[term],
      std_error = sqrt(diag(vv))[term],
      conf_low = coef(fit)[term] - 1.96 * sqrt(diag(vv))[term],
      conf_high = coef(fit)[term] + 1.96 * sqrt(diag(vv))[term],
      n = nrow(dd)
    )
  }))
}
cfps_leaveout <- bind_rows(
  leave_one(cfps_w, "city", change ~ treat, "treat",
            "Leave-one-city-out DID"),
  leave_one(cfps_w, "province", change ~ treat, "treat",
            "Leave-one-province-out DID")
)

ps_vars <- c("baseline_age", "baseline_female", "edu", "baseline_srh",
             "baseline_dw", "urban14", "mar")
pp <- prepare_predictors(cfps_w, ps_vars)
ps_dat <- pp$data
ps_fit <- glm(reformulate(pp$variables, response = "treat"),
              data = ps_dat, family = binomial())
ps <- pmin(pmax(predict(ps_fit, type = "response"), 0.01), 0.99)
att_w <- ifelse(ps_dat$treat == 1, 1, ps / (1 - ps))
qatt <- quantile(att_w, c(0.01, 0.99))
att_w <- pmin(pmax(att_w, qatt[1]), qatt[2])
ps_dat$att_weight <- att_w
ps_did <- lm(change ~ treat, data = ps_dat, weights = att_weight)
ps_v <- cluster_vcov(ps_did, ps_dat$city)
cfps_ps_result <- data.frame(
  analysis = "Propensity-score ATT weighted DID",
  estimate = coef(ps_did)["treat"],
  std_error = sqrt(diag(ps_v))["treat"],
  conf_low = coef(ps_did)["treat"] - 1.96 * sqrt(diag(ps_v))["treat"],
  conf_high = coef(ps_did)["treat"] + 1.96 * sqrt(diag(ps_v))["treat"],
  p_value = 2 * pnorm(-abs(coef(ps_did)["treat"] /
                              sqrt(diag(ps_v))["treat"])),
  effective_sample_size = sum(att_w)^2 / sum(att_w^2)
)

balance_one <- function(v) {
  x <- ps_dat[[v]]
  t <- ps_dat$treat == 1
  smd_unw <- (mean(x[t]) - mean(x[!t])) /
    sqrt((var(x[t]) + var(x[!t])) / 2)
  wm <- function(z, w) sum(z * w) / sum(w)
  wv <- function(z, w) sum(w * (z - wm(z, w))^2) / sum(w)
  smd_w <- (wm(x[t], att_w[t]) - wm(x[!t], att_w[!t])) /
    sqrt((wv(x[t], att_w[t]) + wv(x[!t], att_w[!t])) / 2)
  data.frame(variable = v, smd_unweighted = smd_unw,
             smd_weighted = smd_w)
}
cfps_balance <- bind_rows(lapply(pp$variables, balance_one))
cfps_overlap <- bind_rows(
  data.frame(treat = 0, propensity = ps[ps_dat$treat == 0]),
  data.frame(treat = 1, propensity = ps[ps_dat$treat == 1])
)

main_cfps <- lm(change ~ treat, data = cfps_w)
main_v <- cluster_vcov(main_cfps, cfps_w$city)
est <- coef(main_cfps)["treat"]
se_ <- sqrt(diag(main_v))["treat"]
cfps_equiv <- data.frame(
  estimate = est, std_error = se_,
  ci90_low = est - qnorm(0.95) * se_,
  ci90_high = est + qnorm(0.95) * se_,
  equivalence_margin_low = -0.05,
  equivalence_margin_high = 0.05,
  equivalent_within_5pp =
    est - qnorm(0.95) * se_ > -0.05 &
    est + qnorm(0.95) * se_ < 0.05,
  minimum_detectable_effect_80pct = (qnorm(0.975) + qnorm(0.80)) * se_
)

# -------------------------------------------------------------------------
# Trajectory classification diagnostics from the complete fitted solutions
# -------------------------------------------------------------------------

trajectory_diagnostics <- function(selection_path, quality_path, cohort) {
  sel <- fread(selection_path)
  qual <- fread(quality_path)
  qual2 <- qual |>
    mutate(
      prior_odds = proportion / (1 - proportion),
      posterior_odds = mean_posterior / (1 - mean_posterior),
      odds_correct_classification = posterior_odds / prior_odds,
      cohort = cohort
    )
  list(selection = sel |> mutate(cohort = cohort), quality = qual2)
}
td_charls <- trajectory_diagnostics(
  file.path(diag_dir, "r_charls_lcga_model_selection.csv"),
  file.path(diag_dir, "r_charls_lcga_classification_quality.csv"),
  "CHARLS cognition"
)
td_class <- trajectory_diagnostics(
  file.path(diag_dir, "r_class_lcga_model_selection.csv"),
  file.path(diag_dir, "r_class_lcga_quality.csv"),
  "CLASS depressive symptoms"
)

# -------------------------------------------------------------------------
# Exports
# -------------------------------------------------------------------------

cut_tbl <- bind_rows(c(charls_cut, class_cut))
cont_tbl <- bind_rows(c(charls_cont, class_cont))
ipcw_tbl <- bind_rows(c(charls_ipcw_rows, class_ipcw_rows))
mi_tbl <- bind_rows(charls_mi$result, class_mi$result) |>
  mutate(across(c(estimate, std_error, conf_low, conf_high), ~.x * 100))
raw_tbl <- bind_rows(c(list(charls_raw), class_raw))
spline_tbl <- bind_rows(
  charls_spline$curve,
  bind_rows(lapply(class_spline, `[[`, "curve"))
)
monotonic_tbl <- bind_rows(
  charls_spline$monotonic,
  bind_rows(lapply(class_spline, `[[`, "monotonic"))
)
knots_tbl <- bind_rows(
  charls_spline$knots,
  bind_rows(lapply(class_spline, `[[`, "knots"))
)
attr_tbl <- bind_rows(c(list(charls_attr), class_attr))
attr_rate_tbl <- bind_rows(c(list(charls_attr_rate), class_attr_rate))
weight_tbl <- bind_rows(c(list(charls_weight_diag), class_weight_diag))
extreme_tbl <- bind_rows(c(list(charls_extreme), class_extreme))

fwrite(cut_tbl, file.path(tab_dir, "r_v4_age_cutpoint_sensitivity.csv"))
fwrite(cont_tbl, file.path(tab_dir, "r_v4_continuous_age_gradients.csv"))
fwrite(ipcw_tbl, file.path(tab_dir, "r_v4_ipcw_trim_sensitivity.csv"))
fwrite(mi_tbl, file.path(tab_dir, "r_v4_multiple_imputation_adl.csv"))
fwrite(raw_tbl, file.path(tab_dir, "r_v4_age_group_raw_adl_risks.csv"))
fwrite(spline_tbl, file.path(tab_dir, "r_v4_standardized_adl_age_splines.csv"))
fwrite(monotonic_tbl, file.path(diag_dir, "r_v4_age_gradient_monotonicity.csv"))
fwrite(knots_tbl, file.path(diag_dir, "r_v4_spline_knots.csv"))
fwrite(attr_tbl, file.path(diag_dir, "r_v4_attrition_baseline_comparison.csv"))
fwrite(attr_rate_tbl,
       file.path(diag_dir, "r_v4_attrition_rates_by_age_group.csv"))
fwrite(weight_tbl, file.path(diag_dir, "r_v4_ipcw_weight_diagnostics.csv"))
fwrite(extreme_tbl, file.path(tab_dir, "r_v4_extreme_scenario_sensitivity.csv"))
fwrite(placebo_tbl, file.path(tab_dir, "r_v4_charls_placebo_periods.csv"))
fwrite(trend_adjusted,
       file.path(tab_dir, "r_v4_charls_differential_trend_adjusted.csv"))
fwrite(parallel_bounds,
       file.path(tab_dir, "r_v4_charls_nonparallel_sensitivity_bounds.csv"))
fwrite(pre_extrap,
       file.path(tab_dir, "r_v4_charls_age_counterfactual_2018.csv"))
fwrite(cfps_wild, file.path(tab_dir, "r_v4_cfps_wild_cluster_bootstrap.csv"))
fwrite(cfps_leaveout, file.path(tab_dir, "r_v4_cfps_leaveout.csv"))
fwrite(cfps_ps_result, file.path(tab_dir, "r_v4_cfps_ps_weighted.csv"))
fwrite(cfps_balance, file.path(diag_dir, "r_v4_cfps_balance.csv"))
fwrite(cfps_overlap, file.path(diag_dir, "r_v4_cfps_propensity_overlap.csv"))
fwrite(cfps_equiv, file.path(tab_dir, "r_v4_cfps_equivalence_mde.csv"))
fwrite(bind_rows(td_charls$selection, td_class$selection),
       file.path(diag_dir, "r_v4_trajectory_model_comparison.csv"))
fwrite(bind_rows(td_charls$quality, td_class$quality),
       file.path(diag_dir, "r_v4_trajectory_classification_quality.csv"))

saveRDS(
  list(
    charls = list(
      continuous = zc$model, ipcw = charls_ipcw,
      spline = charls_spline$fit, mi = charls_mi$mids,
      trend = trend_fit, pretrend_extrapolation = pre_fit
    ),
    class = list(
      splines = lapply(class_spline, `[[`, "fit"),
      mi = class_mi$mids
    ),
    cfps = list(ps = ps_fit, weighted_did = ps_did),
    session = sessionInfo()
  ),
  file.path(model_dir, "r_v4_required_models.rds")
)

cat("\nAge-cutpoint estimates (percentage points)\n")
print(cut_tbl)
cat("\nContinuous-age gradients (percentage points per 5 years)\n")
print(cont_tbl)
cat("\nIPCW trimming sensitivity\n")
print(ipcw_tbl)
cat("\nMultiple imputation\n")
print(mi_tbl)
cat("\nCHARLS placebo and trend-adjusted results\n")
print(placebo_tbl)
print(trend_adjusted)
cat("\nCFPS robustness\n")
print(cfps_wild)
print(cfps_ps_result)
print(cfps_equiv)
cat("\nCompleted:", format(Sys.time()), "\n")
