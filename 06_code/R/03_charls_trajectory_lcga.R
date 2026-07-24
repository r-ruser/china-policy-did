#!/usr/bin/env Rscript

# Exploratory CHARLS latent class growth analysis of cognitive trajectories.
# Baseline factors are related to latent-class membership in a one-step model.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(lcmm)
})

options(encoding = "UTF-8")
set.seed(20260724)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
path_tables <- file.path(project_root, "07_results", "tables")
path_models <- file.path(project_root, "07_results", "models")
path_diag <- file.path(project_root, "07_results", "diagnostics")
path_logs <- file.path(project_root, "10_logs")
invisible(lapply(c(path_tables, path_models, path_diag, path_logs), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

log_file <- file.path(path_logs, "r_charls_trajectory_lcga.log")
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("Exploratory CHARLS latent class growth analysis\n")
cat("Started:", format(Sys.time()), "\n")

charls_path <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
stopifnot(file.exists(charls_path))
d <- read_dta(charls_path)

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
d <- d |>
  mutate(
    age_2011 = 2011 - safe_numeric(rabyear),
    age_2015 = 2015 - safe_numeric(rabyear),
    female = ifelse(
      safe_numeric(ragender) %in% c(1, 2),
      as.integer(safe_numeric(ragender) == 2),
      NA_integer_
    ),
    education = ifelse(
      safe_numeric(raeducl) >= 0 & safe_numeric(raeducl) <= 3,
      safe_numeric(raeducl),
      NA_real_
    ),
    poor_srh_2011 = ifelse(
      safe_numeric(r1shlta) %in% 1:5,
      as.integer(safe_numeric(r1shlta) >= 4),
      NA_integer_
    )
  ) |>
  filter(age_2015 >= 50, age_2015 <= 69, safe_numeric(inw3) == 1)

wave_year <- c(`1` = 2011, `2` = 2013, `3` = 2015, `4` = 2018)
long <- bind_rows(lapply(names(wave_year), function(w) {
  yr <- unname(wave_year[[w]])
  orient <- safe_numeric(d[[paste0("r", w, "orient")]])
  imrc <- safe_numeric(d[[paste0("r", w, "imrc")]])
  ser7 <- safe_numeric(d[[paste0("r", w, "ser7")]])
  complete <- !is.na(orient) & !is.na(imrc) & !is.na(ser7)
  data.frame(
    ID_chr = as.character(d$ID),
    year = yr,
    time = (yr - 2011) / 7,
    cognition_raw = ifelse(complete, orient + imrc + ser7, NA_real_),
    age_2011 = d$age_2011,
    age_2015 = d$age_2015,
    female = d$female,
    education = d$education,
    poor_srh_2011 = d$poor_srh_2011
  )
})) |>
  filter(!is.na(cognition_raw))

id_map <- long |>
  distinct(ID_chr) |>
  mutate(ID_num = row_number())
long <- left_join(long, id_map, by = "ID_chr")

eligible_ids <- long |>
  count(ID_num, name = "n_measurements") |>
  filter(n_measurements >= 3)
long <- semi_join(long, eligible_ids, by = "ID_num")

# Standardise to the fixed 2011 distribution so temporal change is retained.
baseline_values <- long$cognition_raw[long$year == 2011]
z_center <- mean(baseline_values, na.rm = TRUE)
z_scale <- sd(baseline_values, na.rm = TRUE)
long <- long |>
  mutate(cognition_z = (cognition_raw - z_center) / z_scale) |>
  arrange(ID_num, year)

cat("Eligible individuals:", n_distinct(long$ID_num), "\n")
cat("Observations:", nrow(long), "\n")
cat("Baseline standardisation:", z_center, z_scale, "\n")

entropy_value <- function(probs) {
  probs <- pmax(as.matrix(probs), 1e-12)
  n <- nrow(probs)
  k <- ncol(probs)
  if (k == 1L) return(1)
  1 + sum(probs * log(probs)) / (n * log(k))
}

posterior_table <- function(model, k, data) {
  if (k == 1L) {
    return(data.frame(
      ID_num = sort(unique(data$ID_num)),
      class = 1L,
      prob1 = 1
    ))
  }
  pp <- as.data.frame(model$pprob)
  names(pp)[1] <- "ID_num"
  pp
}

# LCGA fixes within-class random-effect variance at zero. Quadratic
# class-specific time functions are estimable from the four CHARLS waves.
cat("Fitting CHARLS cognition trajectory: 1 class\n")
m1 <- hlme(
  fixed = cognition_z ~ time + I(time^2),
  random = ~ -1,
  subject = "ID_num",
  ng = 1,
  data = long,
  maxiter = 500,
  verbose = FALSE
)
if (m1$conv != 1) stop("The one-class CHARLS LCGA did not converge.")

cache_signature <- paste0(
  "n", n_distinct(long$ID_num),
  "_o", nrow(long),
  "_lcmm", as.character(packageVersion("lcmm")),
  "_quadratic_scaled7"
)
candidate_cache <- file.path(
  path_models,
  paste0("r_charls_lcga_candidate_cache_", cache_signature, ".rds")
)
models <- list(`1` = m1)
if (file.exists(candidate_cache)) {
  cached <- readRDS(candidate_cache)
  if (is.list(cached) && length(cached)) {
    models[names(cached)] <- cached
    models[["1"]] <- m1
  }
}
fit_candidate <- function(k) {
  fit_call <- substitute(
    gridsearch(
      rep = 4,
      maxiter = 20,
      minit = m1,
      cl = 2,
      hlme(
        fixed = cognition_z ~ time + I(time^2),
        mixture = ~ time + I(time^2),
        random = ~ -1,
        subject = "ID_num",
        ng = K,
        nwg = FALSE,
        data = long,
        maxiter = 500,
        verbose = FALSE
      )
    ),
    list(K = as.integer(k))
  )
  eval(fit_call)
}
for (k in 2:4) {
  if (!is.null(models[[as.character(k)]])) {
    cat("Using cached CHARLS cognition trajectory:", k, "classes\n")
    next
  }
  cat("Fitting CHARLS cognition trajectory:", k, "classes\n")
  models[[as.character(k)]] <- fit_candidate(k)
  saveRDS(models, candidate_cache)
}

diagnostics <- bind_rows(lapply(names(models), function(k) {
  m <- models[[k]]
  tmp <- posterior_table(m, as.integer(k), long)
  prob_cols <- grep("^prob", names(tmp), value = TRUE)
  tab <- prop.table(table(tmp$class))
  data.frame(
    classes = as.integer(k),
    log_likelihood = m$loglik,
    AIC = m$AIC,
    BIC = m$BIC,
    convergence = as.integer(m$conv == 1),
    entropy = entropy_value(tmp[, prob_cols, drop = FALSE]),
    minimum_class_pct = 100 * min(tab),
    maximum_class_pct = 100 * max(tab)
  )
}))

admissible <- diagnostics |>
  filter(convergence == 1, minimum_class_pct >= 10, entropy >= 0.70,
         classes >= 2)
if (!nrow(admissible)) {
  stop("No admissible CHARLS trajectory solution.")
}
selected_k <- admissible$classes[which.min(admissible$BIC)]
selected <- models[[as.character(selected_k)]]
pp <- posterior_table(selected, selected_k, long)
prob_cols <- grep("^prob", names(pp), value = TRUE)
pp$mean_posterior <- apply(pp[, prob_cols, drop = FALSE], 1, max)

class_map <- pp |>
  transmute(
    ID_num = as.integer(ID_num),
    class = as.integer(class),
    mean_posterior
  ) |>
  left_join(id_map, by = "ID_num")

long_class <- left_join(long, class_map, by = c("ID_chr", "ID_num"))
profile_observed <- long_class |>
  group_by(class, year) |>
  summarise(
    n = n(),
    mean_z = mean(cognition_z),
    se_z = sd(cognition_z) / sqrt(n()),
    conf_low = mean_z - 1.96 * se_z,
    conf_high = mean_z + 1.96 * se_z,
    .groups = "drop"
  )

order_2018 <- profile_observed |>
  filter(year == 2018) |>
  arrange(desc(mean_z)) |>
  pull(class)
label_bank <- switch(
  as.character(length(order_2018)),
  `2` = c("Higher-stable", "Lower/declining"),
  `3` = c("Higher-stable", "Intermediate", "Lower/declining"),
  `4` = c(
    "Higher-stable", "Upper-intermediate", "Lower-intermediate",
    "Lower/declining"
  ),
  c(
    "Higher-stable", "Upper-intermediate", "Intermediate",
    "Lower/declining", "Very-low"
  )
)
labels <- setNames(label_bank[seq_along(order_2018)], order_2018)
class_map$class_label <- labels[as.character(class_map$class)]
long_class$class_label <- labels[as.character(long_class$class)]
profile_observed$class_label <- labels[as.character(profile_observed$class)]

quality_by_class <- class_map |>
  group_by(class, class_label) |>
  summarise(
    n = n(),
    proportion = n() / nrow(class_map),
    mean_posterior = mean(mean_posterior),
    pct_posterior_ge_070 = mean(mean_posterior >= 0.70),
    pct_posterior_ge_080 = mean(mean_posterior >= 0.80),
    .groups = "drop"
  )

if (any(quality_by_class$mean_posterior < 0.70)) {
  warning("Selected solution contains a class with mean posterior probability <0.70.")
}

# One-step latent-class regression relates pre-specified 2011 baseline factors
# to latent trajectory membership while retaining classification uncertainty.
assoc_long <- long |>
  mutate(age5 = (age_2011 - 60) / 5) |>
  filter(complete.cases(age5, female, education, poor_srh_2011)) |>
  group_by(ID_num) |>
  filter(n_distinct(year) >= 3) |>
  ungroup() |>
  arrange(ID_num, year)

assoc_m1 <- hlme(
  fixed = cognition_z ~ time + I(time^2),
  random = ~ -1,
  subject = "ID_num",
  ng = 1,
  data = assoc_long,
  maxiter = 500,
  verbose = FALSE
)
assoc_call <- substitute(
  gridsearch(
    rep = 4,
    maxiter = 20,
    minit = assoc_m1,
    cl = 2,
    hlme(
      fixed = cognition_z ~ time + I(time^2),
      mixture = ~ time + I(time^2),
      random = ~ -1,
      classmb = ~ age5 + female + education + poor_srh_2011,
      subject = "ID_num",
      ng = K,
      nwg = FALSE,
      data = assoc_long,
      maxiter = 500,
      verbose = FALSE
    )
  ),
  list(K = as.integer(selected_k))
)
association_model <- eval(assoc_call)
if (association_model$conv != 1) {
  stop("The CHARLS baseline-factor latent-class regression did not converge.")
}

assoc_pp <- as.data.frame(association_model$pprob)
names(assoc_pp)[1] <- "ID_num"
assoc_profiles <- assoc_long |>
  left_join(assoc_pp[, c("ID_num", "class")], by = "ID_num") |>
  group_by(class, year) |>
  summarise(mean_z = mean(cognition_z), .groups = "drop")
assoc_order <- assoc_profiles |>
  filter(year == max(year)) |>
  arrange(desc(mean_z)) |>
  pull(class)
assoc_label_bank <- switch(
  as.character(length(assoc_order)),
  `2` = c("Higher-stable", "Lower/declining"),
  `3` = c("Higher-stable", "Intermediate", "Lower/declining"),
  `4` = c(
    "Higher-stable", "Upper-intermediate", "Lower-intermediate",
    "Lower/declining"
  ),
  c(
    "Higher-stable", "Upper-intermediate", "Intermediate",
    "Lower/declining", "Very-low"
  )
)
assoc_labels <- setNames(assoc_label_bank, assoc_order)

tidy_classmb <- function(model, labels, predictor_labels) {
  beta <- estimates(model)
  vc <- VarCov(model)
  se <- sqrt(diag(vc))
  idx <- unlist(lapply(names(predictor_labels), function(v) {
    grep(paste0("^", v, " class[0-9]+$"), names(beta))
  }))
  cls <- as.integer(sub(".* class", "", names(beta)[idx]))
  data.frame(
    predictor = sub(" class[0-9]+$", "", names(beta)[idx]),
    latent_class = cls,
    reference_class = model$ng,
    log_odds = beta[idx],
    std_error = se[idx],
    stringsAsFactors = FALSE
  ) |>
    mutate(
      predictor_label = unname(predictor_labels[predictor]),
      contrast = paste0(
        labels[as.character(latent_class)], " vs ",
        labels[as.character(reference_class)]
      ),
      odds_ratio = exp(log_odds),
      conf_low = exp(log_odds - 1.96 * std_error),
      conf_high = exp(log_odds + 1.96 * std_error),
      p_value = 2 * pnorm(abs(log_odds / std_error), lower.tail = FALSE)
    ) |>
    group_by(contrast) |>
    mutate(p_fdr = p.adjust(p_value, method = "BH")) |>
    ungroup() |>
    select(predictor, predictor_label, contrast, odds_ratio,
           conf_low, conf_high, p_value, p_fdr)
}

association_results <- tidy_classmb(
  association_model,
  assoc_labels,
  c(
    age5 = "Age, per 5 years",
    female = "Female sex",
    education = "Education, per category",
    poor_srh_2011 = "Poor self-rated health in 2011"
  )
)

fwrite(diagnostics, file.path(path_diag, "r_charls_lcga_model_selection.csv"))
fwrite(quality_by_class,
       file.path(path_diag, "r_charls_lcga_classification_quality.csv"))
fwrite(class_map, file.path(path_tables, "r_charls_lcga_class_membership.csv"))
fwrite(profile_observed,
       file.path(path_tables, "r_charls_lcga_trajectory_profiles.csv"))
fwrite(association_results,
       file.path(path_tables, "r_charls_lcga_associations.csv"))

set.seed(20260724)
spaghetti_ids <- class_map |>
  group_by(class) |>
  group_modify(~slice_sample(.x, n = min(80L, nrow(.x)))) |>
  ungroup() |>
  pull(ID_num)
fwrite(
  long_class |>
    filter(ID_num %in% spaghetti_ids),
  file.path(path_tables, "r_charls_lcga_spaghetti_sample.csv")
)

saveRDS(
  list(
    selected_k = selected_k,
    selected_model = selected,
    candidate_models = models,
    association_model = association_model,
    standardisation = c(center = z_center, scale = z_scale),
    session = sessionInfo()
  ),
  file.path(path_models, "r_charls_lcga_models.rds")
)

cat("\nModel selection\n")
print(diagnostics)
cat("\nSelected classes:", selected_k, "\n")
print(quality_by_class)
cat("\nBaseline-factor associations with CHARLS trajectory membership\n")
print(association_results)
cat("\nCompleted:", format(Sys.time()), "\n")
