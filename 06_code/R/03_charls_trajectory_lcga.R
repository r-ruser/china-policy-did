#!/usr/bin/env Rscript

# Exploratory finite-mixture growth / LCGA-like modelling of CHARLS cognitive trajectories.
# This is a person-centred descriptive analysis, not a causal exposure model.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(flexmix)
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

cat("Exploratory CHARLS finite-mixture growth / LCGA-like trajectory analysis\n")
cat("Started:", format(Sys.time()), "\n")

charls_path <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
stopifnot(file.exists(charls_path))
d <- read_dta(charls_path)

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
d <- d |>
  mutate(
    age_2015 = 2015 - safe_numeric(rabyear),
    female = as.integer(safe_numeric(ragender) == 2)
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
    time = yr - 2011,
    cognition_raw = ifelse(complete, orient + imrc + ser7, NA_real_),
    age_2015 = d$age_2015,
    female = d$female
  )
})) |>
  filter(!is.na(cognition_raw))

# flexmix uses a numeric subject identifier here. Preserve the original ID separately.
id_map <- long |> distinct(ID_chr) |> mutate(ID_num = row_number())
long <- left_join(long, id_map, by = "ID_chr")

eligible_ids <- long |>
  count(ID_num, name = "n_measurements") |>
  filter(n_measurements >= 3)
long <- semi_join(long, eligible_ids, by = "ID_num")

# Use one fixed baseline distribution for standardisation so genuine temporal
# change is preserved; wave-specific z-scoring would erase secular decline.
baseline_values <- long$cognition_raw[long$year == 2011]
z_center <- mean(baseline_values, na.rm = TRUE)
z_scale <- sd(baseline_values, na.rm = TRUE)
long <- long |>
  mutate(cognition_z = (cognition_raw - z_center) / z_scale)

cat("Eligible individuals:", n_distinct(long$ID_num), "\n")
cat("Observations:", nrow(long), "\n")
cat("Baseline standardisation:", z_center, z_scale, "\n")

# Candidate 1-5 class LCGA-like finite mixtures use quadratic time trends and
# subject-level grouping. Multiple starts reduce local-maximum dependence.
# This parsimonious model is intentionally preferred to a high-dimensional
# random-slope GMM because only four observations per person are available.
models <- list()
for (k in 1:5) {
  cat("Fitting", k, "classes\n")
  if (k == 1L) {
    models[[as.character(k)]] <- flexmix(
      cognition_z ~ time + I(time^2) | ID_num,
      data = long,
      k = 1,
      model = FLXMRglm(family = "gaussian"),
      control = list(iter.max = 500, minprior = 0.01, verbose = 0)
    )
  } else {
    candidates <- stepFlexmix(
      cognition_z ~ time + I(time^2) | ID_num,
      data = long,
      k = k,
      nrep = 2,
      model = FLXMRglm(family = "gaussian"),
      control = list(iter.max = 300, minprior = 0.01, verbose = 0)
    )
    models[[as.character(k)]] <- if (inherits(candidates, "stepFlexmix")) {
      getModel(candidates, "BIC")
    } else {
      candidates
    }
  }
}

entropy_from_pp <- function(pp) {
  probs <- as.matrix(pp)
  probs <- pmax(probs, 1e-12)
  n <- nrow(probs)
  k <- ncol(probs)
  if (k == 1L) return(1)
  1 + sum(probs * log(probs)) / (n * log(k))
}

diagnostics <- bind_rows(lapply(names(models), function(k) {
  m <- models[[k]]
  cls <- clusters(m)
  id_class <- data.frame(ID_num = long$ID_num, class = cls) |>
    distinct(ID_num, .keep_all = TRUE)
  class_tab <- prop.table(table(id_class$class))
  post <- posterior(m)
  id_post <- data.frame(ID_num = long$ID_num, post) |>
    distinct(ID_num, .keep_all = TRUE)
  data.frame(
    classes = as.integer(k),
    log_likelihood = as.numeric(logLik(m)),
    AIC = AIC(m),
    BIC = BIC(m),
    convergence = as.integer(m@converged),
    entropy = entropy_from_pp(id_post[, -1, drop = FALSE]),
    minimum_class_pct = 100 * min(class_tab),
    maximum_class_pct = 100 * max(class_tab)
  )
}))

# Selection rule: converged solution, each class >=10%, entropy >=0.70
# after classification; choose the lowest BIC among admissible candidates.
admissible <- diagnostics |>
  filter(convergence == 1, minimum_class_pct >= 10, entropy >= 0.70,
         classes >= 2)
if (nrow(admissible) == 0L) {
  stop("No admissible latent-class solution (convergence, entropy >=0.70, and >=10% class size).")
}
selected_k <- admissible$classes[which.min(admissible$BIC)]
selected <- models[[as.character(selected_k)]]
selected_class <- clusters(selected)
selected_post <- posterior(selected)
pp_obs <- data.frame(
  ID_num = long$ID_num,
  class = selected_class,
  selected_post,
  check.names = FALSE
)
pp <- pp_obs |>
  distinct(ID_num, .keep_all = TRUE)
prob_cols <- setdiff(names(pp), c("ID_num", "class"))
pp$mean_posterior <- apply(pp[, prob_cols, drop = FALSE], 1, max)
quality_by_class <- pp |>
  group_by(class) |>
  summarise(
    n = n(),
    proportion = n() / nrow(pp),
    mean_posterior = mean(mean_posterior),
    pct_posterior_ge_070 = mean(mean_posterior >= 0.70),
    pct_posterior_ge_080 = mean(mean_posterior >= 0.80),
    .groups = "drop"
  )

if (any(quality_by_class$mean_posterior < 0.70)) {
  warning("Selected solution contains a class with mean posterior probability <0.70.")
}

class_map <- pp |>
  transmute(ID_num = as.integer(ID_num), class = as.integer(class),
            mean_posterior)
long_class <- left_join(long, class_map, by = "ID_num")

# Name classes by model-implied ordering at 2018, not arbitrary class number.
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
label_bank <- if (length(order_2018) == 2L) {
  c("Higher-stable", "Lower/declining")
} else if (length(order_2018) == 3L) {
  c("Higher-stable", "Intermediate", "Lower/declining")
} else {
  c("Higher-stable", "Upper-intermediate", "Intermediate",
    "Lower/declining", "Very-low")
}
labels <- setNames(label_bank[seq_along(order_2018)], order_2018)
class_map$class_label <- labels[as.character(class_map$class)]
long_class <- left_join(
  long |> select(-any_of(c("class", "mean_posterior"))),
  class_map,
  by = "ID_num"
)
profile_observed$class_label <- labels[as.character(profile_observed$class)]
quality_by_class$class_label <- labels[as.character(quality_by_class$class)]

fwrite(diagnostics, file.path(path_diag, "r_charls_lcga_model_selection.csv"))
fwrite(quality_by_class, file.path(path_diag, "r_charls_lcga_classification_quality.csv"))
fwrite(class_map, file.path(path_tables, "r_charls_lcga_class_membership.csv"))
fwrite(profile_observed, file.path(path_tables, "r_charls_lcga_trajectory_profiles.csv"))

# A deterministic, bounded sample supports a transparent spaghetti preview.
set.seed(20260724)
spaghetti_ids <- long_class |>
  distinct(ID_num, class) |>
  group_by(class) |>
  group_modify(~slice_sample(.x, n = min(80L, nrow(.x)))) |>
  ungroup() |>
  pull(ID_num)
fwrite(
  long_class |> filter(ID_num %in% spaghetti_ids),
  file.path(path_tables, "r_charls_lcga_spaghetti_sample.csv")
)

saveRDS(
  list(
    selected_k = selected_k,
    selected_model = selected,
    candidate_models = models,
    standardisation = c(center = z_center, scale = z_scale),
    session = sessionInfo()
  ),
  file.path(path_models, "r_charls_lcga_models.rds")
)

cat("\nModel selection\n")
print(diagnostics)
cat("\nSelected classes:", selected_k, "\n")
print(quality_by_class)
cat("\nCompleted:", format(Sys.time()), "\n")
