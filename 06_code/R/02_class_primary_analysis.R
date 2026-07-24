#!/usr/bin/env Rscript

# CLASS primary longitudinal analysis, 2018-2023.
# CLASS waves occur after the 2016 national policy expansion, so this module
# estimates post-policy health trajectories and does not claim a policy DID.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(fixest)
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

log_file <- file.path(path_logs, "r_class_primary_analysis.log")
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("CLASS primary longitudinal analysis\n")
cat("Started:", format(Sys.time()), "\n")

class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"
paths <- c(
  `2018` = file.path(class_root, "CLASS2018-cleaned release.dta"),
  `2020` = file.path(class_root, "individual -2020  cleaned for user.dta"),
  `2023` = file.path(class_root, "2023_individual_release_weighted .dta")
)
stopifnot(all(file.exists(paths)))

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

score_dep9 <- function(df, vars) {
  items <- lapply(vars, function(v) {
    x <- safe_numeric(df[[v]])
    x[!x %in% 1:3] <- NA_real_
    x
  })
  mat <- do.call(cbind, items)
  # Positive-affect items 1, 4 and 9 are reverse scored. The common score
  # ranges from 0 to 18, with higher values indicating more symptoms.
  scored <- mat - 1
  scored[, c(1, 4, 9)] <- 2 - scored[, c(1, 4, 9)]
  observed <- rowSums(!is.na(scored))
  total <- rowSums(scored, na.rm = TRUE)
  ifelse(observed >= 7, total * 9 / observed, NA_real_)
}

extract_wave <- function(year, path) {
  d <- read_dta(path)
  if (year == 2018) {
    id <- as.character(d[["q1__1__open"]])
    sex <- safe_numeric(d[["a1"]])
    srh <- safe_numeric(d[["b1"]])
    dep_vars <- paste0("e2__", 1:9)
  } else {
    id <- as.character(d[["Q1_1_open"]])
    sex <- safe_numeric(d[["A1"]])
    srh <- safe_numeric(d[["B1"]])
    dep_vars <- paste0("E2_", 1:9)
  }
  data.frame(
    class_id = id,
    year = year,
    female = ifelse(sex %in% c(1, 2), as.integer(sex == 2), NA_integer_),
    poor_srh = ifelse(srh %in% 1:5, as.integer(srh >= 4), NA_integer_),
    srh_score = ifelse(srh %in% 1:5, srh, NA_real_),
    depression9 = score_dep9(d, dep_vars)
  )
}

class_long <- bind_rows(lapply(names(paths), function(y) {
  extract_wave(as.integer(y), paths[[y]])
})) |>
  filter(!is.na(class_id), nzchar(class_id))

id_counts <- class_long |>
  distinct(class_id, year) |>
  count(class_id, name = "waves_observed")
class_long <- left_join(class_long, id_counts, by = "class_id")

flow <- class_long |>
  group_by(year) |>
  summarise(
    n = n_distinct(class_id),
    poor_srh_observed = sum(!is.na(poor_srh)),
    depression_observed = sum(!is.na(depression9)),
    repeated_ids = sum(waves_observed >= 2),
    .groups = "drop"
  )
fwrite(flow, file.path(path_diag, "class_sample_flow.csv"))

overlap <- tidyr::expand_grid(year_a = c(2018, 2020, 2023),
                              year_b = c(2018, 2020, 2023)) |>
  filter(year_a < year_b) |>
  rowwise() |>
  mutate(
    n_overlap = length(intersect(
      class_long$class_id[class_long$year == year_a],
      class_long$class_id[class_long$year == year_b]
    ))
  ) |>
  ungroup()
fwrite(overlap, file.path(path_diag, "class_id_overlap.csv"))

# Within-person post-policy trends. These are longitudinal changes, not DID.
class_panel <- class_long |> filter(waves_observed >= 2)
model_srh <- feols(
  poor_srh ~ i(year, ref = 2018) | class_id,
  data = class_panel,
  cluster = ~class_id
)
model_dep <- feols(
  depression9 ~ i(year, ref = 2018) | class_id,
  data = class_panel,
  cluster = ~class_id
)

tidy_event <- function(model, outcome) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  names(ct)[1:4] <- c("estimate", "std_error", "t_value", "p_value")
  ct$year <- as.integer(sub(".*::([0-9]{4}).*", "\\1", ct$term))
  ct$conf_low <- ct$estimate - 1.96 * ct$std_error
  ct$conf_high <- ct$estimate + 1.96 * ct$std_error
  out <- ct[, c("year", "term", "estimate", "std_error",
                "conf_low", "conf_high", "p_value")]
  out$outcome <- outcome
  bind_rows(
    out,
    data.frame(year = 2018, term = "Reference", estimate = 0,
               std_error = NA_real_, conf_low = 0, conf_high = 0,
               p_value = NA_real_, outcome = outcome)
  ) |>
    arrange(year)
}

class_change <- bind_rows(
  tidy_event(model_srh, "Poor self-rated health"),
  tidy_event(model_dep, "Common 9-item depressive symptom score")
)
fwrite(class_change, file.path(path_tables, "r_class_longitudinal_changes.csv"))

raw_trends <- class_long |>
  group_by(year) |>
  summarise(
    n = n_distinct(class_id),
    poor_srh = mean(poor_srh, na.rm = TRUE),
    depression9 = mean(depression9, na.rm = TRUE),
    .groups = "drop"
  )
fwrite(raw_trends, file.path(path_tables, "r_class_raw_trends.csv"))

# LCGA-like finite mixture for common depressive symptoms. With three waves,
# use linear class-specific trajectories; quadratic terms would be saturated.
traj <- class_panel |>
  filter(!is.na(depression9)) |>
  group_by(class_id) |>
  filter(n_distinct(year) >= 2) |>
  ungroup() |>
  mutate(time = year - 2018)

id_map <- traj |> distinct(class_id) |> mutate(ID_num = row_number())
traj <- left_join(traj, id_map, by = "class_id")

models <- list()
for (k in 1:5) {
  cat("Fitting CLASS depression trajectory:", k, "classes\n")
  if (k == 1L) {
    models[[as.character(k)]] <- flexmix(
      depression9 ~ time | ID_num,
      data = traj,
      k = 1,
      model = FLXMRglm(family = "gaussian"),
      control = list(iter.max = 300, minprior = 0.01, verbose = 0)
    )
  } else {
    candidates <- stepFlexmix(
      depression9 ~ time | ID_num,
      data = traj,
      k = k,
      nrep = 3,
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

entropy_value <- function(probs) {
  probs <- pmax(as.matrix(probs), 1e-12)
  n <- nrow(probs)
  k <- ncol(probs)
  if (k == 1L) return(1)
  1 + sum(probs * log(probs)) / (n * log(k))
}

diagnostics <- bind_rows(lapply(names(models), function(k) {
  m <- models[[k]]
  cls <- clusters(m)
  post <- posterior(m)
  tmp <- data.frame(ID_num = traj$ID_num, class = cls, post) |>
    distinct(ID_num, .keep_all = TRUE)
  tab <- prop.table(table(tmp$class))
  data.frame(
    classes = as.integer(k),
    log_likelihood = as.numeric(logLik(m)),
    AIC = AIC(m),
    BIC = BIC(m),
    convergence = as.integer(m@converged),
    entropy = entropy_value(tmp[, setdiff(names(tmp), c("ID_num", "class")),
                                drop = FALSE]),
    minimum_class_pct = 100 * min(tab),
    maximum_class_pct = 100 * max(tab)
  )
}))

admissible <- diagnostics |>
  filter(convergence == 1, minimum_class_pct >= 10, entropy >= 0.75,
         classes >= 3)
if (!nrow(admissible)) {
  stop("No admissible CLASS trajectory solution.")
}
# Prefer the smallest clinically interpretable solution meeting classification
# standards. BIC can keep improving by splitting near-parallel severity levels;
# the primary trajectory solution therefore avoids that over-extraction.
selected_k <- min(admissible$classes)
selected <- models[[as.character(selected_k)]]
cls <- clusters(selected)
post <- posterior(selected)
pp <- data.frame(ID_num = traj$ID_num, class = cls, post) |>
  distinct(ID_num, .keep_all = TRUE)
prob_cols <- setdiff(names(pp), c("ID_num", "class"))
pp$mean_posterior <- apply(pp[, prob_cols, drop = FALSE], 1, max)
membership <- pp |>
  left_join(id_map, by = "ID_num") |>
  select(class_id, ID_num, class, mean_posterior)

traj_class <- left_join(traj, membership, by = c("class_id", "ID_num"))
profiles <- traj_class |>
  group_by(class, year) |>
  summarise(
    n = n(),
    mean_score = mean(depression9),
    se = sd(depression9) / sqrt(n()),
    conf_low = mean_score - 1.96 * se,
    conf_high = mean_score + 1.96 * se,
    .groups = "drop"
  )
order_2023 <- profiles |>
  filter(year == 2023) |>
  arrange(mean_score) |>
  pull(class)
class_labels <- setNames(
  c("Low-stable", "Moderate", "High/persistent", "Increasing", "Residual")[
    seq_along(order_2023)
  ],
  order_2023
)
profiles$class_label <- class_labels[as.character(profiles$class)]
membership$class_label <- class_labels[as.character(membership$class)]

quality <- membership |>
  group_by(class, class_label) |>
  summarise(
    n = n(),
    proportion = n() / nrow(membership),
    mean_posterior = mean(mean_posterior),
    pct_posterior_ge_070 = mean(mean_posterior >= 0.70),
    .groups = "drop"
  )

set.seed(20260724)
sample_ids <- membership |>
  group_by(class) |>
  group_modify(~slice_sample(.x, n = min(80L, nrow(.x)))) |>
  ungroup() |>
  pull(class_id)
spaghetti <- traj_class |>
  filter(class_id %in% sample_ids) |>
  mutate(class_label = class_labels[as.character(class)])

fwrite(diagnostics, file.path(path_diag, "r_class_lcga_model_selection.csv"))
fwrite(quality, file.path(path_diag, "r_class_lcga_quality.csv"))
fwrite(membership, file.path(path_tables, "r_class_lcga_membership.csv"))
fwrite(profiles, file.path(path_tables, "r_class_lcga_profiles.csv"))
fwrite(spaghetti, file.path(path_tables, "r_class_lcga_spaghetti.csv"))

saveRDS(
  list(
    longitudinal_models = list(poor_srh = model_srh, depression9 = model_dep),
    trajectory_models = models,
    selected_k = selected_k,
    selected_model = selected,
    session = sessionInfo()
  ),
  file.path(path_models, "r_class_primary_models.rds")
)

cat("\nCLASS within-person changes\n")
print(class_change)
cat("\nCLASS trajectory diagnostics\n")
print(diagnostics)
cat("\nSelected classes:", selected_k, "\n")
print(quality)
cat("\nCompleted:", format(Sys.time()), "\n")
