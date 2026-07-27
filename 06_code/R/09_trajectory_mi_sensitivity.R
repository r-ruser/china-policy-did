#!/usr/bin/env Rscript

# Multiple-imputation sensitivity analysis for intermittent missing trajectory
# outcomes. Eligibility remains based on at least three genuinely observed
# measurements; imputation is never used to manufacture a third measurement.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(mice)
  library(lcmm)
})

options(encoding = "UTF-8")
set.seed(20260726)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
tables <- file.path(root, "07_results", "tables")
diag <- file.path(root, "07_results", "diagnostics")
models <- file.path(root, "07_results", "models")
logs <- file.path(root, "10_logs")
dir.create(diag, recursive = TRUE, showWarnings = FALSE)
dir.create(models, recursive = TRUE, showWarnings = FALSE)
dir.create(logs, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(logs, "r_trajectory_mi_sensitivity.log")
log_con <- file(log_path, "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("Trajectory multiple-imputation sensitivity analysis\n")
cat("Started:", format(Sys.time()), "\n\n")

entropy_value <- function(probs) {
  probs <- pmax(as.matrix(probs), 1e-12)
  if (ncol(probs) == 1L) return(1)
  1 + sum(probs * log(probs)) / (nrow(probs) * log(ncol(probs)))
}

posterior_table <- function(model, k, dat) {
  if (k == 1L) {
    return(data.frame(
      ID_num = sort(unique(dat$ID_num)),
      class = 1L,
      prob1 = 1
    ))
  }
  pp <- as.data.frame(model$pprob)
  names(pp)[1] <- "ID_num"
  pp
}

charls_long <- fread(
  file.path(tables, "r_charls_trajectory_long.csv")
)
observed_counts <- charls_long |>
  count(ID_num, name = "observed_measurements")
stopifnot(min(observed_counts$observed_measurements) >= 3)

wide <- charls_long |>
  select(ID_num, year, depression_z) |>
  pivot_wider(
    names_from = year,
    values_from = depression_z,
    names_prefix = "z_"
  ) |>
  arrange(ID_num)
stopifnot(
  all(c("z_2011", "z_2013", "z_2015", "z_2018") %in% names(wide))
)

method <- make.method(wide)
method[] <- "pmm"
method["ID_num"] <- ""
predictor <- make.predictorMatrix(wide)
predictor[, "ID_num"] <- 0
predictor["ID_num", ] <- 0
diag(predictor) <- 0

imp <- mice(
  wide,
  m = 20,
  maxit = 10,
  method = method,
  predictorMatrix = predictor,
  printFlag = FALSE,
  seed = 20260726
)
saveRDS(imp, file.path(models, "r_charls_depression_mi_mids.rds"))

original <- readRDS(file.path(models, "r_charls_lcga_models.rds"))
original_candidates <- original$candidate_models
years <- c(2011, 2013, 2015, 2018)
all_diagnostics <- list()
all_quality <- list()
fit_summaries <- vector("list", 20)

for (imputation in 1:20) {
  cat("Imputation", imputation, "of 20\n")
  completed <- complete(imp, imputation)
  dat <- completed |>
    pivot_longer(
      starts_with("z_"),
      names_to = "year",
      names_prefix = "z_",
      values_to = "depression_z"
    ) |>
    mutate(
      year = as.integer(year),
      time = (year - 2011) / 7
    ) |>
    arrange(ID_num, year)

  fits <- list()
  fits[["1"]] <- hlme(
    fixed = depression_z ~ time + I(time^2),
    random = ~ -1,
    subject = "ID_num",
    ng = 1,
    data = dat,
    maxiter = 500,
    verbose = FALSE
  )
  for (k in 2:4) {
    fits[[as.character(k)]] <- hlme(
      fixed = depression_z ~ time + I(time^2),
      mixture = ~ time + I(time^2),
      random = ~ -1,
      subject = "ID_num",
      ng = k,
      nwg = FALSE,
      data = dat,
      B = original_candidates[[as.character(k)]]$best,
      maxiter = 500,
      verbose = FALSE
    )
  }

  diagnostics_i <- bind_rows(lapply(names(fits), function(k) {
    fit <- fits[[k]]
    pp <- posterior_table(fit, as.integer(k), dat)
    prob_cols <- grep("^prob", names(pp), value = TRUE)
    individual_mpp <- apply(pp[, prob_cols, drop = FALSE], 1, max)
    class_mpp <- tapply(individual_mpp, pp$class, mean)
    class_prop <- prop.table(table(pp$class))
    data.frame(
      imputation,
      classes = as.integer(k),
      BIC = fit$BIC,
      convergence = as.integer(fit$conv == 1),
      entropy = entropy_value(pp[, prob_cols, drop = FALSE]),
      minimum_class_pct = 100 * min(class_prop),
      minimum_mean_posterior = min(class_mpp)
    )
  }))
  admissible <- diagnostics_i |>
    filter(
      classes >= 2,
      convergence == 1,
      minimum_class_pct >= 10,
      entropy >= 0.70,
      minimum_mean_posterior >= 0.70
    )
  selected_k <- if (nrow(admissible)) {
    admissible$classes[which.min(admissible$BIC)]
  } else {
    1L
  }
  selected <- fits[[as.character(selected_k)]]
  pp <- posterior_table(selected, selected_k, dat)
  prob_cols <- grep("^prob", names(pp), value = TRUE)
  pp$mean_posterior <- apply(pp[, prob_cols, drop = FALSE], 1, max)
  dat_class <- left_join(
    dat, pp[, c("ID_num", "class")], by = "ID_num"
  )
  order_last <- dat_class |>
    filter(year == 2018) |>
    group_by(class) |>
    summarise(mean_z = mean(depression_z), .groups = "drop") |>
    arrange(desc(mean_z)) |>
    pull(class)
  labels <- if (selected_k == 1L) {
    setNames("Overall mean trajectory", 1)
  } else if (selected_k == 2L) {
    setNames(c("High/increasing burden", "Low-stable burden"), order_last)
  } else if (selected_k == 3L) {
    setNames(
      c(
        "High/increasing burden", "Intermediate burden",
        "Low-stable burden"
      ),
      order_last
    )
  } else {
    setNames(
      c(
        "Very-high burden", "High burden",
        "Intermediate burden", "Low-stable burden"
      ),
      order_last
    )
  }
  pp$class_label <- labels[as.character(pp$class)]
  quality_i <- pp |>
    group_by(class, class_label) |>
    summarise(
      imputation,
      selected_k,
      n = n(),
      proportion = n() / nrow(pp),
      mean_posterior = mean(mean_posterior),
      .groups = "drop"
    )
  all_diagnostics[[imputation]] <- diagnostics_i
  all_quality[[imputation]] <- quality_i
  fit_summaries[[imputation]] <- list(
    selected_k = selected_k,
    diagnostics = diagnostics_i,
    quality = quality_i
  )
}

diagnostics <- bind_rows(all_diagnostics)
quality <- bind_rows(all_quality)
summary <- bind_rows(
  quality |>
    group_by(selected_k, class_label) |>
    summarise(
      imputations = n_distinct(imputation),
      mean_proportion = mean(proportion),
      min_proportion = min(proportion),
      max_proportion = max(proportion),
      mean_posterior = mean(mean_posterior),
      .groups = "drop"
    ) |>
    mutate(cohort = "CHARLS CESD-10"),
  data.frame(
    selected_k = readRDS(
      file.path(models, "r_class_primary_models.rds")
    )$selected_k,
    class_label = "No outcome imputation needed: all three waves observed",
    imputations = 20,
    mean_proportion = NA_real_,
    min_proportion = NA_real_,
    max_proportion = NA_real_,
    mean_posterior = NA_real_,
    cohort = "CLASS depressive symptoms"
  ),
  data.frame(
    selected_k = 1,
    class_label = paste(
      "No outcome imputation needed: all three waves observed;",
      "b21-b27 remains a depressive-affect proxy"
    ),
    imputations = 20,
    mean_proportion = NA_real_,
    min_proportion = NA_real_,
    max_proportion = NA_real_,
    mean_posterior = NA_real_,
    cohort = "CLHLS depressive-affect burden"
  )
)

applicability <- data.frame(
  cohort = c(
    "CHARLS CESD-10",
    "CLASS depressive symptoms",
    "CLHLS depressive-affect burden"
  ),
  observed_wave_rule = c(
    "At least 3 of 4 observed",
    "All 3 observed",
    "All 3 observed"
  ),
  intermittent_outcome_values_imputed = c(
    sum(is.na(wide[, -1])),
    0,
    0
  ),
  role = c(
    "MI sensitivity for the fourth wave only",
    "No trajectory-outcome MI applicable",
    "No trajectory-outcome MI applicable"
  ),
  limitation = c(
    "Does not recover people with fewer than 3 observed waves",
    "Cannot correct exclusion before the complete three-wave sample",
    "Cannot correct death or informative loss to follow-up"
  )
)

fwrite(
  diagnostics,
  file.path(diag, "r_charls_depression_mi_model_selection.csv")
)
fwrite(
  quality,
  file.path(diag, "r_charls_depression_mi_classification_quality.csv")
)
fwrite(
  summary,
  file.path(tables, "r_trajectory_mi_sensitivity_summary.csv")
)
fwrite(
  applicability,
  file.path(diag, "r_trajectory_mi_applicability.csv")
)
saveRDS(
  fit_summaries,
  file.path(models, "r_charls_depression_mi_fit_summaries.rds")
)

cat("\nMI applicability:\n")
print(applicability)
cat("\nMI trajectory summary:\n")
print(summary)
cat("\nCompleted:", format(Sys.time()), "\n")
