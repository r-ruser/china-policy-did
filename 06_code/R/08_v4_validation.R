#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
tables <- file.path(root, "07_results", "tables")
diag <- file.path(root, "07_results", "diagnostics")
models <- file.path(root, "07_results", "models")
numbered <- file.path(tables, "numbered_tables")
logs <- file.path(root, "10_logs")
dir.create(numbered, recursive = TRUE, showWarnings = FALSE)
dir.create(logs, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(logs, "r_v4_validation.log")
con <- file(log_path, "wt", encoding = "UTF-8")
sink(con, type = "output")
sink(con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(con)
}, add = TRUE)

cat("V4 reproducibility and epidemiology validation\n")
cat("Started:", format(Sys.time()), "\n\n")

charls_counts <- fread(
  file.path(diag, "r_charls_trajectory_measurement_counts.csv")
)
class_counts <- fread(
  file.path(diag, "r_class_trajectory_measurement_counts.csv")
)
clhls_counts <- fread(
  file.path(diag, "r_clhls_trajectory_measurement_counts.csv")
)

stopifnot(
  min(charls_counts$n_measurements) >= 3,
  all(class_counts$n_measurements == 3),
  all(
    clhls_counts[
      outcome == "Depressive-affect burden",
      n_measurements
    ] == 3
  )
)
cat("Measurement-count checks passed:\n")
cat("  CHARLS minimum =", min(charls_counts$n_measurements), "\n")
cat("  CLASS unique counts =",
    paste(sort(unique(class_counts$n_measurements)), collapse = ", "), "\n")
cat("  CLHLS depressive-affect unique counts =",
    paste(
      sort(unique(clhls_counts[
        outcome == "Depressive-affect burden",
        n_measurements
      ])),
      collapse = ", "
    ),
    "\n\n")

standardisation <- bind_rows(
  fread(file.path(diag, "r_charls_standardisation_audit.csv")),
  fread(file.path(diag, "r_class_standardisation_audit.csv")),
  fread(file.path(diag, "r_clhls_standardisation_audit.csv"))
)
stopifnot(
  all(abs(standardisation$baseline_z_mean) < 1e-8),
  all(abs(standardisation$baseline_z_sd - 1) < 1e-8),
  all(standardisation$positive_means_greater_burden)
)
cat("Fixed-baseline z-score checks passed for all three trajectories.\n\n")

charls_model <- readRDS(file.path(models, "r_charls_lcga_models.rds"))
class_model <- readRDS(file.path(models, "r_class_primary_models.rds"))
clhls_psych_model <- readRDS(
  file.path(models, "r_clhls_psych_models.rds")
)
stopifnot(
  charls_model$selected_k == 2,
  class_model$selected_k == 3,
  clhls_psych_model$selected_k == 1
)

charls_quality <- fread(
  file.path(diag, "r_charls_lcga_classification_quality.csv")
)
class_quality <- fread(file.path(diag, "r_class_lcga_quality.csv"))
stopifnot(
  min(charls_quality$proportion) >= 0.10,
  min(charls_quality$mean_posterior) >= 0.70,
  min(class_quality$proportion) >= 0.10,
  min(class_quality$mean_posterior) >= 0.70
)
cat("Model-selection checks passed: CHARLS=2, CLASS=3,")
cat(" CLHLS depressive-affect burden=1.\n\n")

mi_quality <- fread(
  file.path(diag, "r_charls_depression_mi_classification_quality.csv")
)
mi_selected <- mi_quality |>
  distinct(imputation, selected_k)
stopifnot(
  nrow(mi_selected) == 20,
  sum(mi_selected$selected_k == 2) == 19,
  sum(mi_selected$selected_k == 3) == 1
)
cat("MI sensitivity passed: 19/20 CHARLS imputations selected two classes;")
cat(" 1/20 selected three classes.\n\n")

membership_n <- c(
  charls = nrow(fread(file.path(
    tables, "r_charls_lcga_class_membership.csv"
  ))),
  class = nrow(fread(file.path(
    tables, "r_class_lcga_membership.csv"
  ))),
  clhls_psych = nrow(fread(file.path(
    tables, "r_clhls_psych_membership.csv"
  )))
)
count_n <- c(
  charls = nrow(charls_counts),
  class = nrow(class_counts),
  clhls_psych = nrow(
    clhls_counts[outcome == "Depressive-affect burden"]
  )
)
stopifnot(identical(unname(membership_n), unname(count_n)))
cat("Membership files match measurement-count audits:\n")
print(membership_n)
cat("\n")

formal_files <- c(
  file.path(tables, "r_corrected_main_results.csv"),
  file.path(tables, "r_cfps_event_study.csv"),
  file.path(tables, "r_cfps_health_raw_trends.csv")
)
formal_text <- paste(
  vapply(formal_files, function(path) {
    paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  }, character(1)),
  collapse = "\n"
)
stopifnot(
  !grepl("Employment|employment|labor", formal_text),
  !grepl("Cognitive|cognition|MMSE", formal_text, ignore.case = TRUE)
)
cat("Legacy employment/labor and prohibited cognitive-scale terms are absent")
cat(" from formal V4 analytical outputs.\n\n")

selection <- fread(
  file.path(diag, "r_clhls_trajectory_selection_audit.csv")
)
cat("CLHLS selection standardized differences:\n")
print(selection)
cat(
  "\nInterpretation: large age and education imbalances indicate material",
  " complete-case selection and require an explicit selection-bias limitation.",
  "\n\n"
)

trajectory_table <- bind_rows(
  charls_quality |>
    transmute(
      database_outcome = "CHARLS cognition",
      trajectory_class = class_label,
      N = n,
      proportion,
      mean_posterior_probability = mean_posterior
    ),
  class_quality |>
    transmute(
      database_outcome = "CLASS depressive symptoms",
      trajectory_class = class_label,
      N = n,
      proportion,
      mean_posterior_probability = mean_posterior
    ),
  fread(file.path(
    diag, "r_clhls_psych_classification_quality.csv"
  )) |>
    transmute(
      database_outcome = "CLHLS depressive-affect burden",
      trajectory_class = class_label,
      N = n,
      proportion,
      mean_posterior_probability = mean_posterior
    )
)
fwrite(
  trajectory_table,
  file.path(numbered, "Table5_Trajectory_Classes_V4.csv")
)

cfps_results <- fread(
  file.path(tables, "r_corrected_main_results.csv")
) |>
  filter(grepl("^CFPS", analysis)) |>
  select(
    analysis, outcome, estimand, estimate, conf_low, conf_high,
    p_value, p_fdr, n_id, n_obs
  )
fwrite(
  cfps_results,
  file.path(numbered, "Supplementary_Table_S1_CFPS_V4.csv")
)

cat("Completed:", format(Sys.time()), "\n")
