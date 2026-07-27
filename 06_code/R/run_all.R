#!/usr/bin/env Rscript

scripts <- c(
  "06_code/R/00_gbd2021_context.R",
  "06_code/R/01_epi_corrected_analysis.R",
  "06_code/R/02_class_primary_analysis.R",
  "06_code/R/03_charls_trajectory_lcga.R",
  "06_code/R/10_v4_required_analyses.R",
  "06_code/R/13_policy_main_figures.R",
  "06_code/R/14_v51_exports_and_figures.R"
)

for (script in scripts) {
  cat("\n============================================================\n")
  cat("Running:", script, "\n")
  cat("============================================================\n")
  status <- system2(file.path(R.home("bin"), "Rscript"), script)
  if (!identical(status, 0L)) {
    stop("Pipeline failed in ", script, " with status ", status)
  }
}

cat("\nFull V5 policy-centred R pipeline completed successfully.\n")
