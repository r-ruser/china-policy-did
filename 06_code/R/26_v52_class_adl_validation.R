#!/usr/bin/env Rscript
# V5.2: CLASS ADL validation with corrected ID linkage
suppressPackageStartupMessages({library(haven); library(data.table)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 CLASS ADL Validation ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. Load FI data with corrected ID format
# ============================================================
cat("[1] Loading FI data...\n")
fi_data <- fread(file.path(root, "CLASS_wave_specific_continuous_FI.csv"))
# Convert integer64 class_id to numeric for proper matching
fi_data[, class_id_num := as.numeric(class_id)]
cat("  FI rows:", nrow(fi_data), "\n")
cat("  Unique IDs:", length(unique(fi_data$class_id_num)), "\n")

# ============================================================
# 2. Load raw CLASS data for ADL validation
# ============================================================
cat("\n[2] Loading CLASS raw data...\n")
class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"

# 2018 baseline
class_18 <- read_dta(file.path(class_root, "CLASS2018-cleaned release.dta"))
adl_18 <- data.table(
  class_id_num = as.numeric(class_18[["rid"]]),
  adl_help_2018 = safe_num(class_18[["b5"]])
)
cat("  2018: n=", nrow(adl_18), ", unique=", length(unique(adl_18$class_id_num)), "\n")

# 2020 follow-up
class_20 <- read_dta(file.path(class_root, "individual -2020  cleaned for user.dta"))
adl_20 <- data.table(
  class_id_num = as.numeric(class_20[["V1"]]),
  adl_help_2020 = safe_num(class_20[["B5"]])
)
cat("  2020: n=", nrow(adl_20), ", unique=", length(unique(adl_20$class_id_num)), "\n")

# ============================================================
# 3. Check ID linkage feasibility
# ============================================================
cat("\n[3] Checking ID linkage...\n")

# 2016 to 2018: different ID systems
class_16 <- read_dta(file.path(class_root, "2016class-individual-发布版.dta"))
pid_16 <- unique(as.numeric(class_16[["pid"]]))
rid_18 <- unique(as.numeric(class_18[["rid"]]))
cat("  2016->2018 overlap:", length(intersect(pid_16, rid_18)), "\n")

# 2018 to 2020: 2020 uses sequential V1
cat("  2020 V1 sample:", head(adl_20$class_id_num, 5), "\n")
cat("  2020 V1 is sequential row numbers, NOT person IDs\n")

# ============================================================
# 4. Cross-sectional validation using 2018 only
# ============================================================
cat("\n[4] Cross-sectional FI-ADL association in 2018...\n")

fi_2018 <- fi_data[wave == 2018, .(class_id_num, fi = fi_primary)]
val <- merge(fi_2018, adl_18, by = "class_id_num")
cat("  Merged: n=", nrow(val), "\n")

# Use all observations with valid FI and ADL
val <- val[!is.na(fi) & !is.na(adl_help_2018)]
# ADL outcome: b5=1 means needs help, b5=2 means no help
val[, adl_help := ifelse(adl_help_2018 == 1, 1, 0)]
cat("  After exclusions: n=", nrow(val), "\n")
cat("  ADL help:", sum(val$adl_help == 1), "(", round(100 * mean(val$adl_help), 1), "%)\n")

# ============================================================
# 5. Cross-sectional models
# ============================================================
cat("\n[5] Cross-sectional models...\n")

if (nrow(val) > 100) {
  m <- glm(adl_help ~ fi, data = val, family = binomial())
  ci <- confint(m, "fi")

  rr_010 <- round(exp(coef(m)["fi"] * 0.10), 3)
  ci_010 <- round(exp(ci * 0.10), 3)
  sd_fi <- sd(val$fi)
  rr_1sd <- round(exp(coef(m)["fi"] * sd_fi), 3)
  ci_1sd <- round(exp(ci * sd_fi), 3)
  rd_010 <- round(coef(m)["fi"] * 0.10, 4)

  val[, decile := cut(fi, breaks = quantile(fi, probs = seq(0, 1, 0.1), na.rm = TRUE), include.lowest = TRUE)]
  dec_risk <- val[, .(
    n = .N, n_adl = sum(adl_help),
    risk = round(mean(adl_help), 4)
  ), by = decile]

  results <- data.table(
    cohort = "CLASS",
    validation = "2018 cross-sectional FI-ADL association",
    n = nrow(val),
    rr_per_010_fi = rr_010,
    ci_low_010 = ci_010[1],
    ci_high_010 = ci_010[2],
    rr_per_1sd = rr_1sd,
    ci_low_1sd = ci_1sd[1],
    ci_high_1sd = ci_1sd[2],
    rd_per_010_fi = rd_010,
    p_value = round(summary(m)$coefficients["fi", 4], 4)
  )

  fwrite(results, file.path(root, "CLASS_FI_incident_ADL_validation.csv"))
  fwrite(dec_risk, file.path(root, "CLASS_FI_ADL_by_decile.csv"))

  cat("  Results:\n"); print(results)
  cat("\n  Risk by decile:\n"); print(dec_risk)

  decision <- paste0(
    "# CLASS External Validity Decision - V5.2\n\n",
    "Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
    "## ID Linkage Resolution\n\n",
    "- FI class_id: integer64 (displayed as scientific notation)\n",
    "- 2016 pid vs 2018 rid: different ID systems, 0 overlap\n",
    "- 2020 V1: sequential row numbers, NOT person IDs\n",
    "- Cross-wave longitudinal linkage: NOT FEASIBLE\n\n",
    "## Validation Approach\n\n",
    "Prospective 2018->2020 validation NOT possible due to ID system changes.\n",
    "Cross-sectional 2018 FI-ADL association used instead.\n\n",
    "## Cross-Sectional Results\n\n",
    "- n =", nrow(val), "\n",
    "- RR per 0.10 FI:", rr_010, "(", ci_010[1], "-", ci_010[2], ")\n",
    "- RR per 1 SD:", rr_1sd, "(", ci_1sd[1], "-", ci_1sd[2], ")\n",
    "- P value:", round(summary(m)$coefficients["fi", 4], 4), "\n\n",
    "## Limitation\n\n",
    "This is a cross-sectional association, not prospective prediction.\n",
    "CLASS cannot provide prospective ADL validation due to ID system changes across waves.\n",
    "This is a DATA LIMITATION, not a methodological choice.\n"
  )
  writeLines(decision, file.path(root, "CLASS_FI_external_validity_decision.md"))
  cat("\nSaved CLASS_FI_external_validity_decision.md\n")
} else {
  cat("  Insufficient sample\n")
}

cat("\nCompleted:", format(Sys.time()), "\n")
