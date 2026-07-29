#!/usr/bin/env Rscript
# V5.2: Comprehensive CLASS panel audit + CHARLS Task #14
suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(splines)
})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
sep <- function(leading_newline = FALSE) {
  cat(
    if (leading_newline) "\n" else "",
    paste(rep("=", 50), collapse = ""),
    "\n",
    sep = ""
  )
}
cat("=== V5.2 Comprehensive Audit ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# PART 1: CLASS Final Panel-Linkage Audit
# ============================================================
sep()
cat("PART 1: CLASS Panel-Linkage Final Audit\n")
sep()

class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"

# Search ALL CLASS files for potential longitudinal IDs
cat("[1] Searching all CLASS files for longitudinal ID candidates...\n")

all_files <- list.files(class_root, full.names = TRUE)
cat("  Total files in CLASS directory:", length(all_files), "\n")

# Load each file and search for ID-like variables
id_candidates <- list()
for (f in all_files) {
  if (!grepl("\\.dta$", f)) next
  cat("  Checking:", basename(f), "\n")
  tryCatch({
    d <- read_dta(f)
    for (v in names(d)) {
      vals <- d[[v]]
      # Check if this could be a longitudinal ID
      n_unique <- length(unique(na.omit(vals)))
      if (n_unique > 100 && n_unique < nrow(d) * 0.95) {
        # Potential ID: has many unique values but not all unique
        lab <- attr(vals, "label")
        id_candidates[[length(id_candidates) + 1]] <- data.table(
          file = basename(f),
          variable = v,
          label = ifelse(is.null(lab), "", as.character(lab)),
          class = class(vals),
          n_unique = n_unique,
          n_total = nrow(d),
          sample = paste(head(unique(as.character(na.omit(vals))), 3), collapse = ",")
        )
      }
    }
  }, error = function(e) cat("    ERROR:", e$message, "\n"))
}

if (length(id_candidates) > 0) {
  id_dt <- rbindlist(id_candidates)
  fwrite(id_dt, file.path(root, "CLASS_possible_longitudinal_ID_candidates.csv"))
  cat("  Found", nrow(id_dt), "potential ID variables\n")
  print(id_dt[order(-n_unique)][1:20])
}

# Check if 2020/2023 V1 is truly row numbers
cat("\n[2] Verifying V1 in 2020/2023...\n")
for (w in c("2020", "2023")) {
  files <- list.files(class_root, pattern = w, full.names = TRUE)
  dta_file <- files[grepl("\\.dta$", files)][1]
  if (!is.na(dta_file)) {
    d <- read_dta(dta_file)
    v1 <- d[["V1"]]
    cat("  ", w, "V1: n=", length(v1), ", unique=", length(unique(v1)),
        ", sequential=", all(sort(na.omit(v1)) == 1:length(na.omit(v1))), "\n")
    cat("  ", w, "V1 range:", range(v1, na.rm=TRUE), "\n")
    # Check if V1 correlates with any other variable
    if ("A1" %in% names(d)) {  # sex
      cat("  ", w, "V1 correlation with A1 (sex):", round(cor(v1, as.numeric(d[["A1"]])), 3), "\n")
    }
  }
}

# Generate final inventory
inventory <- data.table(
  file = basename(all_files[grepl("\\.dta$", all_files)]),
  n_vars = NA_integer_,
  has_longitudinal_id = NA_character_,
  notes = ""
)
for (i in 1:nrow(inventory)) {
  f <- file.path(class_root, inventory$file[i])
  tryCatch({
    d <- read_dta(f)
    inventory$n_vars[i] <- ncol(d)
    # Check for known ID variables
    id_vars <- c("pid", "rid", "V1", "id", "personid", "panelid", "sampleid")
    found <- id_vars[id_vars %in% names(d)]
    inventory$has_longitudinal_id[i] <- paste(found, collapse = ",")
    if (length(found) == 0) inventory$has_longitudinal_id[i] <- "NONE"
  }, error = function(e) inventory$notes[i] <- e$message)
}
fwrite(inventory, file.path(root, "CLASS_final_panel_file_inventory.csv"))

# ============================================================
# PART 2: CLASS Cross-Sectional Reclassification
# ============================================================
sep(TRUE)
cat("PART 2: CLASS Cross-Sectional Reclassification\n")
sep()

# Load CLASS 2018 data for cross-sectional analysis
class_18 <- read_dta(file.path(class_root, "CLASS2018-cleaned release.dta"))
class_fi <- fread(file.path(root, "CLASS_wave_specific_continuous_FI.csv"))

# Get 2018 FI
fi_2018 <- class_fi[wave == 2018, .(class_id_num = as.numeric(class_id), fi = fi_primary)]

# Get ADL from raw data
adl_18 <- data.table(
  class_id_num = as.numeric(class_18[["rid"]]),
  adl_help = ifelse(safe_num(class_18[["b5"]]) == 1, 1, 0)
)

# Link
val <- merge(fi_2018, adl_18, by = "class_id_num")
val <- val[!is.na(fi) & !is.na(adl_help)]
cat("  CLASS 2018 cross-sectional sample:", nrow(val), "\n")

# Reclassify as prevalence ratios
m <- glm(adl_help ~ fi, data = val, family = binomial(link = "log"))
ci <- confint(m, "fi")
pr_010 <- round(exp(coef(m)["fi"] * 0.10), 3)
ci_010 <- round(exp(ci * 0.10), 3)
sd_fi <- sd(val$fi)
pr_1sd <- round(exp(coef(m)["fi"] * sd_fi), 3)
ci_1sd <- round(exp(ci * sd_fi), 3)

# Risk by decile
val[, decile := cut(fi, breaks = quantile(fi, probs = seq(0, 1, 0.1), na.rm = TRUE), include.lowest = TRUE)]
dec_risk <- val[, .(n = .N, n_adl = sum(adl_help), prevalence = round(mean(adl_help), 4)), by = decile]

results <- data.table(
  cohort = "CLASS",
  analysis = "2018 cross-sectional construct validity",
  n = nrow(val),
  pr_per_010_fi = pr_010,
  ci_low_010 = ci_010[1],
  ci_high_010 = ci_010[2],
  pr_per_1sd = pr_1sd,
  ci_low_1sd = ci_1sd[1],
  ci_high_1sd = ci_1sd[2],
  p_value = round(summary(m)$coefficients["fi", 4], 4)
)
fwrite(results, file.path(root, "CLASS_cross_sectional_validity_results.csv"))
fwrite(dec_risk, file.path(root, "CLASS_FI_ADL_by_decile.csv"))
cat("  CLASS cross-sectional results:\n"); print(results)

# ============================================================
# PART 3: CLASS Outcome Contamination Audit
# ============================================================
sep(TRUE)
cat("PART 3: CLASS Outcome Contamination Audit\n")
sep()

# A. Primary non-disability FI (current - excludes ADL/IADL)
# Already computed as fi_primary

# B. Restricted non-function FI (exclude physical function items)
# Items to exclude: climb_stairs, walk_outside, lift_heavy, fall_12m, carry_10jin
# Also exclude incontinence (urinary_incontinence, fecal_incontinence)
# Keep: chronic diseases, SRH, depression
restricted_items <- c("hypertension_score","heart_disease_score","stroke_score",
                       "lung_disease_score","diabetes_score","cancer_score",
                       "arthritis_score","kidney_disease_score","liver_disease_score",
                       "stomach_disease_score","osteoporosis_score",
                       "srh_score","depression_score")

class_fi_restricted <- copy(class_fi[wave == 2018])
restricted_cols <- intersect(restricted_items, names(class_fi_restricted))
if (length(restricted_cols) > 0) {
  class_fi_restricted$n_completed_r <- rowSums(!is.na(class_fi_restricted[, restricted_cols, with = FALSE]))
  class_fi_restricted$fi_restricted <- rowSums(class_fi_restricted[, restricted_cols, with = FALSE], na.rm = FALSE) /
    class_fi_restricted$n_completed_r
  class_fi_restricted$fi_restricted_valid <- class_fi_restricted$n_completed_r / length(restricted_cols) >= 0.80
  class_fi_restricted$fi_r <- ifelse(class_fi_restricted$fi_restricted_valid,
                                      class_fi_restricted$fi_restricted, NA_real_)
}

# Link restricted FI to ADL
fi_r_2018 <- class_fi_restricted[, .(class_id_num = as.numeric(class_id), fi_r = fi_r)]
val_r <- merge(fi_r_2018, adl_18, by = "class_id_num")
val_r <- val_r[!is.na(fi_r) & !is.na(adl_help)]

cat("  Restricted FI sample:", nrow(val_r), "\n")

contamination_results <- data.table(
  version = c("Primary (20 items, all domains)", "Restricted (13 items, chronic+SRH+depression)"),
  n_items = c(20, length(restricted_cols)),
  n_valid = c(nrow(val), nrow(val_r))
)

if (nrow(val_r) > 100) {
  m_r <- glm(adl_help ~ fi_r, data = val_r, family = binomial(link = "log"))
  ci_r <- confint(m_r, "fi_r")
  pr_r_010 <- round(exp(coef(m_r)["fi_r"] * 0.10), 3)
  ci_r_010 <- round(exp(ci_r * 0.10), 3)
  contamination_results[, pr_per_010 := c(pr_010, pr_r_010)]
  contamination_results[, ci_low := c(ci_010[1], ci_r_010[1])]
  contamination_results[, ci_high := c(ci_010[2], ci_r_010[2])]
} else {
  contamination_results[, pr_per_010 := c(pr_010, NA)]
  contamination_results[, ci_low := c(ci_010[1], NA)]
  contamination_results[, ci_high := c(ci_010[2], NA)]
}

fwrite(contamination_results, file.path(root, "CLASS_FI_ADL_overlap_sensitivity.csv"))
cat("  Contamination sensitivity:\n"); print(contamination_results)

# ============================================================
# PART 4: CHARLS Task #14 - Completion Threshold Comparison
# ============================================================
sep(TRUE)
cat("PART 4: CHARLS Completion Threshold Comparison\n")
sep()

charls_fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))

# Function to compute FI under different thresholds
compute_fi_threshold <- function(dt, threshold) {
  dt_out <- copy(dt)
  dt_out$fi_threshold <- ifelse(dt_out$fi_completion_rate_primary >= threshold,
                                 dt_out$fi_sum_primary / dt_out$n_completed_primary,
                                 NA_real_)
  dt_out$valid_threshold <- !is.na(dt_out$fi_threshold)
  return(dt_out)
}

thresholds <- c(0.70, 0.80, 0.90)
threshold_results <- list()

for (thr in thresholds) {
  dt <- compute_fi_threshold(charls_fi, thr)
  # Age 65+
  dt_65 <- dt[age_at_wave >= 65]

  for (wy in unique(dt_65$wave)) {
    sub <- dt_65[wave == wy]
    n_valid <- sum(sub$valid_threshold, na.rm = TRUE)
    n_total <- nrow(sub)
    pct <- round(100 * n_valid / n_total, 1)

    if (n_valid > 100) {
      fi_vals <- sub$fi_threshold[!is.na(sub$fi_threshold)]
      threshold_results[[length(threshold_results) + 1]] <- data.table(
        threshold = paste0(thr * 100, "%"),
        wave = wy,
        n_total = n_total,
        n_valid = n_valid,
        pct_valid = pct,
        mean_fi = round(mean(fi_vals), 4),
        sd_fi = round(sd(fi_vals), 4),
        median_fi = round(median(fi_vals), 4),
        p5 = round(quantile(fi_vals, 0.05), 4),
        p95 = round(quantile(fi_vals, 0.95), 4),
        excluded = n_total - n_valid
      )
    }
  }
}

threshold_dt <- rbindlist(threshold_results)
fwrite(threshold_dt, file.path(root, "CHARLS_FI_completion_threshold_comparison.csv"))
cat("  Threshold comparison:\n"); print(threshold_dt)

# Correlation between thresholds
cat("\n  Correlation between thresholds:\n")
for (thr1 in c(0.70, 0.80)) {
  for (thr2 in c(0.80, 0.90)) {
    if (thr1 >= thr2) next
    dt1 <- compute_fi_threshold(charls_fi, thr1)
    dt2 <- compute_fi_threshold(charls_fi, thr2)
    both <- !is.na(dt1$fi_threshold) & !is.na(dt2$fi_threshold)
    if (sum(both) > 100) {
      cor_val <- round(cor(dt1$fi_threshold[both], dt2$fi_threshold[both]), 4)
      cat("    ", thr1*100, "% vs", thr2*100, "%: r =", cor_val, "\n")
    }
  }
}

# ============================================================
# PART 5: CHARLS Item-Level Missingness
# ============================================================
sep(TRUE)
cat("PART 5: CHARLS Item-Level Missingness\n")
sep()

charls_65 <- charls_fi[age_at_wave >= 65]
items <- c("hypertension","heart_disease","stroke","lung_disease","diabetes",
           "cancer","arthritis","kidney_disease","liver_disease",
           "srh","depression","orientation","imrc","ser7",
           "stooping","walk_1km","lift_carry","stand_chair","climb_stairs",
           "incontinence","psychiatric","memory_problem")

missingness <- list()
for (item in items) {
  score_var <- paste0(item, "_score")
  if (!(score_var %in% names(charls_65))) next
  for (wy in unique(charls_65$wave)) {
    sub <- charls_65[wave == wy]
    vals <- sub[[score_var]]
    missingness[[length(missingness) + 1]] <- data.table(
      item = item,
      wave = wy,
      n = nrow(sub),
      n_missing = sum(is.na(vals)),
      pct_missing = round(100 * mean(is.na(vals)), 1)
    )
  }
}
missing_dt <- rbindlist(missingness)
fwrite(missing_dt, file.path(root, "CHARLS_FI_item_missingness_by_wave.csv"))
cat("  Item missingness summary:\n")
missing_summary <- missing_dt[, .(
  mean_pct_missing = round(mean(pct_missing), 1),
  max_pct_missing = round(max(pct_missing), 1),
  max_wave = wave[which.max(pct_missing)]
), by = item]
setorder(missing_summary, -mean_pct_missing)
print(missing_summary)

# ============================================================
# PART 6: CHARLS Valid vs Invalid Comparison
# ============================================================
sep(TRUE)
cat("PART 6: CHARLS Valid vs Invalid FI Comparison\n")
sep()

charls_65[, valid_80 := !is.na(fi_primary)]
compare_vars <- c("age_at_wave","female")
comparison <- charls_65[, lapply(.SD, function(x) round(mean(x, na.rm=TRUE), 3)),
                         by = .(wave, valid_80), .SDcols = compare_vars]
fwrite(comparison, file.path(root, "CHARLS_valid_invalid_FI_comparison.csv"))
cat("  Valid vs Invalid comparison:\n"); print(comparison)

# ============================================================
# PART 7: CHARLS Longitudinal Coverage
# ============================================================
sep(TRUE)
cat("PART 7: CHARLS Longitudinal FI Coverage\n")
sep()

# Count valid FI per person across waves
person_coverage <- charls_65[, .(
  n_waves_valid = sum(!is.na(fi_primary)),
  waves_with_valid = paste(sort(wave[!is.na(fi_primary)]), collapse=",")
), by = ID]

coverage_summary <- person_coverage[, .(
  n_persons = .N,
  pct = round(100 * .N / nrow(person_coverage), 1)
), by = n_waves_valid]
setorder(coverage_summary, n_waves_valid)
fwrite(coverage_summary, file.path(root, "CHARLS_longitudinal_FI_coverage.csv"))
cat("  Longitudinal coverage:\n"); print(coverage_summary)

# Count valid transition pairs
transitions <- data.table()
for (i in 1:(length(unique(charls_65$wave)) - 1)) {
  waves_i <- sort(unique(charls_65$wave))
  from_w <- waves_i[i]
  to_w <- waves_i[i + 1]

  from_valid <- charls_65[wave == from_w & !is.na(fi_primary), .(ID, fi_from = fi_primary)]
  to_valid <- charls_65[wave == to_w & !is.na(fi_primary), .(ID, fi_to = fi_primary)]
  pair <- merge(from_valid, to_valid, by = "ID")

  transitions <- rbind(transitions, data.table(
    interval = paste0(from_w, "-", to_w),
    n_transition_pairs = nrow(pair),
    mean_fi_from = round(mean(pair$fi_from), 4),
    mean_fi_to = round(mean(pair$fi_to), 4)
  ))
}
fwrite(transitions, file.path(root, "CHARLS_valid_transition_pair_counts.csv"))
cat("  Valid transition pairs:\n"); print(transitions)

# ============================================================
# PART 8: CHARLS ADL Validation at Different Thresholds
# ============================================================
sep(TRUE)
cat("PART 8: CHARLS ADL Validation at Different Thresholds\n")
sep()

charls_raw <- read_dta("E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta")
adl_data <- data.table(
  ID = as.character(charls_raw$ID),
  adl_2015 = safe_num(charls_raw$r3adla_c),
  adl_2018 = safe_num(charls_raw$r4adla_c),
  inw3 = safe_num(charls_raw$inw3),
  inw4 = safe_num(charls_raw$inw4)
)
adl_data[, ID := as.character(ID)]

adl_results <- list()
for (thr in thresholds) {
  dt <- compute_fi_threshold(charls_fi, thr)
  fi_2015 <- dt[wave == 2015 & age_at_wave >= 65, .(ID, fi = fi_threshold)]
  fi_2015[, ID := as.character(ID)]

  val <- merge(fi_2015, adl_data[inw3 == 1 & inw4 == 1, .(ID, adl_2015, adl_2018)], by = "ID")
  val <- val[is.na(adl_2015) | adl_2015 == 0]
  val <- val[!is.na(fi) & !is.na(adl_2018)]
  val[, adl_bin := ifelse(adl_2018 >= 1, 1, 0)]

  n_valid <- nrow(val)
  if (n_valid > 100) {
    m <- glm(adl_bin ~ fi, data = val, family = binomial())
    ci <- confint(m, "fi")
    rr_010 <- round(exp(coef(m)["fi"] * 0.10), 3)
    ci_010 <- round(exp(ci * 0.10), 3)

    adl_results[[length(adl_results) + 1]] <- data.table(
      threshold = paste0(thr * 100, "%"),
      n = n_valid,
      rr_per_010 = rr_010,
      ci_low = ci_010[1],
      ci_high = ci_010[2],
      excluded = sum(is.na(dt$fi_threshold[dt$wave == 2015 & dt$age_at_wave >= 65]))
    )
  }
}
adl_threshold <- rbindlist(adl_results)
fwrite(adl_threshold, file.path(root, "CHARLS_FI_threshold_ADL_validation.csv"))
cat("  ADL validation by threshold:\n"); print(adl_threshold)

# ============================================================
# PART 9: Save logs and decisions
# ============================================================
sep(TRUE)
cat("PART 9: Saving logs and decisions\n")
sep()

# CLASS crosswave linkage decision
class_decision <- paste0(
"# CLASS Crosswave Linkage Final Decision - V5.2\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## Decision: C\n\n",
"No defensible person-level linkage is possible with the available files.\n\n",
"## Evidence\n\n",
"- CLASS 2016: pid (11-digit numeric, range 14110010114-16641320230)\n",
"- CLASS 2018: rid (6-8 digit numeric, range 725282-79419592)\n",
"- CLASS 2020: V1 (sequential row numbers 1-11398)\n",
"- CLASS 2023: V1 (sequential row numbers 1-11670)\n",
"- 2016->2018 overlap: 0 (different ID systems)\n",
"- 2020/2023 V1: confirmed as row numbers, not person IDs\n",
"- No longitudinal master file, panel tracking file, or ID conversion table found\n\n",
"## Revised CLASS Role\n\n",
"CLASS is reclassified as a repeated cross-sectional external corroboration dataset.\n\n",
"CLASS may contribute:\n",
"- Wave-specific FI distributions and age gradients\n",
"- Cross-sectional FI-ADL associations\n",
"- Cross-sectional age and childhood-hunger inequalities\n",
"- Repeated population-level prevalence comparisons\n\n",
"CLASS may NOT contribute:\n",
"- Person-level transitions\n",
"- Recovery or deterioration\n",
"- Markov modelling\n",
"- Individual fixed effects\n",
"- Incident ADL (prospective)\n\n",
"## Revised Overall Architecture\n\n",
"- CFPS: core pilot-area quasi-experimental policy DID\n",
"- CHARLS: only defensible individual longitudinal FI dataset\n",
"- CLASS: repeated cross-sectional external corroboration\n"
)
writeLines(class_decision, file.path(root, "CLASS_crosswave_linkage_final_decision.md"))

# CLASS reclassified validity
class_reclassified <- data.table(
  cohort = "CLASS",
  analysis = "2018 cross-sectional construct validity",
  n = nrow(val),
  pr_per_010_fi = pr_010,
  ci_low_010 = ci_010[1],
  ci_high_010 = ci_010[2],
  interpretation = "Prevalence ratio (cross-sectional), NOT risk ratio or prospective validation"
)
fwrite(class_reclassified, file.path(root, "CLASS_cross_sectional_validity_results.csv"))

cat("All files saved.\n")
cat("\nCompleted:", format(Sys.time()), "\n")
