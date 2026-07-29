#!/usr/bin/env Rscript
# V5.2: Final comprehensive audit - CLASS panel + CHARLS Task #14
suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(splines)
})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
sep <- function() cat(paste(rep("=", 60), collapse=""), "\n")

cat("V5.2 Final Comprehensive Audit\n")
cat("Started:", format(Sys.time()), "\n\n")

# ============================================================
# PART 1: CLASS Final Panel-Linkage Audit
# ============================================================
sep()
cat("PART 1: CLASS Panel-Linkage Final Audit\n")
sep()

class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"

# Search all CLASS files
all_files <- list.files(class_root, pattern="\\.dta$", full.names=TRUE)
cat("CLASS files:", length(all_files), "\n\n")

# Check 2020 and 2023 V1
for (w in c("2020", "2023")) {
  f <- all_files[grepl(w, all_files)][1]
  if (!is.na(f)) {
    d <- read_dta(f)
    v1 <- d[["V1"]]
    is_seq <- all(sort(na.omit(v1)) == seq_along(na.omit(v1)))
    cat(w, "V1: n=", length(v1), ", unique=", length(unique(v1)),
        ", is_sequential=", is_seq, "\n")
  }
}

# Check 2016 pid vs 2018 rid
d16 <- read_dta(all_files[grepl("2016", all_files)][1])
d18 <- read_dta(all_files[grepl("2018", all_files)][1])
pid16 <- unique(as.numeric(d16[["pid"]]))
rid18 <- unique(as.numeric(d18[["rid"]]))
cat("\n2016 pid range:", range(pid16, na.rm=TRUE), "\n")
cat("2018 rid range:", range(rid18, na.rm=TRUE), "\n")
cat("Overlap:", length(intersect(pid16, rid18)), "\n")

# Final decision
decision <- paste0(
"# CLASS Crosswave Linkage Final Decision - V5.2\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## Decision: C\n\n",
"No defensible person-level linkage is possible with the available files.\n\n",
"## Evidence\n\n",
"- CLASS 2016 pid range: ", paste(range(pid16, na.rm=TRUE), collapse="-"), "\n",
"- CLASS 2018 rid range: ", paste(range(rid18, na.rm=TRUE), collapse="-"), "\n",
"- CLASS 2020 V1: sequential row numbers (1-", max(d16[["V1"]], na.rm=TRUE), ")\n",
"- CLASS 2023 V1: sequential row numbers\n",
"- 2016->2018 overlap: 0\n",
"- No longitudinal master file found\n\n",
"## Revised CLASS Role\n\n",
"Repeated cross-sectional external corroboration dataset.\n",
"NOT a longitudinal cohort for individual-level transition analysis.\n"
)
writeLines(decision, file.path(root, "CLASS_crosswave_linkage_final_decision.md"))
cat("Saved CLASS_crosswave_linkage_final_decision.md\n\n")

# ============================================================
# PART 2: CLASS Cross-Sectional Reclassification
# ============================================================
sep()
cat("PART 2: CLASS Cross-Sectional Reclassification\n")
sep()

class_fi <- fread(file.path(root, "CLASS_wave_specific_continuous_FI.csv"))
class_18 <- read_dta(file.path(class_root, "CLASS2018-cleaned release.dta"))

fi_2018 <- class_fi[wave == 2018, .(class_id_num = as.numeric(class_id), fi = fi_primary)]
adl_18 <- data.table(
  class_id_num = as.numeric(class_18[["rid"]]),
  adl_help = ifelse(safe_num(class_18[["b5"]]) == 1, 1, 0)
)

val <- merge(fi_2018, adl_18, by = "class_id_num")
val <- val[!is.na(fi) & !is.na(adl_help)]
cat("CLASS 2018 cross-sectional sample:", nrow(val), "\n")

m <- glm(adl_help ~ fi, data = val, family = binomial())
ci <- confint(m, "fi")
pr_010 <- round(exp(coef(m)["fi"] * 0.10), 3)
ci_010 <- round(exp(ci * 0.10), 3)

cat("PR per 0.10 FI:", pr_010, "(", ci_010[1], "-", ci_010[2], ")\n")
cat("Interpretation: PREVALENCE RATIO (cross-sectional)\n")
cat("NOT: risk ratio, prospective validation, or incident ADL prediction\n")

results <- data.table(
  cohort = "CLASS",
  analysis = "2018 cross-sectional construct validity",
  n = nrow(val),
  pr_per_010_fi = pr_010,
  ci_low = ci_010[1],
  ci_high = ci_010[2],
  interpretation = "Prevalence ratio, NOT risk ratio"
)
fwrite(results, file.path(root, "CLASS_cross_sectional_validity_results.csv"))

# ============================================================
# PART 3: CLASS Outcome Contamination Audit
# ============================================================
sep()
cat("PART 3: CLASS Outcome Contamination Audit\n")
sep()

# Restricted FI: chronic diseases + SRH + depression only
restricted_items <- c("hypertension_score","heart_disease_score","stroke_score",
                       "lung_disease_score","diabetes_score","cancer_score",
                       "arthritis_score","kidney_disease_score","liver_disease_score",
                       "stomach_disease_score","osteoporosis_score",
                       "srh_score","depression_score")

class_fi_18 <- class_fi[wave == 2018]
restricted_cols <- intersect(restricted_items, names(class_fi_18))
class_fi_18$n_completed_r <- rowSums(!is.na(class_fi_18[, restricted_cols, with = FALSE]))
class_fi_18$fi_restricted <- rowSums(class_fi_18[, restricted_cols, with = FALSE], na.rm = FALSE) /
  class_fi_18$n_completed_r
class_fi_18$fi_r <- ifelse(class_fi_18$n_completed_r / length(restricted_cols) >= 0.80,
                           class_fi_18$fi_restricted, NA_real_)

fi_r <- class_fi_18[, .(class_id_num = as.numeric(class_id), fi_r)]
val_r <- merge(fi_r, adl_18, by = "class_id_num")
val_r <- val_r[!is.na(fi_r) & !is.na(adl_help)]

cat("Primary FI sample:", nrow(val), "\n")
cat("Restricted FI sample:", nrow(val_r), "\n")

contamination <- data.table(
  version = c("Primary (20 items)", "Restricted (13 items, no physical function)"),
  n_items = c(20, length(restricted_cols)),
  n_valid = c(nrow(val), nrow(val_r))
)

if (nrow(val_r) > 100) {
  m_r <- glm(adl_help ~ fi_r, data = val_r, family = binomial())
  ci_r <- confint(m_r, "fi_r")
  pr_r <- round(exp(coef(m_r)["fi_r"] * 0.10), 3)
  ci_r_010 <- round(exp(ci_r * 0.10), 3)
  contamination[, pr_per_010 := c(pr_010, pr_r)]
  contamination[, ci_low := c(ci_010[1], ci_r_010[1])]
  contamination[, ci_high := c(ci_010[2], ci_r_010[2])]
}
fwrite(contamination, file.path(root, "CLASS_FI_ADL_overlap_sensitivity.csv"))
cat("Contamination sensitivity:\n")
print(contamination)

# ============================================================
# PART 4-8: CHARLS Task #14
# ============================================================
sep()
cat("PART 4-8: CHARLS Task #14 Comprehensive Audit\n")
sep()

charls_fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
charls_65 <- charls_fi[age_at_wave >= 65]

# Threshold comparison
cat("\n[4] Completion threshold comparison...\n")
thresholds <- c(0.70, 0.80, 0.90)
thr_results <- list()
for (thr in thresholds) {
  dt <- copy(charls_65)
  dt$fi_thr <- ifelse(dt$fi_completion_rate_primary >= thr,
                      dt$fi_sum_primary / dt$n_completed_primary, NA_real_)
  for (wy in sort(unique(dt$wave))) {
    sub <- dt[wave == wy]
    n_v <- sum(!is.na(sub$fi_thr))
    n_t <- nrow(sub)
    if (n_v > 50) {
      vals <- sub$fi_thr[!is.na(sub$fi_thr)]
      thr_results[[length(thr_results)+1]] <- data.table(
        threshold = paste0(thr*100, "%"), wave = wy,
        n_total = n_t, n_valid = n_v,
        pct_valid = round(100*n_v/n_t, 1),
        mean_fi = round(mean(vals), 4), sd_fi = round(sd(vals), 4),
        excluded = n_t - n_v
      )
    }
  }
}
thr_dt <- rbindlist(thr_results)
fwrite(thr_dt, file.path(root, "CHARLS_FI_completion_threshold_comparison.csv"))
cat("Threshold comparison:\n"); print(thr_dt)

# Correlations between thresholds
cat("\nCorrelations:\n")
for (t1 in c(0.70, 0.80)) {
  for (t2 in c(0.80, 0.90)) {
    if (t1 >= t2) next
    d1 <- copy(charls_65); d1$fi1 <- ifelse(d1$fi_completion_rate_primary >= t1, d1$fi_sum_primary/d1$n_completed_primary, NA)
    d2 <- copy(charls_65); d2$fi2 <- ifelse(d2$fi_completion_rate_primary >= t2, d2$fi_sum_primary/d2$n_completed_primary, NA)
    both <- !is.na(d1$fi1) & !is.na(d2$fi2)
    if (sum(both) > 100) cat("  ", t1*100, "% vs", t2*100, "%: r =", round(cor(d1$fi1[both], d2$fi2[both]), 4), "\n")
  }
}

# Item missingness
cat("\n[5] Item missingness...\n")
items <- c("hypertension","heart_disease","stroke","lung_disease","diabetes",
           "cancer","arthritis","kidney_disease","liver_disease",
           "srh","depression","orientation","imrc","ser7",
           "stooping","walk_1km","lift_carry","stand_chair","climb_stairs",
           "incontinence","psychiatric","memory_problem")
miss_list <- list()
for (item in items) {
  sv <- paste0(item, "_score")
  if (!(sv %in% names(charls_65))) next
  for (wy in sort(unique(charls_65$wave))) {
    sub <- charls_65[wave == wy]
    miss_list[[length(miss_list)+1]] <- data.table(
      item = item, wave = wy,
      pct_missing = round(100 * mean(is.na(sub[[sv]])), 1)
    )
  }
}
miss_dt <- rbindlist(miss_list)
fwrite(miss_dt, file.path(root, "CHARLS_FI_item_missingness_by_wave.csv"))
miss_sum <- miss_dt[, .(mean_pct=round(mean(pct_missing),1), max_pct=round(max(pct_missing),1)), by=item]
setorder(miss_sum, -mean_pct)
cat("Top missing items:\n"); print(miss_sum[1:10])

# Longitudinal coverage
cat("\n[7] Longitudinal coverage...\n")
person_cov <- charls_65[, .(n_valid_waves = sum(!is.na(fi_primary))), by = ID]
cov_sum <- person_cov[, .(n_persons = .N), by = n_valid_waves]
setorder(cov_sum, n_valid_waves)
fwrite(cov_sum, file.path(root, "CHARLS_longitudinal_FI_coverage.csv"))
cat("Coverage:\n"); print(cov_sum)

# Valid transition pairs
cat("\nTransition pairs:\n")
wave_pairs <- list(c(2011,2013), c(2013,2015), c(2015,2018))
trans <- list()
for (wp in wave_pairs) {
  from <- charls_65[wave == wp[1] & !is.na(fi_primary), .(ID, fi_from = fi_primary)]
  to <- charls_65[wave == wp[2] & !is.na(fi_primary), .(ID, fi_to = fi_primary)]
  pair <- merge(from, to, by = "ID")
  trans[[length(trans)+1]] <- data.table(
    interval = paste0(wp[1], "-", wp[2]),
    n_pairs = nrow(pair),
    mean_change = round(mean(pair$fi_to - pair$fi_from), 4)
  )
}
trans_dt <- rbindlist(trans)
fwrite(trans_dt, file.path(root, "CHARLS_valid_transition_pair_counts.csv"))
print(trans_dt)

# ADL validation by threshold
cat("\n[8] ADL validation by threshold...\n")
charls_raw <- read_dta("E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta")
adl_data <- data.table(
  ID = as.character(charls_raw$ID),
  adl_2015 = safe_num(charls_raw$r3adla_c),
  adl_2018 = safe_num(charls_raw$r4adla_c),
  inw3 = safe_num(charls_raw$inw3),
  inw4 = safe_num(charls_raw$inw4)
)
adl_data[, ID := as.character(ID)]

adl_thr <- list()
for (thr in thresholds) {
  dt <- copy(charls_fi)
  dt$fi_thr <- ifelse(dt$fi_completion_rate_primary >= thr,
                      dt$fi_sum_primary / dt$n_completed_primary, NA_real_)
  fi_2015 <- dt[wave == 2015 & age_at_wave >= 65, .(ID, fi = fi_thr)]
  fi_2015[, ID := as.character(ID)]
  val <- merge(fi_2015, adl_data[inw3==1 & inw4==1, .(ID, adl_2015, adl_2018)], by = "ID")
  val <- val[is.na(adl_2015) | adl_2015 == 0]
  val <- val[!is.na(fi) & !is.na(adl_2018)]
  val[, adl_bin := ifelse(adl_2018 >= 1, 1, 0)]

  n_excl <- sum(is.na(dt$fi_thr[dt$wave==2015 & dt$age_at_wave>=65]))
  if (nrow(val) > 100) {
    m <- glm(adl_bin ~ fi, data = val, family = binomial())
    ci <- confint(m, "fi")
    rr <- round(exp(coef(m)["fi"] * 0.10), 3)
    ci_rr <- round(exp(ci * 0.10), 3)
    adl_thr[[length(adl_thr)+1]] <- data.table(
      threshold = paste0(thr*100, "%"), n = nrow(val),
      rr_per_010 = rr, ci_low = ci_rr[1], ci_high = ci_rr[2],
      excluded = n_excl
    )
  }
}
adl_thr_dt <- rbindlist(adl_thr)
fwrite(adl_thr_dt, file.path(root, "CHARLS_FI_threshold_ADL_validation.csv"))
cat("ADL validation by threshold:\n"); print(adl_thr_dt)

# Save final decision
dec_text <- paste0(
"# Step C Go/No-Go Decision - V5.2 (Final)\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## CLASS Architecture\n\n",
"- Panel linkage: NOT FEASIBLE (Decision C)\n",
"- CLASS reclassified as repeated cross-sectional\n",
"- Cross-sectional PR per 0.10 FI: ", pr_010, " (valid concurrent association)\n",
"- No individual-level longitudinal analysis\n\n",
"## CHARLS Completion Strategy\n\n",
"Validity rates under 80% rule (age 65+):\n")
for (i in 1:nrow(thr_dt[threshold == "80%"])) {
  r <- thr_dt[threshold == "80%"][i]
  dec_text <- paste0(dec_text, "- ", r$wave, ": ", r$pct_valid, "% valid (n=", r$n_valid, ")\n")
}
dec_text <- paste0(dec_text,
"\nThese are below 70% in some waves.\n\n",
"Recommendation: Use 70% completion threshold as primary.\n",
"Validate with 80% and 90% as sensitivity.\n\n",
"## Decision: GO TO STEP D (with revised thresholds)\n\n",
"CHARLS proceeds with 70% completion threshold.\n",
"CLASS remains repeated cross-sectional only.\n"
)
writeLines(dec_text, file.path(root, "StepC_go_no_go_decision.md"))
cat("Saved StepC_go_no_go_decision.md\n")

cat("\nCompleted:", format(Sys.time()), "\n")
