#!/usr/bin/env Rscript
# V5.2 Step C: CHARLS continuous FI computation
# 22 items, 6 domains, waves 1-4 (2011, 2013, 2015, 2018)
suppressPackageStartupMessages({
  library(haven)
  library(data.table)
})

options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_numeric <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 CHARLS FI Computation ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. Load CHARLS data
# ============================================================
cat("[1] Loading CHARLS data...\n")
charls_path <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
charls_wide <- read_dta(charls_path)
cat("  Shape:", nrow(charls_wide), "x", ncol(charls_wide), "\n")

# ============================================================
# 2. Define approved FI items (22 items, excluding current_smoke)
# ============================================================
# Items: chronic diseases (9), SRH (1), depression (1), cognition (3), physical function (6), psychiatric (2)
# NOTE: current_smoke EXCLUDED per user instructions

fi_items <- list(
  # Chronic diseases - binary 0/1, cumulative
  chronic = data.table(
    item_id = c("hypertension","heart_disease","stroke","lung_disease","diabetes",
                "cancer","arthritis","kidney_disease","liver_disease"),
    prefix = c("hibpe","hearte","stroke","lunge","diabe","cancre","arthre","kidneye","livere"),
    coding = "binary_01",
    carry_forward = TRUE
  ),
  # Self-rated health
  srh = data.table(
    item_id = "srh",
    prefix = "shlta",
    coding = "ordinal_5",
    carry_forward = FALSE
  ),
  # Depression
  depression = data.table(
    item_id = "depression",
    prefix = "cesd10",
    coding = "continuous_030",
    carry_forward = FALSE
  ),
  # Cognition
  cognition = data.table(
    item_id = c("orientation","imrc","ser7"),
    prefix = c("orient","imrc","ser7"),
    coding = c("ordinal_5_rev","ordinal_10_rev","ordinal_6_rev"),
    carry_forward = FALSE
  ),
  # Physical function - binary 0/1
  physical = data.table(
    item_id = c("stooping","walk_1km","lift_carry","stand_chair","climb_stairs","incontinence"),
    prefix = c("stoopa","walk1kma","lifta","chaira","climsa","urina"),
    coding = "binary_01",
    carry_forward = FALSE
  ),
  # Psychiatric - binary 0/1
  psychiatric = data.table(
    item_id = c("psychiatric","memory_problem"),
    prefix = c("psyche","memrye"),
    coding = "binary_01",
    carry_forward = FALSE
  )
)

# Flatten item list
all_items <- rbindlist(lapply(fi_items, function(x) x))
cat("  Approved items:", nrow(all_items), "\n")
cat("  Domains:", paste(names(fi_items), collapse=", "), "\n")

# ============================================================
# 3. Reshape CHARLS to long format and compute deficit scores
# ============================================================
cat("\n[2] Reshaping to long format and computing deficit scores...\n")

waves <- c(`1`=2011, `2`=2013, `3`=2015, `4`=2018)
long_list <- list()

for (w in 1:4) {
  wave_year <- unname(waves[as.character(w)])
  cat("  Processing wave", w, "(", wave_year, ")...\n")

  # Extract person-level variables
  inw_var <- paste0("inw", w)
  age_var <- paste0("r", w, "agey")
  df <- data.table(
    ID = as.character(charls_wide$ID),
    wave = wave_year,
    female = as.integer(safe_numeric(charls_wide$ragender) == 2),
    age_at_wave = safe_numeric(charls_wide[[age_var]]),
    in_wave = as.integer(safe_numeric(charls_wide[[inw_var]]) == 1)
  )

  # Extract FI items
  for (i in 1:nrow(all_items)) {
    vn <- paste0("r", w, all_items$prefix[i])
    if (vn %in% names(charls_wide)) {
      raw <- as.numeric(charls_wide[[vn]])
      df[[all_items$item_id[i]]] <- raw
    } else {
      df[[all_items$item_id[i]]] <- NA_real_
    }
  }

  # Keep only people in this wave
  df <- df[in_wave == 1]
  long_list[[as.character(wave_year)]] <- df
}

charls_long <- rbindlist(long_list, idcol = "wave_year_char")
cat("  Long-format rows:", nrow(charls_long), "\n")

# ============================================================
# 4. Score deficit items
# ============================================================
cat("\n[3] Scoring deficit items...\n")

# Chronic diseases: binary 0/1, negative values = missing
for (item in all_items$item_id[all_items$item_id %in% c("hypertension","heart_disease","stroke",
                                                         "lung_disease","diabetes","cancer",
                                                         "arthritis","kidney_disease","liver_disease")]) {
  v <- charls_long[[item]]
  charls_long[[paste0(item, "_score")]] <- ifelse(v == 1, 1, ifelse(v == 0, 0, NA_real_))
}

# SRH: 1-5 scale, recode to 0-1 deficit
v <- charls_long$srh
charls_long$srh_score <- ifelse(v >= 1 & v <= 2, 0,
                         ifelse(v == 3, 0.5,
                         ifelse(v >= 4 & v <= 5, 1, NA_real_)))

# Depression: CESD-10 total (0-30), standardise to 0-1
v <- charls_long$depression
# Compute within-wave standardisation
for (wy in unique(charls_long$wave)) {
  idx <- charls_long$wave == wy & !is.na(v)
  if (sum(idx) > 0) {
    min_v <- min(v[idx], na.rm=TRUE)
    max_v <- max(v[idx], na.rm=TRUE)
    if (max_v > min_v) {
      charls_long$depression_score[idx] <- (v[idx] - min_v) / (max_v - min_v)
    }
  }
}

# Cognition: reverse and scale to 0-1
# orientation: 0-4 (higher = better), reverse: (4-score)/4
v <- charls_long$orientation
charls_long$orientation_score <- ifelse(v >= 0 & v <= 4, (4 - v) / 4, NA_real_)

# imrc: 0-10 (higher = better), reverse: (10-score)/10
v <- charls_long$imrc
charls_long$imrc_score <- ifelse(v >= 0 & v <= 10, (10 - v) / 10, NA_real_)

# ser7: 0-5 (higher = better), reverse: (5-score)/5
v <- charls_long$ser7
charls_long$ser7_score <- ifelse(v >= 0 & v <= 5, (5 - v) / 5, NA_real_)

# Physical function: binary 0/1
for (item in c("stooping","walk_1km","lift_carry","stand_chair","climb_stairs","incontinence")) {
  v <- charls_long[[item]]
  charls_long[[paste0(item, "_score")]] <- ifelse(v == 1, 1, ifelse(v == 0, 0, NA_real_))
}

# Psychiatric: binary 0/1
for (item in c("psychiatric","memory_problem")) {
  v <- charls_long[[item]]
  charls_long[[paste0(item, "_score")]] <- ifelse(v == 1, 1, ifelse(v == 0, 0, NA_real_))
}

# ============================================================
# 5. Chronic disease carry-forward
# ============================================================
cat("\n[4] Applying chronic disease carry-forward...\n")

chronic_items <- c("hypertension","heart_disease","stroke","lung_disease","diabetes",
                   "cancer","arthritis","kidney_disease","liver_disease")

cf_stats <- data.table()
setorder(charls_long, ID, wave)

for (item in chronic_items) {
  score_var <- paste0(item, "_score")
  n_changed <- 0L
  for (id in unique(charls_long$ID)) {
    idx <- which(charls_long$ID == id)
    if (length(idx) < 2) next
    scores <- charls_long[[score_var]][idx]
    waves_i <- charls_long$wave[idx]
    for (j in 2:length(idx)) {
      # If current is NA but previous was 1, carry forward
      if (is.na(scores[j]) && !is.na(scores[j-1]) && scores[j-1] == 1) {
        charls_long[[score_var]][idx[j]] <- 1
        n_changed <- n_changed + 1L
      }
      # Once 1, always 1 (cumulative)
      if (!is.na(scores[j]) && scores[j] == 1 && !is.na(scores[j-1]) && scores[j-1] == 0) {
        # New diagnosis - valid
      }
    }
  }
  cf_stats <- rbind(cf_stats, data.table(
    item = item, n_changed_by_carryforward = n_changed
  ))
}
fwrite(cf_stats, file.path(root, "CHARLS_chronic_disease_carryforward_audit.csv"))
cat("  Carry-forward applied. Changes:\n")
print(cf_stats)

# ============================================================
# 6. Compute FI scores (primary: exclude incontinence)
# ============================================================
cat("\n[5] Computing FI scores...\n")

# Primary FI: exclude incontinence
primary_items <- all_items$item_id[all_items$item_id != "incontinence"]
primary_score_vars <- paste0(primary_items, "_score")

# Compute denominator and numerator
# NOTE: na.rm = TRUE so participants with partial item completion get valid FI
# The threshold then determines eligibility based on completion fraction
charls_long$n_completed_primary <- rowSums(!is.na(charls_long[, ..primary_score_vars]))
charls_long$fi_sum_primary <- rowSums(charls_long[, ..primary_score_vars], na.rm = TRUE)
charls_long$fi_denominator_primary <- nrow(all_items[all_items$item_id != "incontinence",])
charls_long$fi_completion_rate_primary <- charls_long$n_completed_primary / charls_long$fi_denominator_primary

# Valid FI: >= 80% completion
charls_long$fi_valid_80 <- charls_long$fi_completion_rate_primary >= 0.80
charls_long$fi_primary <- ifelse(charls_long$fi_valid_80,
                                  charls_long$fi_sum_primary / charls_long$n_completed_primary,
                                  NA_real_)

# Sensitivity: 70% and 90% thresholds
charls_long$fi_valid_70 <- charls_long$fi_completion_rate_primary >= 0.70
charls_long$fi_valid_90 <- charls_long$fi_completion_rate_primary >= 0.90
charls_long$fi_70 <- ifelse(charls_long$fi_valid_70,
                             charls_long$fi_sum_primary / charls_long$n_completed_primary,
                             NA_real_)
charls_long$fi_90 <- ifelse(charls_long$fi_valid_90,
                             charls_long$fi_sum_primary / charls_long$n_completed_primary,
                             NA_real_)

# Sensitivity FI: include incontinence
all_score_vars <- paste0(all_items$item_id, "_score")
charls_long$n_completed_all <- rowSums(!is.na(charls_long[, ..all_score_vars]))
charls_long$fi_sum_all <- rowSums(charls_long[, ..all_score_vars], na.rm = FALSE)
charls_long$fi_valid_all <- charls_long$n_completed_all / nrow(all_items) >= 0.80
charls_long$fi_with_incontinence <- ifelse(charls_long$fi_valid_all,
                                            charls_long$fi_sum_all / charls_long$n_completed_all,
                                            NA_real_)

cat("  Primary FI valid (80%):", sum(charls_long$fi_valid_80, na.rm=TRUE), "observations\n")
cat("  FI range:", range(charls_long$fi_primary, na.rm=TRUE), "\n")

# ============================================================
# 7. Save wave-specific FI data
# ============================================================
cat("\n[6] Saving wave-specific FI data...\n")

# Save long-format with FI scores
fwrite(charls_long, file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))

# ============================================================
# 8. Quality control statistics
# ============================================================
cat("\n[7] Computing quality control statistics...\n")

qc_stats <- charls_long[, .(
  wave = first(wave),
  n_total = .N,
  n_valid_fi = sum(!is.na(fi_primary)),
  pct_valid = round(100 * mean(!is.na(fi_primary)), 1),
  mean_fi = round(mean(fi_primary, na.rm=TRUE), 4),
  sd_fi = round(sd(fi_primary, na.rm=TRUE), 4),
  median_fi = round(median(fi_primary, na.rm=TRUE), 4),
  iqr_fi = round(IQR(fi_primary, na.rm=TRUE), 4),
  min_fi = round(min(fi_primary, na.rm=TRUE), 4),
  max_fi = round(max(fi_primary, na.rm=TRUE), 4),
  p1 = round(quantile(fi_primary, 0.01, na.rm=TRUE), 4),
  p5 = round(quantile(fi_primary, 0.05, na.rm=TRUE), 4),
  p95 = round(quantile(fi_primary, 0.95, na.rm=TRUE), 4),
  p99 = round(quantile(fi_primary, 0.99, na.rm=TRUE), 4),
  pct_zero = round(100 * mean(fi_primary == 0, na.rm=TRUE), 1),
  pct_gt_025 = round(100 * mean(fi_primary > 0.25, na.rm=TRUE), 1),
  pct_gt_035 = round(100 * mean(fi_primary > 0.35, na.rm=TRUE), 1),
  mean_completion = round(mean(fi_completion_rate_primary, na.rm=TRUE), 3),
  excluded_80 = sum(!fi_valid_80)
), by = wave]

fwrite(qc_stats, file.path(root, "CHARLS_continuous_FI_summary.csv"))
cat("  CHARLS QC stats:\n")
print(qc_stats)

# ============================================================
# 9. Item completion summary
# ============================================================
cat("\n[8] Computing item completion summary...\n")

item_completion <- data.table()
for (item in primary_items) {
  score_var <- paste0(item, "_score")
  for (wy in unique(charls_long$wave)) {
    vals <- charls_long[wave == wy][[score_var]]
    item_completion <- rbind(item_completion, data.table(
      wave = wy,
      item = item,
      n_nonnull = sum(!is.na(vals)),
      pct_complete = round(100 * mean(!is.na(vals)), 1)
    ))
  }
}
fwrite(item_completion, file.path(root, "FI_item_completion_summary.csv"))

# ============================================================
# 10. Outlier audit
# ============================================================
cat("\n[9] Computing outlier audit...\n")

outlier <- charls_long[!is.na(fi_primary), .(
  wave = first(wave),
  n = .N,
  pct_below_005 = round(100 * mean(fi_primary < 0.05), 1),
  pct_above_050 = round(100 * mean(fi_primary > 0.50), 1),
  pct_above_075 = round(100 * mean(fi_primary > 0.75), 1),
  max_fi = round(max(fi_primary), 4),
  n_above_050 = sum(fi_primary > 0.50),
  n_above_075 = sum(fi_primary > 0.75)
), by = wave]
fwrite(outlier, file.path(root, "FI_outlier_audit.csv"))

# ============================================================
# 11. Domain contribution
# ============================================================
cat("\n[10] Computing domain contribution...\n")

# Compute domain scores
chronic_score_vars <- paste0(chronic_items, "_score")
charls_long$chronic_domain <- rowSums(charls_long[, ..chronic_score_vars], na.rm = FALSE) / length(chronic_items)
charls_long$srh_domain <- charls_long$srh_score
charls_long$depression_domain <- charls_long$depression_score
charls_long$cognition_domain <- rowSums(charls_long[, c("orientation_score","imrc_score","ser7_score")], na.rm = FALSE) / 3
charls_long$physical_domain <- rowSums(charls_long[, c("stooping_score","walk_1km_score","lift_carry_score",
                                                        "stand_chair_score","climb_stairs_score")], na.rm = FALSE) / 5
charls_long$psychiatric_domain <- rowSums(charls_long[, c("psychiatric_score","memory_problem_score")], na.rm = FALSE) / 2

domain_contrib <- charls_long[!is.na(fi_primary), .(
  wave = first(wave),
  chronic_mean = round(mean(chronic_domain, na.rm=TRUE), 4),
  srh_mean = round(mean(srh_domain, na.rm=TRUE), 4),
  depression_mean = round(mean(depression_domain, na.rm=TRUE), 4),
  cognition_mean = round(mean(cognition_domain, na.rm=TRUE), 4),
  physical_mean = round(mean(physical_domain, na.rm=TRUE), 4),
  psychiatric_mean = round(mean(psychiatric_domain, na.rm=TRUE), 4),
  chronic_cor = round(cor(chronic_domain, fi_primary, use="complete.obs"), 3),
  srh_cor = round(cor(srh_domain, fi_primary, use="complete.obs"), 3),
  depression_cor = round(cor(depression_domain, fi_primary, use="complete.obs"), 3),
  cognition_cor = round(cor(cognition_domain, fi_primary, use="complete.obs"), 3),
  physical_cor = round(cor(physical_domain, fi_primary, use="complete.obs"), 3),
  psychiatric_cor = round(cor(psychiatric_domain, fi_primary, use="complete.obs"), 3)
), by = wave]
fwrite(domain_contrib, file.path(root, "CHARLS_FI_domain_contribution.csv"))
cat("  Domain contribution:\n")
print(domain_contrib)

# ============================================================
# 12. Multimorbidity correlation
# ============================================================
cat("\n[11] Computing multimorbidity correlation...\n")

charls_long$n_chronic <- rowSums(charls_long[, ..chronic_score_vars], na.rm = FALSE)
mm_cor <- charls_long[!is.na(fi_primary), .(
  wave = first(wave),
  cor_fi_chronic_count = round(cor(fi_primary, n_chronic, use="complete.obs"), 3),
  mean_chronic = round(mean(n_chronic, na.rm=TRUE), 2)
), by = wave]
fwrite(mm_cor, file.path(root, "FI_multimorbidity_correlation.csv"))
cat("  Multimorbidity correlation:\n")
print(mm_cor)

# ============================================================
# 13. Leave-one-domain-out
# ============================================================
cat("\n[12] Computing leave-one-domain-out...\n")

domain_names <- c("chronic","srh","depression","cognition","physical","psychiatric")
lod <- data.table()
for (dom in domain_names) {
  exclude_vars <- grep(paste0("^", dom), names(charls_long), value=TRUE)
  exclude_vars <- exclude_vars[grepl("_score$", exclude_vars)]
  remaining <- setdiff(primary_score_vars, exclude_vars)
  n_remaining <- length(remaining)
  lod_fi <- rowSums(charls_long[, ..remaining], na.rm = FALSE) / charls_long$n_completed_primary
  # Adjust for excluded items
  lod <- rbind(lod, data.table(
    excluded_domain = dom,
    n_items_removed = length(exclude_vars),
    n_items_remaining = n_remaining,
    mean_fi_change = round(mean(charls_long$fi_primary - lod_fi, na.rm=TRUE), 4)
  ))
}
fwrite(lod, file.path(root, "FI_leave_one_domain_out.csv"))

# ============================================================
# 14. Incontinence sensitivity
# ============================================================
cat("\n[13] Computing incontinence sensitivity...\n")

valid_both <- !is.na(charls_long$fi_primary) & !is.na(charls_long$fi_with_incontinence)
sens <- data.table(
  n_valid_both = sum(valid_both),
  mean_primary = round(mean(charls_long$fi_primary[valid_both]), 4),
  mean_with_incontinence = round(mean(charls_long$fi_with_incontinence[valid_both]), 4),
  mean_diff = round(mean(charls_long$fi_with_incontinence[valid_both] - charls_long$fi_primary[valid_both]), 4),
  correlation = round(cor(charls_long$fi_primary[valid_both], charls_long$fi_with_incontinence[valid_both]), 4)
)
fwrite(sens, file.path(root, "FI_incontinence_sensitivity.csv"))
cat("  Incontinence sensitivity:\n")
print(sens)

# ============================================================
# 15. Age and sex gradients
# ============================================================
cat("\n[14] Computing age and sex gradients...\n")

age_grad <- charls_long[!is.na(fi_primary), .(
  wave = first(wave),
  mean_fi = round(mean(fi_primary), 4),
  n = .N
), by = .(wave, age_group = cut(age_at_wave, breaks = c(64, 69, 74, 79, 84, Inf),
                                  labels = c("65-69","70-74","75-79","80-84","85+")))]
fwrite(age_grad, file.path(root, "CHARLS_FI_age_gradient.csv"))

# ============================================================
# 16. Save log
# ============================================================
log_text <- paste0(
"# CHARLS FI Score Computation Log - V5.2\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## Item Definition\n",
"- 22 items (chronic diseases 9, SRH 1, depression 1, cognition 3, physical function 5, psychiatric 2)\n",
"- current_smoke EXCLUDED per user instructions\n",
"- Incontinence included in sensitivity only\n\n",
"## Waves\n",
"- 2011, 2013, 2015, 2018\n",
"- Fixed item composition across all waves\n\n",
"## Carry-forward\n",
"- Chronic diseases: cumulative, carry forward once reported\n",
"- All other items: no carry-forward\n\n",
"## Completion thresholds\n",
"- Primary: 80%\n",
"- Sensitivity: 70% and 90%\n\n",
"## Scoring\n",
"- Binary items: 0/1\n",
"- SRH: recoded to 0/0.5/1\n",
"- Depression: standardised to 0-1 within wave\n",
"- Cognition: reversed and scaled to 0-1\n\n",
"## Output files generated\n",
"- CHARLS_wave_specific_continuous_FI.csv\n",
"- CHARLS_continuous_FI_summary.csv\n",
"- FI_item_completion_summary.csv\n",
"- FI_outlier_audit.csv\n",
"- CHARLS_FI_domain_contribution.csv\n",
"- FI_multimorbidity_correlation.csv\n",
"- FI_leave_one_domain_out.csv\n",
"- FI_incontinence_sensitivity.csv\n",
"- CHARLS_chronic_disease_carryforward_audit.csv\n",
"- CHARLS_FI_age_gradient.csv\n"
)
writeLines(log_text, file.path(root, "CHARLS_FI_score_computation_log.md"))
cat("Saved CHARLS_FI_score_computation_log.md\n")

cat("\nCompleted:", format(Sys.time()), "\n")
