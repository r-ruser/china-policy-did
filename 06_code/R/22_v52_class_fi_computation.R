#!/usr/bin/env Rscript
# V5.2 Step C: CLASS continuous FI computation
# 20 items, 5 domains, waves 2016, 2018, 2020, 2023
suppressPackageStartupMessages({
  library(haven)
  library(data.table)
})

options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_numeric <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 CLASS FI Computation ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. Load CLASS data
# ============================================================
cat("[1] Loading CLASS data...\n")
class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"
class_files <- list(
  `2016` = file.path(class_root, "2016class-individual-发布版.dta"),
  `2018` = file.path(class_root, "CLASS2018-cleaned release.dta"),
  `2020` = file.path(class_root, "individual -2020  cleaned for user.dta"),
  `2023` = file.path(class_root, "2023_individual_release_weighted .dta")
)

# Load all waves
class_data <- list()
for (w in names(class_files)) {
  cat("  Loading CLASS", w, "...\n")
  class_data[[w]] <- read_dta(class_files[[w]])
  cat("    ", nrow(class_data[[w]]), "x", ncol(class_data[[w]]), "\n")
}

# ============================================================
# 2. Define variable mapping across waves
# ============================================================
# CLASS uses different naming conventions:
# 2016: b11_1_X (chronic), b1 (SRH), b6_X (physical), b4_7/b4_8 (incontinence), e2_X (depression)
# 2018: b9_1__X (chronic), b1 (SRH), b6_X (physical), b4_7/b4_8 (incontinence), e2__X (depression)
# 2020: B9_1_X (chronic), B1 (SRH), B6_X (physical), B4_7/B4_8 (incontinence), E2_X (depression)
# 2023: B9_1_X (chronic), B1 (SRH), B6_X (physical), B4_7/B4_8 (incontinence), E2_X (depression)

# Chronic disease mapping (11 items)
chronic_map <- list(
  hypertension = list(`2016`="b11_1_1", `2018`="b9_1__1", `2020`="B9_1_1", `2023`="B9_1_1"),
  heart_disease = list(`2016`="b11_1_2", `2018`="b9_1__2", `2020`="B9_1_2", `2023`="B9_1_2"),
  stroke = list(`2016`="b11_1_4", `2018`="b9_1__3", `2020`="B9_1_3", `2023`="B9_1_3"),
  lung_disease = list(`2016`="b11_1_19", `2018`="b9_1__4", `2020`="B9_1_4", `2023`="B9_1_4"),
  diabetes = list(`2016`="b11_1_3", `2018`="b9_1__5", `2020`="B9_1_5", `2023`="B9_1_5"),
  cancer = list(`2016`="b11_1_16", `2018`="b9_1__7", `2020`="B9_1_7", `2023`="B9_1_7"),
  arthritis = list(`2016`="b11_1_10", `2018`="b9_1__9", `2020`="B9_1_9", `2023`="B9_1_9"),
  kidney_disease = list(`2016`="b11_1_5", `2018`="b9_1__11", `2020`="B9_1_11", `2023`="B9_1_11"),
  liver_disease = list(`2016`="b11_1_6", `2018`="b9_1__6", `2020`="B9_1_6", `2023`="B9_1_6"),
  stomach_disease = list(`2016`="b11_1_21", `2018`="b9_1__20", `2020`="B9_1_20", `2023`="B9_1_20"),
  osteoporosis = list(`2016`="b11_1_18", `2018`="b9_1__18", `2020`="B9_1_18", `2023`="B9_1_18")
)

# Other items
other_map <- list(
  srh = list(`2016`="b1", `2018`="b1", `2020`="B1", `2023`="B1"),
  climb_stairs = list(`2016`="b6_1", `2018`="b6_1", `2020`="B6_1", `2023`="B6_1"),
  walk_outside = list(`2016`="b6_3", `2018`="b6_3", `2020`="B6_3", `2023`="B6_3"),
  lift_heavy = list(`2016`="b6_7", `2018`="b6_7", `2020`="B6_7", `2023`="B6_7"),
  fall_12m = list(`2016`="b6_2", `2018`="b6_2", `2020`="B6_2", `2023`="B6_2"),
  urinary_incontinence = list(`2016`="b4_7", `2018`="b4_7", `2020`="B4_7", `2023`="B4_7"),
  fecal_incontinence = list(`2016`="b4_8", `2018`="b4_8", `2020`="B4_8", `2023`="B4_8"),
  depression_items = list(
    `2016`=paste0("e2_", 1:9),
    `2018`=paste0("e2__", 1:9),
    `2020`=paste0("E2_", 1:9),
    `2023`=paste0("E2_", 1:9)
  ),
  childhood_hunger = list(`2016`="b14", `2018`="b14", `2020`=NA, `2023`=NA)
)

# ID variable
id_map <- list(`2016`="pid", `2018`="rid", `2020`="V1", `2023`="V1")

# ============================================================
# 3. Extract and reshape to long format
# ============================================================
cat("\n[2] Reshaping to long format...\n")

long_list <- list()
for (w in names(class_data)) {
  cat("  Processing wave", w, "...\n")
  df <- class_data[[w]]
  id_var <- id_map[[w]]

  out <- data.table(
    class_id = as.character(df[[id_var]]),
    wave = as.integer(w)
  )

  # Chronic diseases
  for (item_name in names(chronic_map)) {
    vn <- chronic_map[[item_name]][[w]]
    if (!is.null(vn) && vn %in% names(df)) {
      out[[item_name]] <- safe_numeric(df[[vn]])
    } else {
      out[[item_name]] <- NA_real_
    }
  }

  # Other items
  for (item_name in names(other_map)) {
    if (item_name == "depression_items") next
    if (item_name == "childhood_hunger") next
    vn <- other_map[[item_name]][[w]]
    if (!is.null(vn) && !is.na(vn) && vn %in% names(df)) {
      out[[item_name]] <- safe_numeric(df[[vn]])
    } else {
      out[[item_name]] <- NA_real_
    }
  }

  # Depression: compute score from 9 items
  dep_vars <- other_map$depression_items[[w]]
  dep_available <- dep_vars[dep_vars %in% names(df)]
  if (length(dep_available) >= 7) {
    dep_mat <- as.matrix(sapply(dep_available, function(v) safe_numeric(df[[v]])))
    # Recode: 1-3 scale, reverse items 1,4,9 (positive affect)
    dep_scored <- dep_mat - 1
    reverse_items <- grep("_[149]$", dep_available)
    if (length(reverse_items) > 0) {
      dep_scored[, reverse_items] <- 2 - dep_scored[, reverse_items]
    }
    observed <- rowSums(!is.na(dep_scored))
    dep_total <- rowSums(dep_scored, na.rm = TRUE)
    out$depression <- ifelse(observed >= 7, dep_total * 9 / observed, NA_real_)
  } else {
    out$depression <- NA_real_
  }

  long_list[[w]] <- out
}

class_long <- rbindlist(long_list, idcol = "wave_char")
cat("  Long-format rows:", nrow(class_long), "\n")

# ============================================================
# 4. Score deficit items
# ============================================================
cat("\n[3] Scoring deficit items...\n")

# Chronic diseases: 1=Yes, 2=No, 3=Uncertain -> 0/1
for (item in names(chronic_map)) {
  v <- class_long[[item]]
  class_long[[paste0(item, "_score")]] <- ifelse(v == 1, 1, ifelse(v %in% c(2,3), 0, NA_real_))
}

# SRH: 1-5 scale, recode
v <- class_long$srh
class_long$srh_score <- ifelse(v >= 1 & v <= 2, 0,
                        ifelse(v == 3, 0.5,
                        ifelse(v >= 4 & v <= 5, 1, NA_real_)))

# Physical function: 1=Yes can, 2=No cannot, 3=With difficulty
for (item in c("climb_stairs","walk_outside")) {
  v <- class_long[[item]]
  class_long[[paste0(item, "_score")]] <- ifelse(v == 1, 0, ifelse(v %in% c(2,3), 1, NA_real_))
}

# lift_heavy: 1=Yes can, 2=No cannot
v <- class_long$lift_heavy
class_long$lift_heavy_score <- ifelse(v == 1, 0, ifelse(v == 2, 1, NA_real_))

# fall_12m: 1=Yes fell, 2=No
v <- class_long$fall_12m
class_long$fall_12m_score <- ifelse(v == 1, 1, ifelse(v == 2, 0, NA_real_))

# Incontinence: 1=Yes, 2=No, 3=Uncertain
for (item in c("urinary_incontinence","fecal_incontinence")) {
  v <- class_long[[item]]
  class_long[[paste0(item, "_score")]] <- ifelse(v == 1, 1, ifelse(v %in% c(2,3), 0, NA_real_))
}

# Depression: standardise to 0-1 within wave
for (wy in unique(class_long$wave)) {
  idx <- class_long$wave == wy & !is.na(class_long$depression)
  if (sum(idx) > 0) {
    min_v <- min(class_long$depression[idx])
    max_v <- max(class_long$depression[idx])
    if (max_v > min_v) {
      class_long$depression_score[idx] <- (class_long$depression[idx] - min_v) / (max_v - min_v)
    }
  }
}

# ============================================================
# 5. Compute FI scores
# ============================================================
cat("\n[4] Computing FI scores...\n")

# Primary FI: exclude incontinence
chronic_items <- names(chronic_map)
primary_items <- c(chronic_items, "srh", "climb_stairs", "walk_outside",
                   "lift_heavy", "fall_12m", "depression")
primary_score_vars <- paste0(primary_items, "_score")

class_long$n_completed_primary <- rowSums(!is.na(class_long[, ..primary_score_vars]))
class_long$fi_sum_primary <- rowSums(class_long[, ..primary_score_vars], na.rm = FALSE)
class_long$fi_denominator_primary <- length(primary_items)
class_long$fi_completion_rate_primary <- class_long$n_completed_primary / class_long$fi_denominator_primary

class_long$fi_valid_80 <- class_long$fi_completion_rate_primary >= 0.80
class_long$fi_primary <- ifelse(class_long$fi_valid_80,
                                 class_long$fi_sum_primary / class_long$n_completed_primary,
                                 NA_real_)

# Sensitivity thresholds
class_long$fi_valid_70 <- class_long$fi_completion_rate_primary >= 0.70
class_long$fi_valid_90 <- class_long$fi_completion_rate_primary >= 0.90
class_long$fi_70 <- ifelse(class_long$fi_valid_70,
                            class_long$fi_sum_primary / class_long$n_completed_primary, NA_real_)
class_long$fi_90 <- ifelse(class_long$fi_valid_90,
                            class_long$fi_sum_primary / class_long$n_completed_primary, NA_real_)

# Sensitivity FI: include incontinence
all_items_class <- c(primary_items, "urinary_incontinence", "fecal_incontinence")
all_score_vars <- paste0(all_items_class, "_score")
class_long$n_completed_all <- rowSums(!is.na(class_long[, ..all_score_vars]))
class_long$fi_valid_all <- class_long$n_completed_all / length(all_items_class) >= 0.80
class_long$fi_with_incontinence <- ifelse(class_long$fi_valid_all,
                                           rowSums(class_long[, ..all_score_vars], na.rm=FALSE) / class_long$n_completed_all,
                                           NA_real_)

cat("  Primary FI valid (80%):", sum(class_long$fi_valid_80, na.rm=TRUE), "observations\n")

# ============================================================
# 6. Quality control
# ============================================================
cat("\n[5] Computing QC statistics...\n")

qc <- class_long[, .(
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
  excluded_80 = sum(!fi_valid_80)
), by = wave]
fwrite(qc, file.path(root, "CLASS_continuous_FI_summary.csv"))
cat("  CLASS QC stats:\n")
print(qc)

# ============================================================
# 7. Save data and logs
# ============================================================
fwrite(class_long, file.path(root, "CLASS_wave_specific_continuous_FI.csv"))

# Incontinence sensitivity
valid_both <- !is.na(class_long$fi_primary) & !is.na(class_long$fi_with_incontinence)
sens <- data.table(
  cohort = "CLASS",
  n_valid_both = sum(valid_both),
  mean_primary = round(mean(class_long$fi_primary[valid_both]), 4),
  mean_with_incontinence = round(mean(class_long$fi_with_incontinence[valid_both]), 4),
  mean_diff = round(mean(class_long$fi_with_incontinence[valid_both] - class_long$fi_primary[valid_both]), 4),
  correlation = round(cor(class_long$fi_primary[valid_both], class_long$fi_with_incontinence[valid_both]), 4)
)
# Append to CHARLS incontinence sensitivity
charls_sens <- fread(file.path(root, "FI_incontinence_sensitivity.csv"))
# Add cohort column if missing
if (!"cohort" %in% names(charls_sens)) charls_sens[, cohort := "CHARLS"]
class_sens <- data.table(
  cohort = "CLASS",
  n_valid_both = sens$n_valid_both,
  mean_primary = sens$mean_primary,
  mean_with_incontinence = sens$mean_with_incontinence,
  mean_diff = sens$mean_diff,
  correlation = sens$correlation
)
combined_sens <- rbind(charls_sens, class_sens, fill = TRUE)
fwrite(combined_sens, file.path(root, "FI_incontinence_sensitivity.csv"))

log_text <- paste0(
"# CLASS FI Score Computation Log - V5.2\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## Item Definition\n",
"- 20 items (chronic diseases 11, SRH 1, physical function 5, incontinence 2, depression 1)\n",
"- Incontinence in primary FI: YES (CLASS ADL outcome is b5 'needs help with daily activities', not incontinence)\n",
"- Incontinence sensitivity analysis also conducted\n\n",
"## Waves\n",
"- 2016, 2018, 2020, 2023\n",
"- Fixed item composition across all waves\n\n",
"## Scoring\n",
"- Chronic diseases: 1=Yes->1, 2/3=No/Uncertain->0\n",
"- SRH: recoded to 0/0.5/1\n",
"- Physical function: 1=Can->0, 2/3=Difficulty->1\n",
"- Falls: 1=Yes->1, 2=No->0\n",
"- Incontinence: 1=Yes->1, 2/3=No/Uncertain->0\n",
"- Depression: standardised to 0-1 within wave\n\n",
"## Completion thresholds\n",
"- Primary: 80%\n",
"- Sensitivity: 70% and 90%\n"
)
writeLines(log_text, file.path(root, "CLASS_FI_score_computation_log.md"))

cat("\nCompleted:", format(Sys.time()), "\n")
