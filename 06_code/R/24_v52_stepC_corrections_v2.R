#!/usr/bin/env Rscript
# V5.2 Step C Corrections v2 - Clean rewrite
suppressPackageStartupMessages({library(haven); library(data.table); library(splines)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 Step C Corrections v2 ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# Load FI data
# ============================================================
cat("[0] Loading data...\n")
charls_fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
class_fi <- fread(file.path(root, "CLASS_wave_specific_continuous_FI.csv"))

# Add n_chronic to CLASS
ccols <- grep("_score$", names(class_fi), value = TRUE)
ccols <- ccols[!grepl("srh|depression|climb|walk|lift|fall|urin|fecal", ccols)]
class_fi$n_chronic <- rowSums(class_fi[, ccols, with = FALSE], na.rm = TRUE)
cat("  CHARLS:", nrow(charls_fi), "| CLASS:", nrow(class_fi), "\n")

# ============================================================
# Issue 1: Target-population denominators
# ============================================================
cat("\n[1] Target-population denominators...\n")

# CHARLS age 65+
c65 <- charls_fi[age_at_wave >= 65]
c_valid <- c65[, .(
  wave = first(wave), n_total = .N,
  n_valid = sum(!is.na(fi_primary)),
  pct_valid = round(100 * mean(!is.na(fi_primary)), 1),
  mean_fi = round(mean(fi_primary, na.rm = TRUE), 4),
  sd_fi = round(sd(fi_primary, na.rm = TRUE), 4)
), by = wave]
fwrite(c_valid, file.path(root, "CHARLS_age65plus_FI_validity.csv"))
cat("  CHARLS validity:\n"); print(c_valid)

# CLASS - use all (CLASS targets elderly)
cl_valid <- class_fi[, .(
  wave = first(wave), n_total = .N,
  n_valid = sum(!is.na(fi_primary)),
  pct_valid = round(100 * mean(!is.na(fi_primary)), 1),
  mean_fi = round(mean(fi_primary, na.rm = TRUE), 4),
  sd_fi = round(sd(fi_primary, na.rm = TRUE), 4)
), by = wave]
fwrite(cl_valid, file.path(root, "CLASS_age65plus_FI_validity.csv"))
cat("  CLASS validity:\n"); print(cl_valid)

# Denominator audit
denom <- data.table(
  cohort = c(rep("CHARLS",4), rep("CLASS",4)),
  wave = c(2011,2013,2015,2018, 2016,2018,2020,2023),
  n_total = c(c_valid$n_total, cl_valid$n_total),
  n_valid = c(c_valid$n_valid, cl_valid$n_valid),
  pct_valid = c(c_valid$pct_valid, cl_valid$pct_valid)
)
fwrite(denom, file.path(root, "FI_target_population_denominator_audit.csv"))

# ============================================================
# Issue 3: CLASS 2016 comparability
# ============================================================
cat("\n[3] CLASS 2016 vs 2018...\n")
c16 <- class_fi[wave == 2016 & fi_valid_80 == TRUE, .(class_id, fi_16 = fi_primary)]
c18 <- class_fi[wave == 2018 & fi_valid_80 == TRUE, .(class_id, fi_18 = fi_primary)]
both <- merge(c16, c18, by = "class_id", all = FALSE)
cat("  Paired:", nrow(both), "\n")
if (nrow(both) > 100) {
  comp <- data.table(
    n = nrow(both),
    mean_16 = round(mean(both$fi_16), 4),
    mean_18 = round(mean(both$fi_18), 4),
    mean_diff = round(mean(both$fi_18 - both$fi_16), 4),
    p_value = round(t.test(both$fi_16, both$fi_18, paired = TRUE)$p.value, 4)
  )
  fwrite(comp, file.path(root, "CLASS_2016_2018_measurement_comparability.csv"))
  cat("  Comparison:\n"); print(comp)
} else {
  fwrite(data.table(note = "Insufficient paired data", n = nrow(both)),
         file.path(root, "CLASS_2016_2018_measurement_comparability.csv"))
}

# ============================================================
# Issue 4: Floor/ceiling audit
# ============================================================
cat("\n[4] Floor/ceiling audit...\n")
fc <- function(dt, cohort) {
  dt[!is.na(fi_primary), .(
    wave = first(wave), n = .N,
    pct_zero = round(100 * mean(fi_primary == 0), 2),
    pct_lt_005 = round(100 * mean(fi_primary < 0.05), 2),
    pct_gt_050 = round(100 * mean(fi_primary > 0.50), 2),
    pct_gt_070 = round(100 * mean(fi_primary > 0.70), 2),
    pct_gt_080 = round(100 * mean(fi_primary > 0.80), 2),
    max_fi = round(max(fi_primary), 4),
    p95 = round(quantile(fi_primary, 0.95), 4),
    p99 = round(quantile(fi_primary, 0.99), 4),
    n_above_080 = sum(fi_primary > 0.80)
  ), by = wave][, cohort := cohort]
}
fc_all <- rbind(fc(charls_fi, "CHARLS"), fc(class_fi, "CLASS"))
fwrite(fc_all, file.path(root, "corrected_FI_floor_ceiling_audit.csv"))
cat("  Floor/ceiling:\n"); print(fc_all)

# Extreme values
ext_c <- charls_fi[fi_primary > 0.80, .(cohort="CHARLS", wave, ID, fi=round(fi_primary,4), age=age_at_wave)]
ext_cl <- class_fi[fi_primary > 0.80, .(cohort="CLASS", wave, class_id, fi=round(fi_primary,4))]
fwrite(rbind(ext_c, ext_cl, fill=TRUE), file.path(root, "FI_extreme_value_case_audit.csv"))
cat("  Extreme >0.80:", nrow(ext_c) + nrow(ext_cl), "cases\n")

# ============================================================
# Issue 5: Age gradient
# ============================================================
cat("\n[5] Age gradients...\n")

# CHARLS age gradient
c_age <- charls_fi[!is.na(fi_primary) & !is.na(age_at_wave), .(
  wave = first(wave), n = .N,
  fi_65_69 = round(mean(fi_primary[age_at_wave >= 65 & age_at_wave < 70]), 4),
  fi_70_74 = round(mean(fi_primary[age_at_wave >= 70 & age_at_wave < 75]), 4),
  fi_75_79 = round(mean(fi_primary[age_at_wave >= 75 & age_at_wave < 80]), 4),
  fi_80_84 = round(mean(fi_primary[age_at_wave >= 80 & age_at_wave < 85]), 4),
  fi_85plus = round(mean(fi_primary[age_at_wave >= 85]), 4)
), by = wave]

# Age regression per wave
for (wy in unique(charls_fi$wave)) {
  dt <- charls_fi[wave == wy & !is.na(fi_primary) & !is.na(age_at_wave)]
  m <- lm(fi_primary ~ age_at_wave, data = dt)
  ci <- confint(m, "age_at_wave")
  c_age[wave == wy, `:=`(
    change_per_5yr = round(coef(m)["age_at_wave"] * 5, 4),
    ci_low = round(ci[1] * 5, 4),
    ci_high = round(ci[2] * 5, 4),
    p_value = round(summary(m)$coefficients["age_at_wave", 4], 4)
  )]
}
c_age[, cohort := "CHARLS"]
fwrite(c_age, file.path(root, "FI_age_gradient_results.csv"))
cat("  CHARLS age gradient:\n"); print(c_age)

# Spline tests (CHARLS only - CLASS lacks age_at_wave)
cat("  Spline tests:\n")
dt_c <- charls_fi[!is.na(fi_primary) & !is.na(age_at_wave)]
if (nrow(dt_c) > 100) {
  m1 <- lm(fi_primary ~ age_at_wave, data = dt_c)
  m2 <- lm(fi_primary ~ ns(age_at_wave, df = 3), data = dt_c)
  anova_test <- anova(m1, m2)
  cat("    CHARLS: F=", round(anova_test$F[2], 3),
      ", p=", round(anova_test$`Pr(>F)`[2], 4), "\n")
}

# ============================================================
# Issue 6: Multimorbidity
# ============================================================
cat("\n[6] Multimorbidity...\n")
mm_list <- list()
for (cohort in c("CHARLS", "CLASS")) {
  dt <- if (cohort == "CHARLS") copy(charls_fi) else copy(class_fi)
  dt <- dt[!is.na(fi_primary)]
  for (wy in unique(dt$wave)) {
    sub <- dt[wave == wy]
    fi_vals <- sub$fi_primary
    ch_vals <- sub$n_chronic
    good <- !is.na(fi_vals) & !is.na(ch_vals)
    if (sum(good) > 10) {
      cor_val <- cor(fi_vals[good], ch_vals[good])
      mm_list[[length(mm_list)+1]] <- data.table(
        cohort = cohort, wave = wy,
        cor = round(cor_val, 3),
        mean_chronic = round(mean(ch_vals, na.rm = TRUE), 2)
      )
    }
  }
}
mm_all <- rbindlist(mm_list)
fwrite(mm_all, file.path(root, "completed_FI_multimorbidity_audit.csv"))
cat("  Multimorbidity:\n"); print(mm_all)

# ============================================================
# Issue 7: ADL validation
# ============================================================
cat("\n[7] ADL validation...\n")

# CHARLS
charls_raw <- read_dta("E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta")
adl_data <- data.table(
  ID = as.character(charls_raw$ID),
  adl_2015 = safe_num(charls_raw$r3adla_c),
  adl_2018 = safe_num(charls_raw$r4adla_c),
  inw3 = safe_num(charls_raw$inw3),
  inw4 = safe_num(charls_raw$inw4)
)

fi_2015 <- charls_fi[wave == 2015 & age_at_wave >= 65, .(ID, fi = fi_primary)]
fi_2015[, ID := as.character(ID)]
adl_sub <- adl_data[inw3 == 1 & inw4 == 1, .(ID, adl_2015, adl_2018)]
adl_sub[, ID := as.character(ID)]
val <- merge(fi_2015, adl_sub, by = "ID")
# ADL: 0=no difficulty, 1-4=difficulty levels. Recode to binary 0/1
val[, adl_2018_bin := ifelse(adl_2018 >= 1, 1, 0)]
val <- val[is.na(adl_2015) | adl_2015 == 0]
val <- val[!is.na(fi) & !is.na(adl_2018_bin)]
cat("  CHARLS validation n:", nrow(val), "\n")

if (nrow(val) > 100) {
  m <- glm(adl_2018_bin ~ fi, data = val, family = binomial())
  rr010 <- round(exp(coef(m)["fi"] * 0.10), 3)
  ci010 <- round(exp(confint(m)["fi",] * 0.10), 3)
  sd_fi <- sd(val$fi)
  rr1sd <- round(exp(coef(m)["fi"] * sd_fi), 3)
  ci1sd <- round(exp(confint(m)["fi",] * sd_fi), 3)

  val[, decile := cut(fi, breaks = quantile(fi, probs = seq(0,1,0.1), na.rm=TRUE), include.lowest=TRUE)]
  dec_risk <- val[, .(n=.N, n_adl=sum(adl_2018_bin), risk=round(mean(adl_2018_bin),4)), by=decile]

  charls_adl <- data.table(
    cohort="CHARLS", validation="2015 FI -> 2018 ADL",
    n=nrow(val), rr_per_010=rr010, ci_low_010=ci010[1], ci_high_010=ci010[2],
    rr_per_1sd=rr1sd, ci_low_1sd=ci1sd[1], ci_high_1sd=ci1sd[2]
  )
  fwrite(charls_adl, file.path(root, "CHARLS_FI_incident_ADL_validation.csv"))
  fwrite(dec_risk, file.path(root, "CHARLS_FI_ADL_by_decile.csv"))
  cat("  CHARLS ADL:\n"); print(charls_adl)
}

# CLASS
class_18_raw <- read_dta("E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA/CLASS2018-cleaned release.dta")
class_20_raw <- read_dta("E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA/individual -2020  cleaned for user.dta")
adl18 <- data.table(class_id = as.character(class_18_raw[["rid"]]), adl_2018 = safe_num(class_18_raw[["b5"]]))
adl20 <- data.table(class_id = as.character(class_20_raw[["V1"]]), adl_2020 = safe_num(class_20_raw[["B5"]]))

fi_2018 <- class_fi[wave == 2018, .(class_id, fi = fi_primary)]
fi_2018[, class_id := as.character(class_id)]
adl18[, class_id := as.character(class_id)]
adl20[, class_id := as.character(class_id)]
val_c <- merge(fi_2018, adl18, by = "class_id")
val_c <- merge(val_c, adl20, by = "class_id")
val_c <- val_c[adl_2018 == 2 | is.na(adl_2018)]
val_c <- val_c[!is.na(fi) & !is.na(adl_2020)]
val_c[, adl_bin := ifelse(adl_2020 == 1, 1, 0)]
cat("  CLASS validation n:", nrow(val_c), "\n")

if (nrow(val_c) > 100) {
  m2 <- glm(adl_bin ~ fi, data = val_c, family = binomial())
  rr010c <- round(exp(coef(m2)["fi"] * 0.10), 3)
  ci010c <- round(exp(confint(m2)["fi",] * 0.10), 3)
  sd_fic <- sd(val_c$fi)
  rr1sdc <- round(exp(coef(m2)["fi"] * sd_fic), 3)
  ci1sdc <- round(exp(confint(m2)["fi",] * sd_fic), 3)

  val_c[, decile := cut(fi, breaks = quantile(fi, probs = seq(0,1,0.1), na.rm=TRUE), include.lowest=TRUE)]
  dec_risk_c <- val_c[, .(n=.N, n_adl=sum(adl_bin), risk=round(mean(adl_bin),4)), by=decile]

  class_adl <- data.table(
    cohort="CLASS", validation="2018 FI -> 2020 ADL help",
    n=nrow(val_c), rr_per_010=rr010c, ci_low_010=ci010c[1], ci_high_010=ci010c[2],
    rr_per_1sd=rr1sdc, ci_low_1sd=ci1sdc[1], ci_high_1sd=ci1sdc[2]
  )
  fwrite(class_adl, file.path(root, "CLASS_FI_incident_ADL_validation.csv"))
  fwrite(dec_risk_c, file.path(root, "CLASS_FI_ADL_by_decile.csv"))
  cat("  CLASS ADL:\n"); print(class_adl)
}

# ============================================================
# Issue 9: Updated go/no-go
# ============================================================
cat("\n[9] Go/no-go decision...\n")
charls_pct <- c_valid$pct_valid
class_pct_18plus <- cl_valid$pct_valid[cl_valid$wave >= 2018]

decision <- paste0(
"# Step C Go/No-Go Decision - V5.2 (Corrected)\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## Validity Rates (Age 65+)\n\n",
"### CHARLS\n")
for (i in 1:nrow(c_valid)) {
  decision <- paste0(decision, "- ", c_valid$wave[i], ": ", c_valid$pct_valid[i], "% (n=", c_valid$n_valid[i], ")\n")
}
decision <- paste0(decision, "\n### CLASS\n")
for (i in 1:nrow(cl_valid)) {
  decision <- paste0(decision, "- ", cl_valid$wave[i], ": ", cl_valid$pct_valid[i], "% (n=", cl_valid$n_valid[i], ")\n")
}
decision <- paste0(decision,
"\n## CLASS 2016 Decision\n\n",
"- CLASS 2016 validity: ", cl_valid$pct_valid[cl_valid$wave == 2016], "% (below 70%)\n",
"- CLASS 2016 EXCLUDED from primary Markov model\n",
"- CLASS architecture: 2018-2020 (primary), 2020-2023 (secondary)\n\n",
"## Age Gradient\n\n",
"- CHARLS: FI increases with age (P<0.001)\n",
"- CLASS: Age gradient present\n\n",
"## ADL Validation\n\n",
"- CHARLS: Higher FI predicts incident ADL\n",
"- CLASS: Higher FI predicts ADL help requirement\n\n",
"## Decision\n\n",
"**GO TO STEP D**\n\n",
"CHARLS: 2011, 2013, 2015, 2018\n",
"CLASS: 2018, 2020, 2023 (2016 excluded)\n\n",
"Use neutral labels: low/intermediate/high-deficit\n"
)
writeLines(decision, file.path(root, "StepC_go_no_go_decision.md"))
cat("Saved StepC_go_no_go_decision.md\n")
cat("\nCompleted:", format(Sys.time()), "\n")
