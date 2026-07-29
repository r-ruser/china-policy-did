#!/usr/bin/env Rscript
# V5.2: Remaining analyses - age, social, attrition, FI sensitivity, CLASS
suppressPackageStartupMessages({library(haven); library(data.table); library(nnet)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 Remaining Analyses ===\nStarted:", format(Sys.time()), "\n\n")

# Load data
fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, age_at_wave, female, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)
trans <- dt[, .(wave_from=wave[1:(.N-1)], wave_to=wave[2:.N],
  state_from=state[1:(.N-1)], state_to=state[2:.N],
  age_from=age_at_wave[1:(.N-1)], female=female[1:(.N-1)], fi_from=fi[1:(.N-1)]), by=ID]
trans <- trans[!is.na(state_to)]
trans[, period:=fifelse(wave_to<=2015, 0L, 1L)]
trans[, age_c:=(age_from-70)/5]
trans[, age75:=as.integer(age_from>=75)]
trans[, interval_years:=as.numeric(wave_to-wave_from)]
trans[, state_from_f:=factor(state_from, levels=1:3)]
trans[, state_to_f:=factor(state_to, levels=1:3)]

charls_raw <- read_dta("E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta")
cov <- data.table(ID_cov=as.character(charls_raw$ID),
                  education=safe_num(charls_raw$raeduc_c),
                  rural=as.integer(safe_num(charls_raw$h1rural)==1))
cov[is.na(education), education:=0]; cov[is.na(rural), rural:=0]
trans <- merge(trans, cov, by.x="ID", by.y="ID_cov", all.x=TRUE)
trans[is.na(education), education:=0]; trans[is.na(rural), rural:=0]
trans[, low_edu:=as.integer(education<=1)]
trans[, social_vuln:=as.integer(rural==1 & low_edu==1)]
cat("Records:", nrow(trans), "\n\n")

# ============================================================
# 1. Age-75 distributional analysis
# ============================================================
cat("[1] Age-75 distributional analysis...\n")
m_age <- multinom(state_to_f ~ state_from_f*period*age75 + interval_years +
                   state_from_f*age_c + state_from_f*female, data=trans, trace=FALSE)

# Common 2-year probabilities by age group and period
age_probs <- list()
for (ag in c("65-74","75+")) {
  a75 <- ifelse(ag=="75+",1,0); ac <- ifelse(ag=="75+",1,-1)
  for (p in c(0,1)) {
    new_a <- expand.grid(state_from_f=factor(1:3,levels=1:3), period=p,
                          interval_years=2, age75=a75, age_c=ac, female=0.5)
    pred_a <- predict(m_age, newdata=new_a, type="probs")
    for (s in 1:3) age_probs[[length(age_probs)+1]] <- data.table(
      age_group=ag, period=c("pre-expansion","expansion")[p+1], from=s,
      p_low=round(pred_a[s,1],4), p_mid=round(pred_a[s,2],4), p_high=round(pred_a[s,3],4))
  }
}
age_dt <- rbindlist(age_probs)
fwrite(age_dt, file.path(root, "CHARLS_age75_period_transition_probabilities.csv"))

# Compute DID
age_did <- data.table()
for (s in 1:3) {
  for (nm in c("p_low","p_mid","p_high")) {
    pre_65 <- age_dt[age_group=="65-74"&period=="pre-expansion"&from==s][[nm]]
    exp_65 <- age_dt[age_group=="65-74"&period=="expansion"&from==s][[nm]]
    pre_75 <- age_dt[age_group=="75+"&period=="pre-expansion"&from==s][[nm]]
    exp_75 <- age_dt[age_group=="75+"&period=="expansion"&from==s][[nm]]
    age_did <- rbind(age_did, data.table(from=s, destination=nm,
      diff_65=round(exp_65-pre_65,4), diff_75=round(exp_75-pre_75,4),
      did=round((exp_75-pre_75)-(exp_65-pre_65),4)))
  }
}
fwrite(age_did, file.path(root, "CHARLS_age75_period_probability_DID.csv"))
cat("Age-75 DID:\n"); print(age_did)

# ============================================================
# 2. Social vulnerability
# ============================================================
cat("\n[2] Social vulnerability...\n")
m_social <- multinom(state_to_f ~ state_from_f*period + interval_years +
                     state_from_f*age_c + state_from_f*female +
                     state_from_f*social_vuln, data=trans, trace=FALSE)

# Social vulnerability probabilities
soc_probs <- list()
for (sv in c(0,1)) {
  for (p in c(0,1)) {
    new_s <- expand.grid(state_from_f=factor(1:3,levels=1:3), period=p,
                          interval_years=2, age_c=0, female=0.5, social_vuln=sv)
    pred_s <- predict(m_social, newdata=new_s, type="probs")
    for (s in 1:3) soc_probs[[length(soc_probs)+1]] <- data.table(
      social_vuln=sv, period=c("pre-expansion","expansion")[p+1], from=s,
      p_low=round(pred_s[s,1],4), p_mid=round(pred_s[s,2],4), p_high=round(pred_s[s,3],4))
  }
}
fwrite(rbindlist(soc_probs), file.path(root, "CHARLS_social_vulnerability_transition_probabilities.csv"))

# ============================================================
# 3. Attrition analysis
# ============================================================
cat("\n[3] Attrition analysis...\n")

# Attrition: compare those with and without valid next state
dt_all <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, age_at_wave, female, fi=fi_primary)]
dt_all[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt_all[, ID := as.character(ID)]
setorder(dt_all, ID, wave)
dt_all[, has_next := !is.na(shift(state, 1)) & shift(ID) == ID, by=ID]

# Compare characteristics
attr_comp <- dt_all[, .(
  n = .N,
  pct_with_next = round(100*mean(has_next, na.rm=TRUE), 1),
  mean_fi = round(mean(fi, na.rm=TRUE), 4),
  mean_age = round(mean(age_at_wave, na.rm=TRUE), 1),
  pct_female = round(100*mean(female, na.rm=TRUE), 1)
), by = .(wave, state)]
fwrite(attr_comp, file.path(root, "CHARLS_transition_attrition_comparison.csv"))
cat("Attrition comparison:\n"); print(attr_comp)

# ============================================================
# 4. FI-definition sensitivities
# ============================================================
cat("\n[4] FI-definition sensitivities...\n")

# Use saved models reference for comparison
sensitivity_results <- list()

# 4a. Alternative cut-point (high >= 0.30)
trans_alt <- copy(trans)
trans_alt[, state_alt := ifelse(fi_from < 0.10, 1L, ifelse(fi_from < 0.30, 2L, 3L))]
trans_alt[, state_from_fa := factor(state_alt, levels=1:3)]
trans_alt[, state_to_fa := factor(state_alt, levels=1:3)]

# For state_to, need to recalculate using the end-state FI
fi_end <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID=as.character(ID), wave, fi=fi_primary)]
fi_end[, state_end := ifelse(fi < 0.10, 1L, ifelse(fi < 0.30, 2L, 3L))]
setorder(fi_end, ID, wave)
trans_end <- fi_end[, .(ID, wave_to=wave, state_to_alt=state_end), by=ID]

trans_alt2 <- merge(trans_alt, trans_end[, .(ID, wave_to, state_to_alt)], by=c("ID","wave_to"), all.x=TRUE)
trans_alt2 <- trans_alt2[!is.na(state_to_alt)]
trans_alt2[, state_to_fa := factor(state_to_alt, levels=1:3)]

m_alt <- multinom(state_to_fa ~ state_from_fa*period + interval_years, data=trans_alt2, trace=FALSE)
new_2yr_alt <- expand.grid(state_from_fa=factor(1:3,levels=1:3), period=c(0,1), interval_years=2)
pred_alt <- predict(m_alt, newdata=new_2yr_alt, type="probs")
cat("  Alternative cut-point (0.30):\n")
print(round(pred_alt, 4))

# 4b. 70% completion threshold
new_2yr_std <- expand.grid(state_from_f=factor(1:3,levels=1:3), period=c(0,1), interval_years=2)
fi_70 <- fi[age_at_wave >= 65 & !is.na(fi_70), .(ID, wave, fi=fi_70)]
fi_70[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
setorder(fi_70, ID, wave)
trans_70 <- fi_70[, .(wave_from=wave[1:(.N-1)], wave_to=wave[2:.N],
  state_from=state[1:(.N-1)], state_to=state[2:.N]), by=ID]
trans_70 <- trans_70[!is.na(state_to)]
trans_70[, period:=fifelse(wave_to<=2015, 0L, 1L)]
trans_70[, interval_years:=as.numeric(wave_to-wave_from)]
trans_70[, state_from_f:=factor(state_from, levels=1:3)]
trans_70[, state_to_f:=factor(state_to, levels=1:3)]

if (nrow(trans_70) > 100) {
  m_70 <- multinom(state_to_f ~ state_from_f*period + interval_years, data=trans_70, trace=FALSE)
  pred_70 <- predict(m_70, newdata=new_2yr_std, type="probs")
  cat("  70% threshold:\n"); print(round(pred_70, 4))
}

# 4c. Sex-stratified
for (sex_val in c(0, 1)) {
  trans_sex <- trans[female == sex_val]
  if (nrow(trans_sex) > 100) {
    m_sex <- multinom(state_to_f ~ state_from_f*period + interval_years, data=trans_sex, trace=FALSE)
    pred_sex <- predict(m_sex, newdata=new_2yr_std, type="probs")
    sex_label <- ifelse(sex_val == 0, "Male", "Female")
    cat("  ", sex_label, ":\n"); print(round(pred_sex, 4))
  }
}

cat("\nCompleted:", format(Sys.time()), "\n")
