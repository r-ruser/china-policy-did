#!/usr/bin/env Rscript
# V5.2: Final analyses WITHOUT bootstrap
suppressPackageStartupMessages({library(haven); library(data.table); library(nnet)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 Final Analyses (no bootstrap) ===\nStarted:", format(Sys.time()), "\n\n")

# Load and prepare data
fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, age_at_wave, female, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)

trans <- dt[, .(
  wave_from=wave[1:(.N-1)], wave_to=wave[2:.N],
  state_from=state[1:(.N-1)], state_to=state[2:.N],
  age_from=age_at_wave[1:(.N-1)], female=female[1:(.N-1)]
), by=ID]
trans <- trans[!is.na(state_to)]
trans[, period:=fifelse(wave_to<=2015, 0L, 1L)]
trans[, age_c:=(age_from-70)/5]
trans[, age75:=as.integer(age_from>=75)]
trans[, interval_years:=as.numeric(wave_to-wave_from)]
trans[, state_from_f:=factor(state_from, levels=1:3)]
trans[, state_to_f:=factor(state_to, levels=1:3)]
trans[, period3:=fifelse(wave_from==2011&wave_to==2013, 0L,
                  fifelse(wave_from==2013&wave_to==2015, 1L, 2L))]

charls_raw <- read_dta("E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta")
cov <- data.table(ID_cov=as.character(charls_raw$ID),
                  education=safe_num(charls_raw$raeduc_c),
                  rural=as.integer(safe_num(charls_raw$h1rural)==1))
cov[is.na(education), education:=0]; cov[is.na(rural), rural:=0]
trans <- merge(trans, cov, by.x="ID", by.y="ID_cov", all.x=TRUE)
trans[is.na(education), education:=0]; trans[is.na(rural), rural:=0]
trans[, low_edu:=as.integer(education<=1)]
cat("Records:", nrow(trans), "\n\n")

# 1. Duration-adjusted 2-year probabilities
cat("[1] Duration-adjusted 2-year probabilities...\n")
m_dur <- multinom(state_to_f ~ state_from_f*period + interval_years +
                   state_from_f*age_c + state_from_f*female, data=trans, trace=FALSE)
new_2yr <- expand.grid(state_from_f=factor(1:3,levels=1:3), period=c(0,1),
                        interval_years=2, age_c=0, female=0.5)
pred <- predict(m_dur, newdata=new_2yr, type="probs")
prob <- data.table(period=rep(c("pre-expansion","expansion"),each=3), from=rep(1:3,2),
                   p_low=round(pred[,1],4), p_mid=round(pred[,2],4), p_high=round(pred[,3],4))
fwrite(prob, file.path(root, "CHARLS_common_2year_transition_probabilities.csv"))
print(prob)

# 2. Three-period model
cat("\n[2] Three-period model...\n")
m3 <- multinom(state_to_f ~ state_from_f*factor(period3) + interval_years +
                state_from_f*age_c + state_from_f*female, data=trans, trace=FALSE)
prob3 <- list()
for (p in 0:2) {
  new_p <- expand.grid(state_from_f=factor(1:3,levels=1:3), period3=p,
                        interval_years=2, age_c=0, female=0.5)
  pred_p <- predict(m3, newdata=new_p, type="probs")
  for (s in 1:3) prob3[[length(prob3)+1]] <- data.table(
    period=c("2011-2013","2013-2015","2015-2018(2yr)")[p+1], from=s,
    p_low=round(pred_p[s,1],4), p_mid=round(pred_p[s,2],4), p_high=round(pred_p[s,3],4))
}
fwrite(rbindlist(prob3), file.path(root, "CHARLS_three_period_transition_probabilities.csv"))

# 3. Age interaction
cat("\n[3] Age interaction...\n")
m_age <- multinom(state_to_f ~ state_from_f*period*age75 + interval_years +
                   state_from_f*age_c + state_from_f*female, data=trans, trace=FALSE)
cat("AIC:", AIC(m_age), "\n")

# 4. Social vulnerability
cat("\n[4] Social vulnerability...\n")
trans[, social_vuln:=as.integer(rural==1 & low_edu==1)]
m_social <- multinom(state_to_f ~ state_from_f*period + interval_years +
                     state_from_f*age_c + state_from_f*female +
                     state_from_f*social_vuln, data=trans, trace=FALSE)

# 5. Markov assumption
cat("\n[5] Markov assumption...\n")
trans[, prev_state:=shift(state_from,1), by=ID]
tp <- trans[!is.na(prev_state)]
tp[, prev_state_f:=factor(prev_state, levels=1:3)]
m_h <- multinom(state_to_f ~ state_from_f + prev_state_f + period +
                interval_years + age_c + female, data=tp, trace=FALSE)
m_nh <- multinom(state_to_f ~ state_from_f + period +
                interval_years + age_c + female, data=tp, trace=FALSE)
cat("With history AIC:", AIC(m_h), "\n")
cat("Without history AIC:", AIC(m_nh), "\n")

# 6. Save all results
m_base <- multinom(state_to_f ~ state_from_f, data=trans, trace=FALSE)
m_per <- multinom(state_to_f ~ state_from_f + period, data=trans, trace=FALSE)
aic <- data.table(
  model=c("Base","Period","Duration","3-period","Age","Social","History"),
  n_obs=c(nrow(trans),nrow(trans),nrow(trans),nrow(trans),nrow(trans),nrow(trans),nrow(tp)),
  aic=round(c(AIC(m_base),AIC(m_per),AIC(m_dur),AIC(m3),AIC(m_age),AIC(m_social),AIC(m_h)),1))
fwrite(aic, file.path(root, "CHARLS_AIC_comparability_audit.csv"))
cat("\nAIC:\n"); print(aic)

# Save models for bootstrap later
saveRDS(list(m_dur=m_dur, m3=m3, m_age=m_age, m_social=m_social,
             m_h=m_h, trans=trans, tp=tp, new_2yr=new_2yr),
        file.path(root, "07_results/models/charls_sensitivity_models.rds"))
cat("Saved models for bootstrap\n")
cat("\nCompleted:", format(Sys.time()), "\n")
