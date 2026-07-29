#!/usr/bin/env Rscript
# V5.2: Core sensitivity analyses (no bootstrap)
suppressPackageStartupMessages({library(haven); library(data.table); library(nnet)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 Core Sensitivity ===\nStarted:", format(Sys.time()), "\n\n")

# Load data
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

# Differences
cat("\nProbability differences:\n")
for (from_s in 1:3) {
  pre <- prob[period=="pre-expansion" & from==from_s]
  exp <- prob[period=="expansion" & from==from_s]
  cat("  From state", from_s, ":\n")
  for (nm in c("p_low","p_mid","p_high")) {
    d <- exp[[nm]] - pre[[nm]]
    cat("    ", nm, ":", round(pre[[nm]],4), "->", round(exp[[nm]],4), "diff=", round(d,4), "\n")
  }
}

# 2. Three-period model
cat("\n[2] Three-period model...\n")
m3 <- multinom(state_to_f ~ state_from_f*factor(period3) + interval_years +
                state_from_f*age_c + state_from_f*female, data=trans, trace=FALSE)
cat("AIC:", AIC(m3), "\n")

prob3 <- list()
for (p in 0:2) {
  new_p <- expand.grid(state_from_f=factor(1:3,levels=1:3), period3=p,
                        interval_years=2, age_c=0, female=0.5)
  pred_p <- predict(m3, newdata=new_p, type="probs")
  for (s in 1:3) {
    prob3[[length(prob3)+1]] <- data.table(
      period=c("2011-2013","2013-2015","2015-2018(2yr)")[p+1], from=s,
      p_low=round(pred_p[s,1],4), p_mid=round(pred_p[s,2],4), p_high=round(pred_p[s,3],4))
  }
}
prob3_dt <- rbindlist(prob3)
fwrite(prob3_dt, file.path(root, "CHARLS_three_period_transition_probabilities.csv"))
print(prob3_dt)

# 3. Age interaction
cat("\n[3] Age interaction...\n")
m_age <- multinom(state_to_f ~ state_from_f*period*age75 + interval_years +
                   state_from_f*age_c + state_from_f*female, data=trans, trace=FALSE)
cat("AIC:", AIC(m_age), "\n")

# Predict for age groups
for (ag in c("65-74","75+")) {
  a75 <- ifelse(ag=="75+",1,0)
  ac <- ifelse(ag=="75+",1,-1)
  new_a <- expand.grid(state_from_f=factor(1:3,levels=1:3), period=c(0,1),
                        interval_years=2, age75=a75, age_c=ac, female=0.5)
  pred_a <- predict(m_age, newdata=new_a, type="probs")
  cat("  Age", ag, ":\n")
  print(round(pred_a, 4))
}

# 4. Markov assumption
cat("\n[4] Markov assumption...\n")
trans[, prev_state:=shift(state_from,1), by=ID]
tp <- trans[!is.na(prev_state)]
tp[, prev_state_f:=factor(prev_state, levels=1:3)]
m_h <- multinom(state_to_f ~ state_from_f + prev_state_f + period + interval_years + age_c + female, data=tp, trace=FALSE)
m_nh <- multinom(state_to_f ~ state_from_f + period + interval_years + age_c + female, data=tp, trace=FALSE)
cat("With history AIC:", AIC(m_h), "\n")
cat("Without history AIC:", AIC(m_nh), "\n")
cat("History improves:", AIC(m_h) < AIC(m_nh), "\n")

# 5. AIC audit
cat("\n[5] AIC audit...\n")
m_base <- multinom(state_to_f ~ state_from_f, data=trans, trace=FALSE)
m_per <- multinom(state_to_f ~ state_from_f + period, data=trans, trace=FALSE)
aic <- data.table(model=c("Base","Period","Duration","3-period","Age","History"),
                  n=c(nrow(trans),nrow(trans),nrow(trans),nrow(trans),nrow(trans),nrow(tp)),
                  aic=round(c(AIC(m_base),AIC(m_per),AIC(m_dur),AIC(m3),AIC(m_age),AIC(m_h)),1))
fwrite(aic, file.path(root, "CHARLS_AIC_comparability_audit.csv"))
print(aic)

cat("\nCompleted:", format(Sys.time()), "\n")
