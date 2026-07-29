#!/usr/bin/env Rscript
# V5.2: Sensitivity analyses (optimised for speed)
suppressPackageStartupMessages({library(haven); library(data.table); library(nnet)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 Sensitivity Analyses ===\nStarted:", format(Sys.time()), "\n\n")

# Load and prepare data
fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, age_at_wave, female, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)

trans <- dt[, .(
  wave_from = wave[1:(.N-1)], wave_to = wave[2:.N],
  state_from = state[1:(.N-1)], state_to = state[2:.N],
  age_from = age_at_wave[1:(.N-1)], female = female[1:(.N-1)]
), by = ID]
trans <- trans[!is.na(state_to)]
trans[, period := fifelse(wave_to <= 2015, 0L, 1L)]
trans[, age_c := (age_from - 70) / 5]
trans[, age75 := as.integer(age_from >= 75)]
trans[, interval_years := as.numeric(wave_to - wave_from)]
trans[, state_from_f := factor(state_from, levels = 1:3)]
trans[, state_to_f := factor(state_to, levels = 1:3)]
trans[, period3 := fifelse(wave_from==2011 & wave_to==2013, 0L,
                    fifelse(wave_from==2013 & wave_to==2015, 1L, 2L))]

cat("Transition records:", nrow(trans), "\n\n")

# ============================================================
# Common-horizon 2-year probabilities (interval-duration adjusted)
# ============================================================
cat("[1] Common-horizon 2-year probabilities...\n")
m_dur <- multinom(state_to_f ~ state_from_f * period + interval_years +
                   state_from_f * age_c + state_from_f * female,
                 data = trans, trace = FALSE)

new_2yr <- expand.grid(state_from_f=factor(1:3,levels=1:3), period=c(0,1),
                        interval_years=2, age_c=0, female=0.5)
pred <- predict(m_dur, newdata=new_2yr, type="probs")

prob_2yr <- data.table(
  period = rep(c("pre-expansion","expansion"), each=3),
  from = rep(1:3, 2),
  p_low = round(as.vector(pred[,1]), 4),
  p_mid = round(as.vector(pred[,2]), 4),
  p_high = round(as.vector(pred[,3]), 4)
)
fwrite(prob_2yr, file.path(root, "CHARLS_common_2year_transition_probabilities.csv"))
cat("Common 2-year probabilities:\n"); print(prob_2yr)

# Probability differences
diffs <- data.table()
for (from_s in 1:3) {
  pre <- prob_2yr[period=="pre-expansion" & from==from_s]
  exp <- prob_2yr[period=="expansion" & from==from_s]
  for (to_s in 1:3) {
    pre_p <- pre[[paste0("p_", c("low","mid","high")[to_s])]]
    exp_p <- exp[[paste0("p_", c("low","mid","high")[to_s])]]
    diffs <- rbind(diffs, data.table(
      from=from_s, to=to_s,
      pre=pre_p, expansion=exp_p,
      diff=round(exp_p - pre_p, 4),
      pct_change=round(100*(exp_p - pre_p)/pre_p, 1)
    ))
  }
}
fwrite(diffs, file.path(root, "CHARLS_common_horizon_probability_differences.csv"))
cat("Probability differences:\n"); print(diffs)

# ============================================================
# Cluster bootstrap (100 reps)
# ============================================================
cat("\n[2] Cluster bootstrap (100 reps)...\n")
set.seed(20260728)
boot_list <- list()
n_success <- 0
b <- 0
while (n_success < 100 && b < 150) {
  b <- b + 1
  boot_ids <- sample(unique(trans$ID), replace=TRUE)
  boot_data <- rbindlist(lapply(boot_ids, function(id) trans[ID==id]))
  tryCatch({
    m_b <- multinom(state_to_f ~ state_from_f * period + interval_years +
                     state_from_f * age_c + state_from_f * female,
                   data=boot_data, trace=FALSE)
    pred_b <- predict(m_b, newdata=new_2yr, type="probs")
    for (from_s in 1:3) {
      for (to_s in 1:3) {
        boot_list[[length(boot_list)+1]] <- data.table(
          boot=b, from=from_s, to=to_s,
          diff=pred_b[from_s, to_s] - pred_b[from_s + 3*(1-1), to_s]  # period diff
        )
      }
    }
    n_success <- n_success + 1
    if (n_success %% 20 == 0) cat("  Bootstrap:", n_success, "successful\n")
  }, error=function(e) NULL)
}

if (length(boot_list) > 0) {
  boot_dt <- rbindlist(boot_list)
  boot_ci <- boot_dt[, .(
    mean_diff=round(mean(diff),4), se=round(sd(diff),4),
    ci_low=round(quantile(diff,0.025),4), ci_high=round(quantile(diff,0.975),4),
    n_boot=.N
  ), by=.(from, to)]
  fwrite(boot_ci, file.path(root, "CHARLS_cluster_bootstrap_probability_differences.csv"))
  cat("Bootstrap CI:\n"); print(boot_ci)
}

# ============================================================
# Three-period model
# ============================================================
cat("\n[3] Three-period model...\n")
m3 <- multinom(state_to_f ~ state_from_f * factor(period3) + interval_years +
                state_from_f * age_c + state_from_f * female,
              data=trans, trace=FALSE)
cat("3-period AIC:", AIC(m3), "\n")

prob3 <- list()
for (p in 0:2) {
  new_p <- expand.grid(state_from_f=factor(1:3,levels=1:3), period3=p,
                        interval_years=2, age_c=0, female=0.5)
  pred_p <- predict(m3, newdata=new_p, type="probs")
  for (from_s in 1:3) {
    prob3[[length(prob3)+1]] <- data.table(
      period=c("2011-2013","2013-2015","2015-2018(2yr-adj)")[p+1],
      from=from_s, p_low=round(pred_p[from_s,1],4),
      p_mid=round(pred_p[from_s,2],4), p_high=round(pred_p[from_s,3],4))
  }
}
prob3_dt <- rbindlist(prob3)
fwrite(prob3_dt, file.path(root, "CHARLS_three_period_transition_probabilities.csv"))
cat("3-period probabilities:\n"); print(prob3_dt)

# ============================================================
# Age interaction
# ============================================================
cat("\n[4] Age interaction...\n")
m_age <- multinom(state_to_f ~ state_from_f * period * age75 + interval_years +
                   state_from_f * age_c + state_from_f * female,
                 data=trans, trace=FALSE)
cat("Age model AIC:", AIC(m_age), "\n")

# ============================================================
# Markov assumption
# ============================================================
cat("\n[5] Markov assumption...\n")
trans[, prev_state := shift(state_from, 1), by=ID]
tp <- trans[!is.na(prev_state)]
tp[, prev_state_f := factor(prev_state, levels=1:3)]
m_hist <- multinom(state_to_f ~ state_from_f + prev_state_f + period + interval_years + age_c + female, data=tp, trace=FALSE)
m_nohist <- multinom(state_to_f ~ state_from_f + period + interval_years + age_c + female, data=tp, trace=FALSE)
cat("With history AIC:", AIC(m_hist), "\n")
cat("Without history AIC:", AIC(m_nohist), "\n")

# ============================================================
# AIC comparability
# ============================================================
cat("\n[6] AIC comparability...\n")
m_base <- multinom(state_to_f ~ state_from_f, data=trans, trace=FALSE)
m_per <- multinom(state_to_f ~ state_from_f + period, data=trans, trace=FALSE)
aic <- data.table(
  model=c("Base","Period","Duration","3-period","Age","History"),
  n_obs=c(nrow(trans),nrow(trans),nrow(trans),nrow(trans),nrow(trans),nrow(tp)),
  aic=round(c(AIC(m_base),AIC(m_per),AIC(m_dur),AIC(m3),AIC(m_age),AIC(m_hist)),1)
)
fwrite(aic, file.path(root, "CHARLS_AIC_comparability_audit.csv"))
cat("AIC:\n"); print(aic)

cat("\nCompleted:", format(Sys.time()), "\n")
