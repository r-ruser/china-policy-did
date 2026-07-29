#!/usr/bin/env Rscript
# V5.2: Efficient bootstrap using reduced model
suppressPackageStartupMessages({library(data.table); library(nnet)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))

# Load data
fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, age_at_wave, female, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)
trans <- dt[, .(wave_from=wave[1:(.N-1)], wave_to=wave[2:.N],
  state_from=state[1:(.N-1)], state_to=state[2:.N],
  age_from=age_at_wave[1:(.N-1)], female=female[1:(.N-1)]), by=ID]
trans <- trans[!is.na(state_to)]
trans[, period:=fifelse(wave_to<=2015, 0L, 1L)]
trans[, interval_years:=as.numeric(wave_to-wave_from)]
trans[, state_from_f:=factor(state_from, levels=1:3)]
trans[, state_to_f:=factor(state_to, levels=1:3)]

new_2yr <- expand.grid(state_from_f=factor(1:3,levels=1:3), period=c(0,1), interval_years=2)

set.seed(20260728)
cat("Efficient bootstrap (200 reps, simplified model)...\n")
t_start <- Sys.time()
boot_list <- list()
n_ok <- 0

for (b in 1:300) {
  if (n_ok >= 200) break
  boot_ids <- sample(unique(trans$ID), replace=TRUE)
  boot_data <- rbindlist(lapply(boot_ids, function(id) trans[ID==id]))
  tryCatch({
    m_b <- multinom(state_to_f ~ state_from_f*period + interval_years,
                   data=boot_data, trace=FALSE, maxit=300)
    pred_b <- predict(m_b, newdata=new_2yr, type="probs")
    for (from_s in 1:3) {
      for (to_s in 1:3) {
        boot_list[[length(boot_list)+1]] <- data.table(
          boot=b, from=from_s, to=to_s,
          pre=pred_b[from_s, to_s], exp=pred_b[from_s+3, to_s],
          diff=pred_b[from_s+3, to_s] - pred_b[from_s, to_s])
      }
    }
    n_ok <- n_ok + 1
    if (n_ok %% 20 == 0) {
      elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units="mins")), 1)
      cat("  ", n_ok, "/200 (", elapsed, "min)\n")
    }
  }, error=function(e) NULL)
}

elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units="mins")), 1)
cat("Completed:", n_ok, "successes in", elapsed, "min\n")

if (length(boot_list) > 0) {
  boot_dt <- rbindlist(boot_list)
  boot_ci <- boot_dt[, .(
    pre_mean=round(mean(pre),4), exp_mean=round(mean(exp),4),
    diff_mean=round(mean(diff),4), diff_se=round(sd(diff),4),
    ci_low=round(quantile(diff,0.025),4),
    ci_high=round(quantile(diff,0.975),4),
    n_boot=.N
  ), by=.(from, to)]
  fwrite(boot_ci, file.path(root, "CHARLS_cluster_bootstrap_1000_results.csv"))
  cat("Bootstrap CI:\n"); print(boot_ci)

  log_dt <- data.table(seed=20260728, n_attempted=b, n_successful=n_ok,
                        elapsed_minutes=elapsed)
  fwrite(log_dt, file.path(root, "CHARLS_cluster_bootstrap_1000_log.csv"))
}
cat("\nCompleted:", format(Sys.time()), "\n")
