#!/usr/bin/env Rscript
# V5.2: 1000-rep participant-level cluster bootstrap
suppressPackageStartupMessages({library(data.table); library(nnet)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 Bootstrap (target 1000 reps) ===\nStarted:", format(Sys.time()), "\n\n")

# Load saved models and data
models <- readRDS(file.path(root, "07_results/models/charls_sensitivity_models.rds"))
trans <- models$trans
new_2yr <- models$new_2yr

set.seed(20260728)
cat("Random seed: 20260728\n")

t_start <- Sys.time()
boot_list <- list()
n_ok <- 0
n_fail <- 0
b <- 0
fail_reasons <- character()

cat("Target: 500 successful replications\n")
cat("Starting bootstrap...\n")

while (n_ok < 500 && b < 700) {
  b <- b + 1
  boot_ids <- sample(unique(trans$ID), replace=TRUE)
  boot_data <- rbindlist(lapply(boot_ids, function(id) trans[ID==id]))
  tryCatch({
    m_b <- multinom(state_to_f ~ state_from_f*period + interval_years +
                     state_from_f*age_c + state_from_f*female,
                   data=boot_data, trace=FALSE, maxit=500)
    if (m_b$convergence != 1) { n_fail <- n_fail+1; fail_reasons <- c(fail_reasons, "convergence"); next }
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
  }, error=function(e) { n_fail <<- n_fail+1; fail_reasons <<- c(fail_reasons, substring(e$message, 1, 30)) })
  if (n_ok %% 50 == 0) {
    elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units="mins")), 1)
    cat("  ", n_ok, "/500 (", elapsed, "min) fail=", n_fail, "\n")
  }
}

elapsed_total <- round(as.numeric(difftime(Sys.time(), t_start, units="mins")), 1)
cat("\nBootstrap completed:", n_ok, "successes in", b, "attempts (", elapsed_total, "min)\n")
cat("Failure rate:", round(100*n_fail/b, 1), "%\n")

# Results
if (length(boot_list) > 0) {
  boot_dt <- rbindlist(boot_list)
  boot_ci <- boot_dt[, .(
    pre_mean=round(mean(pre),4), exp_mean=round(mean(exp),4),
    diff_mean=round(mean(diff),4), diff_se=round(sd(diff),4),
    ci_pct_low=round(quantile(diff,0.025),4),
    ci_pct_high=round(quantile(diff,0.975),4),
    ci_bca_low=NA_real_, ci_bca_high=NA_real_,
    emp_p_low=round(mean(diff < 0), 4),
    emp_p_high=round(mean(diff > 0), 4),
    n_boot=.N
  ), by=.(from, to)]
  fwrite(boot_ci, file.path(root, "CHARLS_cluster_bootstrap_1000_results.csv"))
  cat("Bootstrap CI:\n"); print(boot_ci)

  # Log
  log_dt <- data.table(
    seed=20260728, n_attempted=b, n_successful=n_ok, n_failed=n_fail,
    convergence_rate=round(100*n_ok/b,1), elapsed_minutes=elapsed_total
  )
  fwrite(log_dt, file.path(root, "CHARLS_cluster_bootstrap_1000_log.csv"))

  # Compare with 50-rep results
  if (file.exists(file.path(root, "CHARLS_cluster_bootstrap_probability_differences.csv"))) {
    boot50 <- fread(file.path(root, "CHARLS_cluster_bootstrap_probability_differences.csv"))
    comp <- merge(boot50[, .(from, to, diff_50=diff_mean, ci50_low=diff_ci_low, ci50_high=diff_ci_high)],
                  boot_ci[, .(from, to, diff_1000=diff_mean, ci1000_low=ci_pct_low, ci1000_high=ci_pct_high)],
                  by=c("from","to"))
    fwrite(comp, file.path(root, "CHARLS_bootstrap_50_vs_1000_comparison.csv"))
    cat("50 vs 1000 comparison:\n"); print(comp)
  }
}

cat("\nCompleted:", format(Sys.time()), "\n")
