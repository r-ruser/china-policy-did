#!/usr/bin/env Rscript
# V5.2: Bootstrap only
suppressPackageStartupMessages({library(data.table); library(nnet)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))

# Load saved models and data
cat("Loading saved models...\n")
models <- readRDS(file.path(root, "07_results/models/charls_sensitivity_models.rds"))
trans <- models$trans
new_2yr <- models$new_2yr

cat("Running cluster bootstrap (50 reps)...\n")
set.seed(20260728)
boot_list <- list()
n_ok <- 0
b <- 0
t_start <- Sys.time()

while (n_ok < 50 && b < 100) {
  b <- b + 1
  boot_ids <- sample(unique(trans$ID), replace=TRUE)
  boot_data <- rbindlist(lapply(boot_ids, function(id) trans[ID==id]))
  tryCatch({
    m_b <- multinom(state_to_f ~ state_from_f*period + interval_years +
                     state_from_f*age_c + state_from_f*female, data=boot_data, trace=FALSE)
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
    if (n_ok %% 10 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units="mins"))
      cat("  Bootstrap:", n_ok, "/100 (", round(elapsed,1), "min)\n")
    }
  }, error=function(e) NULL)
}

if (length(boot_list) > 0) {
  boot_dt <- rbindlist(boot_list)
  boot_ci <- boot_dt[, .(
    pre_mean=round(mean(pre),4), exp_mean=round(mean(exp),4),
    diff_mean=round(mean(diff),4), diff_se=round(sd(diff),4),
    diff_ci_low=round(quantile(diff,0.025),4),
    diff_ci_high=round(quantile(diff,0.975),4),
    n_boot=.N
  ), by=.(from, to)]
  fwrite(boot_ci, file.path(root, "CHARLS_cluster_bootstrap_probability_differences.csv"))
  cat("Bootstrap CI:\n"); print(boot_ci)

  conv <- data.table(n_attempted=b, n_successful=n_ok, rate=round(100*n_ok/b,1))
  fwrite(conv, file.path(root, "CHARLS_bootstrap_convergence_log.csv"))
  cat("Convergence:", conv$n_successful, "/", conv$n_attempted, "\n")
}
cat("\nCompleted:", format(Sys.time()), "\n")
