#!/usr/bin/env Rscript
# V5.2: Parallel bootstrap using foreach/doParallel
Sys.setenv(OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
suppressPackageStartupMessages({
  library(haven); library(data.table); library(nnet)
  library(doParallel); library(foreach)
})
options(encoding="UTF-8")
root <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))

cat("=== V5.2 Parallel Bootstrap (foreach) ===\n")
n_cores <- parallel::detectCores(logical=TRUE)
cat("Logical cores:", n_cores, "\n")

# Prepare data
fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)
trans <- dt[, .(wave_from=wave[1:(.N-1)], wave_to=wave[2:.N],
  state_from=state[1:(.N-1)], state_to=state[2:.N]), by=ID]
trans <- trans[!is.na(state_to)]
trans[, period:=fifelse(wave_to<=2015, 0L, 1L)]
trans[, interval_years:=as.numeric(wave_to-wave_from)]
trans[, state_from_f:=factor(state_from, levels=1:3)]
trans[, state_to_f:=factor(state_to, levels=1:3)]
new_2yr <- expand.grid(state_from_f=factor(1:3,levels=1:3), period=c(0,1), interval_years=2)
unique_ids <- unique(trans$ID)

# Register cluster
cl <- makeCluster(n_cores, type="PSOCK")
registerDoParallel(cl)
on.exit(stopCluster(cl), add=TRUE)

# Set single-thread on workers
clusterEvalQ(cl, {
  Sys.setenv(OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
  library(nnet); library(data.table)
})

cat("Running 500-rep bootstrap with", n_cores, "cores...\n")
t_start <- Sys.time()

# Run bootstrap
set.seed(20260728)
results_list <- foreach(b=1:500, .options.snow=list(preschedule=FALSE)) %dopar% {
  # Sample participants
  sampled_ids <- sample(unique_ids, size=length(unique_ids), replace=TRUE)
  bl <- list()
  for (i in seq_along(sampled_ids)) {
    sub <- trans[ID==sampled_ids[i]]
    sub[, bid := paste0(sampled_ids[i], "_", i)]
    bl[[i]] <- sub
  }
  bd <- rbindlist(bl)

  tryCatch({
    m <- multinom(state_to_f ~ state_from_f*period + interval_years,
                  data=bd, trace=FALSE, maxit=300)
    pred <- predict(m, newdata=new_2yr, type="probs")
    if (any(is.na(pred)) || any(pred < 0) || any(pred > 1)) {
      return(data.table(rep=b, converged=FALSE))
    }
    res <- list()
    for (from_s in 1:3) for (to_s in 1:3) {
      res[[length(res)+1]] <- data.table(
        rep=b, from=from_s, to=to_s,
        pre=pred[from_s,to_s], exp=pred[from_s+3,to_s],
        diff=pred[from_s+3,to_s]-pred[from_s,to_s], converged=TRUE)
    }
    rbindlist(res)
  }, error=function(e) data.table(rep=b, converged=FALSE))
}

elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units="mins")), 1)
cat("Bootstrap completed in", elapsed, "min\n")

# Process results
all_boot <- rbindlist(results_list)
n_ok <- uniqueN(all_boot[converged==TRUE, rep])
cat("Successful:", n_ok, "/ 500\n")

if (n_ok > 0) {
  boot_ok <- unique(all_boot[converged==TRUE], by="rep")
  boot_summary <- boot_ok[, .(
    n_boot=.N, mean=round(mean(diff),4), median=round(median(diff),4),
    se=round(sd(diff),4),
    ci_low=round(quantile(diff,0.025),4), ci_high=round(quantile(diff,0.975),4)
  ), by=.(from, to)]
  fwrite(boot_summary, file.path(root, "CHARLS_cluster_bootstrap_500_results.csv"))
  cat("500-rep bootstrap results:\n"); print(boot_summary)

  # Save full distribution
  fwrite(boot_ok, file.path(root, "CHARLS_cluster_bootstrap_500_probability_distributions.csv"))

  # Log
  log_dt <- data.table(seed=20260728, n_target=500, n_successful=n_ok,
                        elapsed_minutes=elapsed, n_workers=n_cores)
  fwrite(log_dt, file.path(root, "CHARLS_cluster_bootstrap_500_log.csv"))

  # Final decision
  writeLines(c(
    "# Bootstrap 500-Replication Final Decision",
    paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste("Workers:", n_cores, "logical cores"),
    paste("Successful:", n_ok, "/ 500"),
    paste("Elapsed:", elapsed, "minutes"),
    "",
    "Results:",
    paste(boot_summary$from, "->", boot_summary$to,
          ": diff=", boot_summary$mean,
          "CI:", boot_summary$ci_low, "to", boot_summary$ci_high)
  ), file.path(root, "CHARLS_bootstrap_500_final_decision.md"))
}

cat("\nCompleted:", format(Sys.time()), "\n")
