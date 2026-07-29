#!/usr/bin/env Rscript
# Bootstrap worker v2: inline logic, no nested function scope issues
Sys.setenv(OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
suppressPackageStartupMessages({library(nnet); library(data.table)})

args <- commandArgs(trailingOnly=TRUE)
worker_id <- args[1]
assignment_file <- args[2]
data_file <- args[3]
target_file <- args[4]
output_dir <- args[5]

cat("Worker", worker_id, ": starting\n")

# Load data
trans <- readRDS(data_file)
new_2yr <- readRDS(target_file)
assignments <- fread(assignment_file)
trans[, state_from_f := factor(state_from, levels=1:3)]
trans[, state_to_f := factor(state_to, levels=1:3)]

cat("Worker", worker_id, ": data loaded (", nrow(trans), "records,", uniqueN(trans$ID), "subjects)\n")
cat("Worker", worker_id, ": assigned:", nrow(assignments), "reps\n")

# Results storage
results_all <- list()
n_ok <- 0
n_fail <- 0

for (i in 1:nrow(assignments)) {
  rep_id <- assignments$replicate_id[i]
  seed_val <- assignments$seed[i]

  set.seed(seed_val)
  unique_ids <- unique(trans$ID)
  sampled_ids <- sample(unique_ids, size=length(unique_ids), replace=TRUE)

  # Build bootstrap dataset
  bl <- list()
  for (j in seq_along(sampled_ids)) {
    sub <- trans[ID == sampled_ids[j]]
    sub[, boot_id := paste0(sampled_ids[j], "_", j)]
    bl[[j]] <- sub
  }
  bd <- rbindlist(bl)

  # Fit ONE model for all start states (same as original analysis)
  tryCatch({
    m <- multinom(state_to_f ~ state_from_f * period + interval_years + age_c + female,
                  data=bd, trace=FALSE, maxit=300, MaxNWts=1000)
    pred <- predict(m, newdata=new_2yr, type="probs")

    if (any(is.na(pred)) || any(pred < 0) || any(pred > 1)) {
      n_fail <- n_fail + 1
    } else {
      rep_results <- list()
      for (ss in 1:3) {
        for (pv in 0:1) {
          row_idx <- which(new_2yr$state_from_f == factor(ss, levels=1:3))
          ci <- row_idx[which(new_2yr$period[row_idx] == pv)]
          probs <- pred[ci[1], ]
          rep_results[[length(rep_results)+1]] <- data.table(
            rep=rep_id, start=ss, period=pv,
            p_low=probs[1], p_mid=probs[2], p_high=probs[3])
        }
      }
      results_all[[length(results_all)+1]] <- rbindlist(rep_results)
      n_ok <- n_ok + 1
      rm(m, pred, bd); gc()
    }
  }, error=function(e) { n_fail <<- n_fail + 1 })

  # Checkpoint every 10 reps
  if (i %% 10 == 0 && length(results_all) > 0) {
    saveRDS(rbindlist(results_all),
            file.path(output_dir, paste0("bootstrap_worker_", worker_id, "_checkpoint.rds")))
  }
}

# Save final results
if (length(results_all) > 0) {
  final <- rbindlist(results_all)
  saveRDS(final, file.path(output_dir, paste0("bootstrap_worker_", worker_id, "_results.rds")))
}

# Save failure info
fwrite(data.table(worker=worker_id, n_assigned=nrow(assignments),
                   n_ok=n_ok, n_fail=n_fail),
       file.path(output_dir, paste0("bootstrap_worker_", worker_id, "_failures.csv")))

cat("Worker", worker_id, ": done. OK=", n_ok, "Fail=", n_fail, "\n")
