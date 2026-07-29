#!/usr/bin/env Rscript
# Bootstrap worker: runs assigned replications sequentially
# Usage: Rscript 41_bootstrap_worker.R --args <worker_id> <assignment_file> <data_file> <target_file> <output_dir>

# Prevent nested multithreading
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
           NUMEXPR_NUM_THREADS = "1")
tryCatch({RhpcBLASctl::blas_set_num_threads(1); RhpcBLASctl::omp_set_num_threads(1)},
         error = function(e) NULL)

suppressPackageStartupMessages({library(nnet); library(data.table)})

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  cat("Usage: Rscript 41_bootstrap_worker.R <worker_id> <assignment_file> <data_file> <target_file> <output_dir>\n")
  stop("Insufficient arguments")
}

worker_id <- args[1]
assignment_file <- args[2]
data_file <- args[3]
target_file <- args[4]
output_dir <- args[5]

cat("Worker", worker_id, ": starting at", format(Sys.time()), "\n")
cat("Worker", worker_id, ": PID =", Sys.getpid(), "\n")

# Set up logging
log_file <- file.path(output_dir, paste0("bootstrap_worker_", worker_id, ".log"))
log_con <- file(log_file, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({sink(type = "message"); sink(type = "output"); close(log_con)}, add = TRUE)

cat("Worker", worker_id, ": loaded at", format(Sys.time()), "\n")

# Load data
trans <- readRDS(data_file)
new_2yr <- readRDS(target_file)
assignments <- fread(assignment_file)

cat("Worker", worker_id, ": data loaded (", nrow(trans), "records)\n")
cat("Worker", worker_id, ": assigned replications:", nrow(assignments), "\n")

# Fix factor levels
trans[, state_from_f := factor(state_from, levels = 1:3)]
trans[, state_to_f := factor(state_to, levels = 1:3)]

# Bootstrap function
run_bootstrap <- function(rep_id, seed_val) {
  set.seed(seed_val)

  # Sample participants
  unique_ids <- unique(trans$ID)
  sampled_ids <- sample(unique_ids, size = length(unique_ids), replace = TRUE)

  # Create bootstrap dataset
  boot_list <- vector("list", length(sampled_ids))
  for (i in seq_along(sampled_ids)) {
    sub <- trans[ID == sampled_ids[i]]
    sub[, boot_id := paste0(sampled_ids[i], "_", i)]
    boot_list[[i]] <- sub
  }
  boot_data <- rbindlist(boot_list)

  # Fit three start-state models
  results <- list()

  for (start_state in 1:3) {
    sub_data <- boot_data[state_from == start_state]
    if (nrow(sub_data) < 20) {
      results[[start_state]] <- data.table(
        rep = rep_id, start = start_state, converged = FALSE,
        error = "insufficient_data")
      cat("    Start", start_state, ": insufficient data (n=", nrow(sub_data), ")\n")
      next
    }

    cat("    Start", start_state, ": fitting (n=", nrow(sub_data), ")\n")
    tryCatch({
      m <- multinom(state_to_f ~ state_from_f * period + interval_years,
                    data = sub_data, trace = FALSE, maxit = 300)
      cat("    Start", start_state, ": model fitted, convergence=", m$convergence, "\n")

      pred <- predict(m, newdata = new_2yr, type = "probs")
      cat("    Start", start_state, ": predictions computed\n")

      # Validate predictions
      if (any(is.na(pred)) || any(pred < 0) || any(pred > 1)) {
        results[[start_state]] <- data.table(
          rep = rep_id, start = start_state, converged = FALSE,
          error = "invalid_predictions")
        cat("    Start", start_state, ": INVALID PREDICTIONS\n")
        next
      }

      # Extract probabilities for start_state
      row_idx <- which(new_2yr$state_from_f == factor(start_state, levels = 1:3))
      cat("    Start", start_state, ": row_idx=", paste(row_idx, collapse=","), "\n")
      for (period_val in 0:1) {
        col_idx <- row_idx[which(new_2yr$period[row_idx] == period_val)]
        probs <- pred[col_idx[1], ]
        cat("    Start", start_state, "period", period_val, ": probs=", paste(round(probs,4), collapse=","), "\n")
        results[[start_state]] <- rbind(results[[start_state]], data.table(
          rep = rep_id, start = start_state,
          period = period_val,
          p_low = probs[1], p_mid = probs[2], p_high = probs[3],
          converged = TRUE))
      }

      # Clean up
      rm(m, pred, sub_data, boot_data)
      gc()

    }, error = function(e) {
      results[[start_state]] <- data.table(
        rep = rep_id, start = start_state, converged = FALSE,
        error = substring(e$message, 1, 50))
    })
  }

  return(rbindlist(results))
}

# Run assigned replications
results_list <- list()
checkpoint_interval <- 5
n_ok <- 0
n_fail <- 0

for (i in 1:nrow(assignments)) {
  rep_id <- assignments$replicate_id[i]
  seed_val <- assignments$seed[i]

  cat("  Rep", rep_id, ": starting\n")

  result <- tryCatch(
    run_bootstrap(rep_id, seed_val),
    error = function(e) {
      cat("  Rep", rep_id, "ERROR:", e$message, "\n")
      data.table(rep = rep_id, start = 0, converged = FALSE,
                 error = substring(e$message, 1, 50))
    }
  )

  # Check if all three start-state models converged
  if (!is.null(result) && nrow(result) > 0 && "converged" %in% names(result) && all(result$converged == TRUE)) {
    results_list[[length(results_list) + 1]] <- result
    n_ok <- n_ok + 1
    cat("  Rep", rep_id, ": OK\n")
  } else {
    n_fail <- n_fail + 1
    if (is.null(result)) {
      cat("  Rep", rep_id, ": FAILED (result is NULL)\n")
    } else if (nrow(result) == 0) {
      cat("  Rep", rep_id, ": FAILED (empty result)\n")
    } else {
      cat("  Rep", rep_id, ": FAILED (converged=", paste(result$converged, collapse=","), ")\n")
    }
  }

  # Save checkpoint every 5 reps
  if (i %% checkpoint_interval == 0 && length(results_list) > 0) {
    checkpoint <- rbindlist(results_list)
    saveRDS(checkpoint, file.path(output_dir,
      paste0("bootstrap_worker_", worker_id, "_checkpoint.rds")))
  }
}

# Save final results
if (length(results_list) > 0) {
  final_results <- rbindlist(results_list)
  saveRDS(final_results, file.path(output_dir,
    paste0("bootstrap_worker_", worker_id, "_results.rds")))
}

# Save failure log
if (n_fail > 0) {
  failures <- data.table(
    worker = worker_id,
    n_assigned = nrow(assignments),
    n_ok = n_ok,
    n_fail = n_fail
  )
  fwrite(failures, file.path(output_dir,
    paste0("bootstrap_worker_", worker_id, "_failures.csv")))
}

cat("Worker", worker_id, ": completed. Success:", n_ok, "Failed:", n_fail, "\n")
cat("Worker", worker_id, ": finished at", format(Sys.time()), "\n")
