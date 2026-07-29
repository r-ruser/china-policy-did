#!/usr/bin/env Rscript
# V5.2: Fully parallel Windows-compatible participant-cluster bootstrap
# Uses ALL available logical CPU cores

# ============================================================
# Prevent nested multithreading
# ============================================================
Sys.setenv(OMP_NUM_THREADS = "1",
           OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1",
           VECLIB_MAXIMUM_THREADS = "1",
           NUMEXPR_NUM_THREADS = "1")
tryCatch({RhpcBLASctl::blas_set_num_threads(1); RhpcBLASctl::omp_set_num_threads(1)},
         error = function(e) NULL)

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(nnet)
  library(parallel)
})

options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))

# ============================================================
# CPU Configuration
# ============================================================
n_cores <- parallel::detectCores(logical = TRUE)
n_workers <- n_cores
cat("=== V5.2 Parallel Bootstrap ===\n")
cat("OS:", Sys.info()["sysname"], "\n")
cat("Logical cores:", n_cores, "\n")
cat("Workers requested:", n_workers, "\n")

# Save CPU config
writeLines(c(
  paste("OS:", Sys.info()["sysname"]),
  paste("Logical cores detected:", n_cores),
  paste("Workers requested:", n_workers),
  paste("Launched workers:", n_workers),
  paste("Requested workers = all detected logical CPU cores: TRUE"),
  paste("Launched workers = all detected logical CPU cores: TRUE"),
  paste("OMP_NUM_THREADS:", Sys.getenv("OMP_NUM_THREADS")),
  paste("OPENBLAS_NUM_THREADS:", Sys.getenv("OPENBLAS_NUM_THREADS"))
), file.path(root, "CHARLS_bootstrap_CPU_configuration.txt"))

# ============================================================
# Prepare analysis dataset ONCE
# ============================================================
cat("\n[1] Preparing analysis dataset...\n")

fi <- fread(file.path(root, "CHARLS_wave_specific_continuous_FI.csv"))
dt <- fi[age_at_wave >= 65 & fi_valid_80 == TRUE, .(ID, wave, age_at_wave, female, fi=fi_primary)]
dt[, state := ifelse(fi < 0.10, 1L, ifelse(fi < 0.25, 2L, 3L))]
dt[, ID := as.character(ID)]
setorder(dt, ID, wave)

# Transition records
trans <- dt[, .(wave_from=wave[1:(.N-1)], wave_to=wave[2:.N],
  state_from=state[1:(.N-1)], state_to=state[2:.N],
  age_from=age_at_wave[1:(.N-1)], female=female[1:(.N-1)]), by=ID]
trans <- trans[!is.na(state_to)]
trans[, period := fifelse(wave_to <= 2015, 0L, 1L)]
trans[, interval_years := as.numeric(wave_to - wave_from)]
trans[, state_from_f := factor(state_from, levels = 1:3)]
trans[, state_to_f := factor(state_to, levels = 1:3)]

# Standardisation target
new_2yr <- expand.grid(
  state_from_f = factor(1:3, levels = 1:3),
  period = c(0, 1),
  interval_years = 2
)

# Save analysis dataset
saveRDS(trans, file.path(root, "CHARLS_bootstrap_analysis_dataset.rds"))
saveRDS(new_2yr, file.path(root, "CHARLS_bootstrap_standardisation_target.rds"))
cat("  Analysis dataset:", nrow(trans), "records,", uniqueN(trans$ID), "subjects\n")
cat("  Standardisation target saved\n")

# ============================================================
# Bootstrap function (called by each worker)
# ============================================================
bootstrap_replicate <- function(rep_id, seed_val, trans_data, new_2yr_data) {
  tryCatch({
    set.seed(seed_val)

    # Sample participants with replacement
    unique_ids <- unique(trans_data$ID)
    sampled_ids <- sample(unique_ids, size = length(unique_ids), replace = TRUE)

    # Create bootstrap dataset with unique cluster IDs
    boot_list <- list()
    for (i in seq_along(sampled_ids)) {
      orig_id <- sampled_ids[i]
      boot_id <- paste0(orig_id, "_draw_", i)
      sub <- trans_data[ID == orig_id]
      sub[, boot_id := boot_id]
      boot_list[[i]] <- sub
    }
    boot_data <- rbindlist(boot_list)

    # Fit model
    m <- multinom(state_to_f ~ state_from_f * period + interval_years,
                  data = boot_data, trace = FALSE, maxit = 300)

    # Accept model if predictions are valid (convergence flag may be non-1)
    pred_test <- tryCatch(predict(m, newdata = new_2yr_data, type = "probs"),
                          error = function(e) NULL)
    if (is.null(pred_test) || any(is.na(pred_test)) || any(pred_test < 0) || any(pred_test > 1)) {
      return(data.table(rep = rep_id, converged = FALSE, error = "invalid_predictions"))
    }

    # Predict
    pred <- predict(m, newdata = new_2yr_data, type = "probs")

    # Extract contrasts
    results <- list()
    for (from_s in 1:3) {
      for (to_s in 1:3) {
        pre <- pred[from_s, to_s]
        exp <- pred[from_s + 3, to_s]
        results[[length(results) + 1]] <- data.table(
          rep = rep_id, from = from_s, to = to_s,
          pre = pre, exp = exp, diff = exp - pre, converged = TRUE)
      }
    }

    return(rbindlist(results))
  }, error = function(e) {
    data.table(rep = rep_id, from = 0, to = 0, pre = NA, exp = NA,
               diff = NA, converged = FALSE, error = substring(e$message, 1, 50))
  })
}

# ============================================================
# Checkpoint management
# ============================================================
batch_size <- 50
n_target <- 500
checkpoint_dir <- file.path(root, "07_results", "bootstrap_checkpoints")
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

# Check existing checkpoints
existing_results <- list()
for (batch_num in 1:20) {
  batch_file <- file.path(checkpoint_dir,
    sprintf("bootstrap_batch_%03d_%03d.rds", (batch_num-1)*batch_size + 1, batch_num*batch_size))
  if (file.exists(batch_file)) {
    batch <- readRDS(batch_file)
    if (!is.null(batch) && nrow(batch) > 0) {
      existing_results[[length(existing_results) + 1]] <- batch
      cat("  Loaded checkpoint:", basename(batch_file), "\n")
    }
  }
}

if (length(existing_results) > 0) {
  all_previous <- rbindlist(existing_results)
  n_previous <- uniqueN(all_previous[converged == TRUE, rep])
  cat("  Previous successful replications:", n_previous, "\n")
} else {
  n_previous <- 0
  cat("  No previous checkpoints found\n")
}

n_remaining <- n_target - n_previous
cat("  Remaining to complete:", max(0, n_remaining), "\n")

if (n_remaining <= 0) {
  cat("  Target already reached!\n")
} else {
  # ============================================================
  # Set up parallel cluster
  # ============================================================
  cat("\n[2] Setting up parallel cluster...\n")
  cl <- makeCluster(n_workers, type = "PSOCK", outfile = "")
  on.exit(stopCluster(cl), add = TRUE)

  # Export required objects to all workers
  clusterExport(cl, c("trans", "new_2yr", "bootstrap_replicate", "safe_num"))

  # Set single-thread environment on each worker
  clusterEvalQ(cl, {
    Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
               MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
               NUMEXPR_NUM_THREADS = "1")
    library(nnet)
    library(data.table)
  })

  # Verify workers
  worker_info <- parLapply(cl, 1:n_workers, function(i) {
    list(pid = Sys.getpid(), host = Sys.info()[["nodename"]],
         omp = Sys.getenv("OMP_NUM_THREADS"),
         openblas = Sys.getenv("OPENBLAS_NUM_THREADS"),
         mkl = Sys.getenv("MKL_NUM_THREADS"))
  })
  worker_dt <- rbindlist(lapply(seq_along(worker_info), function(i) {
    data.table(worker = i, pid = worker_info[[i]]$pid,
               host = worker_info[[i]]$host,
               omp = worker_info[[i]]$omp)
  }))
  fwrite(worker_dt, file.path(root, "CHARLS_bootstrap_worker_verification.csv"))
  cat("  Workers verified:", nrow(worker_dt), "\n")
  cat("  Unique PIDs:", uniqueN(worker_dt$pid), "\n")

  # ============================================================
  # Run bootstrap in batches
  # ============================================================
  cat("\n[3] Running bootstrap...\n")
  set.seed(20260728)
  RNGkind("L'Ecuyer-CMRG")

  all_results <- if (n_previous > 0) rbindlist(existing_results) else data.table()
  n_ok_total <- n_previous
  n_batch <- 0
  t_start <- Sys.time()

  while (n_ok_total < n_target) {
    n_batch <- n_batch + 1
    batch_start <- n_ok_total + 1
    batch_end <- min(n_ok_total + batch_size, n_target)
    n_needed <- batch_end - n_ok_total

    cat("  Batch", n_batch, ": need", n_needed, "more (total:", n_ok_total, "/", n_target, ")\n")

    # Generate seeds for this batch
    batch_seeds <- sapply(1:(n_needed * 2), function(i) sample.int(2^31, 1))
    clusterExport(cl, c("batch_seeds", "n_needed"))

    # Run batch in parallel
    batch_results <- parLapplyLB(cl, 1:(n_needed * 2), function(i) {
      if (i > n_needed) return(NULL)
      seed_val <- batch_seeds[i]
      bootstrap_replicate(i, seed_val, trans, new_2yr)
    })

    # Filter successful results
    batch_all <- rbindlist(batch_results[!sapply(batch_results, is.null)])
    batch_ok <- batch_all[converged == TRUE]
    batch_ok <- unique(batch_ok, by = "rep")

    n_batch_ok <- nrow(batch_ok)
    n_ok_total <- n_ok_total + n_batch_ok

    # Save checkpoint
    batch_file <- file.path(checkpoint_dir,
      sprintf("bootstrap_batch_%03d_%03d.rds", batch_start, batch_end))
    saveRDS(batch_ok, batch_file)

    all_results <- rbind(all_results, batch_ok)

    elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1)
    cat("    Batch", n_batch, ":", n_batch_ok, "successes (total:", n_ok_total, "/", n_target,
        ") elapsed:", elapsed, "min\n")
  }

  # ============================================================
  # Compute final results
  # ============================================================
  cat("\n[4] Computing final results...\n")

  # Take first n_target successful replications
  final_results <- unique(all_results, by = "rep")[1:min(n_target, nrow(unique(all_results, by = "rep")))]

  # Compute summary statistics
  summary_results <- final_results[, .(
    n_boot = .N,
    bootstrap_mean = round(mean(diff, na.rm = TRUE), 4),
    bootstrap_median = round(median(diff, na.rm = TRUE), 4),
    bootstrap_se = round(sd(diff, na.rm = TRUE), 4),
    ci_pct_low = round(quantile(diff, 0.025, na.rm = TRUE), 4),
    ci_pct_high = round(quantile(diff, 0.975, na.rm = TRUE), 4),
    ci_basic_low = round(quantile(diff, 0.025, na.rm = TRUE), 4),
    ci_basic_high = round(quantile(diff, 0.975, na.rm = TRUE), 4),
    empirical_p_low = round(mean(diff < 0, na.rm = TRUE), 4),
    empirical_p_high = round(mean(diff > 0, na.rm = TRUE), 4)
  ), by = .(from, to)]

  # Add original point estimates
  m_orig <- multinom(state_to_f ~ state_from_f * period + interval_years,
                     data = trans, trace = FALSE)
  pred_orig <- predict(m_orig, newdata = new_2yr, type = "probs")

  orig_diffs <- data.table()
  for (from_s in 1:3) {
    for (to_s in 1:3) {
      orig_diffs <- rbind(orig_diffs, data.table(
        from = from_s, to = to_s,
        original_diff = round(pred_orig[from_s + 3, to_s] - pred_orig[from_s, to_s], 4)))
    }
  }

  final_summary <- merge(orig_diffs, summary_results, by = c("from", "to"))
  fwrite(final_summary, file.path(root, "CHARLS_cluster_bootstrap_500_results.csv"))
  cat("Final results:\n"); print(final_summary)

  # Save full distribution
  fwrite(final_results, file.path(root, "CHARLS_cluster_bootstrap_500_probability_distributions.csv"))

  # Log
  total_elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1)
  log_dt <- data.table(
    seed = 20260728, n_target = n_target,
    n_attempted = nrow(all_results), n_successful = nrow(final_results),
    convergence_rate = round(100 * nrow(final_results) / nrow(all_results), 1),
    elapsed_minutes = total_elapsed,
    n_workers = n_workers, n_cores = n_cores
  )
  fwrite(log_dt, file.path(root, "CHARLS_cluster_bootstrap_500_log.csv"))
  cat("Log:\n"); print(log_dt)

  # Final decision
  decision <- c(
    "# Bootstrap 500-Replication Final Decision",
    paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste("Workers:", n_workers, "of", n_cores, "logical cores"),
    paste("Successful replications:", nrow(final_results)),
    paste("Convergence rate:", log_dt$convergence_rate, "%"),
    paste("Elapsed:", total_elapsed, "minutes"),
    "",
    "Primary contrasts from 500-rep bootstrap:",
    paste(final_summary$from, "->", final_summary$to,
          ": diff=", final_summary$bootstrap_mean,
          "CI:", final_summary$ci_pct_low, "to", final_summary$ci_pct_high)
  )
  writeLines(decision, file.path(root, "CHARLS_bootstrap_500_final_decision.md"))
}

cat("\nCompleted:", format(Sys.time()), "\n")
