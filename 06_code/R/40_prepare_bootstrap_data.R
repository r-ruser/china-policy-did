#!/usr/bin/env Rscript
# Step 1: Prepare compact bootstrap analysis data and seed list
cat("=== Preparing Bootstrap Data ===\n")

suppressPackageStartupMessages({library(haven); library(data.table)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))

# Load and prepare analysis data
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
trans[, age_c := (age_from - 70) / 5]
trans[, state_from_f := factor(state_from, levels = 1:3)]
trans[, state_to_f := factor(state_to, levels = 1:3)]

# Save analysis data
saveRDS(trans, file.path(root, "CHARLS_bootstrap_analysis_dataset.rds"))
cat("Analysis dataset:", nrow(trans), "records,", uniqueN(trans$ID), "subjects\n")

# Standardisation target
new_2yr <- expand.grid(
  state_from_f = factor(1:3, levels = 1:3),
  period = c(0L, 1L),
  interval_years = 2,
  age_c = 0,
  female = 0.5
)
saveRDS(new_2yr, file.path(root, "CHARLS_bootstrap_standardisation_target.rds"))

# Generate seed list
set.seed(20260728)
RNGkind("L'Ecuyer-CMRG")
seeds <- sample.int(2^31 - 1, size = 500, replace = FALSE)
seed_dt <- data.table(replicate_id = 1:500, seed = seeds)
fwrite(seed_dt, file.path(root, "CHARLS_bootstrap_500_seed_list.csv"))

# CPU configuration
n_cores <- parallel::detectCores(logical = TRUE)
n_physical <- parallel::detectCores(logical = FALSE)
cat("Logical cores:", n_cores, "\n")
cat("Physical cores:", n_physical, "\n")
cat("Seeds generated: 500\n")

# Create checkpoint directory
dir.create(file.path(root, "07_results", "bootstrap_checkpoints"), showWarnings = FALSE, recursive = TRUE)

cat("Bootstrap data preparation complete.\n")
cat("Files created:\n")
cat("  CHARLS_bootstrap_analysis_dataset.rds\n")
cat("  CHARLS_bootstrap_standardisation_target.rds\n")
cat("  CHARLS_bootstrap_500_seed_list.csv\n")
