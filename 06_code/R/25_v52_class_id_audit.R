#!/usr/bin/env Rscript
# V5.2: CLASS ID linkage audit for ADL validation
suppressPackageStartupMessages({library(haven); library(data.table)})
options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
safe_num <- function(x) suppressWarnings(as.numeric(x))
cat("=== V5.2 CLASS ID Linkage Audit ===\nStarted:", format(Sys.time()), "\n\n")

class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"

# ============================================================
# 1. Load all CLASS files and extract ID variables
# ============================================================
cat("[1] Loading CLASS files...\n")

# FI analytic data
fi_data <- fread(file.path(root, "CLASS_wave_specific_continuous_FI.csv"))
cat("  FI data: n=", nrow(fi_data), ", unique class_id=", length(unique(fi_data$class_id)), "\n")
cat("  FI class_id class:", class(fi_data$class_id), "\n")
cat("  FI class_id sample:", head(fi_data$class_id, 5), "\n")

# Raw CLASS files
raw_files <- list(
  `2016` = file.path(class_root, "2016class-individual-发布版.dta"),
  `2018` = file.path(class_root, "CLASS2018-cleaned release.dta"),
  `2020` = file.path(class_root, "individual -2020  cleaned for user.dta"),
  `2023` = file.path(class_root, "2023_individual_release_weighted .dta")
)

# ID variable names per wave
id_vars <- list(`2016`="pid", `2018`="rid", `2020`="V1", `2023`="V1")

raw_ids <- list()
for (w in names(raw_files)) {
  cat("\n  Loading CLASS", w, "...\n")
  d <- read_dta(raw_files[[w]])
  id_var <- id_vars[[w]]
  ids <- d[[id_var]]
  cat("    ID var:", id_var, "\n")
  cat("    ID class:", class(ids), "\n")
  cat("    ID type:", typeof(ids), "\n")
  cat("    n rows:", nrow(d), "\n")
  cat("    unique IDs:", length(unique(ids)), "\n")
  cat("    NA IDs:", sum(is.na(ids)), "\n")
  cat("    duplicates:", sum(duplicated(ids)), "\n")
  cat("    sample IDs:", head(ids, 5), "\n")

  # Check for leading zeros or string formatting
  id_str <- as.character(ids)
  cat("    has leading zeros:", any(grepl("^0", id_str[!is.na(ids)])), "\n")
  cat("    has non-numeric chars:", any(grepl("[^0-9.]", id_str[!is.na(ids)])), "\n")
  cat("    nchar range:", range(nchar(id_str[!is.na(ids)]), na.rm=TRUE), "\n")

  raw_ids[[w]] <- data.table(wave = w, raw_id = ids, raw_id_char = id_str)
}

# ============================================================
# 2. Check if FI class_id matches raw IDs
# ============================================================
cat("\n[2] Testing ID linkage...\n")

linkage_results <- list()
for (w in names(raw_files)) {
  raw_dt <- raw_ids[[w]]
  # Get FI IDs for this wave
  fi_ids <- unique(fi_data[wave == as.integer(w)]$class_id)

  # Convert raw IDs to character for comparison
  raw_ids_char <- unique(na.omit(raw_dt$raw_id_char))

  # Test different linkage approaches
  # Approach 1: direct character match
  match_direct <- length(intersect(fi_ids, raw_ids_char))

  # Approach 2: numeric match (if both numeric)
  fi_ids_num <- as.numeric(fi_ids)
  raw_ids_num <- as.numeric(raw_dt$raw_id)
  match_numeric <- length(intersect(na.omit(fi_ids_num), na.omit(raw_ids_num)))

  # Approach 3: trim whitespace
  fi_ids_trim <- trimws(fi_ids)
  raw_ids_trim <- trimws(raw_ids_char)
  match_trim <- length(intersect(fi_ids_trim, raw_ids_trim))

  # Approach 4: strip leading zeros
  fi_ids_nz <- sub("^0+", "", fi_ids_trim)
  raw_ids_nz <- sub("^0+", "", raw_ids_trim)
  match_nz <- length(intersect(fi_ids_nz, raw_ids_nz))

  # Count unmatched
  unmatched_fi <- setdiff(fi_ids, raw_ids_char)
  unmatched_raw <- setdiff(raw_ids_char, fi_ids)

  linkage_results[[w]] <- data.table(
    wave = w,
    n_fi = length(fi_ids),
    n_raw = length(raw_ids_char),
    match_direct = match_direct,
    match_numeric = match_numeric,
    match_trim = match_trim,
    match_nz = match_nz,
    n_unmatched_fi = length(unmatched_fi),
    n_unmatched_raw = length(unmatched_raw),
    fi_sample = paste(head(fi_ids, 3), collapse=","),
    raw_sample = paste(head(raw_ids_char, 3), collapse=",")
  )

  cat("\n  Wave", w, ":\n")
  cat("    FI IDs:", length(fi_ids), "| Raw IDs:", length(raw_ids_char), "\n")
  cat("    Direct match:", match_direct, "\n")
  cat("    Numeric match:", match_numeric, "\n")
  cat("    Trimmed match:", match_trim, "\n")
  cat("    No-leading-zeros match:", match_nz, "\n")
  cat("    Unmatched FI:", length(unmatched_fi), "\n")
  cat("    Unmatched raw:", length(unmatched_raw), "\n")
  cat("    FI sample:", head(fi_ids, 3), "\n")
  cat("    Raw sample:", head(raw_ids_char, 3), "\n")
}

linkage_dt <- rbindlist(linkage_results)
fwrite(linkage_dt, file.path(root, "CLASS_ID_structure_audit.csv"))
cat("\nSaved CLASS_ID_structure_audit.csv\n")

# ============================================================
# 3. Detailed linkage attempts for 2018 (ADL validation wave)
# ============================================================
cat("\n[3] Detailed 2018 linkage analysis...\n")

# Load 2018 raw data with ADL variable
class_18_raw <- read_dta(file.path(class_root, "CLASS2018-cleaned release.dta"))
raw_2018_ids <- as.character(class_18_raw[["rid"]])
fi_2018_ids <- unique(fi_data[wave == 2018]$class_id)

# Create detailed comparison
cat("  2018 raw ID variable: rid\n")
cat("  2018 raw ID class:", class(class_18_raw[["rid"]]), "\n")
cat("  2018 raw ID sample:", head(raw_2018_ids, 10), "\n")
cat("  2018 FI ID sample:", head(fi_2018_ids, 10), "\n")

# Check if the merge key from the FI computation script is different
# The FI script used: class_data[[w]][[id_var]] where id_var was "rid" for 2018
# But the FI data has class_id - let me check what it contains

cat("\n  FI class_id for wave 2018:\n")
cat("  Sample:", head(fi_data[wave == 2018]$class_id, 10), "\n")
cat("  Class:", class(fi_data[wave == 2018]$class_id), "\n")
cat("  nchar sample:", head(nchar(fi_data[wave == 2018]$class_id), 10), "\n")

# Compare with raw rid
cat("\n  Raw rid for wave 2018:\n")
cat("  Sample:", head(raw_2018_ids, 10), "\n")
cat("  Class:", class(class_18_raw[["rid"]]), "\n")

# Check exact match
exact_match <- sum(fi_2018_ids %in% raw_2018_ids)
cat("\n  Exact matches:", exact_match, "out of", length(fi_2018_ids), "FI IDs\n")

# Check if FI IDs are a subset of raw IDs
subset_check <- all(fi_2018_ids %in% raw_2018_ids)
cat("  All FI IDs in raw?", subset_check, "\n")

# Show some unmatched examples
if (exact_match < length(fi_2018_ids)) {
  unmatched <- setdiff(fi_2018_ids, raw_2018_ids)
  cat("  First 10 unmatched FI IDs:", head(unmatched, 10), "\n")

  # Check if these match any raw ID with different formatting
  for (uid in head(unmatched, 5)) {
    # Try as numeric
    uid_num <- as.numeric(uid)
    if (!is.na(uid_num)) {
      matches <- grep(paste0("^", uid_num), raw_2018_ids, value = TRUE)
      cat("    FI ID", uid, "-> numeric", uid_num, "-> matches:", head(matches, 3), "\n")
    }
  }
}

# ============================================================
# 4. Try linkage with ADL variables
# ============================================================
cat("\n[4] Attempting ADL linkage...\n")

adl_2018 <- data.table(
  class_id = as.character(class_18_raw[["rid"]]),
  adl_help_2018 = safe_num(class_18_raw[["b5"]])
)

# Try different merge approaches
approaches <- list(
  direct = function(fi_ids, raw_ids) length(intersect(fi_ids, raw_ids)),
  numeric = function(fi_ids, raw_ids) length(intersect(as.numeric(fi_ids), as.numeric(raw_ids))),
  trim = function(fi_ids, raw_ids) length(intersect(trimws(fi_ids), trimws(raw_ids))),
  no_zeros = function(fi_ids, raw_ids) length(intersect(sub("^0+","",trimws(fi_ids)), sub("^0+","",trimws(raw_ids))))
)

attempt_results <- list()
for (name in names(approaches)) {
  n_match <- approaches[[name]](fi_2018_ids, adl_2018$class_id)
  attempt_results[[name]] <- data.table(
    approach = name,
    n_match = n_match,
    pct_fi_matched = round(100 * n_match / length(fi_2018_ids), 1),
    pct_raw_matched = round(100 * n_match / length(unique(adl_2018$class_id)), 1)
  )
  cat("  ", name, ":", n_match, "matches\n")
}

attempt_dt <- rbindlist(attempt_results)
fwrite(attempt_dt, file.path(root, "CLASS_FI_ADL_linkage_attempts.csv"))

# Show unmatched examples
unmatched_fi <- setdiff(fi_2018_ids, adl_2018$class_id)
unmatched_raw <- setdiff(adl_2018$class_id, fi_2018_ids)
examples <- data.table(
  type = c(rep("unmatched_fi", min(10, length(unmatched_fi))),
           rep("unmatched_raw", min(10, length(unmatched_raw)))),
  id = c(head(unmatched_fi, 10), head(unmatched_raw, 10))
)
fwrite(examples, file.path(root, "CLASS_unmatched_ID_examples.csv"))

# ============================================================
# 5. Write decision document
# ============================================================
best_approach <- attempt_dt[which.max(n_match)]
decision <- paste0(
"# CLASS ID Linkage Decision - V5.2\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## Audit Results\n\n",
"### FI analytic data\n",
"- class_id variable type:", class(fi_data$class_id), "\n",
"- n unique FI IDs (all waves):", length(unique(fi_data$class_id)), "\n",
"- n unique FI IDs (2018):", length(fi_2018_ids), "\n",
"- FI class_id sample:", paste(head(fi_2018_ids, 5), collapse=", "), "\n\n",
"### Raw CLASS 2018 data\n",
"- ID variable: rid\n",
"- rid variable type:", class(class_18_raw[["rid"]]), "\n",
"- n unique raw IDs:", length(unique(raw_2018_ids)), "\n",
"- rid sample:", paste(head(raw_2018_ids, 5), collapse=", "), "\n\n",
"### Linkage attempts\n")
for (i in 1:nrow(attempt_dt)) {
  decision <- paste0(decision, "- ", attempt_dt$approach[i], ": ",
                     attempt_dt$n_match[i], " matches (",
                     attempt_dt$pct_fi_matched[i], "% of FI)\n")
}
decision <- paste0(decision,
"\n## Decision\n\n")
if (best_approach$n_match > 0) {
  decision <- paste0(decision,
    "The best approach is: ", best_approach$approach, " with ",
    best_approach$n_match, " matches.\n\n",
    "If this provides adequate linkage for ADL validation, proceed with that approach.\n",
    "If not, document the linkage limitation and consider alternative validation strategies.\n")
} else {
  decision <- paste0(decision,
    "No reliable deterministic linkage was achieved.\n",
    "The FI data and raw CLASS ADL data cannot be linked reliably using ID variables alone.\n\n",
    "Options:\n",
    "1. Attempt probabilistic linkage using additional variables (age, sex, birth year)\n",
    "2. Re-compute FI directly from raw CLASS data with ADL variables included\n",
    "3. Report CLASS ADL validation as not feasible with current data\n")
}
writeLines(decision, file.path(root, "CLASS_ID_linkage_decision.md"))
cat("\nSaved CLASS_ID_linkage_decision.md\n")

cat("\nCompleted:", format(Sys.time()), "\n")
