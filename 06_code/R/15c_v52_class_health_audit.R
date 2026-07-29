#!/usr/bin/env Rscript
# ============================================================
# V5.2: CLASS health variable audit across waves 2014-2023
# Produces: CLASS_health_variable_inventory.csv
# ============================================================

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
})

options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("=== V5.2 CLASS Health Variable Audit ===\n")
cat("Started:", format(Sys.time()), "\n\n")

class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"
class_files <- list(
  `2014` = file.path(class_root, "2014class数据_发布版.dta"),
  `2016` = file.path(class_root, "2016class-individual-发布版.dta"),
  `2018` = file.path(class_root, "CLASS2018-cleaned release.dta"),
  `2020` = file.path(class_root, "individual -2020  cleaned for user.dta"),
  `2023` = file.path(class_root, "2023_individual_release_weighted .dta")
)

# Load all waves
alldata <- list()
for (w in names(class_files)) {
  if (!file.exists(class_files[[w]])) { cat("SKIP:", class_files[[w]], "\n"); next }
  cat("Loading CLASS", w, "...\n")
  alldata[[w]] <- read_dta(class_files[[w]])
  cat("  ", nrow(alldata[[w]]), "x", ncol(alldata[[w]]), "\n")
}

# Search for health-related variables across all CLASS waves
# CLASS variable naming differs between waves:
# 2018: lowercase (b1, b5, b8, b9_1__1, e2__1)
# 2014/2016/2020/2023: uppercase or different patterns

# First, do an unrestricted search for ALL columns in each wave
cat("\n=== Variable name patterns by wave ===\n")
for (w in names(alldata)) {
  cols <- names(alldata[[w]])
  cat("\n--- CLASS", w, ": ", length(cols), "vars ---\n")
  # Show first 50 variable names
  cat("First 50:", paste(head(cols, 50), collapse=", "), "\n")
}

# Now search for specific health-related patterns
# We'll search both variable names AND labels
health_search_terms <- c(
  "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11",
  "e2", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11",
  "E2", "hypertension", "diabetes", "heart", "stroke", "lung", "cancer",
  "kidney", "liver", "stomach", "arthritis", "asthma", "sleep", "fall",
  "vision", "hearing", "depress", "smoke", "drink", "pain", "bmi",
  "weight", "height", "grip", "balance", "walk", "climb"
)

cat("\n=== Health-related variables found ===\n")
rows_list <- list()

for (w in names(alldata)) {
  df <- alldata[[w]]
  cols <- names(df)
  for (i in seq_along(cols)) {
    col <- cols[i]
    # Check if column name matches any health pattern
    col_lower <- tolower(col)
    matched <- any(sapply(health_search_terms, function(kw) grepl(kw, col_lower, fixed=TRUE)))
    if (!matched) next

    lab <- attr(df[[col]], "label")
    if (is.null(lab)) lab <- ""
    lab_str <- as.character(lab)

    vals <- tryCatch(na.omit(as.numeric(df[[col]])), error=function(e) numeric(0))
    vu <- sort(unique(vals))
    vs <- paste(head(vu, 10), collapse=", ")

    rows_list[[length(rows_list)+1]] <- data.frame(
      wave=as.integer(w), variable=col, label=lab_str,
      n_nonnull=length(vals), n_total=nrow(df),
      pct_nonnull=round(100*length(vals)/nrow(df),1),
      n_unique=length(vu), values_sample=vs,
      stringsAsFactors=FALSE)
  }
}

mat <- rbindlist(rows_list)
fwrite(mat, file.path(root, "CLASS_health_variable_inventory.csv"))
cat("Saved:", nrow(mat), "rows\n")

# Summary by wave
cat("\n=== Variables per wave ===\n")
print(mat[, .N, by=wave])

# Show unique variable names
cat("\n=== Unique variable names (all waves) ===\n")
all_vars <- sort(unique(mat$variable))
cat(paste(all_vars, collapse="\n"), "\n")
cat("\nTotal unique:", length(all_vars), "\n")

cat("\nCompleted:", format(Sys.time()), "\n")
