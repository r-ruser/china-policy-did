#!/usr/bin/env Rscript
# ============================================================
# V5.2: Comprehensive CFPS health variable audit across all waves
# Produces: CFPS_wave_item_availability_matrix.csv
# ============================================================

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
})

options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("=== V5.2 CFPS Health Variable Audit ===\n")
cat("Started:", format(Sys.time()), "\n\n")

cfps_root <- "E:/公共数据库/中国数据库/CFPS/cfps数据及清洗代码"

# Load all waves into a named list (character keys)
wave_defs <- list(
  list(key = "2010", path = file.path(cfps_root, "10", "cfps2010adult_201906.dta"),
       person = file.path(cfps_root, "10", "10个人.dta")),
  list(key = "2012", path = file.path(cfps_root, "12", "cfps2012adult_201906.dta"),
       person = file.path(cfps_root, "12", "12个人.dta")),
  list(key = "2014", path = file.path(cfps_root, "14", "cfps2014adult_201906.dta"),
       person = NULL),
  list(key = "2018", path = file.path(cfps_root, "18", "cfps2018person_202012.dta"),
       person = NULL),
  list(key = "2020", path = file.path(cfps_root, "20", "cfps2020person_202306.dta"),
       person = NULL)
)

alldata <- list()
for (wd in wave_defs) {
  k <- wd$key
  if (!file.exists(wd$path)) { cat("SKIP:", wd$path, "\n"); next }
  cat("Loading CFPS", k, "...\n")
  alldata[[k]] <- read_dta(wd$path)
  cat("  ", nrow(alldata[[k]]), "x", ncol(alldata[[k]]), "\n")
  if (!is.null(wd$person) && file.exists(wd$person)) {
    df_p <- read_dta(wd$person)
    new_cols <- setdiff(names(df_p), names(alldata[[k]]))
    if (length(new_cols) > 0 && nrow(df_p) == nrow(alldata[[k]])) {
      for (col in new_cols) alldata[[k]][[col]] <- df_p[[col]]
      cat("  +", length(new_cols), "person vars\n")
    }
  }
}

cat("\nLoaded waves:", paste(names(alldata), collapse = ", "), "\n")

# Candidate ND-FI items: list(item_id, domain, concept, named-char-vector of var-per-wave)
candidates <- list(
  list("srh", "Self-rated health", "General self-rated health",
       c("2010"="qp3", "2012"="qp201", "2014"="qp201", "2018"="qph2", "2020"="qp201")),
  list("health_change", "Self-rated health", "Change in health",
       c("2010"="qp301", "2014"="qp202", "2020"="qp202")),
  list("health_peers", "Self-rated health", "Health vs peers",
       c("2010"="qp302")),
  list("chronic_count", "Chronic diseases", "Number of chronic diseases",
       c("2010"="qp8")),
  list("pain_any", "Pain", "Any pain in past month",
       c("2010"="qp4")),
  list("pain_severity", "Pain", "Pain severity",
       c("2010"="qp402")),
  list("bmi", "Body composition", "BMI",
       c("2010"="bmivalue", "2014"="bmivalue", "2018"="bmivalue", "2020"="bmivalue")),
  list("falls", "Falls", "Falls in past period",
       c("2010"="qq2", "2014"="qq201")),
  list("sleep_problems", "Sleep", "Sleep problems",
       c("2010"="qq4", "2014"="qq401")),
  list("sleep_duration", "Sleep", "Sleep duration",
       c("2010"="qq402a", "2014"="qq4010")),
  list("depression_tired", "Depression", "Everything an effort",
       c("2010"="qq605", "2014"="qq605")),
  list("depression_hopeless", "Depression", "Felt hopeful (reverse)",
       c("2010"="qq604", "2014"="qq604")),
  list("depression_fearful", "Depression", "Felt fearful",
       c("2010"="qq606", "2014"="qq606")),
  list("depression_lonely", "Depression", "Felt lonely",
       c("2010"="qq603", "2014"="qq603")),
  list("depression_unhappy", "Depression", "Could not get going",
       c("2010"="qq602", "2014"="qq602")),
  list("depression_bored", "Depression", "Bothered by things",
       c("2010"="qq601", "2014"="qq601")),
  list("touch_toes", "Physical function", "Touch toes",
       c("2010"="qx3", "2018"="qx3")),
  list("raise_arms", "Physical function", "Raise arms above head",
       c("2010"="qx4", "2018"="qx4")),
  list("stand_chair", "Physical function", "Stand from chair",
       c("2010"="qx5", "2018"="qx5")),
  list("balance_eyes_closed", "Physical function", "Balance eyes closed",
       c("2010"="qx6", "2018"="qx6")),
  list("turn_around", "Physical function", "Turn around",
       c("2010"="qx7", "2018"="qx7")),
  list("interviewer_health", "Interviewer assessment", "Rated health",
       c("2010"="qz202", "2014"="qz202")),
  list("interviewer_memory", "Interviewer assessment", "Rated memory",
       c("2010"="qz207")),
  list("interviewer_interest", "Interviewer assessment", "Rated interest",
       c("2010"="qz209")),
  list("ever_smoke", "Health behavior", "Ever smoked",
       c("2018"="eversmoke", "2020"="eversmoke")),
  list("daily_limitations", "Functional limitation", "Daily activity limitations",
       c("2010"="qq5", "2014"="qq501")),
  list("exercise", "Health behavior", "Exercise in past week",
       c("2014"="qq301")),
  list("medical_treatment", "Healthcare use", "Received medical treatment",
       c("2010"="qp5", "2014"="qp401")),
  list("hospitalization", "Healthcare use", "Hospitalized past year",
       c("2010"="qp6"))
)

all_waves <- c("2010", "2012", "2014", "2018", "2020")

# Build matrix
rows_list <- list()
for (item in candidates) {
  iid    <- item[[1]]
  dom    <- item[[2]]
  concep <- item[[3]]
  wv     <- item[[4]]
  for (w in all_waves) {
    if (!(w %in% names(alldata))) {
      rows_list[[length(rows_list)+1]] <- data.frame(
        item_id=iid, domain=dom, concept=concep, wave=as.integer(w),
        cfps_variable="WAVE_NOT_LOADED", label="", n_nonnull=0L,
        n_total=0L, pct_nonnull=0, n_unique=0L, values_sample="",
        available=FALSE, stringsAsFactors=FALSE)
      next
    }
    df <- alldata[[w]]
    vn <- unname(wv[w])
    if (is.na(vn) || !nzchar(vn)) {
      rows_list[[length(rows_list)+1]] <- data.frame(
        item_id=iid, domain=dom, concept=concep, wave=as.integer(w),
        cfps_variable="NOT_IN_WAVE", label="", n_nonnull=0L,
        n_total=nrow(df), pct_nonnull=0, n_unique=0L, values_sample="",
        available=FALSE, stringsAsFactors=FALSE)
      next
    }
    if (!(vn %in% names(df))) {
      rows_list[[length(rows_list)+1]] <- data.frame(
        item_id=iid, domain=dom, concept=concep, wave=as.integer(w),
        cfps_variable=paste0("MISSING:",vn), label="", n_nonnull=0L,
        n_total=nrow(df), pct_nonnull=0, n_unique=0L, values_sample="",
        available=FALSE, stringsAsFactors=FALSE)
      next
    }
    vals <- na.omit(as.numeric(df[[vn]]))
    lab <- attr(df[[vn]], "label")
    if (is.null(lab)) lab <- ""
    vu <- sort(unique(vals))
    vs <- paste(head(vu, 10), collapse=", ")
    rows_list[[length(rows_list)+1]] <- data.frame(
      item_id=iid, domain=dom, concept=concep, wave=as.integer(w),
      cfps_variable=vn, label=as.character(lab),
      n_nonnull=length(vals), n_total=nrow(df),
      pct_nonnull=round(100*length(vals)/nrow(df),1),
      n_unique=length(vu), values_sample=vs,
      available=TRUE, stringsAsFactors=FALSE)
  }
}

mat <- rbindlist(rows_list)
fwrite(mat, file.path(root, "CFPS_wave_item_availability_matrix.csv"))
cat("Saved:", nrow(mat), "rows\n")

# Summary
avail <- mat[available == TRUE]
cat("Available rows:", nrow(avail), "\n\n")

if (nrow(avail) > 0) {
  sm <- avail[, .(
    waves = paste(sort(unique(wave)), collapse=","),
    n_waves = as.integer(uniqueN(wave)),
    min_pct = as.numeric(min(pct_nonnull)),
    max_pct = as.numeric(max(pct_nonnull)),
    sample_vars = paste(unique(cfps_variable), collapse=", ")
  ), by = .(item_id, domain)]
  setorder(sm, -n_waves)
  cat("=== CFPS ND-FI Candidate Items ===\n")
  print(sm, nrows=50, rownames=FALSE)
  cat("\nItems in >=2 waves:", nrow(sm[n_waves>=2]), "\n")
  cat("Items in >=3 waves:", nrow(sm[n_waves>=3]), "\n")
  cat("Domains:", length(unique(sm$domain)), "\n")
  cat("Domain list:", paste(unique(sm$domain), collapse="; "), "\n")
} else {
  cat("ERROR: no items found\n")
}

cat("\nCompleted:", format(Sys.time()), "\n")
