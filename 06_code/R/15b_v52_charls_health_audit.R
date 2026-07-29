#!/usr/bin/env Rscript
# ============================================================
# V5.2: CHARLS health variable audit across waves 1-4
# Produces: CHARLS_health_variable_inventory.csv
# ============================================================

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
})

options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("=== V5.2 CHARLS Health Variable Audit ===\n")
cat("Started:", format(Sys.time()), "\n\n")

charls_path <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
cat("Loading CHARLS harmonized data...\n")
d <- read_dta(charls_path)
cat("Shape:", nrow(d), "x", ncol(d), "\n")

# Wave mapping: wave 1=2011, 2=2013, 3=2015, 4=2018
waves <- c(`1`=2011, `2`=2013, `3`=2015, `4`=2018)

# Candidate ND-FI items: search for matching variable patterns
# Each entry: item_id, domain, concept, list of wave-prefixed variable patterns
# Pattern: r{W}{suffix} where W is wave number 1-4

candidate_items <- list(
  # Chronic diseases
  list("hypertension", "Chronic diseases", "Ever had high blood pressure",
       paste0("r", 1:4, "hibpe")),
  list("heart_disease", "Chronic diseases", "Ever had heart problem",
       paste0("r", 1:4, "hearte")),
  list("stroke", "Chronic diseases", "Ever had stroke",
       paste0("r", 1:4, "stroke")),
  list("lung_disease", "Chronic diseases", "Ever had lung disease",
       paste0("r", 1:4, "lunge")),
  list("diabetes", "Chronic diseases", "Ever had diabetes",
       paste0("r", 1:4, "diabe")),
  list("cancer", "Chronic diseases", "Ever had cancer",
       paste0("r", 1:4, "cancre")),
  list("arthritis", "Chronic diseases", "Ever had arthritis",
       paste0("r", 1:4, "arthre")),
  list("kidney_disease", "Chronic diseases", "Ever had kidney disease",
       paste0("r", 1:4, "kidneye")),
  list("liver_disease", "Chronic diseases", "Ever had liver disease",
       paste0("r", 1:4, "liver")),
  list("stomach_disease", "Chronic diseases", "Ever had stomach disease",
       paste0("r", 1:4, "stomach")),

  # Self-rated health
  list("srh", "Self-rated health", "Self-rated health (1-5)",
       paste0("r", 1:4, "shlta")),

  # Depression (CESD-10 total score)
  list("cesd10_score", "Depression", "CESD-10 total score",
       paste0("r", 1:4, "cesd10")),

  # Cognition
  list("orientation", "Cognition", "Orientation score",
       paste0("r", 1:4, "orient")),
  list("immediate_recall", "Cognition", "Immediate word recall",
       paste0("r", 1:4, "imrc")),
  list("serial7", "Cognition", "Serial-7 subtraction",
       paste0("r", 1:4, "ser7")),

  # ADL (EXCLUDED from ND-FI but noted)
  list("adl_any", "ADL (EXCLUDED)", "Any ADL difficulty",
       paste0("r", 1:4, "adla_c")),

  # Smoking
  list("ever_smoke", "Health behavior", "Ever smoked",
       paste0("r", 1:4, "smokev")),
  list("current_smoke", "Health behavior", "Currently smoking",
       paste0("r", 1:4, "smoken")),

  # Drinking
  list("ever_drink", "Health behavior", "Ever drank alcohol",
       paste0("r", 1:4, "drinkev")),

  # Sleep
  list("sleep_duration", "Sleep", "Sleep duration",
       paste0("r", 1:4, "sleepp")),
  list("sleep_quality", "Sleep", "Sleep quality",
       paste0("r", 1:4, "sleepq")),

  # Pain
  list("pain", "Pain", "Body pain",
       paste0("r", 1:4, "pain")),

  # Vision
  list("vision", "Sensory", "Vision problem",
       paste0("r", 1:4, "eyes")),

  # Hearing
  list("hearing", "Sensory", "Hearing problem",
       paste0("r", 1:4, "ears")),

  # Falls
  list("falls", "Falls", "Falls in past period",
       paste0("r", 1:4, "fall")),

  # BMI
  list("bmi", "Body composition", "BMI",
       paste0("r", 1:4, "bmi")),

  # Weight
  list("weight", "Body composition", "Body weight",
       paste0("r", 1:4, "weight")),

  # Grip strength
  list("grip_strength", "Physical function", "Grip strength (kg)",
       paste0("r", 1:4, "gripsum")),

  # Lung function
  list("peak_flow", "Physical function", "Peak expiratory flow",
       paste0("r", 1:4, "peak")),

  # Social participation
  list("social_activity", "Social", "Social activity participation",
       paste0("r", 1:4, "social")),

  # Loneliness
  list("loneliness", "Social", "Loneliness",
       paste0("r", 1:4, "loneliness"))
)

# Build inventory
rows_list <- list()
for (item in candidate_items) {
  iid <- item[[1]]
  dom <- item[[2]]
  concep <- item[[3]]
  var_patterns <- item[[4]]

  for (wnum in 1:4) {
    wave_year <- unname(waves[as.character(wnum)])
    pattern <- var_patterns[wnum]
    # Find matching columns
    matching <- grep(pattern, names(d), value = TRUE)
    if (length(matching) == 0) {
      rows_list[[length(rows_list)+1]] <- data.frame(
        item_id=iid, domain=dom, concept=concep, wave=wave_year,
        charls_variable="NOT_FOUND", label="", n_nonnull=0L,
        n_total=nrow(d), pct_nonnull=0, n_unique=0L, values_sample="",
        available=FALSE, stringsAsFactors=FALSE)
      next
    }
    for (vn in matching) {
      vals <- na.omit(as.numeric(d[[vn]]))
      lab <- attr(d[[vn]], "label")
      if (is.null(lab)) lab <- ""
      vu <- sort(unique(vals))
      vs <- paste(head(vu, 10), collapse=", ")
      rows_list[[length(rows_list)+1]] <- data.frame(
        item_id=iid, domain=dom, concept=concep, wave=wave_year,
        charls_variable=vn, label=as.character(lab),
        n_nonnull=length(vals), n_total=nrow(d),
        pct_nonnull=round(100*length(vals)/nrow(d),1),
        n_unique=length(vu), values_sample=vs,
        available=TRUE, stringsAsFactors=FALSE)
    }
  }
}

mat <- rbindlist(rows_list)
fwrite(mat, file.path(root, "CHARLS_health_variable_inventory.csv"))
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
    variables = paste(unique(charls_variable), collapse=", ")
  ), by = .(item_id, domain)]
  setorder(sm, -n_waves)
  cat("=== CHARLS ND-FI Candidate Items ===\n")
  print(sm, nrows=50, rownames=FALSE)
  cat("\nItems in >=3 waves:", nrow(sm[n_waves>=3]), "\n")
  cat("Domains:", length(unique(sm$domain)), "\n")
  cat("Domain list:", paste(unique(sm$domain), collapse="; "), "\n")
} else {
  cat("ERROR: no items found\n")
}

cat("\nCompleted:", format(Sys.time()), "\n")
