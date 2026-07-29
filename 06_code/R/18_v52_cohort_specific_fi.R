#!/usr/bin/env Rscript
# V5.2: Construct cohort-specific non-disability Frailty Indices
# CHARLS: ≥20 items, ≥5 domains, waves 1-4
# CLASS: ≥20 items, ≥5 domains, waves 2016-2023
suppressPackageStartupMessages({library(haven); library(data.table)})
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("=== V5.2 Cohort-Specific FI Construction ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# CHARLS FI
# ============================================================
cat("[1] CHARLS FI construction\n")
charls_path <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
charls <- read_dta(charls_path)
cat("  Loaded:", nrow(charls), "x", ncol(charls), "\n")

# Define CHARLS FI items: item_id, domain, variable per wave, coding
charls_fi_items <- data.table(
  # Chronic diseases (9 items)
  item_id = c(rep("hypertension",4), rep("heart_disease",4), rep("stroke",4),
              rep("lung_disease",4), rep("diabetes",4), rep("cancer",4),
              rep("arthritis",4), rep("kidney_disease",4), rep("liver_disease",4),
              # SRH
              rep("srh",4),
              # Depression
              rep("depression",4),
              # Cognition
              rep("orientation",4), rep("imrc",4), rep("ser7",4),
              # Physical function
              rep("stooping",4), rep("walk_1km",4), rep("lift_carry",4),
              rep("stand_chair",4), rep("climb_stairs",4), rep("incontinence",4),
              # Psychiatric
              rep("psychiatric",4), rep("memory_problem",4),
              # Health behavior
              rep("current_smoke",4)),
  wave = rep(rep(1:4, each=1), 22),
  wave_year = rep(rep(c(2011,2013,2015,2018), each=1), 22),
  domain = c(rep("Chronic diseases", 9*4), "Self-rated health", "Depression",
             rep("Cognition", 3*4), rep("Physical function", 6*4),
             "Psychiatric", "Health behavior"),
  variable = c(
    # Chronic diseases
    paste0("r",1:4,"hibpe"), paste0("r",1:4,"hearte"), paste0("r",1:4,"stroke"),
    paste0("r",1:4,"lunge"), paste0("r",1:4,"diabe"), paste0("r",1:4,"cancre"),
    paste0("r",1:4,"arthre"), paste0("r",1:4,"kidneye"), paste0("r",1:4,"livere"),
    # SRH
    paste0("r",1:4,"shlta"),
    # Depression
    paste0("r",1:4,"cesd10"),
    # Cognition
    paste0("r",1:4,"orient"), paste0("r",1:4,"imrc"), paste0("r",1:4,"ser7"),
    # Physical function
    paste0("r",1:4,"stoopa"), paste0("r",1:4,"walk1kma"), paste0("r",1:4,"lifta"),
    paste0("r",1:4,"chaira"), paste0("r",1:4,"climsa"), paste0("r",1:4,"urina"),
    # Psychiatric
    paste0("r",1:4,"psyche"), paste0("r",1:4,"memrye"),
    # Current smoking
    paste0("r",1:4,"smoken")),
  coding = c(
    rep("binary_01", 9*4),
    "ordinal_5", "continuous_030",
    "ordinal_5_rev", "ordinal_10_rev", "ordinal_6_rev",
    rep("binary_01", 6*4),
    "binary_01", "binary_01",
    "binary_01")
)

# Verify all variables exist
cat("  Checking variable availability...\n")
missing_vars <- character()
for (i in 1:nrow(charls_fi_items)) {
  v <- charls_fi_items$variable[i]
  if (!(v %in% names(charls))) {
    missing_vars <- c(missing_vars, paste0(charls_fi_items$item_id[i], ":", v))
  }
}
if (length(missing_vars) > 0) {
  cat("  WARNING: Missing variables:", paste(missing_vars, collapse=", "), "\n")
} else {
  cat("  All", nrow(charls_fi_items), "variable slots verified\n")
}

# Count unique items and domains
n_charls_items <- length(unique(charls_fi_items$item_id))
n_charls_domains <- length(unique(charls_fi_items$domain[!charls_fi_items$domain %in% c("Psychiatric","Health behavior")]))
cat("  CHARLS FI items:", n_charls_items, "\n")
cat("  CHARLS FI domains:", n_charls_domains, "-",
    paste(unique(charls_fi_items$domain), collapse=", "), "\n")

# Save CHARLS FI item definition
charls_item_def <- charls_fi_items[, .(item_id, domain, variable, coding)]
charls_item_def <- unique(charls_item_def[, .(item_id, domain, coding)])
fwrite(charls_item_def, file.path(root, "CHARLS_frailty_state_definition.csv"))
cat("  Saved CHARLS_frailty_state_definition.csv\n")

# ============================================================
# CLASS FI
# ============================================================
cat("\n[2] CLASS FI construction\n")
class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"

# Load CLASS 2018 (most complete wave) to verify variables
class18 <- read_dta(file.path(class_root, "CLASS2018-cleaned release.dta"))
cat("  CLASS 2018:", nrow(class18), "x", ncol(class18), "\n")

# CLASS FI items: domain-level harmonisation
# Chronic diseases: 10 items (matching CHARLS 9 + stomach disease)
class_fi_domains <- data.table(
  domain = c(rep("Chronic diseases", 11),
             "Self-rated health",
             "Physical function", "Physical function", "Physical function",
             "Physical function", "Physical function",
             "Incontinence", "Incontinence",
             "Depression"),
  item_id = c(
    "hypertension","heart_disease","stroke","lung_disease","diabetes",
    "cancer","arthritis","kidney_disease","liver_disease","stomach_disease",
    "osteoporosis",
    "srh",
    "climb_stairs","walk_outside","lift_heavy","fall_12m","carry_10jin",
    "urinary_incontinence","fecal_incontinence",
    "depression_score"),
  n_waves = 5,
  waves = "2016,2018,2020,2023 (and 2014 for some)",
  class_var_pattern = c(
    "b11_1_1/B9_1_1", "b11_1_2/B9_1_2", "b11_1_4/B9_1_4", "b11_1_19/B9_1_19",
    "b11_1_3/B9_1_3", "b11_1_16/B9_1_16", "b11_1_10/B9_1_10",
    "b11_1_5/B9_1_5", "b11_1_6/B9_1_6", "b11_1_21/B9_1_21",
    "b11_1_18/B9_1_18",
    "b1/B1",
    "b6_1/B6_1", "b6_3/B6_3", "b6_7/B6_7", "b6_2/B6_2", "b6_7/B6_7",
    "b4_7/B4_7", "b4_8/B4_8",
    "e2__1-e2__9/E2_1-E2_9"),
  coding = c(
    rep("binary_recode", 11),
    "ordinal_5",
    "ordinal_3", "ordinal_3", "binary_01", "ordinal_3", "binary_01",
    "ordinal_3", "ordinal_3",
    "composite_01")
)

cat("  CLASS FI domains:", length(unique(class_fi_domains$domain)), "\n")
cat("  CLASS FI items:", nrow(class_fi_domains), "\n")
cat("  Domain list:", paste(unique(class_fi_domains$domain), collapse=", "), "\n")

# Save CLASS FI definition
fwrite(class_fi_domains, file.path(root, "CLASS_frailty_state_definition.csv"))
cat("  Saved CLASS_frailty_state_definition.csv\n")

# ============================================================
# Summary
# ============================================================
cat("\n=== Cohort-Specific FI Summary ===\n")
cat("CHARLS FI:", n_charls_items, "items,", n_charls_domains, "+ other domains\n")
cat("CLASS FI:", nrow(class_fi_domains), "items,", length(unique(class_fi_domains$domain)), "domains\n")

if (n_charls_items >= 20 && n_charls_domains >= 5) {
  cat("CHARLS: MEETS ≥20 items, ≥5 domains threshold\n")
} else {
  cat("CHARLS: BELOW threshold\n")
}

class_health_domains <- length(unique(class_fi_domains$domain[class_fi_domains$domain != "Incontinence"]))
if (nrow(class_fi_domains) >= 20 && class_health_domains >= 5) {
  cat("CLASS: MEETS ≥20 items, ≥5 domains threshold\n")
} else {
  cat("CLASS: BELOW threshold\n")
}

cat("\nCompleted:", format(Sys.time()), "\n")
