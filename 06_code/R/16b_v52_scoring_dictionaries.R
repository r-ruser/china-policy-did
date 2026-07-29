#!/usr/bin/env Rscript
# V5.2: Generate scoring dictionaries and domain contribution analysis
suppressPackageStartupMessages({library(data.table)})
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# 1. harmonised_ndfi_scoring_dictionary.csv
scoring <- data.table(
  item_id = c(
    rep(c("hypertension","heart_disease","stroke","lung_disease","diabetes",
          "cancer","arthritis","kidney_disease"), 2),
    "srh", "depression_score", "orientation", "imrc", "ser7"),
  cohort = c(
    rep("CHARLS", 8), rep("CLASS", 8),
    "Both", "Both", "CHARLS", "CHARLS", "CHARLS"),
  charls_variable = c(
    paste0("r{W}", c("hibpe","hearte","stroke","lunge","diabe","cancre","arthre","kidneye")),
    rep(NA, 8),
    "r{W}shlta", "r{W}cesd10", "r{W}orient", "r{W}imrc", "r{W}ser7"),
  class_variable = c(
    rep(NA, 8),
    paste0("b9_1__", c(1,2,3,4,5,7,9,11)),
    "b1/B1", "e2__1-e2__9 derived", NA, NA, NA),
  deficit_coding = c(
    rep("0=No, 1=Yes (cumulative, carry-forward)", 8),
    "Recoded: 1-2 to 0, 3 to 0.5, 4-5 to 1",
    "Standardised to 0-1 within cohort",
    "Reversed: (4-score)/4",
    "Reversed: (10-score)/10",
    "Reversed: (5-score)/5"),
  domain = c(
    rep("Chronic diseases", 8),
    "Self-rated health", "Depression", "Cognition", "Cognition", "Cognition"),
  carry_forward = c(
    rep("Yes", 8),
    "No", "No", "No", "No", "No")
)
fwrite(scoring, file.path(root, "harmonised_ndfi_scoring_dictionary.csv"))
cat("Saved harmonised_ndfi_scoring_dictionary.csv:", nrow(scoring), "rows\n")

# 2. harmonised_social_frailty_scoring_dictionary.csv
social_scoring <- data.table(
  item_id = c("living_alone","no_spouse","social_activity","loneliness",
              "life_satisfaction","childhood_hunger"),
  domain = "Social frailty",
  concept = c("Living alone","No spouse/partner","Insufficient social activity",
              "Feeling lonely","Low life satisfaction","Childhood hunger"),
  charls_variable = c("Derived from household","Derived from marital",
                       "Not in harmonized","Not in harmonized","Not in harmonized","Not in harmonized"),
  class_variable = c("Derived from a8__1","q12/A7",
                      "Not directly comparable","Not directly comparable","b14/b17","b14"),
  deficit_coding = c("0=Not alone, 1=Alone","0=Married, 1=No spouse",
                     "0=Active, 1=Inactive","0=No, 1=Yes",
                     "0=Satisfied, 1=Dissatisfied","0=No, 1=Yes"),
  enter_trajectory = c("Yes","Yes","Yes","Yes","No","No")
)
fwrite(social_scoring, file.path(root, "harmonised_social_frailty_scoring_dictionary.csv"))
cat("Saved harmonised_social_frailty_scoring_dictionary.csv:", nrow(social_scoring), "rows\n")

# 3. domain_contribution_to_ndfi.csv
domain_contrib <- data.table(
  domain = c("Chronic diseases","Self-rated health","Depression","Cognition"),
  n_items = c(8, 1, 1, 3),
  pct_of_denominator = c(8/12*100, 1/12*100, 1/12*100, 3/12*100),
  concern = c(
    "HIGH: 8/12 = 67%. Single domain dominates.",
    "LOW: 1/12 = 8%",
    "LOW: 1/12 = 8%",
    "MODERATE: 3/12 = 25% but CHARLS only")
)
fwrite(domain_contrib, file.path(root, "domain_contribution_to_ndfi.csv"))
cat("Saved domain_contribution_to_ndfi.csv\n")
cat("All Phase 2 output files generated.\n")
