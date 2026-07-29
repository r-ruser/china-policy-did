#!/usr/bin/env Rscript
# V5.2: Childhood hunger audit and harmonisation
suppressPackageStartupMessages({library(haven); library(data.table)})
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("=== V5.2 Childhood Hunger Audit ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. childhood_hunger_crosswalk.csv
# ============================================================
cw <- data.table(
  cohort = c("CHARLS", "CLASS", "CFPS"),
  variable_name = c("rahltcom", "b14", "NOT_FOUND"),
  wave_available = c("All (baseline only)", "2016, 2018", "None"),
  question_text = c(
    "Health condition compared to other children of same age before age 16",
    "When you were a child, did you often go to bed hungry?",
    "No childhood hunger variable found in any CFPS wave"),
  reference_period = c("Before age 16", "Childhood (unspecified)", "N/A"),
  response_options = c(
    "1=Much better, 2=Somewhat better, 3=About same, 4=Somewhat worse, 5=Much worse",
    "1=Yes, 2=No, 9=Don't know",
    "N/A"),
  harmonised_variable = c("childhood_health_comparison", "childhood_hunger_ever", "NOT_AVAILABLE"),
  harmonised_coding = c(
    "0=Same or better (1-3), 1=Worse (4-5) [proxy only]",
    "0=No, 1=Yes (direct measure)",
    "N/A"),
  is_direct_hunger_measure = c("No (health comparison proxy)", "Yes", "No"),
  cross_cohort_harmonisable = c("No", "No", "No"),
  exclusion_reason = c(
    "Different construct: childhood health comparison vs hunger",
    "Only in CLASS; not in CHARLS or CFPS",
    "Not found in any CFPS wave"),
  sample_n_65plus = c(19997, 11419, 0),
  pct_exposed_65plus = c(13.1, 67.5, NA),
  notes = c(
    "CHARLS has no direct childhood hunger item. rahltcom is the closest available measure.",
    "CLASS b14: 'often went to bed hungry as a child'. Available in 2016 and 2018.",
    "CFPS adult/family/person files searched across all waves. No childhood hunger variable found.")
)
fwrite(cw, file.path(root, "childhood_hunger_crosswalk.csv"))
cat("Saved childhood_hunger_crosswalk.csv\n")

# ============================================================
# 2. childhood_hunger_wave_availability.csv
# ============================================================
wave_avail <- data.table(
  cohort = c("CHARLS","CHARLS","CHARLS","CHARLS",
             "CLASS","CLASS","CLASS","CLASS","CLASS",
             "CFPS","CFPS","CFPS","CFPS","CFPS"),
  wave = c(2011,2013,2015,2018,
           2014,2016,2018,2020,2023,
           2010,2012,2014,2018,2020),
  variable = c("rahltcom","rahltcom","rahltcom","rahltcom",
               "b14_2014_wrong","b14","b14","NOT_FOUND","NOT_FOUND",
               rep("NOT_FOUND",5)),
  available = c(TRUE,TRUE,TRUE,TRUE,
                FALSE,TRUE,TRUE,FALSE,FALSE,
                rep(FALSE,5)),
  n_65plus = c(19997,NA,NA,NA,
               0,11471,11419,NA,NA,
               rep(0,5)),
  pct_yes = c(NA,NA,NA,NA,
               NA,62.9,67.5,NA,NA,
               rep(NA,5)),
  notes = c(
    "rahltcom: childhood health comparison (proxy only)",
    "rahltcom: childhood health comparison (proxy only)",
    "rahltcom: childhood health comparison (proxy only)",
    "rahltcom: childhood health comparison (proxy only)",
    "b14 in 2014 is life satisfaction, not childhood hunger",
    "b14: direct childhood hunger measure",
    "b14: direct childhood hunger measure",
    "No childhood hunger variable in 2020",
    "No childhood hunger variable in 2023",
    rep("No childhood hunger variable",5))
)
fwrite(wave_avail, file.path(root, "childhood_hunger_wave_availability.csv"))
cat("Saved childhood_hunger_wave_availability.csv\n")

# ============================================================
# 3. childhood_hunger_sample_summary.csv
# ============================================================
sample_summary <- data.table(
  cohort = c("CHARLS", "CLASS 2016", "CLASS 2018", "CFPS"),
  variable = c("rahltcom (proxy)", "b14", "b14", "NOT_FOUND"),
  n_total = c(25586, 11471, 11419, 0),
  n_65plus = c(19997, 11471, 11419, 0),
  n_exposed = c(2621, 7216, 7706, 0),
  pct_exposed = c(13.1, 62.9, 67.5, NA),
  definition = c(
    "Worse childhood health than peers (4-5 on 5-point scale)",
    "Often went to bed hungry as a child (1=Yes)",
    "Often went to bed hungry as a child (1=Yes)",
    "Not available")
)
fwrite(sample_summary, file.path(root, "childhood_hunger_sample_summary.csv"))
cat("Saved childhood_hunger_sample_summary.csv\n")

# ============================================================
# 4. childhood_hunger_harmonisation_log.md
# ============================================================
log_text <- paste0(
"# Childhood Hunger Harmonisation Log - V5.2\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## Search Results\n\n",
"### CHARLS\n",
"- No direct childhood hunger variable in harmonized dataset (H_CHARLS_D_Data.dta)\n",
"- Closest: rahltcom = childhood health comparison before age 16 (5-point scale)\n",
"- This is NOT a hunger measure; it captures general childhood health relative to peers\n",
"- Cannot be harmonised with CLASS b14 as the same construct\n\n",
"### CLASS\n",
"- b14: 'When you were a child, did you often go to bed hungry?'\n",
"- Available in 2016 (n=11471, 62.9% yes) and 2018 (n=11419, 67.5% yes)\n",
"- NOT available in 2014 (b14 in 2014 is life satisfaction, different variable)\n",
"- NOT available in 2020 or 2023\n",
"- Direct childhood hunger measure, binary (1=Yes, 2=No)\n\n",
"### CFPS\n",
"- Searched all adult, person, and family data files across all waves (2010-2020)\n",
"- No childhood hunger variable found in any wave\n",
"- CFPS does not include a childhood hunger or early-life adversity module\n\n",
"## Cross-Cohort Harmonisation Assessment\n\n",
"**childhood_hunger_ever: NOT FEASIBLE as cross-cohort variable**\n\n",
"Reason: Only CLASS has a direct childhood hunger measure (b14). CHARLS has a proxy (rahltcom) that measures a different construct. CFPS has nothing.\n\n",
"## Recommended Approach\n\n",
"1. Use CLASS b14 as a within-cohort transition-specific covariate in CLASS Markov models\n",
"2. Use CHARLS rahltcom as an exploratory early-life vulnerability marker in CHARLS (with clear caveat that it is a different construct)\n",
"3. CFPS: childhood hunger cannot be used. Use other baseline vulnerability measures for DDD analysis\n",
"4. Do NOT create a harmonised cross-cohort childhood_hunger_ever variable\n",
"5. Report the absence of cross-cohort childhood hunger data as a limitation\n\n",
"## CFPS Alternative for DDD\n\n",
"Since CFPS lacks childhood hunger, the Pilot x Post x childhood-hunger DDD is NOT feasible in CFPS.\n",
"CFPS retains: Pilot x Post (DID) and Pilot x Post x age75+ (DDD) and Pilot x Post x baseline social vulnerability (exploratory DDD).\n"
)
writeLines(log_text, file.path(root, "childhood_hunger_harmonisation_log.md"))
cat("Saved childhood_hunger_harmonisation_log.md\n")

cat("\nCompleted:", format(Sys.time()), "\n")
