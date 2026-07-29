#!/usr/bin/env Rscript
# ============================================================
# V5.2 Phase 2 Step 2.1-2.3: Item adjudication and set creation
# Produces: all required CSV files for revised ND-FI architecture
# ============================================================

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
})

options(encoding = "UTF-8")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
cat("=== V5.2 Phase 2: Item Adjudication ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. Read CHARLS data and verify wave-level availability
# ============================================================
cat("[1] Reading CHARLS harmonized data...\n")
charls_path <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
charls <- read_dta(charls_path)
cat("  Shape:", nrow(charls), "x", ncol(charls), "\n")

# Verify specific variables in CHARLS waves 1-3 (2011, 2013, 2015)
charls_vars <- list(
  # Chronic diseases - binary 0/1
  hypertension = paste0("r", 1:3, "hibpe"),
  heart_disease = paste0("r", 1:3, "hearte"),
  stroke = paste0("r", 1:3, "stroke"),
  lung_disease = paste0("r", 1:3, "lunge"),
  diabetes = paste0("r", 1:3, "diabe"),
  cancer = paste0("r", 1:3, "cancre"),
  arthritis = paste0("r", 1:3, "arthre"),
  kidney_disease = paste0("r", 1:3, "kidneye"),
  liver_disease = paste0("r", 1:3, "livere"),
  # Self-rated health
  srh = paste0("r", 1:3, "shlta"),
  # Depression (CESD-10 total)
  cesd10 = paste0("r", 1:3, "cesd10"),
  # Cognition
  orientation = paste0("r", 1:3, "orient"),
  imrc = paste0("r", 1:3, "imrc"),
  ser7 = paste0("r", 1:3, "ser7"),
  # Grip strength
  grip = paste0("r", 1:3, "gripsum"),
  # Smoking
  smokev = paste0("r", 1:3, "smokev"),
  smoken = paste0("r", 1:3, "smoken"),
  # Drinking
  drinkev = paste0("r", 1:3, "drinkev")
)

cat("\n  CHARLS wave-level variable verification:\n")
charls_check <- data.table()
for (item_name in names(charls_vars)) {
  for (w in 1:3) {
    vn <- charls_vars[[item_name]][w]
    exists <- vn %in% names(charls)
    if (exists) {
      n <- sum(!is.na(charls[[vn]]))
      vals <- sort(unique(na.omit(as.numeric(charls[[vn]]))))
      charls_check <- rbind(charls_check, data.table(
        item=item_name, wave=as.integer(2010+2*w), variable=vn,
        exists=TRUE, n_nonnull=n, n_unique=length(vals),
        values=paste(head(vals,6), collapse=",")
      ))
    } else {
      charls_check <- rbind(charls_check, data.table(
        item=item_name, wave=as.integer(2010+2*w), variable=vn,
        exists=FALSE, n_nonnull=0L, n_unique=0L, values=""
      ))
    }
  }
}
fwrite(charls_check, file.path(root, "CHARLS_NDFI_wave_consistency.csv"))
cat("  Saved CHARLS_NDFI_wave_consistency.csv\n")

# ============================================================
# 2. Read CLASS data and verify wave-level availability
# ============================================================
cat("\n[2] Reading CLASS data files...\n")
class_root <- "E:/公共数据库/中国数据库/CLASS数据全/两种格式/STATA"

# CLASS variable mapping across waves
# 2014: b110101-b110124 (chronic diseases), b1 (SRH), e2_1-e2_9 (depression)
# 2016: b11_1_1-b11_1_24 (chronic diseases), b1 (SRH), e2_1-e2_9 (depression)
# 2018: b9_1__1-b9_1__23 (chronic diseases), b1 (SRH), e2__1-e2__9 (depression)
# 2020: B9_1_1-B9_1_23 (chronic diseases), B1 (SRH), E2_1-E2_9 (depression)
# 2023: B9_1_1-B9_1_23 (chronic diseases), B1 (SRH), E2_1-E2_9 (depression)

class_wave_defs <- list(
  `2014` = list(
    path = file.path(class_root, "2014class数据_发布版.dta"),
    chronic = paste0("b1101", sprintf("%02d", 1:24)),
    srh = "b1",
    depression = paste0("e2_", 1:9),
    adl = paste0("b4_", 1:11),
    falls = "b6_2",
    pain = "b9"
  ),
  `2016` = list(
    path = file.path(class_root, "2016class-individual-发布版.dta"),
    chronic = paste0("b11_1_", 1:24),
    srh = "b1",
    depression = paste0("e2_", 1:9),
    adl = paste0("b4_", 1:11),
    falls = "b6_2",
    pain = "b9"
  ),
  `2018` = list(
    path = file.path(class_root, "CLASS2018-cleaned release.dta"),
    chronic = paste0("b9_1__", 1:23),
    srh = "b1",
    depression = paste0("e2__", 1:9),
    adl = paste0("b4_", 1:11),
    falls = "b6_2",
    pain = NA
  ),
  `2020` = list(
    path = file.path(class_root, "individual -2020  cleaned for user.dta"),
    chronic = paste0("B9_1_", 1:23),
    srh = "B1",
    depression = paste0("E2_", 1:9),
    adl = paste0("B4_", 1:11),
    falls = "B6_2",
    pain = NA
  ),
  `2023` = list(
    path = file.path(class_root, "2023_individual_release_weighted .dta"),
    chronic = paste0("B9_1_", 1:23),
    srh = "B1",
    depression = paste0("E2_", 1:9),
    adl = paste0("B4_", 1:11),
    falls = "B6_2",
    pain = NA
  )
)

class_check <- data.table()
for (w in names(class_wave_defs)) {
  wd <- class_wave_defs[[w]]
  if (!file.exists(wd$path)) { cat("  SKIP:", wd$path, "\n"); next }
  cat("  Loading CLASS", w, "...\n")
  df <- read_dta(wd$path)
  cat("    ", nrow(df), "x", ncol(df), "\n")

  for (domain in c("chronic", "srh", "depression", "adl", "falls", "pain")) {
    vars <- wd[[domain]]
    if (is.null(vars) || length(vars) == 0 || any(is.na(vars))) next
    for (vn in vars) {
      exists <- vn %in% names(df)
      if (exists) {
        n <- sum(!is.na(df[[vn]]))
        vals <- sort(unique(na.omit(as.numeric(df[[vn]]))))
        class_check <- rbind(class_check, data.table(
          wave=as.integer(w), domain=domain, variable=vn,
          exists=TRUE, n_nonnull=n, n_unique=length(vals),
          values=paste(head(vals,6), collapse=",")
        ))
      } else {
        class_check <- rbind(class_check, data.table(
          wave=as.integer(w), domain=domain, variable=vn,
          exists=FALSE, n_nonnull=0L, n_unique=0L, values=""
        ))
      }
    }
  }
}
fwrite(class_check, file.path(root, "CLASS_NDFI_wave_consistency.csv"))
cat("  Saved CLASS_NDFI_wave_consistency.csv\n")

# ============================================================
# 3. Revised crosswalk with adjudication
# ============================================================
cat("\n[3] Building revised CHARLS-CLASS ND-FI crosswalk...\n")

# Chronic disease items: map between CHARLS and CLASS
# CHARLS has: hypertension, heart_disease, stroke, lung_disease, diabetes, cancer, arthritis, kidney_disease, liver_disease
# CLASS has same + many more
# Use the 8 that overlap in both cohorts and are in CHARLS waves 1-3

chronic_diseases <- data.table(
  harmonised_item_id = c("hypertension","heart_disease","stroke","lung_disease",
                          "diabetes","cancer","arthritis","kidney_disease"),
  domain = "Chronic diseases",
  concept = c("Ever diagnosed hypertension","Ever diagnosed heart disease",
              "Ever diagnosed stroke","Ever diagnosed lung disease",
              "Ever diagnosed diabetes","Ever diagnosed cancer",
              "Ever diagnosed arthritis","Ever diagnosed kidney disease"),
  charls_var_2011 = paste0("r1", c("hibpe","hearte","stroke","lunge","diabe","cancre","arthre","kidneye")),
  charls_var_2013 = paste0("r2", c("hibpe","hearte","stroke","lunge","diabe","cancre","arthre","kidneye")),
  charls_var_2015 = paste0("r3", c("hibpe","hearte","stroke","lunge","diabe","cancre","arthre","kidneye")),
  charls_var_2018 = paste0("r4", c("hibpe","hearte","stroke","lunge","diabe","cancre","arthre","kidneye")),
  charls_coding = "1=Yes, 0=No (cumulative ever-diagnosed)",
  class_var_2014 = paste0("b11010", c(1,2,4,9,3,7,8,5)),
  class_var_2016 = paste0("b11_1_", c(1,2,4,10,3,16,8,5)),
  class_var_2018 = paste0("b9_1__", c(1,2,3,4,5,7,9,11)),
  class_var_2020 = paste0("B9_1_", c(1,2,3,4,5,7,9,11)),
  class_var_2023 = paste0("B9_1_", c(1,2,3,4,5,7,9,11)),
  class_coding_original = "1=Yes, 2=No, 3=Uncertain (recode to 0/1)",
  unified_coding = "0=No/Uncertain, 1=Yes (cumulative, carry-forward)",
  adjudication = "Strict-core",
  justification = "Identical concept: physician-diagnosed chronic disease. Both cohorts use cumulative ever-diagnosed framing."
)

# Other health deficit items
other_items <- data.table(
  harmonised_item_id = c("srh","depression_score","orientation","imrc","ser7","grip"),
  domain = c("Self-rated health","Depression","Cognition","Cognition","Cognition","Physical function"),
  concept = c("General self-rated health (1-5)",
              "CESD-10 total score (0-30), domain-level deficit 0-1",
              "Orientation score (0-4)",
              "Immediate word recall (0-10)",
              "Serial-7 subtraction (0-5)",
              "Dominant hand grip strength (kg)"),
  charls_var_2011 = c("r1shlta","r1cesd10","r1orient","r1imrc","r1ser7","r1gripsum"),
  charls_var_2013 = c("r2shlta","r2cesd10","r2orient","r2imrc","r2ser7","r2gripsum"),
  charls_var_2015 = c("r3shlta","r3cesd10","r3orient","r3imrc","r3ser7","r3gripsum"),
  charls_var_2018 = c("r4shlta","r4cesd10","r4orient","r4imrc","r4ser7",NA),
  charls_coding = c("1=Excellent to 5=Poor","0-30 total","0-4","0-10","0-5","Continuous kg"),
  class_var_2014 = c("b1",NA,NA,NA,NA,NA),
  class_var_2016 = c("b1",NA,NA,NA,NA,NA),
  class_var_2018 = c("b1","e2__1-e2__9 derived",NA,NA,NA,NA),
  class_var_2020 = c("B1","E2_1-E2_9 derived",NA,NA,NA,NA),
  class_var_2023 = c("B1","E2_1-E2_9 derived",NA,NA,NA,NA),
  class_coding_original = c("1=Very good to 5=Very poor","1-3 per item, 9 items","N/A","N/A","N/A","N/A"),
  unified_coding = c("Recoded: 1-2=0, 3=0.5, 4-5=1 (poor health deficit)",
                      "Standardised to 0-1 within cohort (domain-level score)",
                      "Reversed: low score = deficit. (4-score)/4","(10-score)/10","(5-score)/5",
                      "Threshold by sex: below cutpoint = 1"),
  adjudication = c("Strict-core","Strict-core (domain-level)","Strict-core","Strict-core","Strict-core",
                    "Expanded only (CHARLS only in pre-expansion)"),
  justification = c(
    "Identical concept across cohorts. Different scale anchors require recoding.",
    "Different instruments (CESD-10 vs CESD-9). Use domain-level composite score to prevent over-weighting.",
    "CHARLS only - not in CLASS. Eligible for strict-core ND-FI within CHARLS trajectory.",
    "CHARLS only - not in CLASS.",
    "CHARLS only - not in CLASS.",
    "CHARLS only in pre-expansion waves. CLASS lacks grip strength."
  )
)

# Items EXCLUDED from strict-core
excluded_items <- data.table(
  harmonised_item_id = c("living_alone","no_spouse","social_activity","loneliness",
                          "current_smoke","ever_smoke","ever_drink","bmi",
                          "adl_items","pain_any","sleep_problems","falls",
                          "stooping","walk_1km","lift_carry","stand_chair",
                          "climb_stairs","incontinence","balance_test",
                          "psychiatric_prob","memory_problem"),
  domain = c(rep("Social (EXCLUDED from ND-FI)",4),
             rep("Health behavior (EXCLUDED)",3),
             "Body composition (EXCLUDED from strict-core)",
             "ADL (EXCLUDED)","Pain","Sleep","Falls",
             rep("Physical function (CHARLS only, not in CLASS pre-2016)",7)),
  concept = c("Living alone","No spouse/partner","Social activity participation","Loneliness",
              "Current smoking","Ever smoked","Ever drank alcohol","BMI",
              "ADL items (excluded from ND-FI)","Body pain","Sleep problems","Falls",
              "Difficulty stooping/kneeling","Difficulty walking 1km",
              "Difficulty lifting/carrying 10jin","Difficulty standing from chair",
              "Difficulty climbing stairs","Difficulty controlling urination",
              "Balance test summary score","Ever had psychiatric problem",
              "Ever had memory problem"),
  charls_var_2011 = rep(NA,21),
  charls_var_2013 = rep(NA,21),
  charls_var_2015 = rep(NA,21),
  charls_var_2018 = rep(NA,21),
  charls_coding = rep("",21),
  class_var_2014 = rep(NA,21),
  class_var_2016 = rep(NA,21),
  class_var_2018 = rep(NA,21),
  class_var_2020 = rep(NA,21),
  class_var_2023 = rep(NA,21),
  class_coding_original = rep("",21),
  unified_coding = rep("",21),
  adjudication = c(
    rep("Social Frailty Index",4),
    rep("Predictor/covariate only",3),
    "Expanded sensitivity only",
    rep("Excluded (ADL = external outcome)",1),
    rep("Excluded (CHARLS harmonized not found)",3),
    rep("CHARLS only - not in CLASS pre-2016 waves",7)),
  justification = c(
    rep("Social item: belongs in separate Social Frailty Index per user instructions.",4),
    "Behavioural exposure, not health deficit. Exclude from strict-core per user instructions.",
    "Behavioural exposure, not health deficit.",
    "Behavioural exposure, not health deficit.",
    "BMI: exclude from strict-core. Include in expanded sensitivity only per user instructions.",
    "ADL/IADL items are external outcomes. Never enter ND-FI.",
    "CHARLS harmonized dataset lacks pain variable.",
    "CHARLS harmonized dataset lacks sleep variable.",
    "CHARLS harmonized dataset lacks falls variable.",
    "CHARLS has r{W}stoopa in waves 1-3 but CLASS 2014 lacks equivalent. Only CLASS 2016+ has b6_1.",
    "CHARLS has r{W}walk1kma but CLASS 2014 lacks equivalent. CLASS 2016+ has b6_3.",
    "CHARLS has r{W}lifta but CLASS 2014 lacks equivalent. CLASS 2016+ has b6_7.",
    "CHARLS has r{W}chaira but CLASS 2014 lacks equivalent.",
    "CHARLS has r{W}climsa but CLASS 2014 lacks equivalent. CLASS 2016+ has b6_1.",
    "CHARLS has r{W}urina but CLASS 2014 lacks equivalent. CLASS 2016+ has b4_7/b4_8.",
    "CHARLS has r{W}balance but CLASS lacks comparable measure.",
    "CHARLS has r{W}psyche but CLASS lacks comparable measure.",
    "CHARLS has r{W}memrye but CLASS lacks comparable measure.")
)

# Combine all into revised crosswalk
revised_cw <- rbind(
  chronic_diseases[, .(harmonised_item_id, domain, concept,
                       charls_var_2011, charls_var_2013, charls_var_2015, charls_var_2018,
                       charls_coding, class_var_2014, class_var_2016, class_var_2018,
                       class_var_2020, class_var_2023, class_coding_original,
                       unified_coding, adjudication, justification)],
  other_items[, .(harmonised_item_id, domain, concept,
                  charls_var_2011, charls_var_2013, charls_var_2015, charls_var_2018,
                  charls_coding, class_var_2014, class_var_2016, class_var_2018,
                  class_var_2020, class_var_2023, class_coding_original,
                  unified_coding, adjudication, justification)],
  excluded_items[, .(harmonised_item_id, domain, concept,
                     charls_var_2011, charls_var_2013, charls_var_2015, charls_var_2018,
                     charls_coding, class_var_2014, class_var_2016, class_var_2018,
                     class_var_2020, class_var_2023, class_coding_original,
                     unified_coding, adjudication, justification)]
)

fwrite(revised_cw, file.path(root, "revised_CHARLS_CLASS_NDFI_crosswalk.csv"))
cat("  Saved revised_CHARLS_CLASS_NDFI_crosswalk.csv:", nrow(revised_cw), "items\n")

# ============================================================
# 4. Strict-core and expanded item lists
# ============================================================
cat("\n[4] Creating item lists...\n")

strict_core <- revised_cw[adjudication == "Strict-core"]
fwrite(strict_core, file.path(root, "strict_core_ndfi_item_list.csv"))
cat("  Strict-core items:", nrow(strict_core), "\n")
cat("  Domains:", paste(unique(strict_core$domain), collapse=", "), "\n")

expanded <- revised_cw[adjudication %in% c("Strict-core", "Expanded only")]
fwrite(expanded, file.path(root, "expanded_ndfi_item_list.csv"))
cat("  Expanded items:", nrow(expanded), "\n")

# Social frailty items
social <- revised_cw[adjudication == "Social Frailty Index"]
fwrite(social, file.path(root, "social_frailty_item_crosswalk.csv"))
cat("  Social frailty items:", nrow(social), "\n")

# ============================================================
# 5. Summary and naming decision
# ============================================================
cat("\n[5] ND-FI Naming Decision ===\n")
n_strict <- nrow(strict_core)
n_domains <- length(unique(strict_core$domain))
cat("Strict-core items:", n_strict, "\n")
cat("Health domains:", n_domains, "-", paste(unique(strict_core$domain), collapse=", "), "\n")

if (n_strict >= 20 && n_domains >= 5) {
  ndfi_name <- "Harmonised non-disability Frailty Index"
  cat("Name:", ndfi_name, "\n")
} else if (n_strict >= 15 && n_domains >= 5) {
  ndfi_name <- "Harmonised non-disability health-deficit index"
  cat("Name:", ndfi_name, "\n")
  cat("NOTE: Do NOT call it a standard Frailty Index\n")
} else {
  ndfi_name <- "Reduced health-deficit score"
  cat("Name:", ndfi_name, "\n")
  cat("WARNING: Fewer than 15 items or 5 domains. Consider feasibility.\n")
}

cat("\nCompleted:", format(Sys.time()), "\n")
