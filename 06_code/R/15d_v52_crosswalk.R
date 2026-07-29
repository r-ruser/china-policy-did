#!/usr/bin/env Rscript
# V5.2: Generate cross_cohort_frailty_item_crosswalk.csv
suppressPackageStartupMessages({library(data.table)})
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# Direct data.table construction
cw <- data.table(
  harmonised_item_id = c(
    "hypertension","heart_disease","stroke","lung_disease","diabetes",
    "cancer","arthritis","kidney_disease","liver_disease","stomach_disease",
    "srh","depression","falls","sleep_problems","vision","hearing",
    "grip_strength","smoking","bmi","living_alone","no_spouse","social_activity","loneliness"),
  domain = c(
    rep("Chronic diseases",10),
    "Self-rated health","Depression","Falls","Sleep","Sensory","Sensory",
    "Physical function","Health behavior","Body composition",
    "Social","Social","Social","Social"),
  concept = c(
    "Ever diagnosed hypertension","Ever diagnosed heart disease","Ever diagnosed stroke",
    "Ever diagnosed lung disease","Ever diagnosed diabetes","Ever diagnosed cancer",
    "Ever diagnosed arthritis","Ever diagnosed kidney disease","Ever diagnosed liver disease",
    "Ever diagnosed stomach disease",
    "General self-rated health","Depressive symptom burden","Falls in past period",
    "Sleep problems/insomnia","Vision problems","Hearing problems",
    "Grip strength (kg)","Current smoking status","Body mass index",
    "Living alone","No spouse/partner","Social activity participation","Feeling lonely"),
  CFPS_variable = c(
    rep("Not available",10),
    "qp3/qp201/qph2","qq601-qq606","qq2/qq201","qq4/qq401",
    rep("Not available",2),
    "Not available","eversmoke","bmivalue",
    "Available","Available","Available (ql)","Not available"),
  CFPS_waves = c(
    rep("2010 only (count)",10),
    "2010,2012,2014,2018,2020","2010,2014","2010,2014","2010,2014",
    rep("N/A",2),
    "N/A","2018,2020","2010 only",
    "2010-2020","2010-2020","2010,2014","N/A"),
  CFPS_question = c(
    rep("Chronic disease count (qp8)",10),
    "Self-rated health (1-5)","6 depression items (CESD-6 equiv)","Falls in past month/year",
    "Sleep problems","N/A","N/A","N/A","Ever smoked","BMI from height/weight",
    "Household composition","Marital status","Social activities","N/A"),
  CFPS_options = c(
    rep("0-12 (count only)",10),
    "1=Excellent to 5=Poor","1-4 per item, 6 items","0=No, 1=Yes",
    "0=No, 1=Yes","N/A","N/A","N/A","0=No, 1=Yes","Continuous",
    "Derived","Derived","Derived","N/A"),
  CHARLS_variable = c(
    "r{W}hibpe","r{W}hearte","r{W}stroke","r{W}lunge","r{W}diabe",
    "r{W}cancre","r{W}arthre","r{W}kidneye","r{W}livere","r{W}stomach",
    "r{W}shlta","r{W}cesd10","Available","Available","r{W}eyes","r{W}ears",
    "r{W}gripsum","r{W}smokev/r{W}smoken","r{W}bmi",
    "Available","Available","Available","Available"),
  CHARLS_waves = c(
    rep("2011,2013,2015,2018",10),
    "2011,2013,2015,2018","2011,2013,2015,2018","2011,2013,2015,2018","2011,2013,2015,2018",
    "2011,2013,2015,2018","2011,2013,2015,2018",
    "2011,2013,2015","2011,2013,2015,2018","2011,2013,2015,2018",
    "2011,2013,2015,2018","2011,2013,2015,2018","2011,2013,2015,2018","2011,2013,2015,2018"),
  CHARLS_question = c(
    rep("Ever had [condition]?",10),
    "Self-rated health (1-5)","CESD-10 total score (0-30)","Falls","Sleep problems",
    "Vision problem","Hearing problem","Dominant hand grip strength (kg)",
    "Ever/currently smoking","BMI",
    "Living arrangement","Marital status","Social activity","Loneliness"),
  CHARLS_options = c(
    rep("1=Yes, 0=No",10),
    "1=Excellent to 5=Poor","0-30","0=No, 1=Yes","0=No, 1=Yes",
    "0=No, 1=Yes","0=No, 1=Yes","Continuous (kg)",
    "0=No, 1=Yes","Continuous",
    "Derived","Derived","Derived","0=No, 1=Yes"),
  CLASS_variable = c(
    "b9_1__1/B9_1_1","b9_1__2/B9_1_2","b9_1__3/B9_1_3","b9_1__4/B9_1_4","b9_1__5/B9_1_5",
    "b9_1__7/B9_1_7","b9_1__9/B9_1_9","b9_1__11/B9_1_11","b9_1__6/B9_1_6","b9_1__20/B9_1_20",
    "b1/B1","e2__1-e2__9/E2_1-E2_9","b6_2/B6_2","b16/B16","b8/B8","b8/B8",
    "Not available","b10/B10","Not directly available",
    "a8__1/A8_1_open","q12/A7","Available","Available"),
  CLASS_waves = c(
    rep("2016,2018,2020,2023",10),
    "2016,2018,2020,2023","2018,2020,2023","2016,2018,2020,2023","2016,2018,2020,2023",
    "2016,2018,2020,2023","2016,2018,2020,2023",
    "N/A","2016,2018,2020,2023","N/A",
    "2016,2018,2020,2023","2016,2018,2020,2023","2016,2018,2020,2023","2018,2020,2023"),
  CLASS_question = c(
    rep("Diagnosed [condition]?",10),
    "Self-rated health (1-5)","CESD-9 depression items","Falls in past 12 months",
    "Sleep quality/problems","Vision difficulty","Hearing difficulty",
    "N/A","Currently smoking","N/A",
    "Who lives with respondent","Marital status","Social activities","Loneliness"),
  CLASS_options = c(
    rep("1=Yes, 0=No",10),
    "1=Very good to 5=Very poor","1-3 per item, 9 items","0=No, 1=Yes",
    "Different scales","0=No, 1=Yes","0=No, 1=Yes",
    "N/A","0=No, 1=Yes","N/A",
    "Derived","Derived","Derived","0=No, 1=Yes"),
  strictly_identical = c(
    rep("No",10),
    "Conceptually identical","Different instruments","Conceptually identical",
    "Conceptually identical","No","No",
    "No","Conceptually identical","No",
    "No","No","No","No"),
  conceptually_harmonisable = c(
    rep("CHARLS+CLASS only",10),
    "Yes (scale anchors differ)","Yes (different instruments)","Yes (different recall)",
    "Yes (different wording)","CHARLS+CLASS only","CHARLS+CLASS only",
    "CHARLS only","Yes (ever vs current)","CFPS+CHARLS only",
    "Yes (different derivation)","Yes","Yes (different instruments)","CHARLS+CLASS only"),
  unified_coding = c(
    rep("0=No, 1=Yes",10),
    "0=Good (1-3), 1=Poor (4-5)","Standardised score 0-1","0=No, 1=Yes",
    "0=No, 1=Yes","0=No, 1=Yes","0=No, 1=Yes",
    "Continuous, threshold by sex","0=No, 1=Yes","Continuous, categorised",
    "0=Not alone, 1=Alone","0=Married, 1=No spouse","0=Active, 1=Inactive","0=No, 1=Yes"),
  missing_coding = c(
    rep("NA=missing",23)),
  enter_strict_core = c(
    rep("No",10),
    "Yes","No","No","No","No","No",
    "No","No","No",
    "No","No","No","No"),
  enter_expanded_only = c(
    rep("Yes",10),
    "No","Yes","Yes","Yes","Yes","Yes",
    "Yes","Yes","Yes",
    "Yes","Yes","Yes","Yes"),
  exclusion_reason = c(
    rep("CFPS lacks individual chronic disease items after 2010",10),
    "All 3 cohorts (primary)","Different instruments","All 3 (different recall periods)",
    "All 3 (different wording)","CHARLS+CLASS only","CHARLS+CLASS only",
    "CHARLS only (grip strength)","All 3 (different definitions)","CFPS+CHARLS only",
    "All 3 (different derivation)","All 3","All 3 (different instruments)","CHARLS+CLASS only")
)

fwrite(cw, file.path(root, "cross_cohort_frailty_item_crosswalk.csv"))
cat("Saved crosswalk:", nrow(cw), "items\n")
cat("Domains:", length(unique(cw$domain)), "\n")
cat("Strict core (all 3):", sum(cw$enter_strict_core == "Yes"), "\n")
cat("Expanded (CHARLS+CLASS):", sum(cw$enter_expanded_only == "Yes"), "\n")
cat("CHARLS+CLASS chronic diseases:", sum(cw$domain == "Chronic diseases"), "\n")
