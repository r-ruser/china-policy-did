#!/usr/bin/env Rscript
# V5.2 Step B: FI item-level eligibility and ADL-overlap audits
suppressPackageStartupMessages({library(data.table)})
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("=== V5.2 Step B: FI Item Audit ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. CHARLS 23-item FI audit
# ============================================================
charls_audit <- data.table(
  item_id = c("hypertension","heart_disease","stroke","lung_disease","diabetes",
              "cancer","arthritis","kidney_disease","liver_disease",
              "srh","depression","orientation","imrc","ser7",
              "stooping","walk_1km","lift_carry","stand_chair","climb_stairs","incontinence",
              "psychiatric","memory_problem","current_smoke"),
  domain = c(rep("Chronic diseases",9),"Self-rated health","Depression","Cognition","Cognition","Cognition",
             rep("Physical function",6),"Psychiatric","Psychiatric","Health behaviour"),
  original_variable = c(
    paste0("r{W}",c("hibpe","hearte","stroke","lunge","diabe","cancre","arthre","kidneye","livere")),
    "r{W}shlta","r{W}cesd10","r{W}orient","r{W}imrc","r{W}ser7",
    paste0("r{W}",c("stoopa","walk1kma","lifta","chaira","climsa","urina")),
    "r{W}psyche","r{W}memrye","r{W}smoken"),
  wave_availability = c(
    rep("2011,2013,2015,2018",9),
    "2011,2013,2015,2018","2011,2013,2015,2018",
    "2011,2013,2015,2018","2011,2013,2015,2018","2011,2013,2015,2018",
    "2011,2013,2015,2018","2011,2013,2015,2018","2011,2013,2015,2018",
    "2011,2013,2015,2018","2011,2013,2015,2018","2011,2013,2015,2018",
    "2011,2013,2015,2018","2011,2013,2015,2018"),
  scoring_rule = c(
    rep("0=No, 1=Yes (cumulative, carry-forward)",9),
    "0=Good(1-3), 1=Poor(4-5) [recoded]","0-1 [standardised to 0-1]",
    "0-1 [reversed: (4-score)/4]","0-1 [reversed: (10-score)/10]","0-1 [reversed: (5-score)/5]",
    rep("0=No difficulty, 1=Difficulty",6),
    "0=No, 1=Yes","0=No, 1=Yes","0=No, 1=Yes"),
  item_type = c(
    rep("disease",9),"symptom","symptom","function","function","function",
    rep("function",6),"symptom","symptom","behaviour"),
  overlaps_adl = c(
    rep("No",9),"No","No","No","No","No",
    "No","No","No","No","No","Maybe (urination control may overlap with incontinence ADL item)",
    "No","No","No"),
  adl_overlap_detail = c(
    rep("Chronic disease: not an ADL item",9),
    "Self-rated health: not ADL","Depression score: not ADL",
    "Cognition: not ADL","Cognition: not ADL","Cognition: not ADL",
    "Physical limitation: not ADL (excludes bathing, dressing, eating, toileting, transferring, bed mobility)",
    "Physical limitation: not ADL","Physical limitation: not ADL",
    "Physical limitation: not ADL","Physical limitation: not ADL",
    "Urination/defecation control: may overlap with incontinence in some ADL definitions - requires exclusion sensitivity",
    "Psychiatric diagnosis: not ADL","Memory problem diagnosis: not ADL","Behavioural: EXCLUDED from primary FI"),
  retained_primary = c(
    rep("Yes",9),"Yes","Yes","Yes","Yes","Yes",
    "Yes","Yes","Yes","Yes","Yes","Sensitive (exclusion sensitivity needed)",
    "Yes","Yes","NO - behaviour excluded"),
  exclusion_reason = c(
    rep("",9),"","","","","",
    "","","","","",
    "May overlap with ADL incontinence item - sensitivity analysis required",
    "","","EXCLUDED: health behaviour, not health deficit per user instructions")
)

fwrite(charls_audit, file.path(root, "CHARLS_23item_FI_item_audit.csv"))
cat("Saved CHARLS_23item_FI_item_audit.csv:", nrow(charls_audit), "items\n")

# ============================================================
# 2. CLASS 20-item FI audit
# ============================================================
class_audit <- data.table(
  item_id = c("hypertension","heart_disease","stroke","lung_disease","diabetes",
              "cancer","arthritis","kidney_disease","liver_disease","stomach_disease","osteoporosis",
              "srh",
              "climb_stairs","walk_outside","lift_heavy","fall_12m","carry_10jin",
              "urinary_incontinence","fecal_incontinence",
              "depression_score"),
  domain = c(rep("Chronic diseases",11),"Self-rated health",
             rep("Physical function",5),"Incontinence","Incontinence","Depression"),
  original_variable = c(
    paste0("b9_1__",c(1,2,3,4,5,7,9,11)),
    "b9_1__6","b9_1__20","b9_1__18",
    "b1/B1",
    "b6_1/B6_1","b6_3/B6_3","b6_7/B6_7","b6_2/B6_2","b6_7/B6_7",
    "b4_7/B4_7","b4_8/B4_8",
    "e2__1-e2__9/E2_1-E2_9"),
  wave_availability = c(
    rep("2016,2018,2020,2023",11),
    "2016,2018,2020,2023",
    "2016,2018,2020,2023","2016,2018,2020,2023","2016,2018,2020,2023",
    "2016,2018,2020,2023","2016,2018,2020,2023",
    "2016,2018,2020,2023","2016,2018,2020,2023",
    "2018,2020,2023"),
  scoring_rule = c(
    rep("0=No, 1=Yes (recode 1=Yes, 2/3=No)",11),
    "0=Good(1-2), 1=Poor(3-5) [recoded]",
    "0=No difficulty(1), 1=Difficulty(2-3)","0=No difficulty(1), 1=Difficulty(2-3)",
    "0=Can(1), 1=Cannot(2)","0=No(2), 1=Yes(1)","0=Can(1), 1=Cannot(2)",
    "0=No(2), 1=Yes(1)","0=No(2), 1=Yes(1)",
    "0-1 [standardised to 0-1]"),
  item_type = c(
    rep("disease",11),"symptom",
    rep("function",5),"symptom","symptom","symptom"),
  overlaps_adl = c(
    rep("No",11),"No",
    "No","No","No","No","No",
    "Sensitive","Sensitive","No"),
  adl_overlap_detail = c(
    rep("Chronic disease: not an ADL item",11),
    "Self-rated health: not ADL",
    "Climbing stairs: not ADL (excludes bathing, dressing, eating, toileting, transferring, bed mobility)",
    "Walking outside: not ADL","Lifting: not ADL",
    "Falls: not ADL (event, not functional limitation)",
    "Carrying: not ADL",
    "Urinary incontinence: may overlap with ADL in some definitions - sensitivity analysis required",
    "Fecal incontinence: may overlap with ADL in some definitions - sensitivity analysis required",
    "Depression score: not ADL"),
  retained_primary = c(
    rep("Yes",11),"Yes",
    "Yes","Yes","Yes","Yes","Yes",
    "Sensitive","Sensitive","Yes"),
  exclusion_reason = c(
    rep("",11),"",
    "","","","","",
    "May overlap with ADL incontinence item - sensitivity analysis required",
    "May overlap with ADL incontinence item - sensitivity analysis required",
    "")
)

fwrite(class_audit, file.path(root, "CLASS_20item_FI_item_audit.csv"))
cat("Saved CLASS_20item_FI_item_audit.csv:", nrow(class_audit), "items\n")

# ============================================================
# 3. FI_ADL_overlap_audit.csv
# ============================================================
overlap <- data.table(
  cohort = c("CHARLS","CHARLS","CLASS","CLASS"),
  item = c("incontinence (urina)","incontinence (urina)","urinary_incontinence (b4_7)","fecal_incontinence (b4_8)"),
  adl_outcome_item = c("urina may overlap with ADL toileting","urina may overlap with ADL toileting",
                        "b4_7 may overlap with ADL toileting","b4_8 may overlap with ADL toileting"),
  decision = c("Sensitive - exclude in sensitivity","Sensitive - exclude in sensitivity",
               "Sensitive - exclude in sensitivity","Sensitive - exclude in sensitivity"),
  sensitivity_action = c("Run FI without incontinence items","Run FI without incontinence items",
                         "Run FI without incontinence items","Run FI without incontinence items")
)
fwrite(overlap, file.path(root, "FI_ADL_overlap_audit.csv"))
cat("Saved FI_ADL_overlap_audit.csv\n")

# ============================================================
# 4. FI_outcome_contamination_decision.md
# ================================================="
decision_text <- paste0(
"# FI Outcome Contamination Decision - V5.2\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## ADL Overlap Assessment\n\n",
"### CHARLS\n",
"- Physical function items (stooping, walking 1km, lifting, chair, climb stairs): These are NOT part of the standard ADL definition (bathing, dressing, eating, toileting, transferring, bed mobility). RETAINED.\n",
"- Incontinence (urina): May overlap with toileting ADL item. RETAINED with exclusion sensitivity analysis.\n\n",
"### CLASS\n",
"- Physical function items (climb stairs, walk outside, lift, carry, falls): NOT part of standard ADL. RETAINED.\n",
"- Incontinence (b4_7 urinary, b4_8 fecal): May overlap with ADL toileting items. RETAINED with exclusion sensitivity analysis.\n\n",
"## Health Behaviour Exclusion\n\n",
"### CHARLS\n",
"- current_smoke (r{W}smoken): EXCLUDED from primary FI. Retained as predictor/covariate.\n\n",
"### CLASS\n",
"- No health behaviour items in the 20-item FI.\n\n",
"## Revised Item Counts After Exclusions\n\n",
"### CHARLS: 22 items (removed current_smoke), 6 domains\n",
"- Chronic diseases (9)\n",
"- Self-rated health (1)\n",
"- Depression (1)\n",
"- Cognition (3)\n",
"- Physical function (6)\n",
"- Psychiatric (2)\n\n",
"### CLASS: 20 items, 5 domains (unchanged)\n",
"- Chronic diseases (11)\n",
"- Self-rated health (1)\n",
"- Physical function (5)\n",
"- Incontinence (2) - sensitivity exclusion\n",
"- Depression (1)\n\n",
"## Minimum Item-Completion Rule\n\n",
"- Primary: 80% of items must be non-missing for valid FI\n",
"- Sensitivity: 70% and 90% thresholds\n",
"- Individual items that are structurally missing (e.g., wave-specific absence) count toward the denominator\n\n",
"## Provisional State Labels\n\n",
"Use neutral labels until cut-points are validated:\n",
"- Low-deficit state\n",
"- Intermediate-deficit state\n",
"- High-deficit state\n\n",
"Do NOT rename to Robust/Prefrail/Frail until validated.\n"
)
writeLines(decision_text, file.path(root, "FI_outcome_contamination_decision.md"))
cat("Saved FI_outcome_contamination_decision.md\n")

# ============================================================
# Summary
# ============================================================
cat("\n=== Revised FI Summary After Exclusions ===\n")
cat("CHARLS:", nrow(charls_audit) - 1, "items (removed current_smoke), 6 domains\n")
cat("CLASS:", nrow(class_audit), "items, 5 domains\n")

cat("\nCompleted:", format(Sys.time()), "\n")
