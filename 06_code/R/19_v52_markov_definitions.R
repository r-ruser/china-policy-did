#!/usr/bin/env Rscript
# V5.2: Markov model definitions, transition matrices, and decision log
suppressPackageStartupMessages({library(data.table)})
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

cat("=== V5.2 Markov Model Definitions ===\nStarted:", format(Sys.time()), "\n\n")

# ============================================================
# 1. Permitted transition matrix
# ============================================================
states <- c("Robust", "Prefrail", "Frail", "Death")
trans <- data.table(
  from = rep(states, each = 4),
  to = rep(states, 4),
  permitted = c(
    # From Robust
    FALSE, TRUE, FALSE, TRUE,
    # From Prefrail
    TRUE, FALSE, TRUE, TRUE,
    # From Frail
    FALSE, TRUE, FALSE, TRUE,
    # From Death
    FALSE, FALSE, FALSE, FALSE
  ),
  rationale = c(
    "Same state: no transition",
    "Deterioration: Robust to Prefrail",
    "Direct Robust to Frail: evaluate from data",
    "Mortality: Robust to Death",
    "Recovery: Prefrail to Robust",
    "Same state: no transition",
    "Deterioration: Prefrail to Frail",
    "Mortality: Prefrail to Death",
    "Direct Frail to Robust: evaluate from data",
    "Deterioration: Frail to Prefrail (unlikely, coded as permitted)",
    "Same state: no transition",
    "Mortality: Frail to Death",
    "Absorbing state: no transitions out",
    "Absorbing state: no transitions out",
    "Absorbing state: no transitions out",
    "Absorbing state: no transitions out"
  )
)
fwrite(trans, file.path(root, "permitted_transition_matrix.csv"))
cat("Saved permitted_transition_matrix.csv\n")

# ============================================================
# 2. Observed transition counts (placeholder - needs data)
# ============================================================
# This will be populated after FI construction and state assignment
# For now, create the structure
charls_waves <- c(2011, 2013, 2015, 2018)
class_waves <- c(2016, 2018, 2020, 2023)

observed <- data.table(
  cohort = character(),
  interval = character(),
  from_state = character(),
  to_state = character(),
  n_transitions = integer(),
  n_persons = integer(),
  notes = character()
)

# CHARLS intervals
for (i in 1:(length(charls_waves)-1)) {
  from_w <- charls_waves[i]
  to_w <- charls_waves[i+1]
  for (from_s in c("Robust","Prefrail","Frail")) {
    for (to_s in states) {
      observed <- rbind(observed, data.table(
        cohort = "CHARLS",
        interval = paste0(from_w, "-", to_w),
        from_state = from_s,
        to_state = to_s,
        n_transitions = NA_integer_,
        n_persons = NA_integer_,
        notes = "To be computed from data"
      ))
    }
  }
}

# CLASS intervals
for (i in 1:(length(class_waves)-1)) {
  from_w <- class_waves[i]
  to_w <- class_waves[i+1]
  for (from_s in c("Robust","Prefrail","Frail")) {
    for (to_s in states) {
      observed <- rbind(observed, data.table(
        cohort = "CLASS",
        interval = paste0(from_w, "-", to_w),
        from_state = from_s,
        to_state = to_s,
        n_transitions = NA_integer_,
        n_persons = NA_integer_,
        notes = "To be computed from data"
      ))
    }
  }
}
fwrite(observed, file.path(root, "observed_transition_counts.csv"))
cat("Saved observed_transition_counts.csv:", nrow(observed), "rows\n")

# ============================================================
# 3. Mortality and attrition audit (placeholder)
# ============================================================
mort <- data.table(
  cohort = c("CHARLS", "CHARLS", "CHARLS",
             "CLASS", "CLASS", "CLASS"),
  interval = c("2011-2013", "2013-2015", "2015-2018",
               "2016-2018", "2018-2020", "2020-2023"),
  n_baseline = NA_integer_,
  n_dead = NA_integer_,
  n_lost_to_followup = NA_integer_,
  n_retained = NA_integer_,
  mortality_rate = NA_real_,
  ltfu_rate = NA_real_,
  notes = c(
    "To be computed: deaths identified from exit status",
    "To be computed",
    "To be computed: includes 2018 follow-up",
    "To be computed",
    "To be computed: primary validation interval",
    "To be computed: severe selective attrition expected")
)
fwrite(mort, file.path(root, "mortality_and_attrition_audit.csv"))
cat("Saved mortality_and_attrition_audit.csv\n")

# ============================================================
# 4. multistate_model_decision_log.md
# ============================================================
decision_log <- paste0(
"# Multistate Model Decision Log - V5.2\n\n",
"Date: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n",
"## Architecture Decision\n\n",
"### Replaced analysis\n",
"- Latent-class growth analysis (LCGA) replaced by multistate Markov transition models\n",
"- Markov models answer: whether deterioration/recovery changed during policy expansion\n\n",
"### Preserved analysis\n",
"- CFPS pilot-area DID remains primary policy analysis\n",
"- CFPS age 75+ DDD retained\n",
"- CFPS high-need analysis retained if defensible\n\n",
"## State Definitions\n\n",
"### CHARLS frailty states\n",
"- Based on cohort-specific FI: 23 items, 7 domains\n",
"- Domains: chronic diseases (9), SRH (1), depression (1), cognition (3), physical function (6), psychiatric (1), health behavior (1)\n",
"- Cut-points: to be determined from FI distribution (tertiles or validated thresholds)\n",
"- If validated cut-points unavailable: low/intermediate/high deficit\n\n",
"### CLASS frailty states\n",
"- Based on cohort-specific FI: 20 items, 5 domains\n",
"- Domains: chronic diseases (11), SRH (1), physical function (5), incontinence (2), depression (1)\n",
"- Cut-points: to be determined from FI distribution\n\n",
"## Permitted Transitions\n\n",
"- Robust to Prefrail (deterioration)\n",
"- Prefrail to Robust (recovery)\n",
"- Prefrail to Frail (deterioration)\n",
"- Frail to Prefrail (recovery - evaluate from data)\n",
"- All transient states to Death (mortality)\n",
"- Death is absorbing\n",
"- Direct Robust to Frail: evaluate whether data supports\n\n",
"## Policy Period Analysis (CHARLS)\n\n",
"- Pre-expansion: 2011-2013, 2013-2015\n",
"- National expansion: 2015-2018\n",
"- Policy period as time-varying transition-specific covariate\n",
"- Estimate whether expansion interval associated with changes in transition intensities\n\n",
"## Childhood Hunger\n\n",
"- CLASS b14 available in 2016 and 2018\n",
"- CHARLS rahltcom (proxy) available in all waves\n",
"- CFPS: NOT AVAILABLE\n",
"- Childhood hunger as fixed transition-specific covariate\n",
"- NOT included in repeated Social Frailty Index\n\n",
"## Model Assumptions\n\n",
"- Markov assumption: future depends on current state, not history\n",
"- Evaluate: time in current state, previous state, calendar period\n",
"- If Markov inadequate: semi-Markov sensitivity model\n",
"- Evaluate: time-homogeneous vs piecewise time-varying intensities\n",
"- Evaluate: proportional covariate effects\n\n",
"## Social Frailty\n\n",
"- Separate social-vulnerability-state Markov model (if valid)\n",
"- Joint physical-social model only if both valid and adequate sample\n",
"- Childhood hunger as predictor of joint-state transitions\n\n",
"## Required Outputs\n\n",
"1. childhood_hunger_crosswalk.csv\n",
"2. CHARLS_frailty_state_definition.csv\n",
"3. CLASS_frailty_state_definition.csv\n",
"4. observed_transition_counts.csv\n",
"5. permitted_transition_matrix.csv\n",
"6. CHARLS_multistate_markov_results.csv (after model fitting)\n",
"7. CLASS_multistate_markov_results.csv (after model fitting)\n",
"8. childhood_hunger_transition_HRs.csv (after model fitting)\n",
"9. childhood_hunger_adjusted_transition_probabilities.csv (after model fitting)\n",
"10. markov_assumption_diagnostics.csv (after model fitting)\n",
"11. semi_markov_sensitivity_results.csv (after model fitting)\n",
"12. mortality_and_attrition_audit.csv\n",
"13. CFPS_childhood_hunger_DDD_results.csv (NOT FEASIBLE - CFPS lacks childhood hunger)\n",
"14. This file: multistate_model_decision_log.md\n\n",
"## Feasibility Notes\n\n",
"- CFPS childhood hunger DDD: NOT FEASIBLE (variable not available)\n",
"- CHARLS childhood hunger: rahltcom is a proxy (different construct)\n",
"- CLASS childhood hunger: b14 available in 2016 and 2018 only\n",
"- Cross-cohort childhood_hunger_ever: NOT FEASIBLE\n"
)
writeLines(decision_log, file.path(root, "multistate_model_decision_log.md"))
cat("Saved multistate_model_decision_log.md\n")

cat("\nCompleted:", format(Sys.time()), "\n")
