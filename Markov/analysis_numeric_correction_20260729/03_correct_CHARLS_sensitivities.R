#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(nnet)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
} else {
  normalizePath(getwd(), winslash = "/")
}
project_root <- normalizePath(
  Sys.getenv("MARKOV_PROJECT_ROOT", unset = file.path(script_dir, "..")),
  winslash = "/",
  mustWork = TRUE
)
out_dir <- file.path(project_root, "analysis_numeric_correction_20260729")
fi <- fread(file.path(project_root, "data", "CHARLS_wave_specific_continuous_FI.csv"))

make_transitions <- function(x, fi_column) {
  d <- x[
    age_at_wave >= 65 & !is.na(get(fi_column)),
    .(
      ID = as.character(ID),
      wave = as.integer(wave),
      age_at_wave = as.numeric(age_at_wave),
      female = as.numeric(female),
      fi = as.numeric(get(fi_column))
    )
  ]
  d[, state := fifelse(fi < 0.10, 1L, fifelse(fi < 0.25, 2L, 3L))]
  setorder(d, ID, wave)
  keep <- d[, .N, by = ID][N >= 2L, ID]
  t <- d[ID %in% keep, .(
    wave_from = wave[-.N],
    wave_to = wave[-1L],
    state_from = state[-.N],
    state_to = state[-1L],
    age_from = age_at_wave[-.N],
    female = female[-.N]
  ), by = ID]
  t[, `:=`(
    period = fifelse(wave_to <= 2015L, 0L, 1L),
    interval_years = as.numeric(wave_to - wave_from),
    age_c = (age_from - 70) / 5,
    age75 = as.integer(age_from >= 75),
    state_from_f = factor(state_from, levels = 1:3),
    state_to_f = factor(state_to, levels = 1:3)
  )]
  stopifnot(all(t$interval_years > 0))
  list(records = d, transitions = t)
}

primary <- make_transitions(fi[fi_valid_80 == TRUE], "fi_primary")
target <- expand.grid(
  state_from_f = factor(1:3, levels = 1:3),
  period = c(0L, 1L),
  interval_years = 2,
  age_c = 0,
  female = 0.5
)

estimate <- function(t, specification) {
  m <- multinom(
    state_to_f ~ state_from_f * period + interval_years + age_c + female,
    data = t,
    trace = FALSE
  )
  p <- predict(m, newdata = target, type = "probs")
  ans <- list()
  for (from in 1:3) {
    for (to in 1:3) {
      ans[[length(ans) + 1L]] <- data.table(
        specification,
        from,
        to,
        pre = p[from, to],
        expansion = p[from + 3L, to],
        difference = p[from + 3L, to] - p[from, to],
        n_transitions = nrow(t),
        n_participants = uniqueN(t$ID)
      )
    }
  }
  rbindlist(ans)
}

results <- list(estimate(primary$transitions, "Primary 80%"))

for (sex in c(0, 1)) {
  results[[length(results) + 1L]] <- estimate(
    primary$transitions[female == sex],
    ifelse(sex == 0, "Men", "Women")
  )
}

wave_counts <- primary$records[, .N, by = ID]
for (minimum_waves in c(3L, 4L)) {
  ids <- wave_counts[N >= minimum_waves, ID]
  results[[length(results) + 1L]] <- estimate(
    primary$transitions[ID %in% ids],
    paste0(minimum_waves, "+ valid waves")
  )
}

for (threshold in c("fi_70", "fi_90")) {
  obj <- make_transitions(fi, threshold)
  results[[length(results) + 1L]] <- estimate(
    obj$transitions,
    paste0(sub("fi_", "", threshold), "% completion")
  )
}

all_results <- rbindlist(results)
fwrite(all_results, file.path(out_dir, "CHARLS_corrected_sensitivity_results.csv"))

age_model <- multinom(
  state_to_f ~ state_from_f * period * age75 + interval_years + age_c + female,
  data = primary$transitions,
  trace = FALSE
)
age_rows <- list()
for (age_group in c("65-74", "75+")) {
  age75_value <- as.integer(age_group == "75+")
  age_c_value <- ifelse(age_group == "75+", 1, -1)
  for (period_value in 0:1) {
    newdata <- expand.grid(
      state_from_f = factor(1:3, levels = 1:3),
      period = period_value,
      interval_years = 2,
      age75 = age75_value,
      age_c = age_c_value,
      female = 0.5
    )
    p <- predict(age_model, newdata = newdata, type = "probs")
    for (from in 1:3) {
      for (to in 1:3) {
        age_rows[[length(age_rows) + 1L]] <- data.table(
          age_group,
          period = c("pre-expansion", "expansion")[period_value + 1L],
          from,
          to,
          probability = p[from, to]
        )
      }
    }
  }
}
age_results <- rbindlist(age_rows)
fwrite(age_results, file.path(out_dir, "CHARLS_corrected_age_probabilities.csv"))

cat("Sensitivity range by transition:\n")
print(all_results[, .(
  min_difference = min(difference),
  max_difference = max(difference)
), by = .(from, to)])
