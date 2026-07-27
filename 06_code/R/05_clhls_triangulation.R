#!/usr/bin/env Rscript

# CLHLS external trajectory triangulation for V4.
# This module does not estimate a policy effect. It validates whether
# heterogeneous cognitive-domain and psychological-wellbeing patterns recur
# in an independent cohort of adults aged 65 years or older.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(lcmm)
})

options(encoding = "UTF-8")
set.seed(20260726)
n_cores <- parallel::detectCores(logical = TRUE)
if (is.na(n_cores) || n_cores < 1L) n_cores <- 1L

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
path_tables <- file.path(project_root, "07_results", "tables")
path_models <- file.path(project_root, "07_results", "models")
path_diag <- file.path(project_root, "07_results", "diagnostics")
path_logs <- file.path(project_root, "10_logs")
invisible(lapply(c(path_tables, path_models, path_diag, path_logs), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

log_file <- file.path(path_logs, "r_clhls_triangulation.log")
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

cat("CLHLS external trajectory triangulation\n")
cat("Started:", format(Sys.time()), "\n")
cat("Logical CPU cores used:", n_cores, "\n")

clhls_path <- paste0(
  "E:/公共数据库/中国数据库/clhls-state/",
  "dataverse_files数据文件-STATA数据/",
  "clhls_2008_2018_longitudinal_dataset_released_version1.dta"
)
stopifnot(file.exists(clhls_path))

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
suffixes <- c(`2008` = "", `2011` = "_11", `2014` = "_14", `2018` = "_18")
cognitive_items <- c(
  "c11", "c12", "c13", "c14", "c15",
  "c21a", "c21b", "c21c", "c41a", "c41b", "c41c",
  "c31a", "c31b", "c31c", "c31d", "c31e"
)
psych_items <- paste0("b2", 1:7)
required <- c(
  "id", "yearin", "yearin_11", "yearin_14", "yearin_18",
  "trueage", "a1", "f1", "b12"
)
for (yr in names(suffixes)) {
  s <- suffixes[[yr]]
  required <- c(required, paste0(cognitive_items, s))
  if (yr %in% c("2008", "2011", "2014")) {
    required <- c(required, paste0(psych_items, s))
  }
}
required <- unique(required)
d <- read_dta(clhls_path, col_select = all_of(required))

d <- d |>
  mutate(
    id_chr = as.character(id),
    baseline_age = safe_numeric(trueage),
    female = ifelse(
      safe_numeric(a1) %in% c(1, 2),
      as.integer(safe_numeric(a1) == 2),
      NA_integer_
    ),
    education_years = ifelse(
      safe_numeric(f1) >= 0 & safe_numeric(f1) <= 40,
      safe_numeric(f1),
      NA_real_
    ),
    poor_srh_2008 = ifelse(
      safe_numeric(b12) %in% 1:5,
      as.integer(safe_numeric(b12) >= 4),
      NA_integer_
    )
  ) |>
  filter(
    !is.na(id_chr), nzchar(id_chr),
    baseline_age >= 65,
    safe_numeric(yearin) > 0
  )

score_cognitive_domain <- function(df, suffix) {
  mat <- do.call(cbind, lapply(cognitive_items, function(v) {
    x <- safe_numeric(df[[paste0(v, suffix)]])
    ifelse(x %in% 0:1, x, NA_real_)
  }))
  ifelse(rowSums(!is.na(mat)) == length(cognitive_items),
         rowSums(mat), NA_real_)
}

score_psychological_wellbeing <- function(df, suffix) {
  mat <- do.call(cbind, lapply(psych_items, function(v) {
    x <- safe_numeric(df[[paste0(v, suffix)]])
    ifelse(x %in% 1:5, x, NA_real_)
  }))
  complete <- rowSums(!is.na(mat)) == length(psych_items)
  # Positive items: bright side, neatness, autonomy, happiness.
  # Negative items: fear/anxiety, loneliness, uselessness.
  burden <- rowSums(cbind(
    mat[, 1] - 1,
    mat[, 2] - 1,
    5 - mat[, 3],
    5 - mat[, 4],
    mat[, 5] - 1,
    5 - mat[, 6],
    mat[, 7] - 1
  ))
  ifelse(complete, burden, NA_real_)
}

cog_wide <- d |>
  transmute(
    id_chr,
    baseline_age,
    female,
    education_years,
    poor_srh_2008,
    cognition_2008 = score_cognitive_domain(d, ""),
    cognition_2011 = score_cognitive_domain(d, "_11"),
    cognition_2014 = score_cognitive_domain(d, "_14"),
    cognition_2018 = score_cognitive_domain(d, "_18")
  )
cog_long <- cog_wide |>
  pivot_longer(
    starts_with("cognition_"),
    names_to = "year",
    names_prefix = "cognition_",
    values_to = "raw_score"
  ) |>
  mutate(year = as.integer(year)) |>
  group_by(id_chr) |>
  filter(sum(!is.na(raw_score)) >= 3) |>
  ungroup() |>
  filter(!is.na(raw_score))

psych_wide <- d |>
  transmute(
    id_chr,
    baseline_age,
    female,
    education_years,
    poor_srh_2008,
    psych_2008 = score_psychological_wellbeing(d, ""),
    psych_2011 = score_psychological_wellbeing(d, "_11"),
    psych_2014 = score_psychological_wellbeing(d, "_14")
  )
psych_long <- psych_wide |>
  pivot_longer(
    starts_with("psych_"),
    names_to = "year",
    names_prefix = "psych_",
    values_to = "raw_score"
  ) |>
  mutate(year = as.integer(year)) |>
  group_by(id_chr) |>
  filter(sum(!is.na(raw_score)) == 3) |>
  ungroup() |>
  filter(!is.na(raw_score))

standardise_fixed_baseline <- function(dat, baseline_year, reverse = FALSE) {
  baseline <- dat$raw_score[dat$year == baseline_year]
  center <- mean(baseline, na.rm = TRUE)
  scale <- sd(baseline, na.rm = TRUE)
  if (!is.finite(center) || !is.finite(scale) || scale <= 0) {
    stop("Invalid CLHLS baseline standardisation.")
  }
  z <- (dat$raw_score - center) / scale
  if (reverse) z <- -z
  list(data = mutate(dat, burden_z = z), center = center, scale = scale)
}

cog_std <- standardise_fixed_baseline(cog_long, 2008, reverse = TRUE)
psych_std <- standardise_fixed_baseline(psych_long, 2008, reverse = FALSE)
cog_long <- cog_std$data |>
  mutate(time = (year - 2008) / 10)
psych_long <- psych_std$data |>
  mutate(time = (year - 2008) / 6)

make_id_map <- function(dat) {
  dat |> distinct(id_chr) |> arrange(id_chr) |> mutate(ID_num = row_number())
}
cog_map <- make_id_map(cog_long)
psych_map <- make_id_map(psych_long)
cog_long <- left_join(cog_long, cog_map, by = "id_chr") |>
  arrange(ID_num, year)
psych_long <- left_join(psych_long, psych_map, by = "id_chr") |>
  arrange(ID_num, year)
fwrite(
  psych_long,
  file.path(path_tables, "r_clhls_psych_trajectory_long.csv")
)
fwrite(
  psych_long |>
    count(id_chr, ID_num, name = "n_measurements") |>
    mutate(outcome = "Depressive-affect burden"),
  file.path(path_diag, "r_clhls_trajectory_measurement_counts.csv")
)

flow <- psych_wide |>
    summarise(
      outcome = "Depressive-affect burden",
      baseline_n = n_distinct(id_chr),
      valid_2008 = sum(!is.na(psych_2008)),
      valid_2011 = sum(!is.na(psych_2011)),
      valid_2014 = sum(!is.na(psych_2014)),
      valid_2018 = NA_integer_,
      trajectory_n = n_distinct(psych_long$id_chr)
    )
fwrite(flow, file.path(path_diag, "r_clhls_sample_flow.csv"))

selection_summary <- function(base, eligible_ids, outcome) {
  x <- base |>
    distinct(
      id_chr, baseline_age, female, education_years, poor_srh_2008
    ) |>
    mutate(trajectory_eligible = id_chr %in% eligible_ids)
  continuous_smd <- function(v) {
    a <- v[x$trajectory_eligible]
    b <- v[!x$trajectory_eligible]
    (mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) /
      sqrt((var(a, na.rm = TRUE) + var(b, na.rm = TRUE)) / 2)
  }
  binary_smd <- function(v) {
    a <- mean(v[x$trajectory_eligible], na.rm = TRUE)
    b <- mean(v[!x$trajectory_eligible], na.rm = TRUE)
    (a - b) / sqrt((a * (1 - a) + b * (1 - b)) / 2)
  }
  bind_rows(
    data.frame(
      outcome, variable = "Baseline age",
      eligible_mean = mean(x$baseline_age[x$trajectory_eligible], na.rm = TRUE),
      noneligible_mean = mean(x$baseline_age[!x$trajectory_eligible], na.rm = TRUE),
      standardized_difference = continuous_smd(x$baseline_age)
    ),
    data.frame(
      outcome, variable = "Female",
      eligible_mean = mean(x$female[x$trajectory_eligible], na.rm = TRUE),
      noneligible_mean = mean(x$female[!x$trajectory_eligible], na.rm = TRUE),
      standardized_difference = binary_smd(x$female)
    ),
    data.frame(
      outcome, variable = "Years of schooling",
      eligible_mean = mean(
        x$education_years[x$trajectory_eligible], na.rm = TRUE
      ),
      noneligible_mean = mean(
        x$education_years[!x$trajectory_eligible], na.rm = TRUE
      ),
      standardized_difference = continuous_smd(x$education_years)
    ),
    data.frame(
      outcome, variable = "Poor self-rated health",
      eligible_mean = mean(
        x$poor_srh_2008[x$trajectory_eligible], na.rm = TRUE
      ),
      noneligible_mean = mean(
        x$poor_srh_2008[!x$trajectory_eligible], na.rm = TRUE
      ),
      standardized_difference = binary_smd(x$poor_srh_2008)
    )
  )
}
selection_audit <- selection_summary(
  d, psych_map$id_chr, "Depressive-affect burden"
)
fwrite(
  selection_audit,
  file.path(path_diag, "r_clhls_trajectory_selection_audit.csv")
)
fwrite(
  data.frame(
      outcome = "Depressive-affect burden",
      baseline_year = 2008,
      center = psych_std$center,
      scale = psych_std$scale,
      baseline_z_mean = mean(
        psych_long$burden_z[psych_long$year == 2008], na.rm = TRUE
      ),
      baseline_z_sd = sd(
        psych_long$burden_z[psych_long$year == 2008], na.rm = TRUE
      ),
      positive_means_greater_burden = TRUE
    ),
  file.path(path_diag, "r_clhls_standardisation_audit.csv")
)

entropy_value <- function(probs) {
  probs <- pmax(as.matrix(probs), 1e-12)
  n <- nrow(probs)
  k <- ncol(probs)
  if (k == 1L) return(1)
  1 + sum(probs * log(probs)) / (n * log(k))
}

posterior_table <- function(model, k, dat) {
  if (k == 1L) {
    return(data.frame(
      ID_num = sort(unique(dat$ID_num)),
      class = 1L,
      prob1 = 1
    ))
  }
  pp <- as.data.frame(model$pprob)
  names(pp)[1] <- "ID_num"
  pp
}

trajectory_labels <- function(k, outcome) {
  if (outcome == "cognition") {
    banks <- list(
      `1` = "Overall mean trajectory",
      `2` = c("High/increasing burden", "Low-stable burden"),
      `3` = c(
        "High/increasing burden", "Intermediate burden", "Low-stable burden"
      ),
      `4` = c(
        "Very-high/increasing burden", "High burden",
        "Intermediate burden", "Low-stable burden"
      ),
      `5` = c(
        "Very-high/increasing burden", "High burden", "Intermediate burden",
        "Low burden", "Very-low/stable burden"
      )
    )
  } else {
    banks <- list(
      `1` = "Overall mean trajectory",
      `2` = c("High/persistent burden", "Low-stable burden"),
      `3` = c(
        "High/persistent burden", "Intermediate burden", "Low-stable burden"
      ),
      `4` = c(
        "Very-high/persistent burden", "High burden",
        "Intermediate burden", "Low-stable burden"
      ),
      `5` = c(
        "Very-high/persistent burden", "High burden", "Intermediate burden",
        "Low burden", "Very-low/stable burden"
      )
    )
  }
  banks[[as.character(k)]]
}

tidy_classmb <- function(model, labels, predictor_labels) {
  beta <- estimates(model)
  vc <- VarCov(model)
  se <- sqrt(diag(vc))
  idx <- unlist(lapply(names(predictor_labels), function(v) {
    grep(paste0("^", v, " class[0-9]+$"), names(beta))
  }))
  cls <- as.integer(sub(".* class", "", names(beta)[idx]))
  data.frame(
    predictor = sub(" class[0-9]+$", "", names(beta)[idx]),
    latent_class = cls,
    reference_class = model$ng,
    log_odds = beta[idx],
    std_error = se[idx],
    stringsAsFactors = FALSE
  ) |>
    mutate(
      predictor_label = unname(predictor_labels[predictor]),
      contrast = paste0(
        labels[as.character(latent_class)], " vs ",
        labels[as.character(reference_class)]
      ),
      odds_ratio = exp(log_odds),
      conf_low = exp(log_odds - 1.96 * std_error),
      conf_high = exp(log_odds + 1.96 * std_error),
      p_value = 2 * pnorm(abs(log_odds / std_error), lower.tail = FALSE)
    ) |>
    group_by(contrast) |>
    mutate(p_fdr = p.adjust(p_value, method = "BH")) |>
    ungroup() |>
    select(
      predictor, predictor_label, contrast, odds_ratio,
      conf_low, conf_high, p_value, p_fdr
    )
}

fit_trajectory <- function(dat, prefix, outcome_key, quadratic, max_classes) {
  fixed_formula <- if (quadratic) {
    burden_z ~ time + I(time^2)
  } else {
    burden_z ~ time
  }
  mixture_formula <- if (quadratic) {
    ~ time + I(time^2)
  } else {
    ~ time
  }
  # gridsearch exports the data object named in the model call to its workers.
  # Keep formulas literal in that call so no unexported formula objects are
  # needed during parallel evaluation.
  assign(".clhls_fit_data", dat, envir = .GlobalEnv)

  cat("\nFitting", prefix, "one-class model\n")
  m1 <- hlme(
    fixed = fixed_formula,
    random = ~ -1,
    subject = "ID_num",
    ng = 1,
    data = dat,
    maxiter = 500,
    verbose = FALSE
  )
  if (m1$conv != 1) stop(prefix, " one-class model did not converge.")

  cache_file <- file.path(
    path_models,
    paste0(
      "r_", prefix, "_candidate_cache_n", n_distinct(dat$ID_num),
      "_o", nrow(dat), "_", ifelse(quadratic, "quadratic", "linear"),
      "_baseline_burden_z.rds"
    )
  )
  models <- list(`1` = m1)
  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (is.list(cached)) {
      models[names(cached)] <- cached
      models[["1"]] <- m1
    }
  }
  for (k in 2:max_classes) {
    if (!is.null(models[[as.character(k)]])) next
    cat("Fitting", prefix, k, "classes\n")
    if (quadratic) {
      models[[as.character(k)]] <- eval(substitute(
        gridsearch(
          rep = 6,
          maxiter = 30,
          minit = m1,
          cl = n_cores,
          hlme(
            fixed = burden_z ~ time + I(time^2),
            mixture = ~ time + I(time^2),
            random = ~ -1,
            subject = "ID_num",
            ng = K,
            nwg = FALSE,
            data = .clhls_fit_data,
            maxiter = 500,
            verbose = FALSE
          )
        ),
        list(K = as.integer(k))
      ))
    } else {
      models[[as.character(k)]] <- eval(substitute(
        gridsearch(
          rep = 6,
          maxiter = 30,
          minit = m1,
          cl = n_cores,
          hlme(
            fixed = burden_z ~ time,
            mixture = ~ time,
            random = ~ -1,
            subject = "ID_num",
            ng = K,
            nwg = FALSE,
            data = .clhls_fit_data,
            maxiter = 500,
            verbose = FALSE
          )
        ),
        list(K = as.integer(k))
      ))
    }
    saveRDS(models, cache_file)
  }

  diagnostics <- bind_rows(lapply(names(models), function(k) {
    m <- models[[k]]
    tmp <- posterior_table(m, as.integer(k), dat)
    prob_cols <- grep("^prob", names(tmp), value = TRUE)
    individual_mpp <- apply(tmp[, prob_cols, drop = FALSE], 1, max)
    tab <- prop.table(table(tmp$class))
    class_mpp <- tapply(individual_mpp, tmp$class, mean)
    data.frame(
      classes = as.integer(k),
      log_likelihood = m$loglik,
      AIC = m$AIC,
      BIC = m$BIC,
      convergence = as.integer(m$conv == 1),
      entropy = entropy_value(tmp[, prob_cols, drop = FALSE]),
      minimum_class_pct = 100 * min(tab),
      maximum_class_pct = 100 * max(tab),
      minimum_mean_posterior = min(class_mpp)
    )
  }))
  admissible <- diagnostics |>
    filter(
      classes >= 2,
      convergence == 1,
      minimum_class_pct >= 10,
      entropy >= 0.70,
      minimum_mean_posterior >= 0.70
    )
  if (!nrow(admissible)) {
    selected_k <- 1L
    cat(
      "No reliable multi-class solution for", prefix,
      "; retaining the one-class trajectory without forced subgrouping.\n"
    )
  } else {
    selected_k <- admissible$classes[which.min(admissible$BIC)]
  }
  selected <- models[[as.character(selected_k)]]
  pp <- posterior_table(selected, selected_k, dat)
  prob_cols <- grep("^prob", names(pp), value = TRUE)
  pp$mean_posterior <- apply(pp[, prob_cols, drop = FALSE], 1, max)

  membership <- pp |>
    transmute(
      ID_num = as.integer(ID_num),
      class = as.integer(class),
      mean_posterior
    ) |>
    left_join(distinct(dat, ID_num, id_chr), by = "ID_num")
  dat_class <- left_join(dat, membership, by = c("ID_num", "id_chr"))
  profiles <- dat_class |>
    group_by(class, year) |>
    summarise(
      n = n(),
      mean_z = mean(burden_z),
      se_z = sd(burden_z) / sqrt(n()),
      conf_low = mean_z - 1.96 * se_z,
      conf_high = mean_z + 1.96 * se_z,
      .groups = "drop"
    )
  last_order <- profiles |>
    filter(year == max(year)) |>
    arrange(desc(mean_z)) |>
    pull(class)
  label_values <- trajectory_labels(selected_k, outcome_key)
  labels <- setNames(label_values, last_order)
  membership$class_label <- labels[as.character(membership$class)]
  dat_class$class_label <- labels[as.character(dat_class$class)]
  profiles$class_label <- labels[as.character(profiles$class)]
  quality <- membership |>
    group_by(class, class_label) |>
    summarise(
      n = n(),
      proportion = n() / nrow(membership),
      mean_posterior = mean(mean_posterior),
      pct_posterior_ge_070 = mean(mean_posterior >= 0.70),
      pct_posterior_ge_080 = mean(mean_posterior >= 0.80),
      .groups = "drop"
    )

  if (selected_k >= 2L) {
    assoc_dat <- dat |>
      mutate(age5 = (baseline_age - 70) / 5) |>
      filter(
        complete.cases(
          age5, female, education_years, poor_srh_2008
        )
      ) |>
      group_by(ID_num) |>
      filter(n_distinct(year) >= 3) |>
      ungroup() |>
      arrange(ID_num, year)
    assign(".clhls_assoc_data", assoc_dat, envir = .GlobalEnv)
    assoc_m1 <- hlme(
      fixed = fixed_formula,
      random = ~ -1,
      subject = "ID_num",
      ng = 1,
      data = assoc_dat,
      maxiter = 500,
      verbose = FALSE
    )
    if (quadratic) {
    association_model <- eval(substitute(
      gridsearch(
        rep = 6,
        maxiter = 30,
        minit = assoc_m1,
        cl = n_cores,
        hlme(
          fixed = burden_z ~ time + I(time^2),
          mixture = ~ time + I(time^2),
          random = ~ -1,
          classmb = ~ age5 + female + education_years + poor_srh_2008,
          subject = "ID_num",
          ng = K,
          nwg = FALSE,
          data = .clhls_assoc_data,
          maxiter = 500,
          verbose = FALSE
        )
      ),
      list(K = as.integer(selected_k))
    ))
    } else {
    association_model <- eval(substitute(
      gridsearch(
        rep = 6,
        maxiter = 30,
        minit = assoc_m1,
        cl = n_cores,
        hlme(
          fixed = burden_z ~ time,
          mixture = ~ time,
          random = ~ -1,
          classmb = ~ age5 + female + education_years + poor_srh_2008,
          subject = "ID_num",
          ng = K,
          nwg = FALSE,
          data = .clhls_assoc_data,
          maxiter = 500,
          verbose = FALSE
        )
      ),
      list(K = as.integer(selected_k))
    ))
    }
    if (association_model$conv != 1) {
      stop(prefix, " baseline-factor model did not converge.")
    }
    assoc_pp <- as.data.frame(association_model$pprob)
    names(assoc_pp)[1] <- "ID_num"
    assoc_profiles <- assoc_dat |>
      left_join(assoc_pp[, c("ID_num", "class")], by = "ID_num") |>
      group_by(class, year) |>
      summarise(mean_z = mean(burden_z), .groups = "drop")
    assoc_order <- assoc_profiles |>
      filter(year == max(year)) |>
      arrange(desc(mean_z)) |>
      pull(class)
    assoc_labels <- setNames(label_values, assoc_order)
    associations <- tidy_classmb(
      association_model,
      assoc_labels,
      c(
        age5 = "Age, per 5 years",
        female = "Female sex",
        education_years = "Education, per year",
        poor_srh_2008 = "Poor self-rated health in 2008"
      )
    )
  } else {
    association_model <- NULL
    associations <- data.frame(
      predictor = character(),
      predictor_label = character(),
      contrast = character(),
      odds_ratio = numeric(),
      conf_low = numeric(),
      conf_high = numeric(),
      p_value = numeric(),
      p_fdr = numeric()
    )
  }

  set.seed(20260726)
  sample_ids <- membership |>
    group_by(class) |>
    group_modify(~slice_sample(.x, n = min(60L, nrow(.x)))) |>
    ungroup() |>
    pull(ID_num)
  spaghetti <- dat_class |> filter(ID_num %in% sample_ids)

  fwrite(
    diagnostics,
    file.path(path_diag, paste0("r_", prefix, "_model_selection.csv"))
  )
  fwrite(
    quality,
    file.path(path_diag, paste0("r_", prefix, "_classification_quality.csv"))
  )
  fwrite(
    membership,
    file.path(path_tables, paste0("r_", prefix, "_membership.csv"))
  )
  fwrite(
    profiles,
    file.path(path_tables, paste0("r_", prefix, "_profiles.csv"))
  )
  fwrite(
    associations,
    file.path(path_tables, paste0("r_", prefix, "_associations.csv"))
  )
  fwrite(
    spaghetti,
    file.path(path_tables, paste0("r_", prefix, "_spaghetti_sample.csv"))
  )
  saveRDS(
    list(
      selected_k = selected_k,
      selected_model = selected,
      candidate_models = models,
      association_model = association_model,
      session = sessionInfo()
    ),
    file.path(path_models, paste0("r_", prefix, "_models.rds"))
  )

  cat("\n", prefix, "selected classes:", selected_k, "\n")
  print(diagnostics)
  print(quality)
  print(associations)
  list(
    selected_k = selected_k,
    diagnostics = diagnostics,
    quality = quality,
    profiles = profiles,
    associations = associations
  )
}

psych_result <- fit_trajectory(
  psych_long,
  prefix = "clhls_psych",
  outcome_key = "psych",
  quadratic = FALSE,
  max_classes = 5
)

cat("\nCompleted:", format(Sys.time()), "\n")
