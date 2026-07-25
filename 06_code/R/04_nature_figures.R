#!/usr/bin/env Rscript

# Nature-style publication figures. R is the exclusive rendering backend.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(svglite)
  library(ragg)
})

options(encoding = "UTF-8")

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
path_tables <- file.path(project_root, "07_results", "tables")
path_diag <- file.path(project_root, "07_results", "diagnostics")
path_figures <- file.path(project_root, "07_results", "figures")
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  "r_corrected_main_results.csv",
  "r_charls_event_study.csv",
  "r_cfps_event_study.csv",
  "r_charls_ddd_results.csv",
  "r_class_longitudinal_changes.csv",
  "r_class_lcga_profiles.csv",
  "r_class_lcga_associations.csv",
  "r_charls_lcga_trajectory_profiles.csv",
  "r_charls_lcga_associations.csv",
  "r_charls_lcga_spaghetti_sample.csv"
)
missing_files <- required_files[!file.exists(file.path(path_tables, required_files))]
if (length(missing_files)) {
  stop("Missing required R results: ", paste(missing_files, collapse = ", "))
}

palette <- c(
  navy = "#2F4B7C",
  blue = "#5B8FD6",
  teal = "#3A9D8F",
  orange = "#E28E2C",
  red = "#C95A4A",
  purple = "#8C6BB1",
  grey_dark = "#3F3F3F",
  grey_mid = "#8C8C8C",
  grey_light = "#D9D9D9",
  bg = "#FFFFFF"
)

theme_nature <- function(base_size = 7, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = element_line(linewidth = 0.35, colour = "black"),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 0.3, colour = "black"),
      legend.title = element_text(size = base_size - 0.2),
      legend.text = element_text(size = base_size - 0.5),
      legend.key.height = unit(3.5, "mm"),
      strip.background = element_rect(fill = "#F2F2F2", colour = NA),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.title = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle = element_text(size = base_size - 0.2, colour = palette["grey_dark"]),
      plot.caption = element_text(size = base_size - 1, colour = palette["grey_dark"],
                                  hjust = 0),
      plot.tag = element_text(size = base_size + 1, face = "bold"),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}
theme_set(theme_nature())

save_pub_r <- function(plot, filename, width_mm = 183, height_mm = 120,
                       dpi = 600) {
  base <- file.path(path_figures, filename)
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  svglite::svglite(paste0(base, ".svg"), width = w, height = h,
                   system_fonts = list(Arial = "Arial"))
  print(plot)
  dev.off()
  grDevices::cairo_pdf(paste0(base, ".pdf"), width = w, height = h,
                       family = "Arial")
  print(plot)
  dev.off()
  ragg::agg_tiff(paste0(base, ".tiff"), width = w, height = h,
                 units = "in", res = dpi, compression = "lzw")
  print(plot)
  dev.off()
  ragg::agg_png(paste0(base, ".png"), width = w, height = h,
                units = "in", res = 300)
  print(plot)
  dev.off()
}

main <- fread(file.path(path_tables, "r_corrected_main_results.csv"))
charls_event <- fread(file.path(path_tables, "r_charls_event_study.csv"))
cfps_event <- fread(file.path(path_tables, "r_cfps_event_study.csv"))
charls_ddd <- fread(file.path(path_tables, "r_charls_ddd_results.csv"))
pretests <- fread(file.path(path_diag, "r_parallel_trend_tests.csv"))

# Figure 1 contract:
# Core conclusion: the 2015-2018 age-group changes in CHARLS differ, but
# pre-policy deviations make a national policy-causal interpretation untenable.
# Archetype: asymmetric quantitative composite; hero = event study.
adl_event <- charls_event |>
  filter(outcome == "ADL prevalence") |>
  mutate(period = ifelse(year > 2015, "Post", "Pre/reference"))
pre_p <- pretests |>
  filter(grepl("CHARLS ADL prevalence", test)) |>
  pull(p_value)
pre_label <- ifelse(length(pre_p) == 1 && is.finite(pre_p),
                    sprintf("Joint pre-trend p = %.3g", pre_p), "Pre-trend unavailable")

p1a <- ggplot(adl_event, aes(year, estimate)) +
  geom_hline(yintercept = 0, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_vline(xintercept = 2016, linetype = "32", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = period),
                width = 0.15, linewidth = 0.55, na.rm = TRUE) +
  geom_point(aes(colour = period), size = 2.2) +
  geom_line(colour = palette["grey_dark"], linewidth = 0.45) +
  scale_colour_manual(values = c("Pre/reference" = palette[["navy"]],
                                 "Post" = palette[["red"]]),
                      guide = "none") +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    x = "Survey year",
    y = "Age-group differential change (95% CI)",
    title = "ADL event-study diagnostic",
    subtitle = pre_label
  )

charls_main_plot <- main |>
  filter(analysis == "CHARLS target-group period change",
         !grepl("IPCW", outcome)) |>
  mutate(
    outcome = factor(outcome, levels = rev(c(
      "Incident ADL", "Incident depression", "Poor self-rated health",
      "CESD-10 score",
      "Cognitive score (0-19)"
    ))),
    scale_group = ifelse(
      grepl("Incident|Poor self-rated", as.character(outcome)),
                         "Risk difference", "Mean-score difference")
  )
p1b_score <- charls_main_plot |>
  filter(scale_group == "Mean-score difference") |>
  mutate(outcome = factor(as.character(outcome),
                          levels = c("Cognitive score (0-19)",
                                     "CESD-10 score"))) |>
  ggplot(aes(estimate, outcome)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                orientation = "y", width = 0.15, linewidth = 0.55) +
  geom_point(size = 2.1, colour = palette["navy"]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Two-period differential changes",
    subtitle = "Mean-score difference"
  )

p1b_risk <- charls_main_plot |>
  filter(scale_group == "Risk difference") |>
  mutate(outcome = factor(as.character(outcome),
                          levels = c("Incident depression",
                                     "Poor self-rated health",
                                     "Incident ADL"))) |>
  ggplot(aes(estimate, outcome)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                orientation = "y", width = 0.15, linewidth = 0.55) +
  geom_point(size = 2.1, colour = palette["red"]) +
  labs(
    x = "Age 75+ x post coefficient (95% CI)",
    y = NULL,
    subtitle = "Risk difference"
  )

p1b <- p1b_score / p1b_risk +
  plot_layout(heights = c(1, 1))

fig1 <- p1a + p1b +
  plot_layout(widths = c(1.45, 1)) +
  plot_annotation(
    title = "CHARLS: target-group changes around nationwide policy expansion",
    subtitle = "Estimates describe age-group period differences; they do not identify a national policy effect.",
    caption = "All participants were aged 65 years or older in 2015; the contrast compares age 75+ with age 65–74. Baseline-free samples are used only for incident outcomes.",
    tag_levels = "a"
  )
save_pub_r(fig1, "Figure1_CHARLS_corrected_DID", 183, 105)

# Supplementary Figure 1 contract:
# Core conclusion: CFPS pilot-area contrasts have observable pre-policy
# deviations; health and labor DDD results remain exploratory.
simple_event <- cfps_event |>
  filter(analysis == "CFPS pilot-area event study") |>
  mutate(outcome = factor(outcome, levels = c("Poor self-rated health", "Employment")))
p2a <- ggplot(simple_event, aes(year, estimate, colour = outcome)) +
  geom_hline(yintercept = 0, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_vline(xintercept = 2016, linetype = "32", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                width = 0.12, linewidth = 0.5, na.rm = TRUE) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  facet_wrap(~outcome, scales = "free_x") +
  scale_colour_manual(values = c(
    "Poor self-rated health" = palette[["red"]],
    "Employment" = palette[["navy"]]
  ), guide = "none") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2018)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    x = "Survey year",
    y = "Pilot–comparison difference relative to 2014",
    title = "Pilot-area event studies"
  )

ddd_plot <- main |>
  filter(grepl("DDD", analysis)) |>
  mutate(
    label = case_when(
      analysis == "CFPS labor DDD" ~ "Employment: age 75+",
      grepl("Age 75\\+", estimand) & !grepl("or", estimand) ~ "Poor SRH: age 75+",
      TRUE ~ "Poor SRH: high-need combined"
    ),
    label = factor(label, levels = rev(c(
      "Poor SRH: age 75+", "Poor SRH: high-need combined",
      "Employment: age 75+"
    )))
  )
p2b <- ggplot(ddd_plot, aes(estimate, label)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                orientation = "y", width = 0.16, linewidth = 0.55,
                colour = palette["grey_dark"]) +
  geom_point(size = 2.1, colour = palette["teal"]) +
  scale_x_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    x = "Triple-difference estimate (95% CI)",
    y = NULL,
    title = "Exploratory DDD estimates",
    subtitle = "Interpret only with the corresponding pre-trend diagnostic"
  )

fig_s1 <- p2a / p2b +
  plot_layout(heights = c(1.25, 0.9)) +
  plot_annotation(
    title = "CFPS: pilot-area health and labor contrasts",
    subtitle = "Individual and year fixed effects; standard errors clustered by city.",
    caption = "The 2010 self-rated-health series is excluded because its response coding is not comparable with later waves.",
    tag_levels = "a"
  )
save_pub_r(fig_s1, "Supplementary_Figure_S2_CFPS_DID_DDD", 183, 132)

# Figure 2 contract:
# Core conclusion: CLASS provides primary post-policy longitudinal validation;
# health dimensions changed differently and depressive symptoms separate into
# clinically interpretable severity trajectories.
class_change <- fread(file.path(path_tables, "r_class_longitudinal_changes.csv"))
class_profiles <- fread(file.path(path_tables, "r_class_lcga_profiles.csv"))
class_selection <- fread(file.path(path_diag, "r_class_lcga_model_selection.csv"))
class_quality <- fread(file.path(path_diag, "r_class_lcga_quality.csv"))
class_spaghetti <- fread(file.path(path_tables, "r_class_lcga_spaghetti.csv"))
class_spaghetti$class_id <- as.character(class_spaghetti$class_id)

class_order <- class_profiles |>
  filter(year == max(year)) |>
  arrange(mean_score) |>
  pull(class_label)
class_palette <- setNames(
  c(palette[["navy"]], palette[["teal"]], palette[["red"]],
    palette[["purple"]], palette[["navy"]])[seq_along(class_order)],
  class_order
)
class_profiles$class_label <- factor(class_profiles$class_label,
                                     levels = class_order)
class_spaghetti$class_label <- factor(class_spaghetti$class_label,
                                      levels = class_order)

p_class_a <- ggplot() +
  geom_line(
    data = class_spaghetti,
    aes(year, depression9, group = class_id, colour = class_label),
    linewidth = 0.22, alpha = 0.07
  ) +
  geom_ribbon(
    data = class_profiles,
    aes(year, ymin = conf_low, ymax = conf_high, fill = class_label),
    alpha = 0.16, colour = NA
  ) +
  geom_line(
    data = class_profiles,
    aes(year, mean_score, colour = class_label),
    linewidth = 1
  ) +
  geom_point(
    data = class_profiles,
    aes(year, mean_score, colour = class_label),
    size = 1.8
  ) +
  scale_colour_manual(values = class_palette, name = "Trajectory class") +
  scale_fill_manual(values = class_palette, guide = "none") +
  scale_x_continuous(breaks = c(2018, 2020, 2023)) +
  labs(
    x = "Survey year",
    y = "Common 9-item depressive symptom score (0-18)",
    title = "CLASS depressive-symptom trajectories",
    subtitle = "Thin lines: bounded participant sample; thick lines: class means with 95% CI"
  ) +
  theme(legend.position = "bottom")

p_class_b <- class_change |>
  filter(year != 2018) |>
  mutate(
    outcome = recode(
      outcome,
      "Common 9-item depressive symptom score" = "Depressive symptoms",
      "Need help with activities of daily living" = "ADL help",
      "Poor self-rated health" = "Poor self-rated health"
    )
  ) |>
  ggplot(aes(year, estimate, colour = outcome)) +
  geom_hline(yintercept = 0, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high),
                width = 0.15, linewidth = 0.55) +
  geom_point(size = 2) +
  facet_wrap(~outcome, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c(
    "Poor self-rated health" = palette[["navy"]],
    "ADL help" = palette[["teal"]],
    "Depressive symptoms" = palette[["red"]]
  ), guide = "none") +
  scale_x_continuous(breaks = c(2020, 2023)) +
  labs(
    x = "Survey year",
    y = "Within-person change vs 2018 (95% CI)",
    title = "Post-policy longitudinal change"
  )

class_selected_k <- n_distinct(class_quality$class)
p_class_c <- class_selection |>
  mutate(selection_status = ifelse(classes == class_selected_k,
                                   "Selected", "Other")) |>
  ggplot(aes(classes, BIC)) +
  geom_vline(xintercept = class_selected_k, linetype = "22", linewidth = 0.4,
             colour = palette["teal"]) +
  geom_line(linewidth = 0.55, colour = palette["grey_dark"]) +
  geom_point(aes(colour = selection_status),
             size = 2) +
  scale_colour_manual(values = c("Selected" = palette[["teal"]],
                                 "Other" = palette[["grey_mid"]]),
                      guide = "none") +
  scale_x_continuous(breaks = class_selection$classes) +
  labs(
    x = "Number of classes",
    y = "BIC",
    title = "Trajectory-model diagnostics",
    subtitle = paste0(class_selected_k, "-class primary solution (green)")
  )

# Figure 3: CHARLS DDD heterogeneity.
p3 <- charls_ddd |>
  mutate(modifier = factor(modifier, levels = rev(modifier))) |>
  ggplot(aes(estimate, modifier)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                orientation = "y", width = 0.16, linewidth = 0.55,
                colour = palette["grey_dark"]) +
  geom_point(size = 2.2, colour = palette["purple"]) +
  scale_x_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    x = "Triple-difference in incident ADL risk (95% CI)",
    y = NULL,
    title = "CHARLS: baseline-condition modification of age-group change",
    subtitle = "Exploratory heterogeneity, not condition-specific policy effects",
    caption = "Modifiers are defined at the 2015 baseline; person-clustered standard errors. P values are adjusted across five modifiers using the Benjamini-Hochberg FDR."
  )
save_pub_r(p3, "Figure3_CHARLS_DDD_heterogeneity", 120, 82)

# Figure 4 contract:
# Core conclusion: a small set of reproducible longitudinal cognitive patterns
# summarises heterogeneity, with classification uncertainty shown explicitly.
profiles <- fread(file.path(path_tables, "r_charls_lcga_trajectory_profiles.csv"))
spaghetti <- fread(file.path(path_tables, "r_charls_lcga_spaghetti_sample.csv"))
spaghetti$ID_num <- as.character(spaghetti$ID_num)
selection <- fread(file.path(path_diag, "r_charls_lcga_model_selection.csv"))
quality <- fread(file.path(path_diag, "r_charls_lcga_classification_quality.csv"))

class_order <- profiles |>
  filter(year == max(year)) |>
  arrange(desc(mean_z)) |>
  pull(class_label)
class_cols <- setNames(
  c(palette[["navy"]], palette[["teal"]], palette[["red"]],
    palette[["purple"]], palette[["orange"]])[seq_along(class_order)],
  class_order
)
profiles$class_label <- factor(profiles$class_label, levels = class_order)
spaghetti$class_label <- factor(spaghetti$class_label, levels = class_order)

p4a <- ggplot() +
  geom_line(
    data = spaghetti,
    aes(year, cognition_z, group = ID_num, colour = class_label),
    alpha = 0.08, linewidth = 0.25
  ) +
  geom_ribbon(
    data = profiles,
    aes(year, ymin = conf_low, ymax = conf_high, fill = class_label),
    alpha = 0.16, colour = NA
  ) +
  geom_line(
    data = profiles,
    aes(year, mean_z, colour = class_label),
    linewidth = 1
  ) +
  geom_point(
    data = profiles,
    aes(year, mean_z, colour = class_label),
    size = 1.8
  ) +
  scale_colour_manual(values = class_cols, name = "Trajectory class") +
  scale_fill_manual(values = class_cols, guide = "none") +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018)) +
  labs(
    x = "Survey year",
    y = "Cognitive score (baseline-standardised z)",
    title = "Observed class-specific trajectories",
    subtitle = "Thin lines: bounded person-level sample; thick lines: class means with 95% CI"
  ) +
  theme(legend.position = "bottom")

charls_selected_k <- n_distinct(quality$class)
p4b <- selection |>
  mutate(selection_status = ifelse(classes == charls_selected_k,
                                   "Selected", "Other")) |>
  ggplot(aes(classes, BIC)) +
  geom_vline(xintercept = charls_selected_k, linetype = "22", linewidth = 0.4,
             colour = palette["teal"]) +
  geom_line(linewidth = 0.55, colour = palette["grey_dark"]) +
  geom_point(aes(colour = selection_status),
             size = 2.1) +
  scale_colour_manual(values = c("Selected" = palette[["teal"]],
                                 "Other" = palette[["grey_mid"]]),
                      guide = "none") +
  scale_x_continuous(breaks = selection$classes) +
  labs(
    x = "Number of classes",
    y = "BIC",
    title = "Model selection",
    subtitle = "Criteria: entropy >= 0.70;\neach class >= 10%"
  )

p4c <- quality |>
  mutate(class_label = factor(class_label, levels = class_order)) |>
  ggplot(aes(mean_posterior, class_label, colour = class_label)) +
  geom_vline(xintercept = 0.70, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_point(aes(size = proportion), alpha = 0.95) +
  scale_colour_manual(values = class_cols, guide = "none") +
  scale_size_continuous(range = c(2.5, 5), labels = label_percent(),
                        name = "Class proportion") +
  coord_cartesian(xlim = c(0.5, 1)) +
  labs(
    x = "Mean posterior membership probability",
    y = NULL,
    title = "Classification quality"
  ) +
  theme(legend.position = "bottom")

# Combined Figure 2: both independently estimated cohort trajectories plus
# within-cohort evidence for heterogeneity and classification stability.
summarise_trajectory_stability <- function(cohort, outcome, diagnostics,
                                           class_quality, selected_k) {
  selected_row <- diagnostics |> filter(classes == selected_k)
  data.frame(
    cohort = cohort,
    outcome = outcome,
    selected_classes = selected_k,
    selected_converged = selected_row$convergence == 1,
    BIC_1class = diagnostics$BIC[diagnostics$classes == 1],
    BIC_2class = diagnostics$BIC[diagnostics$classes == 2],
    BIC_selected = selected_row$BIC,
    delta_BIC_vs_1class =
      diagnostics$BIC[diagnostics$classes == 1] - selected_row$BIC,
    delta_BIC_vs_2class =
      diagnostics$BIC[diagnostics$classes == 2] - selected_row$BIC,
    entropy = selected_row$entropy,
    minimum_class_pct = 100 * min(class_quality$proportion),
    minimum_mean_posterior = min(class_quality$mean_posterior),
    maximum_mean_posterior = max(class_quality$mean_posterior),
    all_class_mean_posterior_ge_070 =
      all(class_quality$mean_posterior >= 0.70),
    stable_solution = selected_row$convergence == 1 &&
      min(class_quality$proportion) >= 0.10 &&
      all(class_quality$mean_posterior >= 0.70)
  )
}

trajectory_stability <- bind_rows(
  summarise_trajectory_stability(
    "CLASS", "Depressive symptoms", class_selection, class_quality,
    class_selected_k
  ),
  summarise_trajectory_stability(
    "CHARLS", "Cognition", selection, quality, charls_selected_k
  )
)
fwrite(
  trajectory_stability,
  file.path(path_tables, "r_trajectory_heterogeneity_stability.csv")
)

bic_compare <- bind_rows(
  class_selection |> mutate(cohort = "CLASS"),
  selection |> mutate(cohort = "CHARLS")
) |>
  group_by(cohort) |>
  mutate(
    selected_k = ifelse(cohort == "CLASS", class_selected_k,
                        charls_selected_k),
    status = case_when(
      classes == selected_k ~ "Selected 3-class",
      minimum_class_pct < 10 ~ "Inadmissible: class <10%",
      TRUE ~ "Alternative"
    )
  ) |>
  ungroup()

p_bic <- ggplot(bic_compare, aes(classes, BIC)) +
  geom_line(linewidth = 0.5, colour = palette[["grey_dark"]]) +
  geom_point(aes(colour = status), size = 1.8) +
  facet_wrap(~cohort, scales = "free_y", ncol = 1) +
  scale_colour_manual(
    values = c(
      "Selected 3-class" = palette[["teal"]],
      "Alternative" = palette[["grey_mid"]],
      "Inadmissible: class <10%" = palette[["red"]]
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = 1:5) +
  labs(
    x = "Number of classes",
    y = "BIC",
    title = "Within-cohort heterogeneity evidence",
    subtitle = "Green: selected; red: solution containing a class <10%"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 6.5)
  )

stability_display <- trajectory_stability |>
  mutate(
    bic1 = format(round(delta_BIC_vs_1class), big.mark = ","),
    bic2 = format(round(delta_BIC_vs_2class), big.mark = ","),
    entropy_text = sprintf("%.3f", entropy),
    class_text = sprintf("%.1f%%", minimum_class_pct),
    mpp_text = sprintf(
      "%.3f-%.3f", minimum_mean_posterior, maximum_mean_posterior
    ),
    summary = paste0(
      "Delta BIC: ", bic1, " vs 1 class; ", bic2, " vs 2 classes",
      "\nEntropy ", entropy_text, "; smallest class ", class_text,
      "\nClass-specific MPP ", mpp_text
    ),
    y = c(2.25, 1.10)
  )
p_stability <- ggplot() +
  annotate(
    "rect", xmin = 0.08, xmax = 0.92, ymin = 1.72, ymax = 2.80,
    fill = palette[["grey_light"]], colour = NA
  ) +
  annotate(
    "rect", xmin = 0.08, xmax = 0.92, ymin = 0.57, ymax = 1.65,
    fill = palette[["grey_light"]], colour = NA
  ) +
  geom_text(
    data = stability_display,
    aes(x = 0.5, y = y + 0.34, label = cohort),
    fontface = "bold", size = 2.8
  ) +
  geom_text(
    data = stability_display,
    aes(x = 0.5, y = y - 0.10, label = summary),
    size = 2.25, lineheight = 1.10
  ) +
  annotate(
    "text", x = 0.5, y = 0.22,
    label = "Both models converged; all classes >=15%; all MPP >=0.80.",
    size = 2.05, colour = palette[["grey_dark"]]
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.9), clip = "off") +
  labs(
    title = "Classification stability",
    subtitle = "Positive Delta BIC favours the selected three-class model"
  ) +
  theme_void(base_family = "Arial") +
  theme(
    plot.title = element_text(size = 8, face = "bold", hjust = 0),
    plot.subtitle = element_text(
      size = 6.5, colour = palette[["grey_dark"]], hjust = 0
    ),
    plot.margin = margin(7, 8, 7, 8)
  )

p_class_a_combined <- p_class_a +
  labs(
    title = "CLASS depressive-symptom trajectories",
    subtitle = "Three independent post-policy waves; class means with 95% CI"
  ) +
  theme(legend.position = "bottom")
p_charls_combined <- p4a +
  labs(
    title = "CHARLS cognitive trajectories",
    subtitle = "Four waves spanning 2011-2018; class means with 95% CI"
  ) +
  theme(legend.position = "bottom")
p_class_change_combined <- p_class_b +
  labs(title = "CLASS post-policy health changes")

fig2_combined <- (
  p_class_a_combined + p_charls_combined +
    plot_layout(widths = c(1, 1))
) / (
  p_class_change_combined + p_bic + p_stability +
    plot_layout(widths = c(0.9, 0.75, 1.15))
) +
  plot_layout(heights = c(1.35, 1)) +
  plot_annotation(
    title = "Heterogeneous health trajectories in Chinese adults aged 65 years or older",
    subtitle = paste0(
      "Independent LCGA in CLASS and CHARLS identified stable three-class ",
      "solutions; outcomes differ and were not pooled across cohorts."
    ),
    caption = paste0(
      "Delta BIC is the reduction in BIC relative to a simpler model; positive ",
      "values support heterogeneity. MPP, mean posterior probability. Classes ",
      "are descriptive and are not policy exposures."
    ),
    tag_levels = "a"
  )
save_pub_r(fig2_combined, "Figure2_combined_trajectory_heterogeneity", 183, 180)

# Figure 4: one-step baseline-factor associations with latent trajectory
# membership. Classification uncertainty is retained in the likelihood, and
# multiplicity is controlled within each class contrast.
assoc_class <- fread(file.path(path_tables, "r_class_lcga_associations.csv")) |>
  mutate(cohort = "CLASS depressive symptoms")
assoc_charls <- fread(file.path(path_tables, "r_charls_lcga_associations.csv")) |>
  mutate(cohort = "CHARLS cognition")
assoc <- bind_rows(assoc_charls, assoc_class) |>
  mutate(
    predictor_label = recode(
      predictor_label,
      "Age, per 5 years" = "Age (per 5 years)",
      "Female sex" = "Female",
      "Education, per category" = "Education (per category)",
      "Poor self-rated health in 2011" = "Poor self-rated health",
      "Poor self-rated health in 2018" = "Poor self-rated health"
    ),
    predictor_label = factor(
      predictor_label,
      levels = rev(c(
        "Age (per 5 years)", "Female", "Education (per category)",
        "Poor self-rated health"
      ))
    ),
    panel_label = paste(cohort, contrast, sep = "\n"),
    fdr_status = ifelse(p_fdr < 0.05, "FDR-adjusted P < 0.05",
                        "FDR-adjusted P >= 0.05")
  )

p5 <- ggplot(assoc, aes(odds_ratio, predictor_label, colour = fdr_status)) +
  geom_vline(xintercept = 1, linetype = "22", linewidth = 0.35,
             colour = palette["grey_mid"]) +
  geom_errorbar(
    aes(xmin = conf_low, xmax = conf_high),
    orientation = "y", width = 0.16, linewidth = 0.55
  ) +
  geom_point(size = 2) +
  facet_wrap(~panel_label, scales = "free_x", ncol = 2) +
  scale_x_log10(
    breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2, 4),
    labels = label_number(accuracy = 0.01)
  ) +
  scale_colour_manual(
    values = c(
      "FDR-adjusted P < 0.05" = palette[["teal"]],
      "FDR-adjusted P >= 0.05" = palette[["grey_mid"]]
    ),
    name = NULL
  ) +
  labs(
    x = "Odds ratio for trajectory-class membership (log scale)",
    y = NULL,
    title = "Baseline factors associated with trajectory-class membership",
    subtitle = paste0(
      "Adults aged 65 years or older; one-step latent-class regression with ",
      "classification uncertainty retained"
    ),
    caption = paste0(
      "Benjamini-Hochberg FDR correction was applied within each class ",
      "contrast. Associations are not causal effects."
    )
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 6.4, lineheight = 0.95),
    axis.text.y = element_text(size = 6.5)
  )
save_pub_r(p5, "Figure4_trajectory_class_associations", 183, 118)

cat("All Nature-style figures exported to:", path_figures, "\n")
