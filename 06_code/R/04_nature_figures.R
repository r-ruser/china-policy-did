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
  "r_charls_lcga_trajectory_profiles.csv",
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
    y = "Older–younger differential change (95% CI)",
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
    x = "Older x post coefficient (95% CI)",
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
    caption = "Older: age 60–69 in 2015; younger: age 50–59 in 2015. Baseline-free samples are used only for incident outcomes.",
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
      analysis == "CFPS labor DDD" ~ "Employment: older household",
      grepl("Age 75\\+", estimand) & !grepl("or", estimand) ~ "Poor SRH: age 75+",
      TRUE ~ "Poor SRH: high-need combined"
    ),
    label = factor(label, levels = rev(c(
      "Poor SRH: age 75+", "Poor SRH: high-need combined",
      "Employment: older household"
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
  c(palette[["teal"]], palette[["orange"]], palette[["red"]],
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
    "Depressive symptoms" = palette[["red"]]
  ), guide = "none") +
  scale_x_continuous(breaks = c(2020, 2023)) +
  labs(
    x = "Survey year",
    y = "Within-person change vs 2018 (95% CI)",
    title = "Post-policy longitudinal change"
  )

p_class_c <- class_selection |>
  mutate(selection_status = ifelse(classes == 3, "Selected", "Other")) |>
  ggplot(aes(classes, BIC)) +
  geom_vline(xintercept = 3, linetype = "22", linewidth = 0.4,
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
    subtitle = "Three-class primary solution (green)"
  )

fig2 <- p_class_a + (p_class_b / p_class_c) +
  plot_layout(widths = c(1.5, 1)) +
  plot_annotation(
    title = "CLASS: health trajectories among older adults, 2018-2023",
    subtitle = "Primary longitudinal validation; all waves are post-policy and do not identify a policy effect.",
    caption = "The depressive-symptom score uses nine identically worded items across all three waves; positive-affect items are reverse scored.",
    tag_levels = "a"
  )
save_pub_r(fig2, "Figure2_CLASS_primary_longitudinal_trajectories", 183, 118)

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

p4b <- selection |>
  mutate(selection_status = ifelse(classes == 2, "Selected", "Other")) |>
  ggplot(aes(classes, BIC)) +
  geom_vline(xintercept = 2, linetype = "22", linewidth = 0.4,
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

fig4 <- p4a + (p4b / p4c) +
  plot_layout(widths = c(1.5, 1)) +
  plot_annotation(
    title = "Exploratory CHARLS cognitive trajectory classes",
    subtitle = "Latent class growth analysis (1–5 classes); classes are descriptive summaries, not causal exposures.",
    caption = "Cognition combines immediate recall, orientation and serial-7 scores (0–19), standardised to the 2011 distribution.",
    tag_levels = "a"
  )
save_pub_r(fig4, "Figure4_CHARLS_LCGA_trajectories", 183, 112)

cat("All Nature-style figures exported to:", path_figures, "\n")
