#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(grid)
  library(svglite)
  library(ragg)
})

options(encoding = "UTF-8")

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
tab_dir <- file.path(root, "07_results", "tables")
diag_dir <- file.path(root, "07_results", "diagnostics")
fig_dir <- file.path(root, "07_results", "figures")
model_dir <- file.path(root, "07_results", "models")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

ink <- "#202124"
muted <- "#666666"
grid_col <- "#E5E5E5"
charls_col <- "#26456E"
class_col <- "#167D74"
older_col <- "#C44E52"
younger_col <- "#4D6A86"
middle_col <- "#7A7A7A"

theme_pub <- function(base_size = 7.5) {
  theme_classic(base_family = "Arial", base_size = base_size) +
    theme(
      text = element_text(colour = ink),
      axis.text = element_text(colour = ink, size = base_size - 0.2),
      axis.title = element_text(size = base_size),
      axis.line = element_line(linewidth = 0.45, colour = ink),
      axis.ticks = element_line(linewidth = 0.4, colour = ink),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = base_size + 0.2,
                                hjust = 0),
      panel.spacing.x = unit(7, "mm"),
      plot.tag = element_text(face = "bold", size = base_size + 1),
      plot.margin = margin(4, 5, 4, 5)
    )
}

save_pub <- function(plot, filename, width_mm, height_mm, dpi = 600) {
  base <- file.path(fig_dir, filename)
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4

  svglite::svglite(paste0(base, ".svg"), width = width_in,
                   height = height_in)
  print(plot)
  dev.off()

  grDevices::cairo_pdf(paste0(base, ".pdf"), width = width_in,
                       height = height_in, family = "Arial")
  print(plot)
  dev.off()

  ragg::agg_tiff(paste0(base, ".tiff"), width = width_in,
                 height = height_in, units = "in", res = dpi,
                 background = "white", compression = "lzw")
  print(plot)
  dev.off()

  ragg::agg_png(paste0(base, ".png"), width = width_in,
                height = height_in, units = "in", res = 300,
                background = "white")
  print(plot)
  dev.off()
}

flow_panel <- function(cohort, baseline_text, final_boxes, accent) {
  boxes <- bind_rows(
    data.frame(
      xmin = -0.82, xmax = 0.82, ymin = 2.15, ymax = 2.78,
      text = baseline_text, type = "baseline"
    ),
    final_boxes
  )

  p <- ggplot() +
    geom_rect(
      data = boxes,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", colour = ink, linewidth = 0.45
    ) +
    geom_text(
      data = boxes,
      aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = text),
      size = 2.45, lineheight = 0.98, colour = ink
    )

  if (cohort == "CHARLS") {
    p <- p +
      geom_segment(
        aes(x = 0, xend = 0, y = 2.15, yend = 1.02),
        linewidth = 0.45, colour = ink,
        arrow = arrow(length = unit(1.8, "mm"), type = "closed")
      ) +
      geom_segment(
        aes(x = 0, xend = 0.98, y = 1.58, yend = 1.58),
        linewidth = 0.4, colour = muted
      ) +
      annotate(
        "label", x = 1.0, y = 1.58,
        label = "2018 ADL outcome missing\nn = 742",
        hjust = 0, size = 2.2, lineheight = 0.95,
        linewidth = 0.25, label.padding = unit(1.5, "mm"),
        colour = muted, fill = "white"
      )
  } else {
    p <- p +
      geom_segment(
        aes(x = 0, xend = 0, y = 2.15, yend = 1.02),
        linewidth = 0.45, colour = ink,
        arrow = arrow(length = unit(1.8, "mm"), type = "closed")
      ) +
      annotate(
        "text", x = 0.75, y = 1.58, label = "2023 ADL outcome missing\nn = 4,822",
        size = 2.1, colour = muted
      )
  }

  p +
    annotate("segment", x = -1.15, xend = 1.15, y = 3.12, yend = 3.12,
             linewidth = 1.2, colour = accent) +
    annotate("text", x = -1.15, y = 3.27, label = cohort,
             hjust = 0, fontface = "bold", size = 3.1, colour = ink) +
    coord_cartesian(xlim = c(-1.25, 1.85), ylim = c(0.25, 3.45),
                    clip = "off") +
    theme_void(base_family = "Arial", base_size = 7.5) +
    theme(plot.margin = margin(6, 8, 4, 8))
}

charls_final <- data.frame(
  xmin = -0.82, xmax = 0.82, ymin = 0.38, ymax = 1.01,
  text = "Analytic sample\n2018 outcome observed\nn = 4,292",
  type = "final"
)
class_final <- data.frame(
  xmin = -0.82, xmax = 0.82,
  ymin = 0.38, ymax = 1.01,
  text = "2023 analytic sample\nADL outcome observed\nn = 3,651",
  type = "final"
)

fig1a <- flow_panel(
  "CHARLS",
  "2015 baseline\nAge 65+ and no ADL limitation\nn = 5,034",
  charls_final,
  charls_col
)
fig1b <- flow_panel(
  "CLASS",
  "2018 baseline\nAge 65+ and no need for ADL help\nn = 8,473",
  class_final,
  class_col
)
fig1 <- fig1a + fig1b +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 8.5))
save_pub(fig1, "Figure1_study_design_v4", 183, 83)

raw <- fread(file.path(tab_dir, "r_v4_age_group_raw_adl_risks.csv")) |>
  filter(cohort == "CHARLS" | wave == 2023) |>
  mutate(
    estimate = 100 * risk,
    low = 100 * conf_low,
    high = 100 * conf_high,
    interval = case_when(
      cohort == "CHARLS" ~ "CHARLS\n2015-2018",
      TRUE ~ "CLASS\n2018-2023"
    ),
    age_group = factor(age_group, levels = c("65-74", "75+"))
  )
cut75 <- fread(file.path(tab_dir, "r_v4_age_cutpoint_sensitivity.csv")) |>
  filter(grepl("cutpoint 75", specification), cohort == "CHARLS" | wave == 2023) |>
  mutate(
    interval = case_when(
      cohort == "CHARLS" ~ "CHARLS\n2015-2018",
      TRUE ~ "CLASS\n2018-2023"
    ),
    label = sprintf("Adjusted RD %.2f pp\n95%% CI %.2f to %.2f",
                    estimate, conf_low, conf_high)
  )
raw$interval <- factor(
  raw$interval,
  levels = c("CHARLS\n2015-2018", "CLASS\n2018-2023")
)
cut75$interval <- factor(cut75$interval, levels = levels(raw$interval))

fig2 <- ggplot(raw, aes(age_group, estimate, group = interval)) +
  geom_line(linewidth = 0.55, colour = "#A7A7A7") +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.10,
                linewidth = 0.55, colour = ink) +
  geom_point(aes(fill = age_group), shape = 21, size = 2.8,
             stroke = 0.45, colour = "white") +
  geom_text(
    aes(label = sprintf("%.1f%%", estimate)),
    nudge_y = 1.7, size = 2.35, colour = ink
  ) +
  geom_text(
    data = cut75, aes(x = 1.5, y = 29.0, label = label),
    inherit.aes = FALSE, size = 2.25, lineheight = 0.98,
    colour = muted
  ) +
  facet_wrap(~interval, nrow = 1) +
  scale_fill_manual(values = c("65-74" = younger_col, "75+" = older_col),
                    guide = "none") +
  scale_y_continuous(
    limits = c(0, 31), breaks = seq(0, 30, 5),
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(x = "Baseline age group", y = "Incident ADL risk") +
  theme_pub(7.5)
save_pub(fig2, "Figure2_age_group_raw_ADL_risk_v4", 183, 80)

models <- readRDS(file.path(model_dir, "r_v4_required_models.rds"))
curve <- fread(file.path(tab_dir, "r_v4_standardized_adl_age_splines.csv")) |>
  filter(cohort == "CHARLS" | wave == 2023) |>
  mutate(
    risk = 100 * standardized_risk,
    low = 100 * conf_low,
    high = 100 * conf_high,
    series = case_when(
      cohort == "CHARLS" ~ "CHARLS: 2015-2018",
      TRUE ~ "CLASS: 2018-2023"
    ),
    panel = case_when(
      cohort == "CHARLS" ~ "CHARLS\n2015-2018",
      TRUE ~ "CLASS\n2018-2023"
    )
  )

rcs_test <- function(fit, age_var, cohort, wave, panel) {
  dat <- fit$data
  response <- all.vars(formula(fit))[1]
  labels <- attr(terms(fit), "term.labels")
  covars <- labels[!grepl("^ns\\(", labels)]
  null_fit <- glm(reformulate(covars, response), data = dat,
                  family = binomial())
  linear_fit <- glm(reformulate(c(age_var, covars), response), data = dat,
                    family = binomial())
  p_overall <- anova(null_fit, fit, test = "Chisq")[2, "Pr(>Chi)"]
  p_nonlinear <- anova(linear_fit, fit, test = "Chisq")[2, "Pr(>Chi)"]
  data.frame(
    cohort, wave, panel, p_overall, p_nonlinear,
    label = sprintf(
      "P-overall %s\nP-nonlinear %s",
      ifelse(p_overall < 0.001, "<0.001", sprintf("=%.3f", p_overall)),
      ifelse(p_nonlinear < 0.001, "<0.001",
             sprintf("=%.3f", p_nonlinear))
    )
  )
}

rcs_tests <- bind_rows(
  rcs_test(models$charls$spline, "age_2015", "CHARLS", 2018,
           "CHARLS\n2015-2018"),
  rcs_test(models$class$splines[[2]], "age_2018", "CLASS", 2023,
           "CLASS\n2018-2023")
)
fwrite(rcs_tests, file.path(diag_dir, "r_v4_rcs_tests.csv"))

curve$panel <- factor(
  curve$panel,
  levels = c("CHARLS\n2015-2018", "CLASS\n2018-2023")
)
rcs_annotation <- bind_rows(
  rcs_tests |>
    filter(cohort == "CHARLS") |>
    transmute(
      panel = "CHARLS\n2015-2018",
      label = sprintf("P-overall <0.001\nP-nonlinear = %.3f",
                      p_nonlinear)
    ),
  rcs_tests |>
    filter(cohort == "CLASS") |>
    transmute(
      panel = "CLASS\n2018-2023",
      label = sprintf("P-overall <0.001\nP-nonlinear = %.3f", p_nonlinear)
    )
)
rcs_annotation$panel <- factor(
  rcs_annotation$panel,
  levels = levels(curve$panel)
)

fig3 <- ggplot(curve, aes(baseline_age, risk, colour = series,
                          fill = series)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.14,
              colour = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 75, linetype = "22", linewidth = 0.4,
             colour = "#777777") +
  geom_text(
    data = rcs_annotation,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE, hjust = -0.08, vjust = 1.35,
    size = 2.15, lineheight = 0.98, colour = muted
  ) +
  facet_wrap(~panel, nrow = 1) +
  scale_colour_manual(
    values = c(
      "CHARLS: 2015-2018" = charls_col,
      "CLASS: 2018-2020" = class_col,
      "CLASS: 2018-2023" = "#45A79C"
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "CHARLS: 2015-2018" = charls_col,
      "CLASS: 2018-2020" = class_col,
      "CLASS: 2018-2023" = "#45A79C"
    ),
    guide = "none"
  ) +
  scale_x_continuous(breaks = c(65, 70, 75, 80)) +
  scale_y_continuous(
    limits = c(0, 30), breaks = seq(0, 30, 5),
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Baseline age (years)",
       y = "Standardized incident ADL risk") +
  theme_pub(7.3)
save_pub(fig3, "Figure3_replication_age_gradient_sensitivity_v4",
         183, 82)

charls_prof <- fread(
  file.path(tab_dir, "r_charls_lcga_trajectory_profiles.csv")
)
charls_q <- fread(
  file.path(diag_dir, "r_charls_lcga_classification_quality.csv")
)
class_prof <- fread(file.path(tab_dir, "r_class_lcga_profiles.csv"))
class_q <- fread(file.path(diag_dir, "r_class_lcga_quality.csv"))

charls_prof <- left_join(
  charls_prof,
  charls_q |> select(class, proportion),
  by = "class"
) |>
  mutate(
    label = sprintf("%s (%.1f%%)", class_label, 100 * proportion),
    class_label = factor(
      class_label,
      levels = c("Higher-stable", "Intermediate", "Lower/declining")
    )
  )
class_prof <- left_join(
  class_prof,
  class_q |> select(class, proportion),
  by = "class"
) |>
  mutate(
    label = sprintf("%s (%.1f%%)", class_label, 100 * proportion),
    class_label = factor(
      class_label,
      levels = c("Low-stable", "Moderate", "High/persistent")
    )
  )

charls_labels <- charls_prof |>
  group_by(class_label) |>
  filter(year == max(year)) |>
  ungroup()
class_labels <- class_prof |>
  group_by(class_label) |>
  filter(year == max(year)) |>
  ungroup()

p4a <- ggplot(
  charls_prof,
  aes(year, mean_z, colour = class_label, fill = class_label,
      group = class_label)
) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.6) +
  geom_text(
    data = charls_labels,
    aes(x = year + 0.22, label = label),
    hjust = 0, size = 2.15, show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Higher-stable" = "#2B6C9F",
      "Intermediate" = middle_col,
      "Lower/declining" = older_col
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Higher-stable" = "#2B6C9F",
      "Intermediate" = middle_col,
      "Lower/declining" = older_col
    ),
    guide = "none"
  ) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018),
                     limits = c(2011, 2020.2)) +
  labs(x = "Survey year", y = "Cognition z score",
       title = "CHARLS cognition") +
  coord_cartesian(clip = "off") +
  theme_pub(7.3) +
  theme(plot.title = element_text(face = "bold", size = 7.8))

p4b <- ggplot(
  class_prof,
  aes(year, mean_score, colour = class_label, fill = class_label,
      group = class_label)
) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.6) +
  geom_text(
    data = class_labels,
    aes(x = year + 0.15, label = label),
    hjust = 0, size = 2.15, show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Low-stable" = "#2B6C9F",
      "Moderate" = middle_col,
      "High/persistent" = older_col
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "Low-stable" = "#2B6C9F",
      "Moderate" = middle_col,
      "High/persistent" = older_col
    ),
    guide = "none"
  ) +
  scale_x_continuous(breaks = c(2018, 2020, 2023),
                     limits = c(2018, 2025.1)) +
  labs(x = "Survey year", y = "Depressive-symptom score",
       title = "CLASS depressive symptoms") +
  coord_cartesian(clip = "off") +
  theme_pub(7.3) +
  theme(plot.title = element_text(face = "bold", size = 7.8))

fig4 <- p4a + p4b +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 8.5))
save_pub(fig4, "Figure4_secondary_trajectory_heterogeneity_v4",
         183, 86)

cat("Reference-informed V4 figure set regenerated in", fig_dir, "\n")
