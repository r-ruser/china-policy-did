#!/usr/bin/env Rscript

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
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
tab_dir <- file.path(root, "07_results", "tables")
diag_dir <- file.path(root, "07_results", "diagnostics")
fig_dir <- file.path(root, "07_results", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

palette <- c(
  young = "#4C78A8",
  old = "#C95A4A",
  charls = "#2F4B7C",
  class = "#3A9D8F",
  neutral = "#6F6F6F",
  light = "#D9D9D9",
  high = "#C95A4A",
  middle = "#C7A33D",
  low = "#3A9D8F"
)

theme_nature <- function(base_size = 7) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = element_line(linewidth = 0.35, colour = "black"),
      axis.text = element_text(colour = "#222222", size = base_size - 0.4),
      axis.title = element_text(colour = "#111111", size = base_size),
      legend.title = element_text(size = base_size - 0.2),
      legend.text = element_text(size = base_size - 0.4),
      legend.key.height = unit(3, "mm"),
      strip.text = element_text(face = "bold", size = base_size),
      strip.background = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 0.6,
                                hjust = 0),
      plot.subtitle = element_text(size = base_size - 0.2,
                                   colour = "#444444"),
      plot.tag = element_text(face = "bold", size = 8),
      panel.grid = element_blank(),
      plot.margin = margin(4, 5, 4, 5)
    )
}
theme_set(theme_nature())

save_pub <- function(plot, filename, width_mm = 183, height_mm = 120,
                     dpi = 600) {
  base <- file.path(fig_dir, filename)
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  svglite::svglite(paste0(base, ".svg"), width = w, height = h)
  print(plot)
  dev.off()
  grDevices::cairo_pdf(paste0(base, ".pdf"), width = w, height = h,
                       family = "Arial")
  print(plot)
  dev.off()
  ragg::agg_tiff(paste0(base, ".tiff"), width = w, height = h,
                 units = "in", res = dpi, background = "white")
  print(plot)
  dev.off()
  ragg::agg_png(paste0(base, ".png"), width = w, height = h,
                units = "in", res = 300, background = "white")
  print(plot)
  dev.off()
}

# Figure contract
# Core conclusion: Policy expansion is temporal context; CHARLS and CLASS test
# age equity, while only CFPS supplies a geographic quasi-experimental contrast.
# Archetype: schematic-led composite. Hero: aligned policy/cohort timeline.

timeline <- data.frame(
  cohort = c(rep("CHARLS", 4), rep("CLASS", 3), rep("CFPS", 3)),
  year = c(2011, 2013, 2015, 2018, 2018, 2020, 2023, 2012, 2014, 2018),
  role = c(rep("National ageing cohort", 7),
           rep("Pilot-area comparison", 3))
)
timeline$cohort <- factor(timeline$cohort,
                          levels = c("CFPS", "CLASS", "CHARLS"))
p1a <- ggplot(timeline, aes(year, cohort)) +
  geom_segment(data = data.frame(
    cohort = factor(c("CHARLS", "CLASS", "CFPS"),
                    levels = levels(timeline$cohort)),
    xmin = c(2011, 2018, 2012), xmax = c(2018, 2023, 2018)
  ), aes(x = xmin, xend = xmax, y = cohort, yend = cohort),
  inherit.aes = FALSE, linewidth = 1, colour = "#B6B6B6") +
  geom_vline(xintercept = 2016, linetype = "22", linewidth = 0.55,
             colour = unname(palette["old"])) +
  geom_point(aes(fill = cohort), shape = 21, size = 2.7,
             colour = "white", stroke = 0.35) +
  scale_fill_manual(values = c(CFPS = unname(palette["neutral"]),
                               CLASS = unname(palette["class"]),
                               CHARLS = unname(palette["charls"])),
                    guide = "none") +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2016, 2018, 2020, 2023),
                     limits = c(2010.5, 2023.5)) +
  annotate("label", x = 2016, y = 3.35,
           label = "National expansion\n(temporal context)",
           size = 2.25, linewidth = 0.25,
           colour = unname(palette["old"]),
           fill = "white") +
  labs(x = "Survey year", y = NULL,
       title = "Policy timeline and analytic roles",
       subtitle = "CHARLS and CLASS estimate age inequalities, not a national causal policy effect") +
  theme(axis.ticks.y = element_blank())

flow <- data.frame(
  x = c(1, 2, 3),
  y = c(1, 1, 1),
  cohort = c("CHARLS", "CLASS", "CFPS"),
  text = c(
    "2015 age 65+\nNo baseline ADL: 5,034\nObserved in 2018: 4,292",
    "2018 age 65+\nNo baseline ADL help: 8,473\nObserved: 6,669 (2020), 3,651 (2023)",
    "2014 age 65+\nPilot vs non-pilot areas\nSupplementary quasi-experiment"
  )
)
p1b <- ggplot(flow, aes(x = x, y = y)) +
  geom_label(aes(label = text, colour = cohort),
             hjust = 0.5, size = 2.15, lineheight = 0.95,
             linewidth = 0.3, fill = "white",
             label.padding = unit(2.2, "mm")) +
  scale_colour_manual(values = c(CHARLS = unname(palette["charls"]),
                                 CLASS = unname(palette["class"]),
                                 CFPS = unname(palette["neutral"])),
                      guide = "none") +
  coord_cartesian(xlim = c(0.45, 3.55), ylim = c(0.65, 1.35),
                  clip = "off") +
  labs(title = "Independent cohort-specific analytic samples") +
  theme_void(base_family = "Arial", base_size = 7) +
  theme(plot.title = element_text(face = "bold", size = 7.6),
        plot.margin = margin(4, 8, 4, 8))

fig1 <- p1a / p1b + plot_layout(heights = c(1.3, 0.8)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 8))
save_pub(fig1, "Figure1_study_design_v4", 183, 112)

# Figure contract
# Core conclusion: Incident ADL risk was consistently higher at ages 75+ in
# both national cohorts.
# Archetype: quantitative grid. Hero: directly comparable raw-risk panels.

raw <- fread(file.path(tab_dir, "r_v4_age_group_raw_adl_risks.csv")) |>
  mutate(
    estimate = 100 * risk,
    low = 100 * conf_low,
    high = 100 * conf_high,
    panel = case_when(
      cohort == "CHARLS" ~ "CHARLS: 2015 to 2018",
      wave == 2020 ~ "CLASS: 2018 to 2020",
      TRUE ~ "CLASS: 2018 to 2023"
    ),
    age_group = factor(age_group, levels = c("65-74", "75+"))
  )
p2 <- ggplot(raw, aes(age_group, estimate, colour = age_group)) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.12,
                linewidth = 0.55) +
  geom_point(size = 2.7) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", estimate,
                                format(n, big.mark = ","))),
            vjust = -1.05, size = 2.25, show.legend = FALSE) +
  facet_wrap(~panel, nrow = 1) +
  scale_colour_manual(values = c("65-74" = unname(palette["young"]),
                                 "75+" = unname(palette["old"])),
                      guide = "none") +
  scale_y_continuous(limits = c(0, 31), breaks = seq(0, 30, 5),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Baseline age group", y = "Incident ADL risk (%)",
       title = "Incident ADL risk by baseline age group",
       subtitle = "Samples were restricted to participants without ADL limitation or need for help at baseline") +
  theme_nature() +
  theme(panel.spacing.x = unit(7, "mm"))
save_pub(p2, "Figure2_age_group_raw_ADL_risk_v4", 183, 86)

# Figure contract
# Core conclusion: Incident functional risk increased continuously with
# baseline age in each national cohort. Follow-up intervals, rather than
# outcome years alone, are shown to avoid a cross-sectional interpretation.
# Archetype: paired quantitative panels with a common risk scale.
spline <- fread(file.path(tab_dir, "r_v4_standardized_adl_age_splines.csv")) |>
  mutate(
    risk = 100 * standardized_risk,
    low = 100 * conf_low,
    high = 100 * conf_high,
    comparison = case_when(
      cohort == "CHARLS" ~ "CHARLS: 2015-2018",
      wave == 2020 ~ "CLASS: 2018-2020",
      TRUE ~ "CLASS: 2018-2023"
    )
  )

p3a <- ggplot(filter(spline, cohort == "CHARLS"),
              aes(baseline_age, risk, colour = comparison,
                  fill = comparison)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.10,
              colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 75, linetype = "22", linewidth = 0.35,
             colour = "#777777") +
  scale_colour_manual(values = c(
    "CHARLS: 2015-2018" = unname(palette["charls"])
  )) +
  scale_fill_manual(values = c(
    "CHARLS: 2015-2018" = unname(palette["charls"])
  )) +
  scale_y_continuous(labels = label_number(suffix = "%"),
                     limits = c(0, 45)) +
  labs(x = "Baseline age (years)", y = "Standardized incident ADL risk",
       colour = NULL, fill = NULL,
       title = "CHARLS restricted cubic spline") +
  theme_nature(6.5) +
  theme(legend.position = c(0.03, 0.97),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha("white", 0.85),
                                         colour = NA))

p3b <- ggplot(filter(spline, cohort == "CLASS"),
              aes(baseline_age, risk, colour = comparison,
                  fill = comparison)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.10,
              colour = NA) +
  geom_line(aes(linetype = comparison), linewidth = 0.8) +
  geom_vline(xintercept = 75, linetype = "22", linewidth = 0.35,
             colour = "#777777") +
  scale_colour_manual(values = c(
    "CLASS: 2018-2020" = unname(palette["class"]),
    "CLASS: 2018-2023" = "#79B8AE"
  )) +
  scale_fill_manual(values = c(
    "CLASS: 2018-2020" = unname(palette["class"]),
    "CLASS: 2018-2023" = "#79B8AE"
  )) +
  scale_linetype_manual(values = c(
    "CLASS: 2018-2020" = "solid",
    "CLASS: 2018-2023" = "22"
  )) +
  scale_y_continuous(labels = label_number(suffix = "%"),
                     limits = c(0, 45)) +
  labs(x = "Baseline age (years)", y = NULL,
       colour = NULL, fill = NULL, linetype = NULL,
       title = "CLASS restricted cubic splines") +
  theme_nature(6.5) +
  theme(legend.position = c(0.03, 0.97),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha("white", 0.85),
                                         colour = NA))

fig3 <- p3a + p3b + plot_layout(widths = c(1, 1.12)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 8))
save_pub(fig3, "Figure3_replication_age_gradient_sensitivity_v4", 183, 112)

# Figure contract
# Core conclusion: Cohort averages conceal a lower/declining cognitive subgroup
# in CHARLS and a high/persistent depressive-symptom subgroup in CLASS.
# Archetype: quantitative grid; trajectories are secondary evidence.

charls_prof <- fread(file.path(tab_dir,
                               "r_charls_lcga_trajectory_profiles.csv"))
charls_q <- fread(file.path(diag_dir,
                            "r_charls_lcga_classification_quality.csv"))
class_prof <- fread(file.path(tab_dir, "r_class_lcga_profiles.csv"))
class_q <- fread(file.path(diag_dir, "r_class_lcga_quality.csv"))

charls_prof <- left_join(charls_prof, charls_q |>
                           select(class, proportion, mean_posterior),
                         by = "class") |>
  mutate(
    cohort = "CHARLS cognition",
    display = sprintf("%s: %.1f%%, MPP %.2f",
                      class_label, 100 * proportion, mean_posterior)
  )
class_prof <- left_join(class_prof, class_q |>
                          select(class, proportion, mean_posterior),
                        by = "class") |>
  mutate(
    mean_z = mean_score,
    cohort = "CLASS depressive symptoms",
    display = sprintf("%s: %.1f%%, MPP %.2f",
                      class_label, 100 * proportion, mean_posterior)
  )
traj <- bind_rows(charls_prof, class_prof) |>
  mutate(
    direction = case_when(
      grepl("Lower/declining|High/persistent", display) ~ "Less favourable",
      grepl("Intermediate|Moderate", display) ~ "Intermediate",
      TRUE ~ "More favourable"
    )
  )
p4 <- ggplot(traj, aes(year, mean_z, colour = direction, group = display)) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high, fill = direction),
              alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.7) +
  geom_text(
    data = traj |> group_by(cohort, display) |>
      filter(year == max(year)) |> ungroup(),
    aes(x = year - 0.12, label = display),
    hjust = 1, size = 1.8, lineheight = 0.9, show.legend = FALSE
  ) +
  facet_wrap(~cohort, scales = "free", nrow = 1) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018, 2020, 2023)) +
  scale_colour_manual(values = c(
    "More favourable" = unname(palette["low"]),
    "Intermediate" = unname(palette["middle"]),
    "Less favourable" = unname(palette["high"])
  )) +
  scale_fill_manual(values = c(
    "More favourable" = unname(palette["low"]),
    "Intermediate" = unname(palette["middle"]),
    "Less favourable" = unname(palette["high"])
  )) +
  labs(x = "Survey year",
       y = "Cognition z score / depressive-symptom score",
       colour = NULL, fill = NULL,
       title = "Secondary trajectory evidence",
       subtitle = "Higher cognition is favourable; higher depressive-symptom scores are unfavourable") +
  theme_nature(6.5) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 5.8),
        panel.spacing.x = unit(9, "mm"),
        plot.margin = margin(4, 5, 4, 5)) +
  coord_cartesian(clip = "off")
save_pub(p4, "Figure4_secondary_trajectory_heterogeneity_v4", 183, 105)

cat("Generated four V4 main figures in", fig_dir, "\n")
