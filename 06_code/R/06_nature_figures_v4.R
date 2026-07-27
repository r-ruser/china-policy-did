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
tables <- file.path(root, "07_results", "tables")
diag <- file.path(root, "07_results", "diagnostics")
figures <- file.path(root, "07_results", "figures")
dir.create(figures, recursive = TRUE, showWarnings = FALSE)

pal <- c(
  "#2F4B7C", "#C95A4A", "#3A9D8F", "#E28E2C",
  "#8C6BB1", "#5B8FD6"
)
theme_set(
  theme_classic(base_size = 8, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.35),
      axis.ticks = element_line(linewidth = 0.35),
      strip.background = element_rect(fill = "#F2F2F2", colour = NA),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(colour = "#444444"),
      plot.caption = element_text(hjust = 0, colour = "#444444", size = 6.5)
    )
)

save_pub <- function(plot, name, width_mm = 183, height_mm = 125) {
  base <- file.path(figures, name)
  width <- width_mm / 25.4
  height <- height_mm / 25.4
  svglite(paste0(base, ".svg"), width = width, height = height)
  print(plot)
  dev.off()
  agg_tiff(
    paste0(base, ".tiff"), width = width, height = height,
    units = "in", res = 600, compression = "lzw"
  )
  print(plot)
  dev.off()
  agg_png(
    paste0(base, ".png"), width = width, height = height,
    units = "in", res = 300
  )
  print(plot)
  dev.off()
}

main_results <- fread(file.path(tables, "r_corrected_main_results.csv"))
charls_event <- fread(file.path(tables, "r_charls_event_study.csv"))

adl_event <- charls_event |>
  filter(outcome == "ADL prevalence") |>
  mutate(period = ifelse(year > 2015, "Post", "Pre/reference"))
p_main_a <- ggplot(adl_event, aes(year, estimate)) +
  geom_hline(yintercept = 0, linetype = "22", colour = "#888888") +
  geom_vline(xintercept = 2016, linetype = "32", colour = "#888888") +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high, colour = period),
    width = 0.12
  ) +
  geom_line(colour = "#444444", linewidth = 0.55) +
  geom_point(aes(colour = period), size = 2) +
  scale_colour_manual(
    values = c("Pre/reference" = "#2F4B7C", "Post" = "#C95A4A"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    x = "Survey year",
    y = "Age-group differential change (95% CI)",
    title = "a  ADL event-study diagnostic"
  )

charls_forest <- main_results |>
  filter(grepl("^CHARLS", analysis)) |>
  mutate(
    unit = ifelse(
      grepl("CESD", outcome),
      "CESD-10 score difference",
      "Risk difference (percentage points)"
    ),
    multiplier = ifelse(grepl("CESD", outcome), 1, 100),
    estimate_plot = estimate * multiplier,
    low_plot = conf_low * multiplier,
    high_plot = conf_high * multiplier,
    outcome = recode(
      outcome,
      "Incident ADL (IPCW)" = "Incident ADL, IPCW"
    )
  )
p_main_b <- ggplot(charls_forest, aes(estimate_plot, outcome)) +
  geom_vline(xintercept = 0, linetype = "22", colour = "#888888") +
  geom_errorbar(
    aes(xmin = low_plot, xmax = high_plot),
    orientation = "y", width = 0.14
  ) +
  geom_point(size = 1.9, colour = "#2F4B7C") +
  facet_wrap(~unit, scales = "free", ncol = 1) +
  labs(
    x = "Age 75+ x post coefficient (95% CI)",
    y = NULL,
    title = "b  Primary and secondary period contrasts"
  )

fig_main <- p_main_a + p_main_b +
  plot_layout(widths = c(1.1, 1)) +
  plot_annotation(
    title = "CHARLS age-group differential period changes",
    subtitle = paste(
      "The estimates describe age-group period differences and do not",
      "identify a national policy effect."
    )
  )
save_pub(fig_main, "Figure2_CHARLS_corrected_DID_v4", 183, 105)

read_profile <- function(file, panel) {
  fread(file.path(tables, file)) |>
    mutate(panel = panel)
}
profiles <- bind_rows(
  read_profile(
    "r_charls_lcga_trajectory_profiles.csv",
    "CHARLS CESD-10 (2011-2018)"
  ),
  read_profile(
    "r_class_lcga_profiles.csv",
    "CLASS depressive symptoms (2018-2023)"
  ),
  read_profile(
    "r_clhls_psych_profiles.csv",
    "CLHLS depressive-affect burden (2008-2014)"
  )
) |>
  mutate(
    class_label = factor(class_label, levels = unique(class_label)),
    panel = factor(
      panel,
      levels = c(
        "CHARLS CESD-10 (2011-2018)",
        "CLASS depressive symptoms (2018-2023)",
        "CLHLS depressive-affect burden (2008-2014)"
      )
    )
  )

read_spaghetti <- function(file, panel, id_field, score_field) {
  x <- fread(file.path(tables, file))
  x |>
    transmute(
      panel = panel,
      person = as.character(.data[[id_field]]),
      year,
      burden_z = .data[[score_field]],
      class_label
    )
}
spaghetti <- bind_rows(
  read_spaghetti(
    "r_charls_lcga_spaghetti_sample.csv",
    "CHARLS CESD-10 (2011-2018)", "ID_num", "depression_z"
  ),
  read_spaghetti(
    "r_class_lcga_spaghetti.csv",
    "CLASS depressive symptoms (2018-2023)", "ID_num", "depression_z"
  ),
  read_spaghetti(
    "r_clhls_psych_spaghetti_sample.csv",
    "CLHLS depressive-affect burden (2008-2014)", "ID_num", "burden_z"
  )
) |>
  mutate(panel = factor(panel, levels = levels(profiles$panel)))

class_colours <- c(
  "High/increasing burden" = "#C95A4A",
  "Intermediate burden" = "#3A9D8F",
  "Low-stable burden" = "#2F4B7C",
  "Moderate burden" = "#3A9D8F",
  "High/persistent burden" = "#C95A4A",
  "Overall mean trajectory" = "#2F4B7C"
)

fig3 <- ggplot(
  profiles,
  aes(year, mean_z, colour = class_label, group = class_label)
) +
  geom_hline(yintercept = 0, linetype = "22", colour = "#9A9A9A") +
  geom_line(
    data = spaghetti,
    aes(
      year, burden_z,
      colour = class_label,
      group = interaction(panel, person)
    ),
    inherit.aes = FALSE,
    alpha = 0.10,
    linewidth = 0.28
  ) +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.1) +
  facet_wrap(~panel, scales = "free", ncol = 3) +
  scale_colour_manual(values = class_colours) +
  scale_x_continuous(breaks = sort(unique(profiles$year))) +
  labs(
    x = "Survey year",
    y = "Fixed-baseline burden z score",
    title = "Depressive-symptom burden trajectories across three ageing cohorts",
    subtitle = "Positive values consistently indicate greater health burden",
    caption = paste(
      "Every participant contributed at least three valid measurements;",
      "CLASS and CLHLS depressive-affect trajectories required all",
      "three specified waves. CLHLS b21-b27 is a depressive-affect proxy,",
      "not CES-D; its one-class solution reflects the prespecified",
      "classification thresholds."
    )
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))
save_pub(fig3, "Figure3_combined_trajectory_heterogeneity_v4", 183, 105)

read_assoc <- function(file, panel) {
  x <- fread(file.path(tables, file))
  if (!nrow(x)) return(NULL)
  x |> mutate(panel = panel)
}
associations <- bind_rows(
  read_assoc(
    "r_charls_lcga_associations.csv", "CHARLS CESD-10"
  ),
  read_assoc(
    "r_class_lcga_associations.csv", "CLASS depressive symptoms"
  ),
  read_assoc(
    "r_clhls_psych_associations.csv", "CLHLS depressive-affect burden"
  )
) |>
  mutate(
    row_label = paste(predictor_label, contrast, sep = "\n"),
    row_label = factor(row_label, levels = rev(unique(row_label)))
  )

fig5 <- ggplot(
  associations,
  aes(odds_ratio, row_label, colour = panel)
) +
  geom_vline(xintercept = 1, linetype = "22", colour = "#888888") +
  geom_errorbarh(
    aes(xmin = conf_low, xmax = conf_high),
    height = 0.16, linewidth = 0.5
  ) +
  geom_point(size = 1.8) +
  facet_wrap(~panel, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_x_log10() +
  labs(
    x = "Odds ratio for the indicated class contrast (log scale)",
    y = NULL,
    title = "Baseline factors associated with trajectory membership",
    subtitle = "Point estimates and 95% confidence intervals",
    caption = paste(
      "CLHLS depressive-affect burden has no class-membership model because",
      "its reliable solution was one class. FDR-adjusted",
      "p values are retained in the underlying result tables."
    )
  )
save_pub(fig5, "Figure5_trajectory_class_associations_v4", 183, 175)

event <- fread(file.path(tables, "r_cfps_event_study.csv"))
did_event <- event |>
  filter(analysis == "CFPS pilot-area event study")
ddd_event <- event |>
  filter(analysis == "CFPS pilot-area DDD event study") |>
  mutate(
    need_group = case_when(
      grepl("Age 75\\+", interaction) ~ "Age ≥75",
      TRUE ~ "Age ≥75 or activity limitation"
    )
  )

p_s2a <- ggplot(
  did_event,
  aes(year, estimate)
) +
  geom_hline(yintercept = 0, linetype = "22", colour = "#888888") +
  geom_vline(xintercept = 2016, linetype = "32", colour = "#888888") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.12) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = c(2012, 2014, 2018)) +
  labs(
    x = "Survey year",
    y = "Pilot-area interaction (95% CI)",
    title = "a  Pilot-area event-study estimates"
  )

p_s2b <- ggplot(
  ddd_event,
  aes(year, estimate, colour = need_group, group = need_group)
) +
  geom_hline(yintercept = 0, linetype = "22", colour = "#888888") +
  geom_vline(xintercept = 2016, linetype = "32", colour = "#888888") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.12) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c("#3A9D8F", "#C95A4A")) +
  scale_x_continuous(breaks = c(2012, 2014, 2018)) +
  labs(
    x = "Survey year",
    y = "Triple-difference interaction (95% CI)",
    colour = NULL,
    title = "b  Health triple differences"
  )

fig_s2 <- p_s2a / p_s2b +
  plot_annotation(
    title = "CFPS poor-self-rated-health analyses",
    subtitle = "Pilot-area DID and prespecified high-need DDD contrasts",
    caption = paste(
      "Models include individual and year fixed effects with city-clustered",
      "standard errors. CFPS depressive-symptom measures were not sufficiently",
      "comparable across three waves, so no depression DID was forced."
    )
  )
save_pub(fig_s2, "Supplementary_Figure_S2_CFPS_DID_DDD_v4", 183, 135)

cat("V4 figures completed.\n")
