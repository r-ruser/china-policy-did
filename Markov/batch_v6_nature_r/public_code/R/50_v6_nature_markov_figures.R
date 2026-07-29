#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(svglite)
  library(ragg)
  library(scales)
})

project_root <- normalizePath(
  Sys.getenv("MARKOV_PROJECT_ROOT", unset = getwd()),
  winslash = "/",
  mustWork = TRUE
)
input_dir <- Sys.getenv(
  "MARKOV_INPUT_DIR",
  unset = file.path(project_root, "Markov", "data")
)
output_root <- Sys.getenv(
  "MARKOV_OUTPUT_DIR",
  unset = file.path(
    project_root,
    "Markov",
    "batch_v6_nature_r",
    "private_outputs"
  )
)
figure_dir <- file.path(output_root, "figures")
table_dir <- file.path(output_root, "tables")
qa_dir <- file.path(output_root, "qa")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  "r_corrected_main_results.csv",
  "r_cfps_event_study.csv",
  "CHARLS_common_2year_transition_probabilities.csv",
  "CHARLS_cluster_bootstrap_2000_corrected_results.csv",
  "CHARLS_state_ADL_validation_corrected.csv",
  "CLASS_concurrent_FI_ADL_PR.csv"
)
missing_files <- required_files[
  !file.exists(file.path(input_dir, required_files))
]
if (length(missing_files) > 0L) {
  stop(
    "Missing required input files: ",
    paste(missing_files, collapse = ", ")
  )
}

ink <- "#202124"
neutral_dark <- "#4B4F54"
neutral_mid <- "#7A7F85"
neutral_light <- "#D9DDE1"
navy <- "#315A86"
teal <- "#2A8C82"
coral <- "#C95A50"
gold <- "#B4872D"

theme_nature_v6 <- function(base_size = 7, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(colour = ink),
      axis.line = element_line(linewidth = 0.35, colour = ink),
      axis.ticks = element_line(linewidth = 0.35, colour = ink),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 0.3, colour = ink),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 0.5),
      legend.key.height = unit(3.5, "mm"),
      legend.key.width = unit(5, "mm"),
      plot.title = element_text(
        size = base_size + 0.4,
        face = "bold",
        margin = margin(b = 2.5)
      ),
      plot.subtitle = element_text(
        size = base_size - 0.4,
        colour = neutral_mid,
        margin = margin(b = 4)
      ),
      plot.tag = element_text(
        size = base_size + 1.2,
        face = "bold",
        colour = ink
      ),
      strip.text = element_text(size = base_size - 0.1, face = "bold"),
      panel.grid = element_blank(),
      plot.margin = margin(5, 7, 5, 7)
    )
}

theme_set(theme_nature_v6())
skip_pdf <- identical(Sys.getenv("MARKOV_SKIP_PDF", unset = "0"), "1")

save_nature <- function(plot, filename, width_mm, height_mm, dpi = 600) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4
  base <- file.path(figure_dir, filename)

  svglite::svglite(
    paste0(base, ".svg"),
    width = width_in,
    height = height_in
  )
  print(plot)
  dev.off()

  if (!skip_pdf) {
    grDevices::cairo_pdf(
      paste0(base, ".pdf"),
      width = width_in,
      height = height_in,
      family = "Arial"
    )
    print(plot)
    dev.off()
  }

  ragg::agg_tiff(
    paste0(base, ".tiff"),
    width = width_in,
    height = height_in,
    units = "in",
    res = dpi,
    compression = "lzw"
  )
  print(plot)
  dev.off()

  ragg::agg_png(
    paste0(base, ".png"),
    width = width_in,
    height = height_in,
    units = "in",
    res = 300
  )
  print(plot)
  dev.off()
}

transition_label <- function(from, to) {
  states <- c("Low deficit", "Intermediate", "High deficit")
  paste(states[from], "\u2192", states[to])
}

transition_role <- function(from, to) {
  fifelse(
    from == to,
    "Persistence",
    fifelse(to < from, "Recovery", "Deterioration")
  )
}

# Figure 1: evidence architecture ---------------------------------------------

timeline <- data.table(
  survey = rep(c("CFPS", "CHARLS", "CLASS"), c(3L, 4L, 4L)),
  year = c(
    2012, 2014, 2018,
    2011, 2013, 2015, 2018,
    2016, 2018, 2020, 2023
  )
)
timeline[, survey := factor(survey, levels = c("CFPS", "CHARLS", "CLASS"))]
timeline[, role := fcase(
  survey == "CFPS", "Pilot-area DID and DDD",
  survey == "CHARLS", "Longitudinal health-state transitions",
  survey == "CLASS", "External construct corroboration"
)]

role_labels <- timeline[
  ,
  .(
    role = role[1],
    label_x = max(year) + 0.55
  ),
  by = survey
]

figure_1 <- ggplot(timeline, aes(x = year, y = survey)) +
  geom_line(
    aes(group = survey),
    colour = neutral_light,
    linewidth = 1.0
  ) +
  geom_point(
    aes(
      colour = survey,
      shape = survey
    ),
    size = 2.7,
    stroke = 0.6
  ) +
  geom_vline(
    xintercept = 2016,
    linewidth = 0.55,
    linetype = "22",
    colour = coral
  ) +
  annotate(
    "text",
    x = 2016,
    y = 3.55,
    label = "2016 national expansion",
    hjust = 0.5,
    size = 2.5,
    colour = coral,
    family = "Arial"
  ) +
  geom_text(
    data = role_labels,
    aes(x = label_x, label = role, colour = survey),
    hjust = 0,
    size = 2.45,
    family = "Arial",
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(CFPS = coral, CHARLS = navy, CLASS = teal)
  ) +
  scale_shape_manual(
    values = c(CFPS = 16, CHARLS = 17, CLASS = 15)
  ) +
  scale_x_continuous(
    breaks = c(2011:2016, 2018, 2020, 2023),
    limits = c(2010.5, 2028.6),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_discrete(expand = expansion(add = c(0.45, 0.72))) +
  labs(x = "Survey year", y = NULL) +
  theme_nature_v6(base_size = 7.2) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 7.4),
    legend.position = "none",
    plot.margin = margin(8, 10, 5, 8)
  )

fwrite(
  timeline,
  file.path(table_dir, "Figure1_source_data.csv")
)
save_nature(figure_1, "Figure1_evidence_architecture", 183, 62)

# Figure 2: CFPS pilot-area estimates -----------------------------------------

main_results <- fread(
  file.path(input_dir, "r_corrected_main_results.csv")
)
cfps <- main_results[grepl("^CFPS pilot-area", analysis)]
cfps[, display := fcase(
  analysis == "CFPS pilot-area DID", "Average DID",
  grepl("Age 75\\+ at baseline$", estimand), "Age 75+ DDD",
  default = "Higher-need DDD"
)]
cfps[, `:=`(
  estimate_pp = 100 * estimate,
  conf_low_pp = 100 * conf_low,
  conf_high_pp = 100 * conf_high
)]
cfps[, display := factor(
  display,
  levels = rev(c("Average DID", "Age 75+ DDD", "Higher-need DDD"))
)]
cfps[, estimate_text := sprintf(
  "%+.1f  (%+.1f, %+.1f)",
  estimate_pp,
  conf_low_pp,
  conf_high_pp
)]

figure_2a <- ggplot(
  cfps,
  aes(x = estimate_pp, y = display, colour = display, shape = display)
) +
  geom_vline(xintercept = 0, colour = neutral_mid, linewidth = 0.4) +
  geom_errorbar(
    aes(xmin = conf_low_pp, xmax = conf_high_pp),
    width = 0.13,
    linewidth = 0.6,
    orientation = "y"
  ) +
  geom_point(size = 2.4, stroke = 0.7) +
  geom_text(
    aes(x = 8.0, label = estimate_text),
    hjust = 0,
    colour = ink,
    size = 2.35,
    family = "Arial",
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Average DID" = coral,
      "Age 75+ DDD" = navy,
      "Higher-need DDD" = teal
    )
  ) +
  scale_shape_manual(
    values = c(
      "Average DID" = 16,
      "Age 75+ DDD" = 17,
      "Higher-need DDD" = 15
    )
  ) +
  scale_x_continuous(
    breaks = seq(-25, 15, by = 5),
    limits = c(-26, 20),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    tag = "a",
    title = "Policy estimates",
    subtitle = "Estimate (95% CI), percentage points",
    x = "Change in poor self-rated health (pp)",
    y = NULL
  ) +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  )

event_study <- fread(
  file.path(input_dir, "r_cfps_event_study.csv")
)
event_study <- event_study[
  analysis == "CFPS pilot-area event study"
]
event_study[, `:=`(
  estimate_pp = 100 * estimate,
  conf_low_pp = 100 * conf_low,
  conf_high_pp = 100 * conf_high
)]

figure_2b <- ggplot(
  event_study,
  aes(x = year, y = estimate_pp)
) +
  geom_hline(yintercept = 0, colour = neutral_mid, linewidth = 0.4) +
  geom_vline(
    xintercept = 2016,
    colour = coral,
    linewidth = 0.5,
    linetype = "22"
  ) +
  geom_line(colour = navy, linewidth = 0.65) +
  geom_errorbar(
    aes(ymin = conf_low_pp, ymax = conf_high_pp),
    width = 0.16,
    linewidth = 0.5,
    colour = navy
  ) +
  geom_point(
    shape = 21,
    size = 2.4,
    stroke = 0.7,
    fill = "white",
    colour = navy
  ) +
  annotate(
    "text",
    x = 2014,
    y = 1.35,
    label = "Reference",
    size = 2.25,
    colour = neutral_mid,
    family = "Arial"
  ) +
  scale_x_continuous(
    breaks = c(2012, 2014, 2018),
    limits = c(2011.4, 2018.6)
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),
    limits = c(-11, 11)
  ) +
  labs(
    tag = "b",
    title = "Event-study diagnostic",
    subtitle = "Pilot-area contrast relative to 2014",
    x = "Survey year",
    y = "Difference (pp)"
  )

figure_2 <- figure_2a + figure_2b +
  plot_layout(widths = c(1.35, 1))

fwrite(
  cfps[, .(
    analysis,
    estimand,
    display,
    estimate_pp,
    conf_low_pp,
    conf_high_pp,
    p_value,
    n_obs,
    n_id
  )],
  file.path(table_dir, "Figure2a_source_data.csv")
)
fwrite(
  event_study,
  file.path(table_dir, "Figure2b_source_data.csv")
)
save_nature(figure_2, "Figure2_CFPS_policy_estimates", 183, 82)

# Figure 3: state-dependent transition dynamics -------------------------------

common_prob <- fread(
  file.path(input_dir, "CHARLS_common_2year_transition_probabilities.csv")
)
common_long <- melt(
  common_prob,
  id.vars = c("period", "from"),
  measure.vars = c("p_low", "p_mid", "p_high"),
  variable.name = "destination",
  value.name = "probability"
)
common_long[, to := match(
  destination,
  c("p_low", "p_mid", "p_high")
)]
common_long[, transition := transition_label(from, to)]
common_long[, role := transition_role(from, to)]
common_long[, probability_pct := 100 * probability]
common_long[, period := factor(
  period,
  levels = c("pre-expansion", "expansion"),
  labels = c("Pre-expansion", "Expansion")
)]

bootstrap <- fread(
  file.path(
    input_dir,
    "CHARLS_cluster_bootstrap_2000_corrected_results.csv"
  )
)
bootstrap[, transition := transition_label(from, to)]
bootstrap[, role := transition_role(from, to)]
bootstrap[, `:=`(
  difference_pp = 100 * original_diff,
  ci_low_pp = 100 * ci_low,
  ci_high_pp = 100 * ci_high
)]

transition_levels <- unlist(lapply(1:3, function(i) {
  transition_label(rep(i, 3), 1:3)
}))
common_long[, transition := factor(
  transition,
  levels = rev(transition_levels)
)]
bootstrap[, transition := factor(
  transition,
  levels = rev(transition_levels)
)]

paired_prob <- dcast(
  common_long,
  transition + role ~ period,
  value.var = "probability_pct"
)

state_nodes <- data.table(
  x = 1:3,
  y = 0,
  state = c("Low deficit", "Intermediate", "High deficit"),
  node_colour = c(teal, gold, coral)
)
forward_arrows <- data.table(
  x = c(1.25, 2.25),
  xend = c(1.75, 2.75),
  y = c(0.08, 0.08),
  yend = c(0.08, 0.08)
)
backward_arrows <- data.table(
  x = c(1.75, 2.75),
  xend = c(1.25, 2.25),
  y = c(-0.08, -0.08),
  yend = c(-0.08, -0.08)
)
cross_level_arrows <- data.table(
  x = c(1.12, 2.88),
  xend = c(2.88, 1.12),
  y = c(0.12, -0.12),
  yend = c(0.12, -0.12)
)

figure_3a <- ggplot() +
  geom_curve(
    data = cross_level_arrows,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = -0.24,
    linewidth = 0.46,
    linetype = "22",
    colour = neutral_mid,
    arrow = arrow(length = unit(1.6, "mm"), type = "closed")
  ) +
  geom_curve(
    data = forward_arrows,
    aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.18,
    linewidth = 0.55,
    colour = neutral_dark,
    arrow = arrow(length = unit(1.7, "mm"), type = "closed")
  ) +
  geom_curve(
    data = backward_arrows,
    aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.18,
    linewidth = 0.55,
    colour = neutral_dark,
    arrow = arrow(length = unit(1.7, "mm"), type = "closed")
  ) +
  geom_label(
    data = state_nodes,
    aes(x = x, y = y, label = state, colour = state),
    size = 2.35,
    family = "Arial",
    fontface = "bold",
    fill = "white",
    linewidth = 0.45,
    label.padding = unit(0.17, "lines"),
    label.r = unit(0.12, "lines"),
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Low deficit" = teal,
      "Intermediate" = gold,
      "High deficit" = coral
    )
  ) +
  annotate(
    "text",
    x = 1:3,
    y = -0.32,
    label = "Persistence",
    size = 2.15,
    colour = navy,
    family = "Arial"
  ) +
  coord_cartesian(xlim = c(0.55, 3.45), ylim = c(-0.56, 0.58)) +
  labs(
    tag = "a",
    title = "Health-state space",
    subtitle = "Next-wave mobility and persistence"
  ) +
  theme_void(base_family = "Arial", base_size = 7) +
  theme(
    plot.title = element_text(size = 7.4, face = "bold"),
    plot.subtitle = element_text(size = 6.5, colour = neutral_mid),
    plot.tag = element_text(size = 8.2, face = "bold"),
    plot.margin = margin(5, 6, 0, 6)
  )

figure_3b <- ggplot(
  paired_prob,
  aes(y = transition)
) +
  geom_hline(
    yintercept = c(3.5, 6.5),
    colour = neutral_light,
    linewidth = 0.4
  ) +
  geom_segment(
    aes(
      x = `Pre-expansion`,
      xend = Expansion,
      yend = transition
    ),
    linewidth = 0.6,
    colour = neutral_light
  ) +
  geom_point(
    aes(x = `Pre-expansion`),
    shape = 21,
    size = 2.15,
    stroke = 0.65,
    fill = "white",
    colour = neutral_dark
  ) +
  geom_point(
    aes(x = Expansion, colour = role, shape = role),
    size = 2.25,
    stroke = 0.65
  ) +
  scale_colour_manual(
    values = c(
      Persistence = navy,
      Recovery = teal,
      Deterioration = coral
    )
  ) +
  scale_shape_manual(
    values = c(
      Persistence = 16,
      Recovery = 17,
      Deterioration = 15
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    tag = "b",
    title = "Model-standardised two-year probabilities",
    subtitle = "Open: pre-expansion; filled: expansion",
    x = "Transition probability (%)",
    y = NULL,
    colour = NULL,
    shape = NULL
  ) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 1, 0),
    legend.box.margin = margin(0, 0, 0, 0)
  )

figure_3c <- ggplot(
  bootstrap,
  aes(
    x = difference_pp,
    y = transition,
    colour = role,
    shape = role
  )
) +
  geom_hline(
    yintercept = c(3.5, 6.5),
    colour = neutral_light,
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    colour = neutral_mid,
    linewidth = 0.45
  ) +
  geom_errorbar(
    aes(xmin = ci_low_pp, xmax = ci_high_pp),
    width = 0.13,
    linewidth = 0.55,
    orientation = "y"
  ) +
  geom_point(size = 2.3, stroke = 0.65) +
  scale_colour_manual(
    values = c(
      Persistence = navy,
      Recovery = teal,
      Deterioration = coral
    )
  ) +
  scale_shape_manual(
    values = c(
      Persistence = 16,
      Recovery = 17,
      Deterioration = 15
    )
  ) +
  scale_x_continuous(
    breaks = seq(-15, 15, by = 5),
    limits = c(-15, 15),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    tag = "c",
    title = "Expansion-period difference",
    subtitle = "Point estimate and 95% cluster-bootstrap CI",
    x = "Expansion minus pre-expansion (pp)",
    y = NULL
  ) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  )

figure_3 <- figure_3a / (figure_3b + figure_3c) +
  plot_layout(heights = c(0.33, 1))

fwrite(
  common_long,
  file.path(table_dir, "Figure3b_source_data.csv")
)
fwrite(
  bootstrap,
  file.path(table_dir, "Figure3c_source_data.csv")
)
save_nature(figure_3, "Figure3_CHARLS_state_transitions", 183, 142)

# Figure 4: cross-survey construct corroboration ------------------------------

charls_adl <- fread(
  file.path(input_dir, "CHARLS_state_ADL_validation_corrected.csv")
)
charls_adl[, state_label := factor(
  label,
  levels = c(
    "low-deficit",
    "intermediate-deficit",
    "high-deficit"
  ),
  labels = c("Low deficit", "Intermediate", "High deficit")
)]
charls_adl[, risk_pct := 100 * risk]
charls_adl[, ci_low_pct := 100 * mapply(
  function(x, n_total) {
    prop.test(x, n_total, correct = FALSE)$conf.int[1]
  },
  nadl,
  n
)]
charls_adl[, ci_high_pct := 100 * mapply(
  function(x, n_total) {
    prop.test(x, n_total, correct = FALSE)$conf.int[2]
  },
  nadl,
  n
)]
charls_adl[, label_y := ci_high_pct + 0.85]

figure_4a <- ggplot(
  charls_adl,
  aes(x = state_label, y = risk_pct, colour = state_label)
) +
  geom_errorbar(
    aes(ymin = ci_low_pct, ymax = ci_high_pct),
    width = 0.13,
    linewidth = 0.55
  ) +
  geom_point(size = 2.5, stroke = 0.65) +
  geom_text(
    aes(y = label_y, label = sprintf("%.1f%%", risk_pct)),
    vjust = 0,
    colour = ink,
    size = 2.35,
    family = "Arial",
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Low deficit" = teal,
      "Intermediate" = gold,
      "High deficit" = coral
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    limits = c(0, 32),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    tag = "a",
    title = "CHARLS longitudinal validation",
    subtitle = "Incident ADL limitation by 2018",
    x = "2015 health-deficit state",
    y = "Three-year risk (%)"
  ) +
  theme(legend.position = "none")

class_adl <- fread(
  file.path(input_dir, "CLASS_concurrent_FI_ADL_PR.csv")
)
class_adl[, y := 1]

figure_4b <- ggplot(
  class_adl,
  aes(x = pr_per_010_fi, y = y)
) +
  geom_vline(xintercept = 1, colour = neutral_mid, linewidth = 0.45) +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    width = 0.12,
    linewidth = 0.65,
    colour = teal,
    orientation = "y"
  ) +
  geom_point(size = 2.8, colour = teal) +
  geom_text(
    aes(
      x = 1.95,
      label = sprintf(
        "PR %.2f (%.2f, %.2f)",
        pr_per_010_fi,
        ci_low,
        ci_high
      )
    ),
    hjust = 1,
    size = 2.45,
    family = "Arial",
    colour = ink
  ) +
  scale_x_continuous(
    breaks = seq(1, 2.3, by = 0.2),
    limits = c(1, 2.35),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(limits = c(0.82, 1.18)) +
  labs(
    tag = "b",
    title = "CLASS external corroboration",
    subtitle = "Concurrent ADL-help prevalence per 0.10 higher FI",
    x = "Prevalence ratio",
    y = NULL
  ) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

figure_4 <- figure_4a + figure_4b +
  plot_layout(widths = c(1.05, 1))

fwrite(
  charls_adl,
  file.path(table_dir, "Figure4a_source_data.csv")
)
fwrite(
  class_adl,
  file.path(table_dir, "Figure4b_source_data.csv")
)
save_nature(figure_4, "Figure4_cross_survey_validation", 183, 76)

qa_manifest <- data.table(
  figure = c(
    "Figure1_evidence_architecture",
    "Figure2_CFPS_policy_estimates",
    "Figure3_CHARLS_state_transitions",
    "Figure4_cross_survey_validation"
  ),
  width_mm = c(183, 183, 183, 183),
  height_mm = c(62, 82, 142, 76),
  backend = "R 4.4.3",
  formats = if (skip_pdf) {
    "SVG; TIFF 600 dpi; PNG 300 dpi"
  } else {
    "SVG; PDF; TIFF 600 dpi; PNG 300 dpi"
  },
  background = "white",
  editable_text = if (skip_pdf) "SVG" else "SVG and PDF",
  source_data = "private_outputs/tables"
)
fwrite(
  qa_manifest,
  file.path(qa_dir, "figure_export_manifest.csv")
)

message("V6 Nature figures completed: ", figure_dir)
