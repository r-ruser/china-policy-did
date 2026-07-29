#!/usr/bin/env Rscript
# V5.2: Nature-style figures for BMC Geriatrics
# Single column: 89mm (3.5in), Double column: 183mm (7.2in)
# Arial font, 600 DPI TIFF, SVG source
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(svglite)
  library(ragg)
  library(dplyr)
})

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
fig_dir <- file.path(root, "07_results", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Nature color palette
navy <- "#294C7A"
coral <- "#C8574D"
teal <- "#138A81"
gray <- "#737373"
ink <- "#202124"
light_gray <- "#EAEAEA"

# Nature theme
theme_nature <- function(base_size = 7) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(colour = ink),
      axis.line = element_line(linewidth = 0.35),
      axis.ticks = element_line(linewidth = 0.35),
      axis.text = element_text(size = base_size - 0.5),
      axis.title = element_text(size = base_size),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 0.5),
      plot.title = element_text(face = "bold", size = base_size + 0.5),
      plot.subtitle = element_text(size = base_size - 0.5, colour = gray),
      plot.tag = element_text(face = "bold", size = base_size + 1.5),
      panel.grid.major.y = element_line(colour = light_gray, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 5, 4, 5)
    )
}

# Save function: Nature specs
save_nature <- function(p, name, w_mm = 89, h_mm = 110) {
  wi <- w_mm / 25.4
  hi <- h_mm / 25.4
  base <- file.path(fig_dir, name)
  svglite::svglite(paste0(base, ".svg"), width = wi, height = hi)
  print(p)
  dev.off()
  grDevices::cairo_pdf(paste0(base, ".pdf"), width = wi, height = hi, family = "Arial")
  print(p)
  dev.off()
  ragg::agg_tiff(paste0(base, ".tiff"), width = wi, height = hi, units = "in", res = 600, compression = "lzw")
  print(p)
  dev.off()
  ragg::agg_png(paste0(base, ".png"), width = wi, height = hi, units = "in", res = 300)
  print(p)
  dev.off()
  cat("  Saved:", name, "\n")
}

# ============================================================
# Figure 1: Study design and evidence architecture
# ============================================================
cat("[1] Figure 1: Study design...\n")

timeline <- data.table(
  cohort = rep(c("CFPS", "CHARLS", "CLASS"), c(3, 4, 4)),
  year = c(2012, 2014, 2018, 2011, 2013, 2015, 2018, 2016, 2018, 2020, 2023),
  role = c(
    rep("Pilot-area quasi-experiment", 3),
    rep("National expansion transition analysis", 4),
    rep("Repeated cross-sectional corroboration", 4)
  )
)

p1 <- ggplot(timeline, aes(year, cohort, colour = role, group = cohort)) +
  geom_line(linewidth = 1.05, colour = "#B8B8B8") +
  geom_point(size = 2.8) +
  geom_vline(xintercept = 2016, linetype = "dashed", colour = coral, linewidth = 0.55) +
  annotate("label", x = 2016, y = 3.55, label = "2016 national expansion",
           size = 2.25, label.size = 0.2, colour = coral, fill = "white") +
  scale_colour_manual(values = c(
    "Pilot-area quasi-experiment" = coral,
    "National expansion transition analysis" = navy,
    "Repeated cross-sectional corroboration" = teal
  )) +
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2018, 2020, 2023)) +
  labs(x = "Survey year", y = NULL, title = "Policy timing and evidence roles") +
  theme_nature() +
  theme(legend.position = "bottom", legend.box = "vertical")

save_nature(p1, "Figure1_study_design", w_mm = 89, h_mm = 55)

# ============================================================
# Figure 2: CFPS policy estimates
# ============================================================
cat("[2] Figure 2: CFPS policy estimates...\n")

main <- fread(file.path(root, "07_results", "tables", "r_corrected_main_results.csv"))
cfps <- main[grepl("CFPS pilot", analysis)]
cfps[, label := case_when(
  grepl("DID$", analysis) ~ "Pilot-area DID",
  grepl("activity limitation", estimand) ~ "High-need DDD",
  TRUE ~ "Age 75+ DDD"
)]
cfps[, estimate := 100 * estimate]
cfps[, conf_low := 100 * conf_low]
cfps[, conf_high := 100 * conf_high]

p2a <- ggplot(cfps, aes(estimate, reorder(label, estimate), colour = label)) +
  geom_vline(xintercept = 0, colour = gray, linewidth = 0.35) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.14, linewidth = 0.55) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(coral, gold = "#B98522", teal)) +
  labs(x = "Risk difference in poor SRH (percentage points)", y = NULL,
       title = "CFPS pilot-area policy estimates") +
  theme_nature() + theme(legend.position = "none")

ev <- fread(file.path(root, "07_results", "tables", "r_cfps_event_study.csv"))
ev <- ev[analysis == "CFPS pilot-area event study"]
ev[, year := as.integer(year)]
ev[, estimate := 100 * estimate]
ev[, conf_low := 100 * conf_low]
ev[, conf_high := 100 * conf_high]

p2b <- ggplot(ev, aes(year, estimate)) +
  geom_hline(yintercept = 0, colour = gray, linewidth = 0.35) +
  geom_vline(xintercept = 2016, linetype = "dashed", colour = coral, linewidth = 0.55) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.12, colour = navy) +
  geom_line(colour = navy, linewidth = 0.65) +
  geom_point(colour = navy, size = 2) +
  scale_x_continuous(breaks = c(2012, 2014, 2018)) +
  labs(x = "Survey year", y = "Pilot-area contrast (pp)", title = "Event-study diagnostic") +
  theme_nature()

p2 <- p2a + p2b + plot_layout(widths = c(1, 1.05)) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(face = "bold", size = 9))
save_nature(p2, "Figure2_CFPS_policy_estimates", w_mm = 183, h_mm = 78)

# ============================================================
# Figure 3: CHARLS transition probability differences
# ============================================================
cat("[3] Figure 3: CHARLS transition differences...\n")

boot <- fread(file.path(root, "CHARLS_cluster_bootstrap_500_corrected_results.csv"))
boot[, from_label := factor(from, levels = 1:3, labels = c("Low start", "Intermediate start", "High start"))]
boot[, dest_label := factor(to, levels = 1:3, labels = c("Low", "Intermediate", "High"))]

# Create transition label
boot[, trans_label := paste0(
  ifelse(from == to, "Maintain ", ""),
  c("Low", "Intermediate", "High")[from], " to ", c("Low", "Intermediate", "High")[to]
)]

# Only show non-self transitions for clarity
boot_show <- boot[from != to | TRUE]  # Keep all for now

p3 <- ggplot(boot, aes(x = original_diff, y = reorder(trans_label, original_diff))) +
  geom_vline(xintercept = 0, colour = gray, linewidth = 0.35) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, linewidth = 0.4, colour = navy) +
  geom_point(size = 2, colour = navy) +
  facet_grid(from_label ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Probability difference (percentage points)",
       y = NULL,
       title = "CHARLS: Expansion-period transition differences",
       subtitle = "Model-standardised 2-year common-horizon estimates") +
  theme_nature() +
  theme(strip.text = element_text(face = "bold", size = 7),
        strip.background = element_blank())

save_nature(p3, "Figure3_CHARLS_transitions", w_mm = 89, h_mm = 120)

cat("All figures generated.\n")
cat("Files in:", fig_dir, "\n")
