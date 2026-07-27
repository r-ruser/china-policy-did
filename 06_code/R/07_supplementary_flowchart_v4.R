#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ggplot2)
  library(ragg)
})

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out <- file.path(
  root, "07_results", "figures",
  "Supplementary_Figure_S1_study_selection_v4.png"
)

boxes <- data.frame(
  x = rep(1:4, each = 3),
  y = rep(c(3, 2, 1), 4),
  label = c(
    "CHARLS\n2015 policy-period cohort\nN = 6,495",
    "CHARLS CESD-10\nage ≥65 in 2011",
    "≥3 valid waves\nTrajectory N = 3,085",
    "CLASS\nage ≥65 in 2018\nN = 9,357",
    "Depression score valid\nin 2018, 2020 and 2023",
    "All 3 waves complete\nTrajectory N = 3,600",
    "CLHLS\nage ≥65 in 2008\nN = 16,563",
    "b21-b27 depressive-affect burden\nall 3 waves valid",
    "Trajectory N = 3,683\nproxy measure, not CES-D",
    "CFPS\npilot-area panel",
    "Poor self-rated health\n2014-2018",
    "Fixed-effects DID/DDD\nwith city-clustered SEs"
  ),
  fill = rep(c("#E9EFF8", "#F8EDEB", "#EAF5F2", "#F3EEF8"), each = 3)
)
arrows <- data.frame(
  x = rep(1:4, each = 2),
  xend = rep(1:4, each = 2),
  y = rep(c(2.72, 1.72), 4),
  yend = rep(c(2.28, 1.28), 4)
)

p <- ggplot() +
  geom_segment(
    data = arrows,
    aes(x, y, xend = xend, yend = yend),
    arrow = arrow(length = unit(2.2, "mm")),
    linewidth = 0.45
  ) +
  geom_label(
    data = boxes,
    aes(x, y, label = label, fill = fill),
    linewidth = 0.3, label.padding = unit(2.5, "mm"),
    size = 2.7, lineheight = 1.05, family = "Arial"
  ) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(0.45, 4.55), ylim = c(0.65, 3.35), clip = "off") +
  theme_void() +
  labs(
    title = "Supplementary Figure S1 | Study selection and analytic roles",
    subtitle = paste(
      "All trajectory cohorts used fixed-baseline standardized scores;",
      "positive values indicate greater burden."
    ),
    caption = paste(
      "CLHLS provides external triangulation only and is not interpreted",
      "as evidence of a policy effect."
    )
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, family = "Arial"),
    plot.subtitle = element_text(size = 9, family = "Arial"),
    plot.caption = element_text(size = 8, hjust = 0, family = "Arial"),
    plot.margin = margin(12, 12, 12, 12)
  )

agg_png(out, width = 210, height = 125, units = "mm", res = 300)
print(p)
dev.off()
cat("V4 flowchart completed.\n")
