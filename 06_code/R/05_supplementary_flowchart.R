suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(project_root, "07_results", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

nodes <- data.frame(
  id = c(
    "S", "T",
    "A", "A1", "A2", "A3", "A4",
    "B", "B1", "B2", "B3", "B4",
    "C", "C1", "C2", "C3"
  ),
  x = c(
    6, 6,
    2, 2, 2, 1.05, 2.95,
    6, 6, 6, 5.05, 6.95,
    10, 10, 10, 10
  ),
  y = c(
    10.05, 8.80,
    7.50, 6.10, 4.65, 2.40, 2.40,
    7.50, 6.10, 4.65, 2.40, 2.40,
    7.50, 6.10, 4.65, 2.40
  ),
  width = c(
    1.80, 2.05,
    1.55, 2.55, 2.55, 1.78, 1.78,
    1.55, 2.55, 2.55, 1.78, 1.78,
    1.55, 2.55, 2.55, 2.55
  ),
  height = c(
    0.62, 0.98,
    0.66, 1.05, 1.05, 1.72, 1.72,
    0.66, 1.05, 1.05, 1.72, 1.72,
    0.66, 1.05, 1.05, 1.72
  ),
  label = c(
    "Study sample selection",
    "Triangulated analysis of\nCHARLS, CLASS, and CFPS",
    "A.  CHARLS",
    "Source cohort\n2015 participants aged 65 years or older\nn = 6,495",
    "Analytic eligibility\nRequired waves and\ncomplete outcome data",
    "Primary longitudinal analysis\nIncident ADL n = 4,292\ndepression n = 2,863\npoor self-rated health n = 4,627\nCESD-10 n = 4,491\ncognition n = 2,718",
    "Cognitive LCGA\nn = 3,395 participants\n11,931 observations\nbaseline low cognition retained",
    "B.  CLASS",
    "Source cohort\n2018 n = 9,357; 2020 n = 7,245\n2023 n = 3,851",
    "Analytic eligibility\n2018 baseline age 65 years or older\nat least two observed waves",
    "Primary longitudinal analysis\nADL help, depressive symptoms,\nand poor self-rated health\nrepeated cohort n = 7,245",
    "Depressive-symptom LCGA\nn = 6,822 participants\nthree stable classes\nbaseline symptoms retained",
    "C.  CFPS",
    "Source cohort\nFixed 2014 baseline cohort\naged 65 years or older",
    "Analytic eligibility\nValid pilot-area mapping, comparable\noutcomes, and complete design data",
    "Primary health analysis\nDID n = 3,509\nage 75+ DDD n = 5,428\nhigh-need DDD n = 5,076"
  ),
  stringsAsFactors = FALSE
)

nodes$xmin <- nodes$x - nodes$width / 2
nodes$xmax <- nodes$x + nodes$width / 2
nodes$ymin <- nodes$y - nodes$height / 2
nodes$ymax <- nodes$y + nodes$height / 2

edge <- function(from, to) {
  a <- nodes[nodes$id == from, ]
  b <- nodes[nodes$id == to, ]
  data.frame(
    x = a$x, y = a$ymin,
    xend = b$x, yend = b$ymax
  )
}

solid_edges <- do.call(
  rbind,
  lapply(
    list(
      c("S", "T"),
      c("A", "A1"), c("A1", "A2"), c("A2", "A3"), c("A2", "A4"),
      c("B", "B1"), c("B1", "B2"), c("B2", "B3"), c("B2", "B4"),
      c("C", "C1"), c("C1", "C2"), c("C2", "C3")
    ),
    function(pair) edge(pair[1], pair[2])
  )
)

top <- nodes[nodes$id == "T", ]
branch_edges <- data.frame(
  x = top$x,
  y = top$ymin,
  xend = c(2, 6, 10),
  yend = nodes$ymax[match(c("A", "B", "C"), nodes$id)]
)

arrow_style <- arrow(
  length = unit(1.7, "mm"),
  type = "closed",
  ends = "last"
)

p <- ggplot() +
  geom_curve(
    data = branch_edges[1, ],
    aes(x, y, xend = xend, yend = yend),
    curvature = -0.10,
    linewidth = 0.35,
    linetype = "22",
    colour = "#777777",
    arrow = arrow_style
  ) +
  geom_curve(
    data = branch_edges[2, ],
    aes(x, y, xend = xend, yend = yend),
    curvature = 0,
    linewidth = 0.35,
    linetype = "22",
    colour = "#777777",
    arrow = arrow_style
  ) +
  geom_curve(
    data = branch_edges[3, ],
    aes(x, y, xend = xend, yend = yend),
    curvature = 0.10,
    linewidth = 0.35,
    linetype = "22",
    colour = "#777777",
    arrow = arrow_style
  ) +
  geom_segment(
    data = solid_edges,
    aes(x, y, xend = xend, yend = yend),
    linewidth = 0.35,
    colour = "#777777",
    arrow = arrow_style
  ) +
  geom_rect(
    data = nodes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "white",
    colour = "#777777",
    linewidth = 0.40
  ) +
  geom_text(
    data = nodes[!nodes$id %in% c("A3", "A4", "B3", "B4", "C3", "C4"), ],
    aes(x, y, label = label),
    family = "Arial",
    size = 3.15,
    lineheight = 0.98,
    colour = "#111111"
  ) +
  geom_text(
    data = nodes[nodes$id %in% c("A3", "A4", "B3", "B4", "C3", "C4"), ],
    aes(x, y, label = label),
    family = "Arial",
    size = 2.45,
    lineheight = 0.98,
    colour = "#111111"
  ) +
  coord_cartesian(xlim = c(-0.45, 12.45), ylim = c(1.42, 10.55), clip = "off") +
  theme_void(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(18, 25, 18, 25)
  )

ggsave(
  file.path(output_dir, "Supplementary_Figure_S1_study_selection.png"),
  p,
  width = 2400,
  height = 1350,
  units = "px",
  dpi = 180,
  bg = "white"
)
