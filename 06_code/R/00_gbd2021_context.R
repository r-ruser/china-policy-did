#!/usr/bin/env Rscript

# Descriptive national disease-burden context for the 2016 expansion of
# integrated health and social care. This module does not estimate a policy
# effect. It uses GBD 2021 point estimates for China, adults aged 65 years or
# older, combining the main download with an age-85-plus supplement.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

setDTthreads(max(1L, parallel::detectCores(logical = TRUE)))

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
path_tables <- file.path(project_root, "07_results", "tables")
path_diag <- file.path(project_root, "07_results", "diagnostics")
path_figures <- file.path(project_root, "07_results", "figures")
path_logs <- file.path(project_root, "10_logs")
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(path_diag, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_logs, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(path_logs, "r_gbd2021_context.log")
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con, type = "output")
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(log_con)
}, add = TRUE)

find_gbd_zip <- function() {
  from_env <- Sys.getenv("GBD2021_ZIP", unset = "")
  if (nzchar(from_env)) {
    if (!file.exists(from_env)) {
      stop("GBD2021_ZIP does not exist: ", from_env)
    }
    return(normalizePath(from_env, winslash = "/", mustWork = TRUE))
  }
  gbd_dir <- normalizePath(
    file.path(project_root, "..", "..", "GBD"),
    winslash = "/",
    mustWork = FALSE
  )
  candidates <- list.files(
    gbd_dir,
    pattern = "^GBD_2021.*[.]zip$",
    full.names = TRUE
  )
  if (length(candidates) != 1L) {
    stop(
      "Set GBD2021_ZIP. Expected exactly one GBD_2021 ZIP in ",
      gbd_dir, "; found ", length(candidates), "."
    )
  }
  normalizePath(candidates, winslash = "/", mustWork = TRUE)
}

find_gbd_85plus_zip <- function() {
  from_env <- Sys.getenv("GBD2021_85PLUS_ZIP", unset = "")
  if (nzchar(from_env)) {
    if (!file.exists(from_env)) {
      stop("GBD2021_85PLUS_ZIP does not exist: ", from_env)
    }
    return(normalizePath(from_env, winslash = "/", mustWork = TRUE))
  }
  candidate <- file.path(
    Sys.getenv("USERPROFILE"), "Downloads",
    "IHME-GBD_2021_DATA-68eca4d5-1.zip"
  )
  if (!file.exists(candidate)) {
    stop(
      "Set GBD2021_85PLUS_ZIP to the supplemental GBD ZIP containing ",
      "ages 85-89, 90-94, and the oldest age category."
    )
  }
  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

read_gbd_zip <- function(path) {
  members <- unzip(path, list = TRUE)$Name
  csv_member <- members[grepl("[.]csv$", members)]
  if (length(csv_member) != 1L) {
    stop("Expected exactly one CSV in ", path, "; found ", length(csv_member))
  }
  d <- as.data.table(read.csv(
    unz(path, csv_member),
    fileEncoding = "UTF-8",
    check.names = FALSE
  ))
  name_map <- c(
    population_group_name = "population_group",
    measure_name = "measure",
    location_name = "location",
    sex_name = "sex",
    age_name = "age",
    cause_name = "cause",
    metric_name = "metric"
  )
  for (old in names(name_map)) {
    if (old %in% names(d)) {
      setnames(d, old, name_map[[old]])
    }
  }
  list(data = d, csv_member = csv_member)
}

zip_path <- find_gbd_zip()
zip_85plus_path <- find_gbd_85plus_zip()
main_source <- read_gbd_zip(zip_path)
older_source <- read_gbd_zip(zip_85plus_path)
gbd_main <- main_source$data
gbd_85plus <- older_source$data

required_columns <- c(
  "population_group", "measure", "location", "sex", "age", "cause",
  "metric", "year", "val", "upper", "lower"
)
for (source_name in c("gbd_main", "gbd_85plus")) {
  d <- get(source_name)
  if (!all(required_columns %in% names(d))) {
    stop(
      source_name, " is missing required columns: ",
      paste(setdiff(required_columns, names(d)), collapse = ", ")
    )
  }
}

key_columns <- c(
  "population_group", "measure", "location", "sex", "age", "cause",
  "metric", "year"
)
for (source_name in c("gbd_main", "gbd_85plus")) {
  d <- get(source_name)
  if (anyDuplicated(d[, ..key_columns])) {
    stop("Duplicate GBD rows detected in ", source_name, ".")
  }
  if (anyNA(d[, .(val, upper, lower)])) {
    stop("Missing GBD estimate or uncertainty bound in ", source_name, ".")
  }
}

main_ages <- c("65-69岁", "70-74岁", "75-79岁", "80-84岁")
older_ages <- c("85-89岁", "90-94岁", ">95岁")
selected_ages <- c(main_ages, older_ages)
selected_causes <- c(
  "慢性阻塞性肺疾病",
  "跌倒",
  "阿尔茨海默病和其他痴呆",
  "抑郁症"
)
cause_labels <- c(
  "慢性阻塞性肺疾病" = "COPD",
  "跌倒" = "Falls",
  "阿尔茨海默病和其他痴呆" = "Alzheimer disease and other dementias",
  "抑郁症" = "Depressive disorders"
)
cause_order <- unname(cause_labels[selected_causes])
figure_labels <- c(
  "慢性阻塞性肺疾病" = "COPD",
  "跌倒" = "Falls",
  "阿尔茨海默病和其他痴呆" = "Dementia",
  "抑郁症" = "Depression"
)
figure_order <- unname(figure_labels[selected_causes])

analysis_main <- gbd_main[
  location == "中国" &
    population_group == "全人口" &
    sex %in% c("男", "女") &
    age %in% main_ages &
    cause %in% selected_causes &
    measure == "伤残调整生命年" &
    metric %in% c("数量", "率") &
    year %between% c(2011L, 2021L)
]
analysis_85plus <- gbd_85plus[
  location == "中国" &
    population_group == "全人口" &
    sex %in% c("男", "女") &
    age %in% older_ages &
    cause %in% selected_causes &
    measure == "伤残调整生命年" &
    metric %in% c("数量", "率") &
    year %between% c(2011L, 2021L)
]
analysis_rows <- rbindlist(
  list(
    analysis_main[, ..required_columns],
    analysis_85plus[, ..required_columns]
  ),
  use.names = TRUE
)

expected_rows <- length(selected_ages) * length(selected_causes) *
  2L * 2L * 11L
if (nrow(analysis_rows) != expected_rows) {
  stop("Unexpected analytic row count: ", nrow(analysis_rows),
       "; expected ", expected_rows, ".")
}

analysis_rows[, metric_en := fifelse(metric == "数量", "count", "rate")]
wide <- dcast(
  analysis_rows,
  sex + age + cause + year ~ metric_en,
  value.var = "val"
)
if (anyNA(wide[, .(count, rate)]) || any(wide$rate <= 0)) {
  stop("Invalid count/rate pairing in the selected GBD rows.")
}
wide[, population := count / rate * 100000]

# The GBD count/rate identity yields a common population denominator across
# causes for each sex-age-year stratum. Verify before aggregating.
population_check <- wide[, .(
  minimum_population = min(population),
  maximum_population = max(population),
  relative_range = (max(population) - min(population)) / mean(population)
), by = .(sex, age, year)]
if (max(population_check$relative_range) > 1e-8) {
  stop("Cause-specific implied population denominators are inconsistent.")
}

trend <- wide[, .(
  dalys = sum(count),
  population = sum(population)
), by = .(cause, year)]
trend[, daly_rate := dalys / population * 100000]
trend[, cause_label := factor(unname(cause_labels[cause]),
                              levels = cause_order)]
trend[, figure_label := factor(unname(figure_labels[cause]),
                               levels = figure_order)]
trend[, index_2011 := 100 * daly_rate / daly_rate[year == 2011],
      by = cause]

period_summary <- trend[, .(
  daly_rate_2011 = daly_rate[year == 2011],
  daly_rate_2015 = daly_rate[year == 2015],
  daly_rate_2017 = daly_rate[year == 2017],
  daly_rate_2021 = daly_rate[year == 2021],
  pre_rate_change_pct =
    100 * (daly_rate[year == 2015] / daly_rate[year == 2011] - 1),
  post_rate_change_pct =
    100 * (daly_rate[year == 2021] / daly_rate[year == 2017] - 1),
  overall_rate_change_pct =
    100 * (daly_rate[year == 2021] / daly_rate[year == 2011] - 1),
  dalys_2011 = dalys[year == 2011],
  dalys_2021 = dalys[year == 2021],
  overall_count_change_pct =
    100 * (dalys[year == 2021] / dalys[year == 2011] - 1)
), by = .(cause, cause_label, figure_label)]

population_summary <- unique(
  wide[cause == selected_causes[1], .(sex, age, year, population)]
)[, .(population = sum(population)), by = year]

audit <- data.table(
  item = c(
    "Main ZIP path", "Age-85-plus ZIP path",
    "Main CSV member", "Age-85-plus CSV member",
    "Main raw rows", "Age-85-plus raw rows", "Analytic rows",
    "Calendar years", "Geography", "Sexes", "Ages",
    "Causes", "Measure", "Missing estimates", "Duplicate dimensional keys",
    "Maximum denominator relative range", "Policy transition year",
    "Pre-expansion descriptive window", "Post-expansion descriptive window",
    "Claim boundary"
  ),
  value = c(
    zip_path, zip_85plus_path,
    main_source$csv_member, older_source$csv_member,
    nrow(gbd_main), nrow(gbd_85plus), nrow(analysis_rows),
    "2011-2021", "China", "Male and female combined",
    paste0(
      "65-69, 70-74, 75-79, 80-84, 85-89, 90-94, and source-labelled ",
      ">95 years"
    ),
    paste(cause_order, collapse = "; "),
    "Disability-adjusted life-years (DALYs)",
    sum(is.na(gbd_main[, .(val, upper, lower)])) +
      sum(is.na(gbd_85plus[, .(val, upper, lower)])),
    sum(duplicated(gbd_main[, ..key_columns])) +
      sum(duplicated(gbd_85plus[, ..key_columns])),
    format(max(population_check$relative_range), scientific = TRUE),
    "2016 (excluded from before/after change calculations)",
    "2011-2015", "2017-2021",
    "Descriptive national background; not a policy-effect estimate"
  )
)

fwrite(trend, file.path(path_tables, "r_gbd2021_older_context_trends.csv"))
fwrite(
  period_summary,
  file.path(path_tables, "r_gbd2021_policy_period_summary.csv")
)
fwrite(
  population_summary,
  file.path(path_tables, "r_gbd2021_population_context.csv")
)
fwrite(audit, file.path(path_diag, "r_gbd2021_context_audit.csv"))

palette <- c(
  "COPD" = "#0072B2",
  "Falls" = "#E69F00",
  "Dementia" = "#CC79A7",
  "Depression" = "#009E73"
)

theme_pub <- theme_classic(base_family = "Arial", base_size = 7) +
  theme(
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6.2, colour = "#222222"),
    plot.title = element_text(size = 8.5, face = "bold"),
    plot.subtitle = element_text(size = 6.5, colour = "#4D4D4D"),
    strip.background = element_rect(fill = "#F0F0F0", colour = NA),
    strip.text = element_text(size = 6.5, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 6.3),
    legend.text = element_text(size = 6.1),
    plot.margin = margin(5, 7, 5, 7)
  )

p_rate <- ggplot(
  trend,
  aes(year, daly_rate, colour = figure_label, group = figure_label)
) +
  annotate(
    "rect", xmin = 2015.5, xmax = 2016.5, ymin = -Inf, ymax = Inf,
    fill = "#D9D9D9", alpha = 0.45
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(
    data = trend[year %in% c(2011, 2015, 2017, 2021)],
    size = 1.35
  ) +
  facet_wrap(~figure_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = palette, guide = "none") +
  scale_x_continuous(breaks = c(2011, 2015, 2017, 2021)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    x = "Year",
    y = "Crude DALY rate per 100,000",
    title = "National burden trends",
    subtitle = "Grey band: 2016 policy-expansion transition year"
  ) +
  theme_pub

change_long <- melt(
  period_summary,
  id.vars = c("cause", "cause_label", "figure_label"),
  measure.vars = c("pre_rate_change_pct", "post_rate_change_pct"),
  variable.name = "period",
  value.name = "change_pct"
)
change_long[, period := factor(
  period,
  levels = c("pre_rate_change_pct", "post_rate_change_pct"),
  labels = c("2011-2015", "2017-2021")
)]

p_change <- ggplot(
  change_long,
  aes(change_pct, figure_label, colour = period)
) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 0.35,
             colour = "#8C8C8C") +
  geom_line(
    aes(group = figure_label),
    colour = "#BDBDBD",
    linewidth = 0.45
  ) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c("2011-2015" = "#7F7F7F", "2017-2021" = "#009E73"),
    name = NULL
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  labs(
    x = "Change in crude DALY rate",
    y = NULL,
    title = "Before- and after-expansion changes",
    subtitle = "Point-estimate changes; 2016 excluded"
  ) +
  theme_pub +
  theme(axis.text.y = element_text(size = 5.8))

p_count <- ggplot(
  period_summary,
  aes(overall_count_change_pct, figure_label, colour = figure_label)
) +
  geom_segment(
    aes(x = 0, xend = overall_count_change_pct,
        y = figure_label, yend = figure_label),
    linewidth = 0.7,
    alpha = 0.55
  ) +
  geom_point(size = 2) +
  geom_text(
    aes(label = sprintf("%+.1f%%", overall_count_change_pct)),
    hjust = -0.15,
    size = 2.2,
    colour = "#333333"
  ) +
  scale_colour_manual(values = palette, guide = "none") +
  scale_x_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    limits = c(0, 110),
    breaks = c(0, 25, 50, 75, 100),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Change in DALY count, 2011-2021",
    y = NULL,
    title = "Absolute burden increased",
    subtitle = "Population aged 65+: 121.9 to 196.0 million"
  ) +
  theme_pub +
  theme(axis.text.y = element_text(size = 5.8))

figure1 <- p_rate + (p_change / p_count) +
  plot_layout(widths = c(1.45, 1)) +
  plot_annotation(
    title = "Disease-burden context before and after the 2016 policy expansion",
    subtitle = paste0(
      "China, adults aged 65 years or older, 2011-2021; GBD 2021 point estimates"
    ),
    caption = paste0(
      "DALY rates are crude within ages 65 years or older and are not ",
      "age-standardised. The oldest source category was labelled >95 years.\n",
      "Before/after changes are descriptive national context and do not estimate ",
      "a policy effect."
    ),
    tag_levels = "a",
    theme = theme(
      plot.title = element_text(
        family = "Arial", size = 10, face = "bold", colour = "#111111"
      ),
      plot.subtitle = element_text(
        family = "Arial", size = 7, colour = "#4D4D4D"
      ),
      plot.caption = element_text(
        family = "Arial", size = 5.8, colour = "#4D4D4D", hjust = 0
      ),
      plot.tag = element_text(
        family = "Arial", size = 9, face = "bold", colour = "#111111"
      )
    )
  )

save_figure <- function(plot, stem, width_mm = 183, height_mm = 130) {
  ggsave(
    file.path(path_figures, paste0(stem, ".png")),
    plot, width = width_mm, height = height_mm, units = "mm",
    dpi = 450, bg = "white"
  )
  ggsave(
    file.path(path_figures, paste0(stem, ".svg")),
    plot, width = width_mm, height = height_mm, units = "mm",
    bg = "white"
  )
  ggsave(
    file.path(path_figures, paste0(stem, ".tiff")),
    plot, width = width_mm, height = height_mm, units = "mm",
    dpi = 600, compression = "lzw", bg = "white"
  )
}

save_figure(figure1, "Figure1_GBD2021_older_burden_context")

cat("GBD ZIP:", zip_path, "\n")
cat("Analytic rows:", nrow(analysis_rows), "\n")
cat("Outputs written to:", path_tables, "and", path_figures, "\n")
print(period_summary)
