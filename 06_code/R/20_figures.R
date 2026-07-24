# ============================================================
# 20_figures.R
# Publication-quality figures for CHARLS + CFPS analysis
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)

PROJECT_ROOT <- "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
OUTPUT_DIR <- file.path(PROJECT_ROOT, "07_results", "figures")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("Generating publication figures...\n")

# ============================================================
# Figure 1: CHARLS Event Study — Incident ADL
# ============================================================
cat("\n[1] Figure 1: Event Study — Incident ADL\n")

CHARLS_PATH <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
df <- read_dta(CHARLS_PATH)
df$age_2015 <- 2015 - df$rabyear
sample <- df[df$age_2015 >= 50 & df$age_2015 <= 69, ]
sample$older_2015 <- as.integer(sample$age_2015 >= 60)

# Build event study data (baseline ADL=0)
es_data <- data.frame()
for (w in 1:4) {
  year <- c(2011, 2013, 2015, 2018)[w]
  adl_var <- paste0("r", w, "adla_c")
  inw_var <- paste0("inw", w)

  sub <- sample[sample[[inw_var]] == 1 & !is.na(sample[[adl_var]]), ]

  # Get baseline ADL from wave 3
  bl_adl <- sample[sample$inw3 == 1, c("ID", adl_var)]
  names(bl_adl) <- c("ID", "bl_adl")
  bl_adl$bl_adl <- as.numeric(bl_adl$bl_adl)
  sub <- merge(sub, bl_adl, by = "ID", all.x = TRUE)

  # Exclude baseline ADL >= 1
  sub <- sub[sub$bl_adl == 0, ]

  older <- sub[sub$older_2015 == 1, ]
  younger <- sub[sub$older_2015 == 0, ]

  es_data <- rbind(es_data, data.frame(
    year = year,
    older_rate = mean(as.numeric(older[[adl_var]]) >= 1, na.rm = TRUE),
    younger_rate = mean(as.numeric(younger[[adl_var]]) >= 1, na.rm = TRUE),
    n_older = nrow(older),
    n_younger = nrow(younger)
  ))
}

es_data$diff <- es_data$older_rate - es_data$younger_rate

# Plot
p1 <- ggplot(es_data, aes(x = year)) +
  geom_point(aes(y = older_rate, color = "Older (60-69)"), size = 3) +
  geom_line(aes(y = older_rate, color = "Older (60-69)"), linewidth = 0.8) +
  geom_point(aes(y = younger_rate, color = "Younger (50-59)"), size = 3) +
  geom_line(aes(y = younger_rate, color = "Younger (50-59)"), linewidth = 0.8) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2015.3, y = max(es_data$older_rate) * 0.95,
           label = "Policy\nannouncement", hjust = 0, size = 3, color = "gray40") +
  labs(x = "Survey year", y = "Incident ADL prevalence",
       color = "Group",
       title = "New-onset ADL by age group (baseline ADL=0 excluded)") +
  scale_color_manual(values = c("Older (60-69)" = "#E41A1C", "Younger (50-59)" = "#377EB8")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 11))

ggsave(file.path(OUTPUT_DIR, "fig1_event_study_adl.pdf"), p1, width = 7, height = 5)
ggsave(file.path(OUTPUT_DIR, "fig1_event_study_adl.png"), p1, width = 7, height = 5, dpi = 300)
cat("  Saved fig1_event_study_adl.pdf/png\n")

# ============================================================
# Figure 2: Subgroup Forest Plot — Incident ADL
# ============================================================
cat("\n[2] Figure 2: Subgroup Forest Plot\n")

subgroup <- read.csv(file.path(PROJECT_ROOT, "07_results", "tables", "corrected_subgroup_results.csv"))

# Create forest plot data
forest_data <- subgroup %>%
  mutate(label = paste0(subgroup, ": ", level),
         row_id = rev(seq_len(n())))

p2 <- ggplot(forest_data, aes(x = coef, y = reorder(label, row_id))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "gray40") +
  geom_point(aes(size = n / 1000), color = "#E41A1C") +
  scale_size_continuous(range = c(2, 5), name = "N (thousands)") +
  labs(x = "Older×Post coefficient (95% CI)",
       y = NULL,
       title = "Subgroup effects on incident ADL") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "fig2_subgroup_forest.pdf"), p2, width = 8, height = 6)
ggsave(file.path(OUTPUT_DIR, "fig2_subgroup_forest.png"), p2, width = 8, height = 6, dpi = 300)
cat("  Saved fig2_subgroup_forest.pdf/png\n")

# ============================================================
# Figure 3: Raw trends — CHARLS ADL by age group
# ============================================================
cat("\n[3] Figure 3: Raw ADL trends by age group\n")

trend_data <- data.frame()
for (w in 1:4) {
  year <- c(2011, 2013, 2015, 2018)[w]
  adl_var <- paste0("r", w, "adla_c")
  inw_var <- paste0("inw", w)

  sub <- sample[sample[[inw_var]] == 1 & !is.na(sample[[adl_var]]), ]
  sub$adl_any <- as.integer(sub[[adl_var]] >= 1)

  for (age_group in c("50-59", "60-69")) {
    if (age_group == "50-59") {
      sg <- sub[sub$age_2015 >= 50 & sub$age_2015 < 60, ]
    } else {
      sg <- sub[sub$age_2015 >= 60 & sub$age_2015 <= 69, ]
    }
    n <- nrow(sg)
    rate <- mean(sg$adl_any, na.rm = TRUE)
    se <- sqrt(rate * (1 - rate) / n)
    trend_data <- rbind(trend_data, data.frame(
      year = year, group = age_group, rate = rate,
      ci_lower = rate - 1.96 * se, ci_upper = rate + 1.96 * se, n = n
    ))
  }
}

p3 <- ggplot(trend_data, aes(x = year, y = rate, color = group, fill = group)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "gray50") +
  labs(x = "Survey year", y = "ADL prevalence", color = "Age group", fill = "Age group",
       title = "ADL prevalence by age group (2011-2018)") +
  scale_color_manual(values = c("50-59" = "#377EB8", "60-69" = "#E41A1C")) +
  scale_fill_manual(values = c("50-59" = "#377EB8", "60-69" = "#E41A1C")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "fig3_raw_trends_adl.pdf"), p3, width = 7, height = 5)
ggsave(file.path(OUTPUT_DIR, "fig3_raw_trends_adl.png"), p3, width = 7, height = 5, dpi = 300)
cat("  Saved fig3_raw_trends_adl.pdf/png\n")

# ============================================================
# Figure 4: DDD results bar chart
# ============================================================
cat("\n[4] Figure 4: DDD Results\n")

ddd_data <- data.frame(
  condition = c("Diabetes", "Any CVD", "Heart Disease", "Hypertension", "Lung Disease"),
  coef = c(-0.0796, -0.0406, -0.0360, -0.0384, 0.0131),
  se = c(0.0232, 0.0114, 0.0180, 0.0124, 0.0196),
  pvalue = c(0.0006, 0.0004, 0.0458, 0.0020, 0.5060)
)
ddd_data$ci_lower <- ddd_data$coef - 1.96 * ddd_data$se
ddd_data$ci_upper <- ddd_data$coef + 1.96 * ddd_data$se
ddd_data$sig <- ifelse(ddd_data$pvalue < 0.05, "p<0.05", "ns")
ddd_data$sig <- factor(ddd_data$sig, levels = c("p<0.05", "ns"))

p4 <- ggplot(ddd_data, aes(x = coef, y = reorder(condition, coef))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = sig), height = 0.25) +
  geom_point(aes(color = sig), size = 4) +
  scale_color_manual(values = c("p<0.05" = "#E41A1C", "ns" = "gray50"),
                     name = "Significance") +
  labs(x = "DDD coefficient (95% CI)", y = NULL,
       title = "Older×Post×Condition DDD: Incident ADL") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig4_ddd_results.pdf"), p4, width = 7, height = 4)
ggsave(file.path(OUTPUT_DIR, "fig4_ddd_results.png"), p4, width = 7, height = 4, dpi = 300)
cat("  Saved fig4_ddd_results.pdf/png\n")

cat("\nAll figures generated successfully.\n")
