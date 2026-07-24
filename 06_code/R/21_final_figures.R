# ============================================================
# 21_final_figures.R
# Final corrected figures with event study coefficients
# ============================================================

library(ggplot2)
library(dplyr)
library(haven)

PROJECT_ROOT <- "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
OUTPUT_DIR <- file.path(PROJECT_ROOT, "07_results", "figures")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("Generating final figures...\n")

# ============================================================
# Figure 1: Event Study — Corrected (with CI bands)
# ============================================================
cat("\n[1] Figure 1: Event Study — Incident ADL\n")

CHARLS_PATH <- "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
df <- read_dta(CHARLS_PATH)
df$age_2015 <- 2015 - df$rabyear
sample <- df[df$age_2015 >= 50 & df$age_2015 <= 69, ]
sample$older_2015 <- as.integer(sample$age_2015 >= 60)

# Compute event study coefficients manually (from regression output)
es_coefs <- data.frame(
  year = c(2011, 2013, 2015, 2018),
  coef = c(0.0565, 0.0556, 0, 0.1049),
  se = c(0.0036, 0.0034, 0, 0.0044),
  ci_lower = c(0.0496, 0.0491, 0, 0.0962),
  ci_upper = c(0.0635, 0.0622, 0, 0.1136)
)

p1 <- ggplot(es_coefs, aes(x = year, y = coef)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#E41A1C") +
  geom_line(color = "#E41A1C", linewidth = 1) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2015.2, y = 0.11, label = "Policy\nannouncement",
           hjust = 0, size = 3, color = "gray40") +
  annotate("text", x = 2015, y = -0.005, label = "Reference\n(2015)", size = 2.5, color = "gray40") +
  labs(x = "Survey year", y = "Coefficient (95% CI)",
       title = "Event study: Older (60-69) vs Younger (50-59) — Incident ADL",
       subtitle = "Baseline ADL=0 excluded; individual FE, age/sex adjusted") +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(color = "gray40", size = 9))

ggsave(file.path(OUTPUT_DIR, "fig1_event_study_corrected.pdf"), p1, width = 7, height = 5)
ggsave(file.path(OUTPUT_DIR, "fig1_event_study_corrected.png"), p1, width = 7, height = 5, dpi = 300)
cat("  Saved fig1_event_study_corrected.pdf/png\n")

# ============================================================
# Figure 2: Subgroup Forest Plot
# ============================================================
cat("\n[2] Figure 2: Subgroup Forest Plot\n")

subgroup <- read.csv(file.path(PROJECT_ROOT, "07_results", "tables", "final_subgroup_results.csv"))

forest_data <- subgroup %>%
  mutate(label = paste0(subgroup, ": ", level),
         row_id = rev(seq_len(n())))

p2 <- ggplot(forest_data, aes(x = coef, y = reorder(label, row_id))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "gray40") +
  geom_point(aes(size = n / 1000), color = "#E41A1C") +
  scale_size_continuous(range = c(2, 5), name = "N (thousands)") +
  labs(x = "Older×Post coefficient (95% CI)", y = NULL,
       title = "Subgroup effects on incident ADL",
       subtitle = "Age/sex adjusted, individual FE") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "fig2_subgroup_forest_corrected.pdf"), p2, width = 8, height = 6)
ggsave(file.path(OUTPUT_DIR, "fig2_subgroup_forest_corrected.png"), p2, width = 8, height = 6, dpi = 300)
cat("  Saved fig2_subgroup_forest_corrected.pdf/png\n")

# ============================================================
# Figure 3: Raw trends with 95% CI bands
# ============================================================
cat("\n[3] Figure 3: Raw ADL trends with CI bands\n")

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
      ci_lower = max(0, rate - 1.96 * se), ci_upper = rate + 1.96 * se, n = n
    ))
  }
}

p3 <- ggplot(trend_data, aes(x = year, y = rate, color = group, fill = group)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2015.2, y = max(trend_data$ci_upper) * 0.9,
           label = "Policy\nannouncement", hjust = 0, size = 3, color = "gray40") +
  labs(x = "Survey year", y = "ADL prevalence (95% CI)", color = "Age group", fill = "Age group",
       title = "ADL prevalence by age group (2011-2018)",
       subtitle = "Includes individuals with baseline ADL") +
  scale_color_manual(values = c("50-59" = "#377EB8", "60-69" = "#E41A1C")) +
  scale_fill_manual(values = c("50-59" = "#377EB8", "60-69" = "#E41A1C")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "fig3_raw_trends_corrected.pdf"), p3, width = 7, height = 5)
ggsave(file.path(OUTPUT_DIR, "fig3_raw_trends_corrected.png"), p3, width = 7, height = 5, dpi = 300)
cat("  Saved fig3_raw_trends_corrected.pdf/png\n")

# ============================================================
# Figure 4: Main results bar chart
# ============================================================
cat("\n[4] Figure 4: Main Results Summary\n")

main_results <- data.frame(
  outcome = c("Incident ADL", "Incident depression", "CESD-10 change", "Cognition change"),
  coef = c(0.0242, 0.2376, 0.5139, -0.0636),
  se = c(0.0053, 0.0070, 0.0893, 0.0216),
  ci_lower = c(0.0138, 0.2237, 0.3389, -0.1064),
  ci_upper = c(0.0346, 0.2514, 0.6890, -0.0208)
)
main_results$significant <- main_results$ci_lower > 0 | main_results$ci_upper < 0

p4 <- ggplot(main_results, aes(x = coef, y = reorder(outcome, coef))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = significant), height = 0.25) +
  geom_point(aes(color = significant), size = 4) +
  scale_color_manual(values = c("TRUE" = "#E41A1C", "FALSE" = "gray50"),
                     labels = c("TRUE" = "Significant", "FALSE" = "Not significant"),
                     name = NULL) +
  labs(x = "Coefficient (95% CI)", y = NULL,
       title = "Main outcomes: Older (60-69) vs Younger (50-69)",
       subtitle = "2015→2018, age/sex adjusted, individual FE") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig4_main_results.pdf"), p4, width = 7, height = 4)
ggsave(file.path(OUTPUT_DIR, "fig4_main_results.png"), p4, width = 7, height = 4, dpi = 300)
cat("  Saved fig4_main_results.pdf/png\n")

cat("\nAll final figures generated.\n")
