library(tidyverse)

data <- read_csv("preliminaryanalysis/preliminarydata.csv",
                 show_col_types = FALSE)

# Identify Species Columns

meta_cols <- c(
  "", "unique.code", "S_C", "S_SC", "S_L", "S_S",
  "longitude", "latitude", "year", "season"
)

species_cols <- setdiff(names(data), meta_cols)
species_cols <- species_cols[!grepl("^Unnamed", species_cols)]



data <- data %>%
  mutate(total_abundance = rowSums(select(., all_of(species_cols)),
                                   na.rm = TRUE))



diagnostic_summary <- data %>%
  group_by(year, season) %>%
  summarise(
    n_samples = n(),
    mean_abundance = mean(total_abundance, na.rm = TRUE),
    median_abundance = median(total_abundance, na.rm = TRUE),
    max_abundance = max(total_abundance, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(diagnostic_summary,
          "preliminaryanalysis/Anish/temporal_diagnostic_summary.csv")

cat("Diagnostic summary saved.\n")

# Plot 1: Mean Abundance

mean_plot <- ggplot(diagnostic_summary,
                    aes(x = year,
                        y = mean_abundance,
                        color = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Temporal Trends (Mean Abundance)",
    x = "Year",
    y = "Mean Total Abundance",
    color = "Season"
  ) +
  theme_minimal()

ggsave(
  "preliminaryanalysis/Anish/visualizations/temporal_trends_mean.png",
  plot = mean_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Plot 2: Median Abundance

median_plot <- ggplot(diagnostic_summary,
                      aes(x = year,
                          y = median_abundance,
                          color = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Temporal Trends (Median Abundance)",
    x = "Year",
    y = "Median Total Abundance",
    color = "Season"
  ) +
  theme_minimal()

ggsave(
  "preliminaryanalysis/Anish/visualizations/temporal_trends_median.png",
  plot = median_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Both temporal plots saved.\n")