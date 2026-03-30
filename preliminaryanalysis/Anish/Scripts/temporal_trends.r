library(tidyverse)

# =====================================================
# USER INPUT: SPECIES FILTER
# =====================================================
selected_species <- NULL
# Example:
# selected_species <- "Engraulis.mordax"

data <- read_csv("preliminaryanalysis/preliminarydata.csv",
                 show_col_types = FALSE)

# Identify Species Columns

meta_cols <- c(
  "", "unique.code", "S_C", "S_SC", "S_L", "S_S",
  "longitude", "latitude", "year", "season"
)

species_cols <- setdiff(names(data), meta_cols)
species_cols <- species_cols[!grepl("^Unnamed", species_cols)]

# =====================================================
# FILTER TO CALCOFI CORE REGION (NEW STEP)
# =====================================================
data <- data %>%
  filter(S_L >= 76.7 & S_L <= 93.3)

# =====================================================
# SPECIES FILTER LOGIC
# =====================================================
if (is.null(selected_species)) {
  data <- data %>%
    mutate(total_abundance = rowSums(select(., all_of(species_cols)),
                                     na.rm = TRUE))
  filename_suffix <- "all_species"
} else {
  data <- data %>%
    mutate(total_abundance = .[[selected_species]])
  filename_suffix <- selected_species
}

# =====================================================
# DIAGNOSTIC SUMMARY
# =====================================================

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
          paste0("preliminaryanalysis/Anish/temporal_diagnostic_summary_",
                 filename_suffix, ".csv"))

cat("Diagnostic summary saved.\n")

# Plot 1: Mean Abundance

mean_plot <- ggplot(diagnostic_summary,
                    aes(x = year,
                        y = mean_abundance,
                        color = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = paste("Temporal Trends (Mean Abundance - ",
                  filename_suffix, ")", sep = ""),
    x = "Year",
    y = "Mean Total Abundance",
    color = "Season"
  ) +
  theme_minimal()

ggsave(
  paste0("preliminaryanalysis/Anish/temporal_trends_mean_",
         filename_suffix, ".png"),
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
    title = paste("Temporal Trends (Median Abundance - ",
                  filename_suffix, ")", sep = ""),
    x = "Year",
    y = "Median Total Abundance",
    color = "Season"
  ) +
  theme_minimal()

ggsave(
  paste0("preliminaryanalysis/Anish/temporal_trends_median_",
         filename_suffix, ".png"),
  plot = median_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Both temporal plots saved.\n")