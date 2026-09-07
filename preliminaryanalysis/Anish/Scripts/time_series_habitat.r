library(tidyverse)

# =====================================================
# Load Data
# =====================================================
data <- read_csv("preliminaryanalysis/preliminarydata.csv",
                 show_col_types = FALSE)

species_info <- read_csv("preliminaryanalysis/species habitat categories2.csv",
                         show_col_types = FALSE)

# =====================================================
# Identify Species Columns
# =====================================================
meta_cols <- c(
  "", "unique.code", "S_C", "S_SC", "S_L", "S_S",
  "longitude", "latitude", "year", "season"
)

species_cols <- setdiff(names(data), meta_cols)
species_cols <- species_cols[!grepl("^Unnamed", species_cols)]
species_cols <- species_cols[!grepl("^\\.\\.\\.", species_cols)]

# =====================================================
# Filter to Core CalCOFI Stations
# =====================================================
data <- data %>%
  filter(S_L >= 76.7 & S_L <= 93.3)

# =====================================================
# Add Time Periods
# =====================================================
data <- data %>%
  mutate(period = case_when(
    year >= 1951 & year <= 1976 ~ "1951-1976",
    year > 1976 & year <= 1998 ~ "1976-1998",
    year > 1998 & year <= 2014 ~ "1999-2014",
    year > 2014 ~ "2014-present",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

# =====================================================
# Convert to Long Format
# =====================================================
long_data <- data %>%
  select(year, season, period, all_of(species_cols)) %>%
  pivot_longer(
    cols = all_of(species_cols),
    names_to = "species",
    values_to = "abundance"
  )

# =====================================================
# Join Habitat Metadata
# =====================================================
long_data <- long_data %>%
  left_join(species_info, by = "species")

# =====================================================
# Aggregate by Period, Year, Habitat
# =====================================================
habitat_summary <- long_data %>%
  filter(!is.na(habitat)) %>%
  group_by(period, year, habitat) %>%
  summarise(
    mean_abundance = mean(abundance, na.rm = TRUE),
    .groups = "drop"
  )

# =====================================================
# Plot Time Series
# =====================================================
habitat_plot <- ggplot(habitat_summary,
                       aes(x = year,
                           y = mean_abundance,
                           color = habitat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~period, scales = "free_x") +
  labs(
    title = "Time Series of Mean Abundance by Habitat and Period",
    x = "Year",
    y = "Mean Abundance",
    color = "Habitat"
  ) +
  theme_minimal()

# =====================================================
# Save Plot
# =====================================================
ggsave(
  "preliminaryanalysis/Anish/time_series_habitat_periods.png",
  plot = habitat_plot,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Habitat time series by period saved.\n")