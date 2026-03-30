library(tidyverse)

# =====================================================
# Load Data
# =====================================================
data <- read_csv("preliminaryanalysis/preliminarydata.csv",
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
# Identify Top 4 Species Overall
# =====================================================
species_totals <- data %>%
  summarise(across(all_of(species_cols), ~ sum(.x, na.rm = TRUE)))

top4_species <- species_totals %>%
  pivot_longer(
    cols = everything(),
    names_to = "species",
    values_to = "total_abundance"
  ) %>%
  arrange(desc(total_abundance)) %>%
  slice(1:4) %>%
  pull(species)

# =====================================================
# Convert to Long Format
# =====================================================
long_data <- data %>%
  select(year, season, period, all_of(top4_species)) %>%
  pivot_longer(
    cols = all_of(top4_species),
    names_to = "species",
    values_to = "abundance"
  )

# =====================================================
# Aggregate for Time Series
# =====================================================
time_series_summary <- long_data %>%
  group_by(period, year, species) %>%
  summarise(
    mean_abundance = mean(abundance, na.rm = TRUE),
    .groups = "drop"
  )

# =====================================================
# Plot Time Series by Period
# =====================================================
time_series_plot <- ggplot(time_series_summary,
                           aes(x = year,
                               y = mean_abundance,
                               color = species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ period, scales = "free_x") +
  labs(
    title = "Time Series of Mean Abundance for Top 4 Species by Period",
    x = "Year",
    y = "Mean Abundance",
    color = "Species"
  ) +
  theme_minimal()

# =====================================================
# Save Plot
# =====================================================
species_name_string <- paste(top4_species, collapse = "_")

file_path <- paste0(
  "preliminaryanalysis/Anish/time_series_",
  species_name_string,
  ".png"
)

ggsave(
  file_path,
  plot = time_series_plot,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Top 4 species used for this plot:\n")
cat(paste(top4_species, collapse = "\n"))
cat("\n")