library(tidyverse)
library(sf)
library(ggspatial)
library(viridis)



data <- read_csv("preliminaryanalysis/preliminarydata.csv",
                 show_col_types = FALSE)



meta_cols <- c(
  "", "unique.code", "S_C", "S_SC", "S_L", "S_S",
  "longitude", "latitude", "year", "season"
)

species_cols <- setdiff(names(data), meta_cols)
species_cols <- species_cols[!grepl("^Unnamed", species_cols)]



data <- data %>%
  mutate(
    total_abundance = rowSums(select(., all_of(species_cols)), na.rm = TRUE)
  )

# Scale point sizes (ggplot uses small size units)
data <- data %>%
  mutate(size_scaled = (total_abundance / max(total_abundance)) * 3)

gdf <- st_as_sf(data,
                coords = c("longitude", "latitude"),
                crs = 4326)



map1 <- ggplot(gdf) +
  annotation_map_tile(type = "cartolight") +
  geom_sf(aes(size = size_scaled,
              color = as.numeric(S_L)),
          alpha = 0.7) +
  scale_color_viridis(option = "D") +
  coord_sf(crs = 3857) +
  labs(
    title = "Spatial Distribution of Standardized Fish Abundance",
    color = "CalCOFI Line (North → South)",
    size = "Relative Abundance"
  ) +
  theme_minimal()

ggsave(
  "preliminaryanalysis/Anish/visualizations/spatial_distribution_abundance.png",
  map1,
  width = 10,
  height = 8,
  dpi = 300
)



# Identify dominant species per station
data$dominant_species <- colnames(select(data, all_of(species_cols)))[
  max.col(select(data, all_of(species_cols)), ties.method = "first")
]

# Top 10 species overall
species_totals <- colSums(select(data, all_of(species_cols)), na.rm = TRUE)
top10_species <- names(sort(species_totals, decreasing = TRUE))[1:10]

data <- data %>%
  mutate(
    dominant_grouped = ifelse(dominant_species %in% top10_species,
                              dominant_species,
                              "Other")
  )

gdf2 <- st_as_sf(data,
                 coords = c("longitude", "latitude"),
                 crs = 4326)

map2 <- ggplot(gdf2) +
  annotation_map_tile(type = "cartolight") +
  geom_sf(aes(size = size_scaled,
              color = dominant_grouped),
          alpha = 0.8) +
  coord_sf(crs = 3857) +
  labs(
    title = "Spatial Distribution of Dominant Species",
    color = "Dominant Species (Top 10)",
    size = "Relative Abundance"
  ) +
  theme_minimal()

ggsave(
  "preliminaryanalysis/Anish/visualizations/dominant_species_map.png",
  map2,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Both spatial maps saved in Anish folder.\n")