library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)

ingest_and_pivot_taxa <- function(csv_path, drop_zero = FALSE) {
  
  # 1. Read raw CSV
  df_raw <- read_csv(csv_path, show_col_types = FALSE)
  
  # 2. Clean column names to SQL-compliant snake_case
  df_clean <- df_raw %>%
    janitor::clean_names()
  
  # 3. Identify metadata columns
  meta_cols <- c(
    "x1",
    "unique_code",
    "s_c",
    "s_sc",
    "s_l",
    "s_s",
    "longitude",
    "latitude",
    "year",
    "season"
  )
  
  # 4. Pivot from wide to tidy long format
  df_long <- df_clean %>%
    pivot_longer(
      cols = -all_of(meta_cols),
      names_to = "taxon",
      values_to = "abundance"
    ) %>%
    # NEW: add standard scientific format (Genus capitalized, species lowercased)
    mutate(
      scientific_name = {
        nm <- gsub("_", ".", taxon)
        genus <- stringr::str_to_title(stringr::word(nm, 1))
        species <- stringr::str_to_lower(stringr::word(nm, 2))
        # handle cases where there is no 2nd word (e.g., higher taxonomic groups)
        ifelse(is.na(species) | species == "", genus, paste(genus, species))
      }
    )
  
  # 5. Optionally drop zero-abundance observations
  if (drop_zero) {
    df_long <- df_long %>%
      filter(as.numeric(abundance) > 0)
  }
  
  return(df_long)
}
