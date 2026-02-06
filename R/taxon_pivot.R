library(dplyr)
library(tidyr)
library(janitor)
library(stringr)

taxon_pivot <- function(
    df,
    meta_cols = c("unique_code", "s_c", "s_sc", "s_l", "s_s", "latitude", "longitude", "year", "season"),
    drop_zero = FALSE
) {
# 1 Standardize headers to snake_case (SQL-friendly and consistent)
  df <- df %>% janitor::clean_names()
  
# 2 Validate required metadata columns exist
  missing <- setdiff(meta_cols, names(df))
  if (length(missing) > 0) {
    stop("Missing required metadata columns: ", paste(missing, collapse = ", "))
  }
  
# 3 Pivot all non-metadata columns into long format (taxon, abundance)
  long_data <- df %>%
    pivot_longer(
      cols = -all_of(meta_cols),
      names_to = "taxon",
      values_to = "abundance"
    ) %>%
    filter(!is.na(abundance)) %>%
    mutate(
# Coerce abundance to numeric
      abundance = as.numeric(abundance),
      
# Standardize taxon strings: dots -> spaces, then trim
      taxon = taxon %>%
        stringr::str_replace_all("\\.", " ") %>%
        stringr::str_trim()
    )
  
# Optional: drop zeros if you want to reduce table size (off by default to match your script)
  if (isTRUE(drop_zero)) {
    long_data <- long_data %>% filter(!is.na(abundance) & abundance > 0)
  }
  
# 4 Collapse to one row per unique_code to satisfy PRIMARY KEY(unique_code)
  collapsed <- long_data %>%
    group_by(unique_code) %>%
    summarise(
      s_c = first(s_c),
      s_sc = first(s_sc),
      s_l = first(s_l),
      s_s = first(s_s),
      latitude = first(latitude),
      longitude = first(longitude),
      year = first(year),
      season = first(season),
      
# Combine multiple taxa into one string (Stage 1 compromise)
      taxon = paste(unique(taxon), collapse = "; "),
      
# Sum abundances across taxa (also a compromise)
      abundance = sum(abundance, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  collapsed
}
