# generalized_ingest.R
library(yaml)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

#' Ingest Dataset using Configuration
#'
#' @param config_path Path to the YAML configuration file.
#' @return A tidy, long-format dataframe ready for database insertion.
ingest_configured_dataset <- function(config_path) {
  config <- yaml::read_yaml(config_path)
  dataset <- readr::read_csv(config$source$file_path, show_col_types = FALSE)
  
  # 1. Convert Global Null Indicators
  global_nulls <- config$columns$null_indicators$global
  if (!is.null(global_nulls)) {
    dataset <- dataset %>%
      mutate(across(everything(), ~ if_else(. %in% global_nulls, NA, .)))
  }
  
  # 2. Dynamic Column Renaming
  # The YAML provides mappings like: list(original_col = "standard_col")
  if (!is.null(config$columns$mappings)) {
    mapping_list <- unlist(config$columns$mappings)
    # dplyr::rename expects c(new_name = "old_name")
    rename_map <- setNames(names(mapping_list), mapping_list)
    dataset <- dataset %>% rename(any_of(rename_map))
  }
  
  # 3. Parse Dates into ISO 8601
  date_structure <- config$date_structure
  if (!is.null(date_structure) && date_structure == "standard") {
    date_col <- config$date_columns$date
    date_format <- config$date_columns$format
    dataset <- dataset %>%
      mutate(!!sym(date_col) := as.Date(!!sym(date_col), format = date_format))
    
  } else if (!is.null(date_structure) && date_structure == "split") {
    year_col <- config$date_columns$year
    season_col <- config$date_columns$season
    
    dataset <- dataset %>%
      mutate(
        month_mapped = case_when(
          tolower(!!sym(season_col)) %in% c("winter", "1") ~ 1,
          tolower(!!sym(season_col)) %in% c("spring", "2") ~ 4,
          tolower(!!sym(season_col)) %in% c("summer", "3") ~ 7,
          tolower(!!sym(season_col)) %in% c("fall", "autumn", "4") ~ 10,
          TRUE ~ 1
        ),
        iso_date = make_date(year = !!sym(year_col), month = month_mapped, day = 1)
      ) %>%
      select(-month_mapped)
  }
  
  # 4. Dynamic Pivoting (Wide to Long via Negative Selection)
  if (!is.null(config$pivot)) {
    id_cols <- config$pivot$id_columns
    name_col <- config$pivot$name_to
    value_col <- config$pivot$value_to
    
    dataset <- dataset %>%
      pivot_longer(
        cols = -all_of(id_cols),
        names_to = name_col,
        values_to = value_col
      ) %>%
      # Clean up: remove zero abundances and clean taxon strings if needed
      filter(!is.na(!!sym(value_col)) & !!sym(value_col) > 0)
  }
  
  return(dataset)
}