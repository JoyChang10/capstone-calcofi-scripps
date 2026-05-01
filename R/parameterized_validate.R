library(dplyr)
library(lubridate)
library(readr)


# Central validation rules

VALIDATION_RULES <- list(
  latitude_range = c(28, 38),
  longitude_range = c(-130, -110),
  date_range = list(
    start = as.Date("1950-01-01"),
    end = as.Date("2026-12-31")
  ),
  non_negative_columns = c("abundance")
)


# -----------------------------
# Helper function: check required columns
# -----------------------------
check_required_columns <- function(df, required_cols) {
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}


# -----------------------------
# Validate latitude and longitude
# -----------------------------
validate_coordinates <- function(df, rules = VALIDATION_RULES) {
  check_required_columns(df, c("latitude", "longitude"))
  
  lat_range <- rules$latitude_range
  lon_range <- rules$longitude_range
  
  invalid_lat <- df %>%
    filter(
      is.na(latitude) |
        latitude < lat_range[1] |
        latitude > lat_range[2]
    )
  
  invalid_lon <- df %>%
    filter(
      is.na(longitude) |
        longitude < lon_range[1] |
        longitude > lon_range[2]
    )
  
  list(
    latitude_min_allowed = lat_range[1],
    latitude_max_allowed = lat_range[2],
    longitude_min_allowed = lon_range[1],
    longitude_max_allowed = lon_range[2],
    invalid_latitude_rows = nrow(invalid_lat),
    invalid_longitude_rows = nrow(invalid_lon)
  )
}


# Validate date range

validate_dates <- function(df, rules = VALIDATION_RULES) {
  check_required_columns(df, c("date"))
  
  start_date <- rules$date_range$start
  end_date <- rules$date_range$end
  
  df_checked <- df %>%
    mutate(parsed_date = as.Date(date))
  
  invalid_dates <- df_checked %>%
    filter(
      is.na(parsed_date) |
        parsed_date < start_date |
        parsed_date > end_date
    )
  
  list(
    start_date_allowed = as.character(start_date),
    end_date_allowed = as.character(end_date),
    invalid_date_rows = nrow(invalid_dates)
  )
}


# Validate non-negative measurement columns

validate_non_negative <- function(df, rules = VALIDATION_RULES) {
  cols <- rules$non_negative_columns
  
  results <- list()
  
  for (col in cols) {
    if (!col %in% names(df)) {
      results[[paste0(col, "_status")]] <- "column_missing"
      next
    }
    
    invalid_rows <- df %>%
      filter(!is.na(.data[[col]]) & .data[[col]] < 0)
    
    results[[paste0("invalid_", col, "_rows")]] <- nrow(invalid_rows)
  }
  
  results
}


# Run all validation checks

run_validation <- function(df, dataset_name = "unknown_dataset", rules = VALIDATION_RULES) {
  coordinate_result <- validate_coordinates(df, rules)
  date_result <- validate_dates(df, rules)
  non_negative_result <- validate_non_negative(df, rules)
  
  validation_report <- list(
    dataset_name = dataset_name,
    total_rows = nrow(df),
    coordinate_validation = coordinate_result,
    date_validation = date_result,
    non_negative_validation = non_negative_result,
    validation_time = as.character(Sys.time())
  )
  
  return(validation_report)
}