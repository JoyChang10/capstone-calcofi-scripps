validate_ichthy_raw <- function(
    df_raw,
    bbox = c(lat_min = 28, lat_max = 38, lon_min = -130, lon_max = -110),
    year_min = 1950,
    year_max = as.integer(format(Sys.Date(), "%Y")),
    valid_seasons = c("winter", "spring", "summer", "fall")
) {
  
  errors <- list()
  
  # Required columns
  required_raw <- c("unique.code", "latitude", "longitude", "year", "season")
  missing_cols <- setdiff(required_raw, names(df_raw))
  if (length(missing_cols) > 0) {
    errors$missing_columns <- missing_cols
  }
  
  if (length(missing_cols) == 0) {
    
    df_raw <- df_raw %>%
      mutate(
        latitude  = suppressWarnings(as.numeric(latitude)),
        longitude = suppressWarnings(as.numeric(longitude)),
        year      = suppressWarnings(as.integer(year)),
        season    = trimws(as.character(season))
      )
    
    # NA checks
    errors$na_unique_code <- which(is.na(df_raw$unique.code))
    errors$na_latitude    <- which(is.na(df_raw$latitude))
    errors$na_longitude   <- which(is.na(df_raw$longitude))
    errors$na_year        <- which(is.na(df_raw$year))
    errors$na_season      <- which(is.na(df_raw$season))
    
    # Duplicate unique_code 
    errors$duplicate_unique_code <- which(
      duplicated(df_raw$unique.code) |
        duplicated(df_raw$unique.code, fromLast = TRUE)
    )
    
    # Invalid season
    errors$invalid_season <- which(!(df_raw$season %in% valid_seasons))
    
    # Global range
    errors$lat_global_range <- which(df_raw$latitude < -90 | df_raw$latitude > 90)
    errors$lon_global_range <- which(df_raw$longitude < -180 | df_raw$longitude > 180)
    
    # CalCOFI bbox
    errors$lat_bbox <- which(df_raw$latitude < bbox["lat_min"] |
                               df_raw$latitude > bbox["lat_max"])
    
    errors$lon_bbox <- which(df_raw$longitude < bbox["lon_min"] |
                               df_raw$longitude > bbox["lon_max"])
    
    # Year range
    errors$year_range <- which(df_raw$year < year_min |
                                 df_raw$year > year_max)
  }
  
  # Remove empty error vectors
  errors <- errors[sapply(errors, length) > 0]
  
  # Print results
  if (length(errors) == 0) {
    message("✅ RAW validation passed.")
  } else {
    message("❌ RAW validation FAILED.")
    for (nm in names(errors)) {
      message("  - ", nm, " : rows = ", paste(errors[[nm]], collapse = ", "))
    }
  }
  
  return(errors)
}