# R/validate_rules.R
library(dplyr)

validate_ichthy <- function(df) {
  
  # ---- 1) Required fields ----
  required <- c("unique_code", "latitude", "longitude", "year", "season", "abundance", "taxon")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("❌ Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # ---- 2) Non-null checks ----
  if (any(is.na(df$unique_code))) stop("❌ unique_code has NA values.")
  if (any(is.na(df$latitude))) stop("❌ latitude has NA values.")
  if (any(is.na(df$longitude))) stop("❌ longitude has NA values.")
  if (any(is.na(df$year))) stop("❌ year has NA values.")
  if (any(is.na(df$season))) stop("❌ season has NA values.")
  
  # ---- 3) Range checks ----
  bad_lat <- which(df$latitude < -90 | df$latitude > 90)
  if (length(bad_lat) > 0) stop("❌ latitude out of range. Example row: ", bad_lat[1])
  
  bad_lon <- which(df$longitude < -180 | df$longitude > 180)
  if (length(bad_lon) > 0) stop("❌ longitude out of range. Example row: ", bad_lon[1])
  
  max_year <- as.integer(format(Sys.Date(), "%Y")) + 1
  bad_year <- which(df$year < 1950 | df$year > max_year)
  if (length(bad_year) > 0) stop("❌ year out of range. Example row: ", bad_year[1])
  
  # ---- 4) Duplicate checks ----
  dup <- which(duplicated(df$unique_code))
  if (length(dup) > 0) stop("❌ duplicated unique_code detected. Example dup row: ", dup[1])
  
  # ---- 5) Abundance sanity ----
  if (any(!is.na(df$abundance) & df$abundance < 0)) stop("❌ abundance has negative values.")
  
  message("✅ Data validation passed.")
  invisible(TRUE)
}
