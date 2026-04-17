library(googleCloudStorageR)
library(readr)
library(xml2)
library(dplyr)
library(glue)

# ---------- Config ----------
bucket_name <- "calcofi-db"
prefix <- "publish/ichthyo/"
max_age_days <- as.numeric(Sys.getenv("EXPECTED_MAX_AGE_DAYS", unset = "8"))

required_files <- c(
  "publish/ichthyo/event.csv",
  "publish/ichthyo/occurrence.csv",
  "publish/ichthyo/extendedMeasurementOrFact.csv",
  "publish/ichthyo/meta.xml"
)

# ---------- Auth ----------
gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket_name)

message("Starting visualization connectivity test")

# ---------- List objects ----------
objs <- gcs_list_objects(prefix = prefix)

if (is.null(objs) || nrow(objs) == 0) {
  stop("No objects found in publish folder")
}

object_names <- objs$name

# ---------- Check required files ----------
missing_files <- setdiff(required_files, object_names)

if (length(missing_files) > 0) {
  stop(glue("Missing required files: {paste(missing_files, collapse=', ')}"))
}

message("All required files exist")

# ---------- Helper functions ----------
download_and_read <- function(object_path) {
  tmp <- tempfile(fileext = ".csv")
  
  gcs_get_object(
    object_name = object_path,
    saveToDisk = tmp,
    overwrite = TRUE
  )
  
  if (!file.exists(tmp)) {
    stop(glue("Failed to download {object_path}"))
  }
  
  df <- read_csv(tmp, show_col_types = FALSE)
  
  if (nrow(df) == 0) {
    stop(glue("{object_path} is empty"))
  }
  
  return(df)
}

check_freshness <- function(object_path) {
  obj_meta <- objs %>%
    filter(name == object_path) %>%
    slice(1)
  
  updated <- as.POSIXct(obj_meta$updated, tz = "UTC")
  age <- as.numeric(difftime(Sys.time(), updated, units = "days"))
  
  if (age > max_age_days) {
    stop(glue("{object_path} is stale ({round(age, 2)} days old)"))
  }
}

# ---------- Load data ----------
event_df <- download_and_read("publish/ichthyo/event.csv")
occ_df   <- download_and_read("publish/ichthyo/occurrence.csv")
emof_df  <- download_and_read("publish/ichthyo/extendedMeasurementOrFact.csv")

# ---------- Freshness ----------
check_freshness("publish/ichthyo/event.csv")
check_freshness("publish/ichthyo/occurrence.csv")
check_freshness("publish/ichthyo/extendedMeasurementOrFact.csv")

message("Freshness check passed")

# ---------- Schema checks ----------
if (!"eventID" %in% names(event_df)) {
  stop("event.csv missing eventID")
}

if (!all(c("occurrenceID", "eventID", "scientificName") %in% names(occ_df))) {
  stop("occurrence.csv missing required columns")
}

if (!all(c("eventID", "measurementType", "measurementValue") %in% names(emof_df))) {
  stop("extendedMeasurementOrFact.csv missing required columns")
}

message("Schema checks passed")

# ---------- Relational integrity ----------
missing_event_links <- occ_df %>%
  filter(!eventID %in% event_df$eventID)

if (nrow(missing_event_links) > 0) {
  stop("occurrence.csv contains eventID not found in event.csv")
}

missing_emof_links <- emof_df %>%
  filter(!eventID %in% event_df$eventID)

if (nrow(missing_emof_links) > 0) {
  stop("extendedMeasurementOrFact.csv contains eventID not found in event.csv")
}

message("Relational integrity checks passed")

# ---------- Basic sanity ----------
if (all(is.na(occ_df$scientificName))) {
  stop("scientificName column is all NA")
}

message("Data sanity checks passed")

message("Visualization connectivity test completed successfully")