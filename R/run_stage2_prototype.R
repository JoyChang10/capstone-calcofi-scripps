library(readr)
library(dplyr)
library(janitor)

source("R/auth_config.R")
source("R/drive_pull.R")
source("R/validate_rules.R")

# Authenticate using the service account
message("Starting authentication...")
authenticate_drive()

# Check for updates and pull the file if it has changed
message("Checking Google Drive for file updates...")
pull_result <- drive_pull_if_changed()

# Process the data if a new version was downloaded
if (pull_result$changed == TRUE) {
  message("New data detected. Starting processing...")
  
  # This result$local_path is the exact, correct path including the ID prefix
  actual_file_path <- pull_result$local_path
  
  message("Attempting to read: ", actual_file_path)
  
  # Read the file using the path the script just created
  raw_data <- readr::read_csv(actual_file_path)
  
  # Run the raw validation checks
  errors <- validate_ichthy_raw(raw_data)
  
  if (length(errors) == 0) {
    message("Stage 2 prototype successful: Data pulled and validated.")
    
    # Standardize names for downstream Stage 1 processing
    clean_data <- janitor::clean_names(raw_data)
    
    # (In Stage 3, you would pass clean_data to run_ingest_pipeline)
  } else {
    stop("Stage 2 prototype failed: Data did not pass raw validation.")
  }
  
} else {
  message("No changes detected in google drive. Pipeline run complete.")
}