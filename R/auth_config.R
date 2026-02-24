# This script handles headless authentication for Google Drive API
library(googledrive)

# Uses the service account key path stored in .Renviron to 
# establish a non-interactive (headless) connection
authenticate_drive <- function() {
  key_path <- Sys.getenv("GDRIVE_AUTH_PATH")
  if (key_path == "") {
    stop("Error: GDRIVE_AUTH_PATH not found in .Renviron. Check your setup!")
  }
  drive_auth(path = key_path)
  message("Successfully authenticated with Google Drive via Service Account.")
}

# Test the connection (comment this out when not testing)
authenticate_drive()
drive_find()