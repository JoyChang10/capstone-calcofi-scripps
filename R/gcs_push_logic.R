# Push Parquet Files to Google Cloud Storage
# This script authenticates with Google Cloud Storage and provides a modular function 
# to upload Parquet files from a local directory to a designated GCS bucket (the "Data Lake").

librarian::shelf(googleCloudStorageR, dplyr, glue)

gcs_auth_file <- Sys.getenv("GCS_AUTH_FILE")
gcs_bucket    <- Sys.getenv("GCS_DEFAULT_BUCKET")
gcs_project   <- Sys.getenv("GCS_PROJECT_ID")

# Perform headless authentication using the Service Account JSON key
if (file.exists(gcs_auth_file)) {
  googleCloudStorageR::gcs_auth(json_file = gcs_auth_file)
} else {
  stop(glue("Authentication file not found at: {gcs_auth_file}"))
}

# Set the global bucket for the session
if (gcs_bucket != "") {
  googleCloudStorageR::gcs_global_bucket(gcs_bucket)
} else {
  stop("GCS_DEFAULT_BUCKET is not defined in .Renviron")
}

# Upload a file to Google Cloud Storage
# Parameters:
#' @param local_path Character. The local path to the file (e.g., a Parquet file).
#' @param remote_name Character. The desired name/path in the GCS bucket. Defaults to the base name of the local file.
#' @param predefined_acl Character. Access control. Defaults to "bucketLevel".
#' @return Metadata about the uploaded object.
#' @export

gcs_push_file <- function(local_path, remote_name = basename(local_path), predefined_acl = "bucketLevel") {
  
  # Ensure the file exists locally before attempting upload
  if (!file.exists(local_path)) {
    stop(glue("Local file does not exist: {local_path}"))
  }
  
  message(glue("Uploading {local_path} to gs://{gcs_bucket}/{remote_name}..."))
  
  # Perform the upload
  upload_status <- googleCloudStorageR::gcs_upload(
    file = local_path,
    name = remote_name,
    predefinedAcl = predefined_acl
  )
  
  message("Upload successful.")
  return(upload_status)
}

# Usage Example (comment out)
gcs_push_file(
  local_path = "data/parquet/ichthyoplankton_observations.parquet",
  remote_name = "staging/ichthyoplankton_observations.parquet"
)