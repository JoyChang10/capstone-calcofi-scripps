source("R/logger.R")
source("R/error_handling_logic.R")
source("R/drive_pull.R")
source("R/ingest_pipeline_updated.R")

run_full_pipeline <- function() {
  init_logger()
  log_pipeline_start()
  
  # Step 1: Pull data
  
  pull_res <- safe_drive_pull()
  
  if (!pull_res$changed) {
    log_info_msg("No new data detected. Pipeline exiting.")
    log_pipeline_end()
    return(invisible(NULL))
  }

  # Step 2: Ingest

  ingest_res <- safe_ingest(
    csv_path = pull_res$local_path,
    dataset_name = "ichthyoplankton",
    dataset_version = pull_res$modified_time,
    change_type = pull_res$reason,
    db_path = "data/prototype.duckdb"
  )
  
  if (!ingest_res$ingested) {
    log_error_msg("Ingestion failed.")
  }
  
  log_pipeline_end()
}

# Run pipeline

run_full_pipeline()