source("R/logger.R")

# Safe wrapper: Drive pull

safe_drive_pull <- function(...) {
  log_step_start("drive_pull")
  
  res <- tryCatch({
    out <- drive_pull_if_changed(...)
    
    log_step_success("drive_pull")
    return(out)
    
  }, error = function(e) {
    log_step_error("drive_pull", e$message)
    
    return(list(
      changed = FALSE,
      reason = "error",
      error_message = e$message
    ))
  })
  
  return(res)
}

# -----------------------------
# Safe wrapper: Ingest
# -----------------------------
safe_ingest <- function(csv_path, ...) {
  log_step_start("ingest")
  
  res <- tryCatch({
    out <- run_ingest_pipeline_v3(
      csv_path = csv_path,
      ...
    )
    
    log_step_success("ingest")
    return(out)
    
  }, error = function(e) {
    log_step_error("ingest", e$message)
    
    return(list(
      ingested = FALSE,
      error_message = e$message
    ))
  })
  
  return(res)
}

# Optional retry logic

retry <- function(expr_func, max_attempts = 3, delay = 2) {
  for (i in seq_len(max_attempts)) {
    result <- tryCatch({
      return(expr_func())
    }, error = function(e) {
      if (i == max_attempts) {
        stop(e)
      } else {
        Sys.sleep(delay)
      }
    })
  }
}