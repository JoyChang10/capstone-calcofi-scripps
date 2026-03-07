library(readr)
library(dplyr)
library(janitor)
library(DBI)
library(duckdb)
library(glue)

run_ingest_pipeline_v3 <- function(
    dataset_name,
    csv_path,
    dataset_version = NULL,
    change_type = c("first_time", "modified_time_updated", "no_change"),
    db_path = "data/prototype.duckdb",
    ...
) {
  change_type <- match.arg(change_type)
  
  if (!dataset_name %in% names(DATASETS)) {
    stop(glue("Unknown dataset_name '{dataset_name}'. Available: {paste(names(DATASETS), collapse=', ')}"))
  }
  
  # Skip ingestion if upstream says no change

  if (change_type == "no_change") {
    message(glue("[INGEST] No change detected for dataset '{dataset_name}'. Skipping ingestion."))
    return(invisible(list(
      ingested = FALSE,
      write_mode = "skip",
      reason = "no_change"
    )))
  }
  
  # Decide write mode from upstream change detection

  write_mode <- if (change_type == "first_time") "append" else "overwrite"
  
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  ensure_ingest_log(con)
  
  # Adapter info
  adapter <- DATASETS[[dataset_name]]
  fn <- adapter$fn
  adapter_args <- adapter$args
  
  # If dataset_version is not supplied, generate a simple timestamp
  if (is.null(dataset_version)) {
    dataset_version <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }
  
  run_id <- paste0(
    dataset_name, "__",
    gsub("[^[:alnum:]_]", "_", dataset_version), "__",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  
  # Run adapter
  extra_args <- list(...)
  all_args <- c(list(csv_path = csv_path), adapter_args, extra_args)
  
  out <- NULL
  tryCatch({
    out <- do.call(fn, all_args)
  }, error = function(e) {
    stop(e)
  })
  
  target_table <- out$target_table
  df <- out$df
  
  # Log start

  log_ingest_start(
    con = con,
    run_id = run_id,
    dataset_name = dataset_name,
    dataset_version = dataset_version,
    write_mode = write_mode,
    target_table = target_table,
    source_path = csv_path,
    source_hash = NA_character_
  )
  
  # Optional dataset_version column support
  # Only add if target table actually has it

  cols <- get_table_columns(con, target_table)
  has_dsver <- "dataset_version" %in% cols
  
  if (has_dsver) {
    if (!"dataset_version" %in% names(df)) {
      df <- df %>% mutate(dataset_version = as.character(dataset_version))
    } else {
      df$dataset_version <- as.character(df$dataset_version)
    }
  } else {
    if ("dataset_version" %in% names(df)) {
      df <- df %>% select(-dataset_version)
    }
  }
  

  # Execute write logic

  res <- tryCatch({
    if (write_mode == "overwrite") {
      DBI::dbWriteTable(con, target_table, df, overwrite = TRUE)
      inserted <- nrow(df)
    } else if (write_mode == "append") {
      DBI::dbAppendTable(con, target_table, df)
      inserted <- nrow(df)
    } else {
      stop(glue("Unsupported write_mode: {write_mode}"))
    }
    
    log_ingest_finish(con, run_id, "SUCCESS", row_count = inserted)
    
    message(glue(
      "✅ Ingested dataset='{dataset_name}' ",
      "version='{dataset_version}' ",
      "change_type='{change_type}' ",
      "write_mode='{write_mode}' ",
      "rows={inserted}"
    ))
    
    invisible(list(
      ingested = TRUE,
      write_mode = write_mode,
      reason = change_type,
      row_count = inserted,
      dataset_version = dataset_version
    ))
  }, error = function(e) {
    log_ingest_finish(con, run_id, "FAILED", message = e$message)
    stop(e)
  })
  
  res
}

# Example usage

source("R/ingest_pipeline.R")
run_pipeline_with_validation <- function(
    csv_path = "data/ichthyoplankton_updatedthru2304_032824.csv",
    dataset_name = "ichthyoplankton",
    dataset_version = "2024-03-28",
    change_type = "modified_time_updated",
    db_path = "data/prototype.duckdb",
    drop_zero = TRUE
) {
  
  df_raw <- readr::read_csv(csv_path, show_col_types = FALSE)
  
  #errors <- validate_ichthy_raw(df_raw)
  
  #if (length(errors) > 0) {
   # stop("RAW validation failed. Ingestion aborted.")
  #}
  
  run_ingest_pipeline_v3(
    dataset_name = dataset_name,
    csv_path = csv_path,
    dataset_version = dataset_version,
    change_type = change_type,
    db_path = db_path,
    drop_zero = drop_zero
  )
}

run_pipeline_with_validation()

#run_ingest_pipeline_v3(
#  dataset_name = "ichthyoplankton",
 # csv_path = "data/ichthyoplankton_updatedthru2304_032824.csv",
  #dataset_version = "2024-03-28",
  #change_type = "first_time",
  #db_path = "data/prototype.duckdb",
  #drop_zero = TRUE
#)