library(readr)
library(dplyr)
library(janitor)
library(DBI)
library(duckdb)
library(glue)

# load project utility files as modules
source("R/taxon_pivot.R")   # provides ingest_and_pivot_taxa()
source("R/validate_rules_wide.R")    # provides validate_ichthy_raw()

# --------------------------------------------------
# 1) ingest log helpers
# --------------------------------------------------
ensure_ingest_log <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ingest_log (
      run_id           VARCHAR,
      dataset_name     VARCHAR,
      dataset_version  VARCHAR,
      write_mode       VARCHAR,
      source_path      VARCHAR,
      row_count        BIGINT,
      started_at       TIMESTAMP,
      finished_at      TIMESTAMP,
      status           VARCHAR,
      message          VARCHAR
    );
  ")
}

log_ingest_start <- function(con, run_id, dataset_name, dataset_version,
                             write_mode, source_path) {
  DBI::dbExecute(con, glue("
    INSERT INTO ingest_log(
      run_id, dataset_name, dataset_version, write_mode,
      source_path, row_count, started_at, finished_at, status, message
    )
    VALUES (
      '{run_id}', '{dataset_name}', '{dataset_version}', '{write_mode}',
      '{source_path}', NULL, now(), NULL, 'RUNNING', NULL
    );
  "))
}

log_ingest_finish <- function(con, run_id, status, row_count = NA, message = NULL) {
  msg_sql <- if (is.null(message)) "NULL" else glue("'{gsub(\"'\", \"''\", message)}'")
  rc_sql  <- if (is.na(row_count)) "NULL" else as.character(row_count)
  
  DBI::dbExecute(con, glue("
    UPDATE ingest_log
    SET finished_at = now(),
        status      = '{status}',
        row_count   = {rc_sql},
        message     = {msg_sql}
    WHERE run_id = '{run_id}';
  "))
}

# --------------------------------------------------
# 2) reuse Stage 1 transform logic
#    prepare dataframe only, no DB write here
# --------------------------------------------------
prepare_ichthyoplankton_data <- function(
    csv_path,
    lookup_path = "data/taxonomy_lookup.csv",
    drop_zero = TRUE
) {
  # wide -> long
  df_long <- ingest_and_pivot_taxa(
    csv_path = csv_path,
    drop_zero = drop_zero
  )
  
  # taxonomy lookup
  lookup <- read_csv(lookup_path, show_col_types = FALSE) %>%
    clean_names()
  
  df_enriched <- df_long %>%
    left_join(
      lookup,
      by = c("scientific_name" = "taxon_raw")
    )
  
  # warn unmatched worms_id
  n_unmatched <- sum(is.na(df_enriched$worms_id))
  if (n_unmatched > 0) {
    unmatched_taxa <- df_enriched %>%
      filter(is.na(worms_id)) %>%
      distinct(scientific_name) %>%
      arrange(scientific_name)
    
    warning(
      sprintf(
        "There are %d rows with missing worms_id. Example taxa: %s",
        n_unmatched,
        paste(head(unmatched_taxa$scientific_name, 10), collapse = ", ")
      )
    )
  }
  
  # align to target schema
  df_to_write <- df_enriched %>%
    transmute(
      unique_code = as.character(unique_code),
      s_c         = as.character(s_c),
      s_sc        = as.character(s_sc),
      s_l         = as.numeric(s_l),
      s_s         = as.numeric(s_s),
      latitude    = as.numeric(latitude),
      longitude   = as.numeric(longitude),
      year        = as.integer(year),
      season      = as.character(season),
      taxon       = as.character(taxon),
      abundance   = as.numeric(abundance),
      worms_id    = as.integer(worms_id)
    )
  
  # dedup within incoming file
  dup_rows <- df_to_write %>%
    add_count(unique_code, taxon, name = "n_key") %>%
    filter(n_key > 1) %>%
    arrange(unique_code, taxon)
  
  if (nrow(dup_rows) > 0) {
    message("⚠️ Found duplicate (unique_code, taxon) inside incoming data. Keeping first occurrence only.")
    print(
      dup_rows %>%
        select(unique_code, taxon, abundance, worms_id, n_key) %>%
        head(50)
    )
  }
  
  df_to_insert <- df_to_write %>%
    distinct(unique_code, taxon, .keep_all = TRUE)
  
  return(df_to_insert)
}

# --------------------------------------------------
# 3) Stage 3 public interface
#    decide append / overwrite / skip
# --------------------------------------------------
run_ingest_pipeline_v3 <- function(
    csv_path,
    config_path = NULL,
    dataset_name = "ichthyoplankton",
    dataset_version = NULL,
    change_type = c("first_time", "modified_time_updated", "no_change"),
    db_path = "data/prototype.duckdb",
    lookup_path = "data/taxonomy_lookup.csv",
    drop_zero = FALSE
) {
  change_type <- match.arg(change_type)
  
  # allow multiple datasets
  
  if (change_type == "no_change") {
    message("[INGEST] No change detected. Skipping ingestion.")
    return(invisible(list(
      ingested = FALSE,
      write_mode = "skip",
      reason = "no_change"
    )))
  }
  
  write_mode <- if (change_type == "first_time") "append" else "overwrite"
  
  if (is.null(dataset_version)) {
    dataset_version <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }
  
  if (!is.null(config_path) && file.exists(config_path)) {
    
    library(yaml)
    library(dplyr)
    library(tidyr)
    
    df <- read_csv(csv_path, show_col_types = FALSE)
    config <- yaml::read_yaml(config_path)
    
    # rename
    df <- df %>%
      rename(
        latitude = all_of(config$columns$latitude),
        longitude = all_of(config$columns$longitude),
        s_c = all_of(config$columns$s_c),
        s_l = all_of(config$columns$s_l),
        s_s = all_of(config$columns$s_s)
      )
    
    # pivot
    df <- df %>%
      pivot_longer(
        cols = matches("Abundance$"),
        names_to = "taxon",
        values_to = "abundance"
      ) %>%
      filter(abundance > 0) %>%
      mutate(
        taxon = gsub("_Abundance$", "", taxon)
      )
    
    # time
    df <- df %>%
      rename(date = all_of(config$time$column)) %>%
      mutate(date = as.Date(date))
    
    df_to_insert <- df  
    
  } else {
    
    df_to_insert <- prepare_ichthyoplankton_data(
      csv_path = csv_path,
      lookup_path = lookup_path,
      drop_zero = drop_zero
    )
  }
  
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit({
    dbDisconnect(con, shutdown = TRUE)
  }, add = TRUE)
  
  ensure_ingest_log(con)
  
  run_id <- paste0(
    dataset_name, "__",
    gsub("[^[:alnum:]_]", "_", dataset_version), "__",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  
  log_ingest_start(
    con = con,
    run_id = run_id,
    dataset_name = dataset_name,
    dataset_version = dataset_version,
    write_mode = write_mode,
    source_path = csv_path
  )
  
  res <- tryCatch({
    if (write_mode == "overwrite") {
      DBI::dbWriteTable(
        con,
        "ichthyoplankton_observations",
        df_to_insert,
        overwrite = TRUE
      )
      inserted <- nrow(df_to_insert)
      
    } else if (write_mode == "append") {
      existing_keys <- dbGetQuery(con, "
        SELECT unique_code, taxon
        FROM ichthyoplankton_observations
      ") %>%
        mutate(
          unique_code = as.character(unique_code),
          taxon = as.character(taxon)
        )
      
      df_new <- df_to_insert %>%
        anti_join(existing_keys, by = c("unique_code", "taxon"))
      
      n_skipped_existing <- nrow(df_to_insert) - nrow(df_new)
      
      if (n_skipped_existing > 0) {
        message(sprintf(
          "⚠️ Skipping %d rows because their (unique_code, taxon) already exist in the database.",
          n_skipped_existing
        ))
      }
      
      if (nrow(df_new) > 0) {
        DBI::dbAppendTable(con, "ichthyoplankton_observations", df_new)
      }
      
      inserted <- nrow(df_new)
      
    } else {
      stop("Unsupported write mode.")
    }
    
    log_ingest_finish(con, run_id, "SUCCESS", row_count = inserted)
    
    message(sprintf(
      "✅ Ingestion complete. dataset_name=%s, dataset_version=%s, change_type=%s, write_mode=%s, inserted_rows=%d",
      dataset_name, dataset_version, change_type, write_mode, inserted
    ))
    
    invisible(list(
      ingested = TRUE,
      dataset_name = dataset_name,
      dataset_version = dataset_version,
      change_type = change_type,
      write_mode = write_mode,
      inserted_rows = inserted
    ))
  }, error = function(e) {
    log_ingest_finish(con, run_id, "FAILED", message = e$message)
    stop(e)
  })
  
  return(res)
}

# --------------------------------------------------
# 4) validation wrapper for testing / entrypoint
# --------------------------------------------------
run_pipeline_with_validation <- function(
    csv_path,
    config_path = NULL,
    dataset_name = "ichthyoplankton",
    dataset_version = "2024-03-28",
    change_type = "modified_time_updated",
    db_path = "data/prototype.duckdb",
    lookup_path = "data/taxonomy_lookup.csv",
    drop_zero = TRUE
) {
  df_raw <- read_csv(csv_path, show_col_types = FALSE)
  
  #errors <- validate_ichthy_raw(df_raw)
  
  #if (length(errors) > 0) {
  #  stop("RAW validation failed. Ingestion aborted.")
  #}
  
  run_ingest_pipeline_v3(
    csv_path = csv_path,
    config_path = config_path,
    dataset_name = dataset_name,
    dataset_version = dataset_version,
    change_type = change_type,
    db_path = db_path,
    lookup_path = lookup_path,
    drop_zero = drop_zero
  )
}

# --------------------------------------------------
# 5) test run
# --------------------------------------------------
run_pipeline_with_validation(
  csv_path = "data/BTEDB_Abundances.csv",
  config_path = "config/BTEDB_Abundances.yaml",
  dataset_name = "krill",
  dataset_version = "2026-04-22",
  change_type = "modified_time_updated",
  db_path = "data/prototype_krill.duckdb",
  lookup_path = "data/taxonomy_lookup.csv",
  drop_zero = FALSE
)