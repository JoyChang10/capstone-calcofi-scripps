# R/ingest_pipeline.R  (Stage 3)
library(readr)
library(dplyr)
library(janitor)
library(DBI)
library(duckdb)
library(glue)
library(digest)

# If you use pivot_longer in other adapters
suppressPackageStartupMessages({
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required for Stage 3 adapters. Please install.packages('tidyr').")
  }
})

# Source your existing pivot logic
source("R/taxon_pivot.R")

# -----------------------------
# 0) Ingest log helpers
# -----------------------------
ensure_ingest_log <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ingest_log (
      run_id           VARCHAR,
      dataset_name     VARCHAR,
      dataset_version  VARCHAR,
      write_mode       VARCHAR,
      target_table     VARCHAR,
      source_path      VARCHAR,
      source_hash      VARCHAR,
      row_count        BIGINT,
      started_at       TIMESTAMP,
      finished_at      TIMESTAMP,
      status           VARCHAR,
      message          VARCHAR
    );
  ")
}

log_ingest_start <- function(con, run_id, dataset_name, dataset_version, write_mode,
                             target_table, source_path, source_hash) {
  DBI::dbExecute(con, glue("
    INSERT INTO ingest_log(run_id, dataset_name, dataset_version, write_mode, target_table,
                           source_path, source_hash, row_count, started_at, finished_at, status, message)
    VALUES ('{run_id}', '{dataset_name}', '{dataset_version}', '{write_mode}', '{target_table}',
            '{source_path}', '{source_hash}', NULL, now(), NULL, 'RUNNING', NULL);
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

# -----------------------------
# 1) Table/column introspection
# -----------------------------
table_exists <- function(con, table_name) {
  q <- DBI::dbGetQuery(con, glue("
    SELECT COUNT(*) AS n
    FROM information_schema.tables
    WHERE table_name = '{table_name}';
  "))
  q$n[1] > 0
}

get_table_columns <- function(con, table_name) {
  if (!table_exists(con, table_name)) return(character(0))
  cols <- DBI::dbGetQuery(con, glue("DESCRIBE {table_name};"))
  cols$column_name
}

# -----------------------------
# 2) Generic writer: append/overwrite/upsert
#    Upsert = staging + DELETE matching keys + INSERT staging
# -----------------------------
write_table_mode <- function(con, target_table, df, key_cols, mode = c("append", "overwrite", "upsert")) {
  mode <- match.arg(mode)
  
  if (mode == "overwrite") {
    DBI::dbWriteTable(con, target_table, df, overwrite = TRUE)
    return(invisible(nrow(df)))
  }
  
  if (mode == "append") {
    DBI::dbAppendTable(con, target_table, df)
    return(invisible(nrow(df)))
  }
  
  # upsert
  staging <- paste0(target_table, "__staging__", as.integer(runif(1, 1e6, 9e6)))
  DBI::dbWriteTable(con, staging, df, overwrite = TRUE)
  
  on_clause <- paste(sprintf("t.%s = s.%s", key_cols, key_cols), collapse = " AND ")
  
  DBI::dbExecute(con, glue("
    DELETE FROM {target_table} t
    USING {staging} s
    WHERE {on_clause};
  "))
  
  DBI::dbExecute(con, glue("INSERT INTO {target_table} SELECT * FROM {staging};"))
  DBI::dbExecute(con, glue("DROP TABLE {staging};"))
  
  invisible(nrow(df))
}

# -----------------------------
# 3) Dataset adapter: ichthyoplankton
# -----------------------------
ingest_ichthyoplankton <- function(csv_path,
                                   lookup_path = "data/taxonomy_lookup.csv",
                                   drop_zero = FALSE) {
  # 1) Pivot wide -> tidy
  df_long <- ingest_and_pivot_taxa(csv_path = csv_path, drop_zero = drop_zero)
  
  # 2) Lookup table
  lookup <- read_csv(lookup_path, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  # 3) Join worms_id
  df_enriched <- df_long %>%
    left_join(lookup, by = c("scientific_name" = "taxon_raw"))
  
  # Warn unmatched
  n_unmatched <- sum(is.na(df_enriched$worms_id))
  if (n_unmatched > 0) {
    unmatched_taxa <- df_enriched %>%
      filter(is.na(worms_id)) %>%
      distinct(scientific_name) %>%
      arrange(scientific_name)
    
    warning(glue(
      "There are {n_unmatched} rows with missing worms_id. Example taxa: {paste(head(unmatched_taxa$scientific_name, 10), collapse = ', ')}"
    ))
  }
  
  # 4) Coerce + match schema ordering (Stage1/2)
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
  
  # dedup on key
  df_dedup <- df_to_write %>% distinct(unique_code, taxon, .keep_all = TRUE)
  
  list(
    df = df_dedup,
    target_table = "ichthyoplankton_observations",
    key_cols = c("unique_code", "taxon")
  )
}

# -----------------------------
# 4) Registry (add more datasets here)
#    IMPORTANT: do NOT hardcode drop_zero here if you want to pass it at runtime
# -----------------------------
DATASETS <- list(
  ichthyoplankton = list(
    fn = ingest_ichthyoplankton,
    args = list(
      lookup_path = "data/taxonomy_lookup.csv"
      # drop_zero can be passed when calling run_ingest_pipeline_v3()
    )
  )
)

# -----------------------------
# 5) Main entry: Stage 3 runner
# -----------------------------
run_ingest_pipeline_v3 <- function(
    dataset_name,
    csv_path,
    dataset_version,
    write_mode = c("append", "overwrite", "upsert"),
    db_path = "data/prototype.duckdb",
    ...
) {
  write_mode <- match.arg(write_mode)
  
  if (!dataset_name %in% names(DATASETS)) {
    stop(glue("Unknown dataset_name '{dataset_name}'. Available: {paste(names(DATASETS), collapse=', ')}"))
  }
  
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  ensure_ingest_log(con)
  
  # Adapter info
  adapter <- DATASETS[[dataset_name]]
  fn <- adapter$fn
  adapter_args <- adapter$args
  
  # Hash + run id
  source_hash <- digest::digest(file = csv_path, algo = "md5")
  run_id <- paste0(dataset_name, "__", dataset_version, "__", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  # Run adapter to get df + table + key
  extra_args <- list(...)
  all_args <- c(list(csv_path = csv_path), adapter_args, extra_args)
  
  out <- NULL
  tryCatch({
    out <- do.call(fn, all_args)
  }, error = function(e) {
    stop(e)
  })
  
  target_table <- out$target_table
  key_cols <- out$key_cols
  df <- out$df
  
  # Log start
  log_ingest_start(
    con, run_id, dataset_name, dataset_version, write_mode,
    target_table, csv_path, source_hash
  )
  
  # Prevent accidental duplicate append for same version
  already_success <- DBI::dbGetQuery(con, glue("
    SELECT COUNT(*) AS n
    FROM ingest_log
    WHERE dataset_name = '{dataset_name}'
      AND dataset_version = '{dataset_version}'
      AND status = 'SUCCESS';
  "))$n[1] > 0
  
  if (already_success && write_mode == "append") {
    msg <- "Refused: append would duplicate an already-successful version. Use upsert/overwrite."
    log_ingest_finish(con, run_id, "FAILED", message = msg)
    stop(msg)
  }
  
  # --- Critical fix: only use dataset_version if target table actually has that column
  cols <- get_table_columns(con, target_table)
  has_dsver <- "dataset_version" %in% cols
  
  if (has_dsver) {
    # Ensure df has dataset_version column
    if (!"dataset_version" %in% names(df)) {
      df <- df %>% mutate(dataset_version = as.character(dataset_version))
    } else {
      df$dataset_version <- as.character(df$dataset_version)
    }
    
    # Include dataset_version in keys for upsert uniqueness
    if (!"dataset_version" %in% key_cols) {
      key_cols <- c(key_cols, "dataset_version")
    }
  } else {
    # Ensure df does NOT contain dataset_version (avoid column mismatch)
    if ("dataset_version" %in% names(df)) {
      df <- df %>% select(-dataset_version)
    }
    # Ensure keys do NOT contain dataset_version (avoid Binder Error)
    key_cols <- setdiff(key_cols, "dataset_version")
  }
  
  # Write
  res <- tryCatch({
    inserted <- write_table_mode(con, target_table, df, key_cols, mode = write_mode)
    log_ingest_finish(con, run_id, "SUCCESS", row_count = inserted)
    message(glue("✅ Ingested dataset='{dataset_name}' version='{dataset_version}' mode='{write_mode}' rows={inserted}"))
    invisible(TRUE)
  }, error = function(e) {
    log_ingest_finish(con, run_id, "FAILED", message = e$message)
    stop(e)
  })
  
  res
}

# -----------------------------
# 6) Example usage
# -----------------------------
 source("R/ingest_pipeline.R")
 run_ingest_pipeline_v3(
   dataset_name = "ichthyoplankton",
   csv_path = "data/ichthyoplankton_updatedthru2304_032824.csv",
   dataset_version = "2024-03-28",
   write_mode = "upsert",
   drop_zero = TRUE
 )