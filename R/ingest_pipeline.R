library(readr)
library(dplyr)
library(janitor)
library(DBI)
library(duckdb)

# Source the pivot logic (must be in the same directory or provide full path)
source("R/taxon_pivot.R")


run_ingest_pipeline <- function(
    csv_path,
    lookup_path = "data/taxonomy_lookup_fcc.csv",
    db_path = "data/prototype.duckdb",
    drop_zero = FALSE,
    overwrite_table = TRUE
) {
  # 1) Pivot wide -> tidy long
  df_long <- ingest_and_pivot_taxa(csv_path = csv_path, drop_zero = drop_zero)
  
  # 2) Read lookup table (taxon_name -> worms_id)
  lookup <- read_csv(lookup_path, show_col_types = FALSE) %>%
    janitor::clean_names()

  
  # 3) Prepare join keys (make join robust to case/whitespace)
  df_enriched <- df_long %>%
    left_join(
      lookup,
      by = c("scientific_name" = "taxon_raw")
    )
  
  # Optional: warn if any taxa failed to map to worms_id
  n_unmatched <- sum(is.na(df_enriched$worms_id))
  if (n_unmatched > 0) {
    unmatched_taxa <- df_enriched %>%
      filter(is.na(worms_id)) %>%
      distinct(taxon) %>%
      arrange(taxon)
    
    warning(
      sprintf(
        "There are %d rows with missing worms_id (unmatched taxa). Example taxa: %s",
        n_unmatched,
        paste(head(unmatched_taxa$taxon, 10), collapse = ", ")
      )
    )
  }
  
  # 4) Connect to DuckDB
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit({
    dbDisconnect(con, shutdown = TRUE)
  }, add = TRUE)
  
  # 5) Create table (overwrite if requested)
  if (overwrite_table) {
    dbExecute(con, "DROP TABLE IF EXISTS ichthyoplankton_observations;")
  }
  
  # IMPORTANT NOTE:
  # In tidy-long format, unique_code repeats across taxa, so primary key must be (unique_code, taxon).
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ichthyoplankton_observations (
      unique_code VARCHAR,
      s_c         VARCHAR,
      s_sc        VARCHAR,
      s_l         DOUBLE,
      s_s         DOUBLE,
      latitude    DOUBLE,
      longitude   DOUBLE,
      year        INTEGER,
      season      VARCHAR,
      taxon       VARCHAR,
      abundance   DOUBLE,
      worms_id    INTEGER,
      PRIMARY KEY (unique_code, taxon)
    );
  ")
  
  # 6) Reorder/convert columns to match table schema exactly
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
  
  dup_rows <- df_to_write %>%
    add_count(unique_code, taxon, name = "n_key") %>%
    filter(n_key > 1) %>%
    arrange(unique_code, taxon)
  
  if (nrow(dup_rows) > 0) {
    message("⚠️ Found duplicate PRIMARY KEYs inside df_to_write. These rows will be handled before insert:")
    print(
      dup_rows %>%
        select(unique_code, taxon, abundance, worms_id, n_key) %>%
        head(50)
    )
  }
  
  df_to_insert <- df_to_write %>%
    distinct(unique_code, taxon, .keep_all = TRUE)
  
  DBI::dbAppendTable(con, "ichthyoplankton_observations", df_to_insert)
  
  message(sprintf(
    "Inserted %d rows after dedup (original %d rows; removed/merged %d duplicate rows).",
    nrow(df_to_insert),
    nrow(df_to_write),
    nrow(df_to_write) - nrow(df_to_insert)
  ))

  }

run_ingest_pipeline(csv_path = "data/ichthyoplankton_updatedthru2304_032824.csv", drop_zero = TRUE)


