library(readr)
library(dplyr)
library(janitor)
library(DBI)
library(duckdb)

source("R/taxon_pivot.R")

run_ingest_pipeline <- function(
    csv_path,
    lookup_path = "data/taxonomy_lookup.csv",
    db_path = "data/prototype.duckdb",
    drop_zero = FALSE
) {
  # 1) Pivot wide -> tidy long
  df_long <- ingest_and_pivot_taxa(csv_path = csv_path, drop_zero = drop_zero)
  
  # 2) Read lookup table
  lookup <- read_csv(lookup_path, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  # 3) Join worms_id
  df_enriched <- df_long %>%
    left_join(
      lookup,
      by = c("scientific_name" = "taxon_raw")
    )
  
  # Warn unmatched worms_id
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
  
  # 5) Match schema
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
  
  # 6) Remove duplicates inside incoming dataframe
  dup_rows <- df_to_write %>%
    add_count(unique_code, taxon, name = "n_key") %>%
    filter(n_key > 1) %>%
    arrange(unique_code, taxon)
  
  if (nrow(dup_rows) > 0) {
    message("⚠️ Found duplicate PRIMARY KEYs inside incoming data. Keeping first occurrence only:")
    print(
      dup_rows %>%
        select(unique_code, taxon, abundance, worms_id, n_key) %>%
        head(50)
    )
  }
  
  df_to_insert <- df_to_write %>%
    distinct(unique_code, taxon, .keep_all = TRUE)
  
  # 7) Remove rows whose keys already exist in the database
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
  
  # 8) Append only truly new rows
  if (nrow(df_new) > 0) {
    dbAppendTable(con, "ichthyoplankton_observations", df_new)
  }
  
  message(sprintf(
    paste(
      "Inserted %d new rows.",
      "Incoming rows after in-file dedup: %d.",
      "Skipped existing DB duplicates: %d.",
      "Dropped in-file duplicates: %d."
    ),
    nrow(df_new),
    nrow(df_to_insert),
    n_skipped_existing,
    nrow(df_to_write) - nrow(df_to_insert)
  ))
}

run_ingest_pipeline(
  csv_path = "data/ichthyoplankton_updatedthru2304_032824.csv",
  drop_zero = TRUE
)