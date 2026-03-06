# Export DuckDB Tables to Compressed Parquet Files
# This script connects to the local DuckDB instance and exports specified tables 
# into the Parquet format.

librarian::shelf(DBI, duckdb, glue, fs)

db_path <- Sys.getenv("DB_PATH")
export_dir <- Sys.getenv("PARQUET_EXPORT_DIR")

# Ensure the export directory exists locally
if (!fs::dir_exists(export_dir)) {
  fs::dir_create(export_dir, recurse = TRUE)
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

#' Export a DuckDB table to Parquet
#' @param table_name Character. The name of the table in the database.
#' @param output_dir Character. The local directory to save the file.
#' @param connection The DBI connection object.
#' @return Logical. TRUE if successful.
export_table_to_parquet <- function(table_name, output_dir, connection) {
  
  file_path <- glue("{output_dir}/{table_name}.parquet")
  
  message(glue("Exporting table '{table_name}' to {file_path}..."))
  
  # Using DuckDB's native copy command for high-performance parquet export
  query <- glue("
    COPY {table_name} 
    TO '{file_path}' 
    (FORMAT 'parquet', COMPRESSION 'zstd')
  ")
  
  tryCatch({
    DBI::dbExecute(connection, query)
    message(glue("Successfully exported {table_name}."))
    return(TRUE)
  }, error = function(e) {
    message(glue("Failed to export {table_name}: {e$message}"))
    return(FALSE)
  })
}

# List of tables to export (as defined in Stage 3 schema expansion)
# This includes the initial ichthyoplankton and new variables
tables_to_export <- c("ichthyoplankton_observations", "temperature", "salinity", "oxygen")

# Alternatively, export all user-defined tables:
# tables_to_export <- DBI::dbListTables(con)

# Run the export for each table
export_results <- lapply(tables_to_export, function(tbl) {
  if (DBI::dbExistsTable(con, tbl)) {
    export_table_to_parquet(tbl, export_dir, con)
  } else {
    message(glue("Table '{tbl}' does not exist in the database. Skipping..."))
  }
})

DBI::dbDisconnect(con, shutdown = TRUE)

message("Parquet export process complete.")