# R/dynamic_metadata.R

library(DBI)
library(duckdb)

# Read central metadata registry
read_metadata_registry <- function(registry_path = "config/metadata_registry.csv") {
  registry <- read.csv(
    registry_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  required_cols <- c(
    "dataset_name",
    "table_name",
    "column_name",
    "long_name",
    "unit",
    "data_type",
    "description"
  )
  
  missing_cols <- setdiff(required_cols, names(registry))
  
  if (length(missing_cols) > 0) {
    stop(
      "Metadata registry is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  return(registry)
}


# Escape single quotes for SQL
escape_sql_string <- function(x) {
  gsub("'", "''", x)
}

# Construct COMMENT ON SQL

build_comment_sql <- function(table_name, column_name, long_name, unit, description) {
  comment_text <- paste0(
    long_name,
    " | Unit: ", unit,
    " | Description: ", description
  )
  
  comment_text <- escape_sql_string(comment_text)
  
  sql <- paste0(
    "COMMENT ON COLUMN ",
    table_name,
    ".",
    column_name,
    " IS '",
    comment_text,
    "';"
  )
  
  return(sql)
}


# Apply metadata comments to DuckDB
apply_dynamic_metadata <- function(
    db_path = "data/prototype.duckdb",
    registry_path = "config/metadata_registry.csv",
    target_dataset = NULL
) {
  registry <- read_metadata_registry(registry_path)
  
  if (!is.null(target_dataset)) {
    registry <- registry[registry$dataset_name == target_dataset, ]
  }
  
  if (nrow(registry) == 0) {
    stop("No metadata rows found for the selected dataset.")
  }
  
  con <- dbConnect(duckdb(), dbdir = db_path)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  executed_sql <- character()
  
  for (i in seq_len(nrow(registry))) {
    sql <- build_comment_sql(
      table_name = registry$table_name[i],
      column_name = registry$column_name[i],
      long_name = registry$long_name[i],
      unit = registry$unit[i],
      description = registry$description[i]
    )
    
    DBI::dbExecute(con, sql)
    executed_sql <- c(executed_sql, sql)
  }
  
  message("Successfully applied metadata comments for ", nrow(registry), " columns.")
  
  return(executed_sql)
}

# -----------------------------
# Example usage
# -----------------------------
# apply_dynamic_metadata(
#   db_path = "data/prototype.duckdb",
#   registry_path = "config/metadata_registry.csv",
#   target_dataset = "example_dataset"
# )