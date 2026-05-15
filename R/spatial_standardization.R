library(DBI)
library(duckdb)
library(glue)

# Central spatial settings
SPATIAL_SETTINGS <- list(
  latitude_col = "latitude",
  longitude_col = "longitude",
  geometry_col = "geom_wgs84",
  crs = "EPSG:4326"
)

# Connect to DuckDB
connect_duckdb <- function(db_path = "data/prototype.duckdb") {
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  return(con)
}

# Load DuckDB spatial extension
load_spatial_extension <- function(con) {
  DBI::dbExecute(con, "INSTALL spatial;")
  DBI::dbExecute(con, "LOAD spatial;")
  
  message("DuckDB spatial extension loaded.")
}

# Check whether required columns exist
check_spatial_columns <- function(con, table_name, settings = SPATIAL_SETTINGS) {
  cols <- DBI::dbGetQuery(
    con,
    glue("DESCRIBE {table_name};")
  )
  
  existing_cols <- cols$column_name
  
  required_cols <- c(
    settings$latitude_col,
    settings$longitude_col
  )
  
  missing_cols <- setdiff(required_cols, existing_cols)
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required spatial columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  message("Required spatial columns found.")
}

# Add WGS84 geometry column
add_wgs84_geometry <- function(con, table_name, settings = SPATIAL_SETTINGS) {
  lat_col <- settings$latitude_col
  lon_col <- settings$longitude_col
  geom_col <- settings$geometry_col
  
  add_col_sql <- glue("
    ALTER TABLE {table_name}
    ADD COLUMN IF NOT EXISTS {geom_col} GEOMETRY;
  ")
  
  DBI::dbExecute(con, add_col_sql)
  
  update_geom_sql <- glue("
    UPDATE {table_name}
    SET {geom_col} = ST_Point(CAST({lon_col} AS DOUBLE), CAST({lat_col} AS DOUBLE))
    WHERE {lon_col} IS NOT NULL
      AND {lat_col} IS NOT NULL;
  ")
  
  DBI::dbExecute(con, update_geom_sql)
  
  message("WGS84-compatible geometry column created: ", geom_col)
}

# Verify geometry creation
verify_geometry <- function(con, table_name, settings = SPATIAL_SETTINGS) {
  geom_col <- settings$geometry_col
  
  verify_sql <- glue("
    SELECT
      COUNT(*) AS total_rows,
      COUNT({geom_col}) AS rows_with_geometry,
      COUNT(*) - COUNT({geom_col}) AS rows_without_geometry
    FROM {table_name};
  ")
  
  result <- DBI::dbGetQuery(con, verify_sql)
  
  return(result)
}

# Run full spatial standardization
run_spatial_standardization <- function(
    db_path = "data/prototype.duckdb",
    table_name,
    settings = SPATIAL_SETTINGS
) {
  con <- connect_duckdb(db_path)
  
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  load_spatial_extension(con)
  
  check_spatial_columns(
    con = con,
    table_name = table_name,
    settings = settings
  )
  
  add_wgs84_geometry(
    con = con,
    table_name = table_name,
    settings = settings
  )
  
  verification_result <- verify_geometry(
    con = con,
    table_name = table_name,
    settings = settings
  )
  
  message("Spatial standardization completed for table: ", table_name)
  message("Assumed coordinate reference system: ", settings$crs)
  
  return(verification_result)
}