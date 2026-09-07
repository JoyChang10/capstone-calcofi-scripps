# R/data.R
# Fetches ichthyoplankton_observations.parquet from GCS and queries it
# directly via DuckDB — no .duckdb file required.

library(googleCloudStorageR)
library(DBI)
library(duckdb)
library(here)

# ── Constants ──────────────────────────────────────────────────────────────────

GCS_BUCKET    <- "calcofi-data-lake"
GCS_OBJECT    <- "staging/ichthyoplankton_observations.parquet"
LOCAL_PARQUET <- file.path(tempdir(), "ichthyoplankton_observations.parquet")

# ── GCS Authentication ─────────────────────────────────────────────────────────

.gcs_authenticated <- FALSE

authenticate_gcs <- function() {
  if (.gcs_authenticated) return(invisible(TRUE))

  # Grab whichever .json file exists in secrets/ (handles any filename)
  secrets_dir <- here::here("secrets")
  key_files   <- list.files(secrets_dir, pattern = "\\.json$", full.names = TRUE)

  if (length(key_files) > 0) {
    googleCloudStorageR::gcs_auth(json_file = key_files[1])
    message("✅ GCS authenticated via: ", basename(key_files[1]))
  } else {
    auth_file <- Sys.getenv("GCS_AUTH_FILE")
    if (nchar(auth_file) > 0 && file.exists(auth_file)) {
      googleCloudStorageR::gcs_auth(json_file = auth_file)
      message("✅ GCS authenticated via GCS_AUTH_FILE")
    } else {
      stop("❌ No GCS credentials found. Add a .json key file to secrets/")
    }
  }

  googleCloudStorageR::gcs_global_bucket(GCS_BUCKET)
  .gcs_authenticated <<- TRUE
  invisible(TRUE)
}

# ── Parquet Download ───────────────────────────────────────────────────────────
# Downloads once per session; force = TRUE re-downloads (used on refresh)

fetch_parquet <- function(force = FALSE) {
  if (!force && file.exists(LOCAL_PARQUET)) {
    message("✅ Using cached parquet")
    return(invisible(LOCAL_PARQUET))
  }
  authenticate_gcs()
  message("⬇️  Downloading parquet from GCS...")
  googleCloudStorageR::gcs_get_object(
    object_name = GCS_OBJECT,
    saveToDisk  = LOCAL_PARQUET,
    overwrite   = TRUE
  )
  message("✅ Parquet ready: ", LOCAL_PARQUET)
  invisible(LOCAL_PARQUET)
}

# ── DuckDB connection ──────────────────────────────────────────────────────────
# Single in-memory connection with a view over the parquet file.
# Call get_con() anywhere — no path argument needed.

.db_con <- NULL

get_con <- function() {
  if (!is.null(.db_con) && DBI::dbIsValid(.db_con)) return(.db_con)

  fetch_parquet()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

  # View = zero-copy: DuckDB reads the parquet on demand, no data duplication
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ichthyoplankton_observations AS ",
    "SELECT * FROM read_parquet('", LOCAL_PARQUET, "')"
  ))

  message("✅ DuckDB view ready over parquet")
  .db_con <<- con
  con
}

# ── load_data ─────────────────────────────────────────────────────────────────

load_data <- function(config) {
  tryCatch({
    con <- get_con()
    tbl <- config$data$table

    df <- DBI::dbGetQuery(con, paste0(
      "SELECT year, season, taxon, abundance ",
      "FROM ", tbl, " ",
      "WHERE abundance IS NOT NULL"
    ))

    df$year      <- as.integer(df$year)
    df$season    <- factor(df$season, levels = c("spring", "summer", "fall", "winter"))
    df$abundance <- as.numeric(df$abundance)

    list(
      data      = df,
      timestamp = Sys.time(),
      error     = NULL,
      n_rows    = nrow(df),
      species   = sort(unique(df$taxon)),
      years     = range(df$year, na.rm = TRUE)
    )

  }, error = function(e) {
    list(data = NULL, timestamp = Sys.time(), error = conditionMessage(e),
         n_rows = 0, species = character(0), years = c(NA, NA))
  })
}

# ── query_aggregated ──────────────────────────────────────────────────────────

query_aggregated <- function(config, year_min, year_max, seasons, species, agg_method) {
  tryCatch({
    con <- get_con()
    tbl <- config$data$table

    agg_sql <- switch(agg_method,
      "mean"   = "AVG(abundance)",
      "median" = "MEDIAN(abundance)",
      "sum"    = "SUM(abundance)",
      "max"    = "MAX(abundance)",
      "AVG(abundance)"
    )

    seasons_str <- paste0("'", seasons, "'", collapse = ", ")
    species_str <- paste0("'", species, "'", collapse = ", ")

    sql <- paste0(
      "SELECT year, taxon, ", agg_sql, " AS abundance ",
      "FROM ", tbl, " ",
      "WHERE abundance IS NOT NULL ",
      "  AND year   BETWEEN ", year_min, " AND ", year_max, " ",
      "  AND season IN (", seasons_str, ") ",
      "  AND taxon  IN (", species_str, ") ",
      "GROUP BY year, taxon ",
      "ORDER BY year, taxon"
    )

    df <- DBI::dbGetQuery(con, sql)
    df$taxon_display <- tools::toTitleCase(gsub("_", " ", df$taxon))
    df$year          <- as.integer(df$year)
    df$abundance     <- as.numeric(df$abundance)
    df

  }, error = function(e) {
    message("query_aggregated error: ", conditionMessage(e))
    data.frame(year = integer(), taxon = character(),
               abundance = numeric(), taxon_display = character())
  })
}

# ── make_data_reactive ────────────────────────────────────────────────────────

make_data_reactive <- function(config, session, initial_data = NULL) {
  refresh_ms <- config$data$refresh_interval_minutes * 60 * 1000
  seed       <- if (!is.null(initial_data)) initial_data else load_data(config)
  result     <- shiny::reactiveVal(seed)

  shiny::observe({
    shiny::invalidateLater(refresh_ms, session)
    shiny::isolate({
      .db_con <<- NULL
      fetch_parquet(force = TRUE)
      result(load_data(config))
    })
  })

  result
}

# ── query_temporal_trends ─────────────────────────────────────────────────────

query_temporal_trends <- function(config, year_min, year_max, seasons, species) {
  tryCatch({
    con <- get_con()
    tbl <- config$data$table

    seasons_str <- paste0("'", seasons, "'", collapse = ", ")
    species_str <- paste0("'", species, "'", collapse = ", ")

    sql <- paste0(
      "SELECT year, season, ",
      "  AVG(station_total)    AS mean_abundance, ",
      "  MEDIAN(station_total) AS median_abundance ",
      "FROM ( ",
      "  SELECT year, season, unique_code, SUM(abundance) AS station_total ",
      "  FROM ", tbl, " ",
      "  WHERE abundance IS NOT NULL ",
      "    AND year   BETWEEN ", year_min, " AND ", year_max, " ",
      "    AND season IN (", seasons_str, ") ",
      "    AND taxon  IN (", species_str, ") ",
      "  GROUP BY year, season, unique_code ",
      ") sub ",
      "GROUP BY year, season ",
      "ORDER BY year, season"
    )

    df <- DBI::dbGetQuery(con, sql)
    df$year   <- as.integer(df$year)
    df$season <- factor(df$season, levels = c("spring", "summer", "fall", "winter"))
    df

  }, error = function(e) {
    message("query_temporal_trends error: ", conditionMessage(e))
    data.frame(year = integer(), season = character(),
               mean_abundance = numeric(), median_abundance = numeric())
  })
}

# ── query_spatial ─────────────────────────────────────────────────────────────

query_spatial <- function(config, year_min, year_max, seasons, species) {
  tryCatch({
    con <- get_con()
    tbl <- config$data$table

    seasons_str <- paste0("'", seasons, "'", collapse = ", ")
    species_str <- paste0("'", species, "'", collapse = ", ")

    sql <- paste0(
      "SELECT latitude, longitude, s_l, taxon, SUM(abundance) AS total_abundance ",
      "FROM ", tbl, " ",
      "WHERE abundance  IS NOT NULL ",
      "  AND latitude   IS NOT NULL ",
      "  AND longitude  IS NOT NULL ",
      "  AND year   BETWEEN ", year_min, " AND ", year_max, " ",
      "  AND season IN (", seasons_str, ") ",
      "  AND taxon  IN (", species_str, ") ",
      "GROUP BY latitude, longitude, s_l, taxon ",
      "ORDER BY latitude, longitude"
    )

    DBI::dbGetQuery(con, sql)

  }, error = function(e) {
    message("query_spatial error: ", conditionMessage(e))
    data.frame(latitude = numeric(), longitude = numeric(),
               s_l = numeric(), taxon = character(), total_abundance = numeric())
  })
}
