# R/data.R
# Loads data from DuckDB, returns dataset + metadata
# Handles refresh behavior defined in config
#
# Performance strategy:
#   - Open one persistent read-only connection per app session
#   - Pull only the columns needed for filtering + plotting
#   - Pre-aggregate mean/median/sum/max in DuckDB before transferring to R

# ── Persistent connection ─────────────────────────────────────────────────────

.db_con <- NULL   # module-level cache

get_con <- function(db_path) {
  if (!is.null(.db_con) && DBI::dbIsValid(.db_con)) return(.db_con)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  .db_con <<- con
  con
}

resolve_db_path <- function(config) {
  p <- config$data$path
  if (file.exists(p)) return(p)
  p2 <- file.path(getwd(), p)
  if (file.exists(p2)) return(p2)
  stop(paste("Database not found:", p))
}

# ── Load raw data (columns needed for filters + plots) ───────────────────────

load_data <- function(config) {
  tryCatch({
    db_path <- resolve_db_path(config)
    con     <- get_con(db_path)
    tbl     <- config$data$table

    # Pull only the columns we actually use — avoids transferring unused fields
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
    list(
      data      = NULL,
      timestamp = Sys.time(),
      error     = conditionMessage(e),
      n_rows    = 0,
      species   = character(0),
      years     = c(NA, NA)
    )
  })
}

# ── Pre-aggregate in DuckDB for a given filter state ─────────────────────────
# Returns a small data.frame (~taxa × years rows) ready for plotting.
# Much faster than pulling 81k rows then aggregating in R.

query_aggregated <- function(config, year_min, year_max, seasons, species, agg_method) {
  tryCatch({
    db_path <- resolve_db_path(config)
    con     <- get_con(db_path)
    tbl     <- config$data$table

    agg_sql <- switch(agg_method,
      "mean"   = "AVG(abundance)",
      "median" = "MEDIAN(abundance)",
      "sum"    = "SUM(abundance)",
      "max"    = "MAX(abundance)",
      "AVG(abundance)"
    )

    # Build WHERE clause safely
    seasons_str <- paste0("'", seasons, "'", collapse = ", ")
    species_str <- paste0("'", species, "'", collapse = ", ")

    sql <- paste0(
      "SELECT year, taxon, ", agg_sql, " AS abundance ",
      "FROM ", tbl, " ",
      "WHERE abundance IS NOT NULL ",
      "  AND year BETWEEN ", year_min, " AND ", year_max,
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

# ── Reactive data loader with timed refresh ───────────────────────────────────

make_data_reactive <- function(config, session, initial_data = NULL) {
  refresh_ms <- config$data$refresh_interval_minutes * 60 * 1000

  seed   <- if (!is.null(initial_data)) initial_data else load_data(config)
  result <- shiny::reactiveVal(seed)

  shiny::observe({
    shiny::invalidateLater(refresh_ms, session)
    shiny::isolate(result(load_data(config)))
  })

  result
}
