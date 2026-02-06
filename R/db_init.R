# R/db_init.R
librarian::shelf(DBI, duckdb, here, glue)

db_path <- here::here("data", "prototype.duckdb")

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = db_path,
  read_only = FALSE
)

DBI::dbExecute(con, "INSTALL spatial;")
DBI::dbExecute(con, "LOAD spatial;")

message(glue::glue("✅ DuckDB ready: {db_path}"))

invisible(con)
