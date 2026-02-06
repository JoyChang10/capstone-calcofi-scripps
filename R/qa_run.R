# R/qa_run.R
source("R/db_init.R")
source("R/validate_rules.R")

# Check tables exist
tables <- DBI::dbGetQuery(con, "SHOW TABLES;")
print(tables)

if (!"ichthyoplankton_observations" %in% tables$name) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  stop("❌ Table ichthyoplankton_observations not found. Run sql/schema.sql first.")
}

# Pull sample + validate
df_sample <- DBI::dbGetQuery(
  con,
  "SELECT * FROM ichthyoplankton_observations LIMIT 200;"
)

DBI::dbDisconnect(con, shutdown = TRUE)

validate_ichthy(df_sample)

message("🎉 QA complete: DB connectivity + validation passed.")
