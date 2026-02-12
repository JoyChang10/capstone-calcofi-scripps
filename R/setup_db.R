# Set up the connection
source("R/db_init.R")

message("Cleaning existing data...")
DBI::dbExecute(con, "Drop table if exists ichthyoplankton_observations;")

# Read the SQL file content into a string
schema_sql <- readr::read_file("sql/schema.sql")

# Execute the SQL string against the database
DBI::dbExecute(con, schema_sql)

# Verify the table was created
DBI::dbListTables(con)


source("R/db_init.R")
metadata_sql <- readr::read_file("sql/metadata_injection.sql")

# Apply the COMMENT ON COLUMN statements to the existing table
DBI::dbExecute(con, metadata_sql)

message("Metadata injection complete.")