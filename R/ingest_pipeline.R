library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(DBI)
library(duckdb)
library(here)
library(stringr)

# 1. Read raw wide dataset
raw_data <- read_csv(
  here("data/ichthyoplankton_updatedthru2304_032824.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names()

# 2. Identify metadata columns
meta_cols <- c(
  "unique_code",
  "s_c", "s_sc",
  "s_l", "s_s",
  "latitude", "longitude",
  "year", "season"
)

# 3. Pivot species columns into long format
long_data <- raw_data %>%
  pivot_longer(
    cols = -all_of(meta_cols),
    names_to = "taxon",
    values_to = "abundance"
  ) %>%
  filter(!is.na(abundance)) %>%
  mutate(
    abundance = as.numeric(abundance),
    taxon = str_replace_all(taxon, "\\.", " "),
    taxon = str_trim(taxon)
  )

# 4. Enforce PRIMARY KEY constraint (unique_code)
# Collapse multiple taxa per unique_code into a single row
# by summing abundance and keeping a representative taxon
collapsed <- long_data %>%
  group_by(unique_code) %>%
  summarise(
    s_c = first(s_c),
    s_sc = first(s_sc),
    s_l = first(s_l),
    s_s = first(s_s),
    latitude = first(latitude),
    longitude = first(longitude),
    year = first(year),
    season = first(season),
    
    taxon = paste(unique(taxon), collapse = "; "),
    abundance = sum(abundance, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Join WoRMS taxonomy lookup
taxonomy_lookup <- read_csv(
  here("data/taxonomy_lookup.csv"),
  show_col_types = FALSE
)

final_data <- collapsed %>%
  left_join(
    taxonomy_lookup,
    by = c("taxon" = "taxon_name")
  )

# ---- 6. Write to DuckDB ----
con <- dbConnect(
  duckdb::duckdb(),
  dbdir = here("data/prototype.duckdb"),
  read_only = FALSE
)

# Find the real table name in this DB 
tables <- DBI::dbGetQuery(con, "SHOW TABLES;")$name

# Try common candidates: unqualified and schema-qualified
candidates <- c("ichthyoplankton_observations", "calcofi.ichthyoplankton_observations")

# Direct match
tbl <- intersect(candidates, tables)
tbl <- if (length(tbl) > 0) tbl[1] else NA_character_

if (is.na(tbl)) {
  tables_base <- sub("^.*\\.", "", tables)
  cand_base <- sub("^.*\\.", "", candidates)
  idx <- match(cand_base, tables_base)
  if (any(!is.na(idx))) tbl <- tables[ idx[which(!is.na(idx))[1]] ]
}

if (is.na(tbl)) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  stop(
    "Target table not found. Tried: ",
    paste(candidates, collapse = ", "),
    "\nTables in this DB: ",
    paste(tables, collapse = ", "),
    "\nFix: run schema.sql against data/prototype.duckdb first."
  )
}

# Now delete + write using the discovered table name
DBI::dbExecute(con, paste0("DELETE FROM ", tbl, ";"))

DBI::dbWriteTable(
  con,
  tbl,
  final_data,
  append = TRUE
)

DBI::dbDisconnect(con, shutdown = TRUE)
