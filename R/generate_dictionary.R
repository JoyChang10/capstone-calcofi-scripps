# R/generate_dictionary.R
# Generate a data dictionary markdown file using the schema SQL file

library(readr)
library(stringr)
library(dplyr)

# Read the extended schema
schema_path <- "sql/extended_schema.sql"
schema_text <- read_lines(schema_path)

# Use regex to extract column name, type, and the description
dict_data <- data.frame(line = schema_text) %>%
  filter(str_detect(line, "--")) %>%
  mutate(
    clean_line = str_trim(line),
    field = str_extract(clean_line, "^[a-z0-9_]+"),
    type = str_extract(clean_line, "(?<= )[A-Z]+(?= )|(?<= )[A-Z]+(?=,)"),
    description = str_extract(clean_line, "(?<=-- ).*")
  ) %>%
  filter(!is.na(field)) %>%
  select(field, type, description)

# Format as a md table
md_header <- "# CalCOFI Data Dictionary\n\n| Field | Type | Description |\n| :--- | :--- | :--- |\n"
md_rows <- paste0("| `", dict_data$field, "` | ", dict_data$type, " | ", dict_data$description, " |")
full_md <- paste(c(md_header, md_rows), collapse = "\n")

write_lines(full_md, "data_dictionary.md")
message("data_dictionary.md has been generated from ", schema_path)