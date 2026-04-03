# This script maps taxon strings to accepted AphiaIDs (worms_id)
librarian::shelf(
  dplyr, 
  readr, 
  worrms, 
  purrr, 
  stringr,
  here
)

# Load raw species names
raw_data <- read_csv(here("data/ichthyoplankton_updatedthru2304_032824.csv"))

# Extract unique taxon names
# Identify the species columns
# Extract taxon names (KEEP original names with trailing digits)
taxa_df <- tibble(
  taxon_raw = colnames(raw_data)[11:ncol(raw_data)]
) %>%
  mutate(
    # Normalize for WoRMS lookup
    taxon_name = taxon_raw %>%
      str_replace_all("\\.", " ") %>%      # dot -> space
      str_trim() %>%
      str_replace_all("\\s*\\d+$", "") %>% # remove trailing digits like "1"
      str_squish()
  ) %>%
  # Drop empty or obviously invalid names
  filter(nchar(taxon_name) > 1) %>%
  distinct(taxon_raw, taxon_name)


taxon_names <- unique(taxa_df$taxon_name)


# get_aphia_id <- function(name) {
#   Sys.sleep(0.3)
#   
#   tryCatch({
#     id <- worrms::wm_name2id(name)
#     return(as.integer(id))
#   }, error = function(e) {s
#     # Print the specific error
#     message(paste("Could not match:", name, "| Reason:", e$message))
#     return(NA_integer_)
#   })
# }

get_aphia_id <- function(name) {
  # Add a small delay to respect API rate limits
  Sys.sleep(0.3)
  
  tryCatch({
    # Retrieve all matching records for the taxon name
    res <- worrms::wm_records_name(name)
    
    # Return the AphiaID from the first result found
    # res$AphiaID[1] ensures you only get one value back
    return(as.integer(res$AphiaID[1]))
    
  }, error = function(e) {
    # If no records are found or the API fails, return NA
    message(paste("Could not match:", name, "| Reason:", e$message))
    return(NA_integer_)
  })
}

message(paste("Starting WoRMS lookup for", length(taxon_names), "taxa..."))

name_map <- tibble(taxon_name = unique(taxa_df$taxon_name)) %>%
  mutate(worms_id = map_int(taxon_name, get_aphia_id))

# 2) Join back to keep taxon_raw exactly as original column names
taxonomy_lookup <- taxa_df %>%
  left_join(name_map, by = "taxon_name") %>%
  select(taxon_raw, taxon_name, worms_id) %>%
  distinct()

# Save the lookup table for Person 2 to use in the ingest pipeline
write_csv(taxonomy_lookup, here("data/taxonomy_lookup.csv"))
message("Taxonomy lookup table created.")