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
taxon_names <- colnames(raw_data)[11:ncol(raw_data)] %>%
  str_replace_all("\\.", " ") %>%
  str_trim() %>%
  # Filter out names that end in numbers or are too short
  grep("^[A-Za-z ]+$", ., value = TRUE) %>% 
  unique()

get_aphia_id <- function(name) {
  Sys.sleep(0.5)
  
  tryCatch({
    id <- worrms::wm_name2id(name)
    return(as.integer(id))
  }, error = function(e) {
    # Print the specific error
    message(paste("Could not match:", name, "| Reason:", e$message))
    return(NA_integer_)
  })
}

message(paste("Starting WoRMS lookup for", length(taxon_names), "taxa..."))

taxonomy_lookup <- tibble(taxon_name = taxon_names) %>%
  mutate(worms_id = map_int(taxon_name, get_aphia_id))

# Save the lookup table for Person 2 to use in the ingest pipeline
write_csv(taxonomy_lookup, here("data/taxonomy_lookup.csv"))
message("Taxonomy lookup table created.")