# taxonomy_with_fallback.R
library(dplyr)
library(readr)

#' Taxonomic Enrichment with Fallback Mechanism
#'
#' @param dataset A tidy, long-format dataframe (output from generalized_ingest).
#' @param config A list loaded from the dataset's YAML configuration.
#' @param lookup_path Path to the master taxonomic lookup CSV.
#' @param fallback_path Path to write unmatched taxa for manual review.
#' @return The dataset enriched with standard taxonomic IDs (e.g., worms_id).
enrich_taxonomy <- function(dataset, config, lookup_path = "data/taxonomy_lookup.csv", fallback_path = "data/unmatched_taxa.csv") {
  
  # 1. Load the master taxonomy lookup table
  if (!file.exists(lookup_path)) {
    stop("Master taxonomy lookup file not found at: ", lookup_path)
  }
  lookup <- readr::read_csv(lookup_path, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  # 2. Identify taxonomic columns based on the configuration
  # If pivot$name_to is defined, the taxa strings are in that column (e.g., "taxon")
  taxon_name_col <- config$pivot$name_to
  legacy_code_col <- config$taxonomy$code_column
  output_id_col <- config$taxonomy$output_id_column
  
  # 3. Perform the taxonomy join
  # Joins the dataset's taxon column to the 'taxon_raw' column in the lookup table
  join_mapping <- setNames("taxon_raw", taxon_name_col)
  
  dataset_enriched <- dataset %>%
    left_join(lookup, by = join_mapping)
  
  # 4. Retain and cross-reference legacy codes (ITIS, CalCOFI codes)
  # If the raw data has an existing code, but the lookup table didn't provide one, keep the raw data's version
  if (!is.null(legacy_code_col) && legacy_code_col %in% names(dataset)) {
    if (!is.null(output_id_col) && output_id_col %in% names(dataset_enriched)) {
      dataset_enriched <- dataset_enriched %>%
        mutate(!!sym(output_id_col) := coalesce(!!sym(output_id_col), !!sym(legacy_code_col)))
    }
  }
  
  # 5. Fallback Mechanism: Identify unmatched strings
  # Assuming 'worms_id' is the primary standard identifier across all CalCOFI datasets
  unmatched_data <- dataset_enriched %>%
    filter(is.na(worms_id)) %>%
    distinct(!!sym(taxon_name_col), .keep_all = TRUE)
  
  if (nrow(unmatched_data) > 0) {
    warning(sprintf("Found %d unmatched taxonomic strings. Writing to fallback file for review.", nrow(unmatched_data)))
    
    # Prepare the fallback dataframe with helpful context
    fallback_df <- unmatched_data %>%
      select(any_of(c(taxon_name_col, legacy_code_col, "iso_date", "year", "latitude", "longitude"))) %>%
      mutate(
        review_status = "PENDING",
        suggested_worms_id = NA,
        detected_at = Sys.time(),
        source_dataset = config$dataset$name
      )
    
    # Append to existing fallback file or create a new one
    if (file.exists(fallback_path)) {
      readr::write_csv(fallback_df, fallback_path, append = TRUE)
    } else {
      readr::write_csv(fallback_df, fallback_path)
    }
  }
  
  return(dataset_enriched)
}