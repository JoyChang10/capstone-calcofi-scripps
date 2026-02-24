# R/run_pipeline.R
# Group prototype: trigger ingestion when Drive file updates

library(googledrive)

# interactive auth for now (Person3 will make headless later)
drive_auth()

source("R/drive_pull.R")
# source("R/ingest_pipeline.R")  # later: ingest_file(path)

res <- drive_pull_if_changed()

if (isTRUE(res$changed)) {
  message("Trigger ingestion for: ", res$local_path)
  # ingest_file(res$local_path)
} else {
  message("No updates. Nothing to ingest.")
}

file.exists("R/run_pipeline.R")
source("R/run_pipeline.R")
