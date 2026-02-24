# R/drive_pull.R
# Stage 2 Person 2 deliverable:
# - Use googledrive to retrieve a specific file programmatically
# - Detect remote changes using file_id + modified_time
# - Download only if changed (prevent redundant ingestion)
# Exposes: drive_pull_if_changed()

suppressPackageStartupMessages({
  library(googledrive)
  library(jsonlite)
  library(here)
  library(fs)
})

# -----------------------------
# Config via env vars
# -----------------------------
.drive_cfg <- list(
  folder_id      = Sys.getenv("CALCOFI_DRIVE_FOLDER_ID", unset = ""),
  target_file_id = Sys.getenv("CALCOFI_TARGET_FILE_ID", unset = ""),
  pull_mode      = Sys.getenv("CALCOFI_PULL_MODE", unset = "single"),  # "single" or "folder" (folder optional)
  state_path     = Sys.getenv("CALCOFI_DRIVE_STATE", unset = here("data", "drive_state.json")),
  download_dir   = Sys.getenv("CALCOFI_RAW_DIR", unset = here("data", "incoming_drive"))
)

# -----------------------------
# State helpers
# state format:
# { "files": { "<file_id>": { "modified_time": "...", "local_path": "..." } } }
# -----------------------------
read_state <- function(path) {
  if (!fs::file_exists(path)) {
    return(list(files = list()))
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

write_state <- function(state, path) {
  fs::dir_create(fs::path_dir(path))
  writeLines(jsonlite::toJSON(state, auto_unbox = TRUE, pretty = TRUE), path)
}

get_prev_mtime <- function(state, file_id) {
  if (!is.null(state$files[[file_id]])) state$files[[file_id]]$modified_time else NA_character_
}

set_file_state <- function(state, file_id, modified_time, local_path) {
  state$files[[file_id]] <- list(
    modified_time = modified_time,
    local_path = local_path
  )
  state
}

# -----------------------------
# Drive metadata helpers
# -----------------------------
get_modified_time <- function(drive_file_dribble) {
  as.character(drive_file_dribble$drive_resource[[1]]$modifiedTime)
}

download_target <- function(drive_file_dribble, download_dir) {
  fs::dir_create(download_dir)
  
  fid  <- drive_file_dribble$id[[1]]
  name <- drive_file_dribble$name[[1]]
  
  # prefix with file id to avoid collisions
  local_path <- fs::path(download_dir, paste0(fid, "__", name))
  
  # Download as-is (CSV)
  drive_download(drive_file_dribble, path = local_path, overwrite = TRUE)
  
  as.character(local_path)
}

# -----------------------------
# Main function
# -----------------------------
drive_pull_if_changed <- function(
    target_file_id = .drive_cfg$target_file_id,
    state_path     = .drive_cfg$state_path,
    download_dir   = .drive_cfg$download_dir
) {
  if (!nzchar(target_file_id)) {
    stop("Missing CALCOFI_TARGET_FILE_ID. Set it via Sys.setenv() or .Renviron.")
  }
  
  # NOTE: Auth handled outside (Person3 headless later). For interactive runs:
  # drive_auth()
  
  state <- read_state(state_path)
  
  # Get fresh metadata from Drive
  target <- drive_get(as_id(target_file_id))
  if (nrow(target) == 0) stop("drive_get() returned 0 rows for target file id.")
  
  remote_mtime <- get_modified_time(target)
  prev_mtime   <- get_prev_mtime(state, target_file_id)
  
  if (!is.na(prev_mtime) && identical(prev_mtime, remote_mtime)) {
    message("[SKIP] No change detected for file_id = ", target_file_id)
    return(list(
      changed = FALSE,
      local_path = NULL,
      file_id = target_file_id,
      modified_time = remote_mtime,
      reason = "no_change",
      state_path = as.character(state_path)
    ))
  }
  
  message("[PULL] Change detected (", ifelse(is.na(prev_mtime), "first_time", "modified_time_updated"), ")")
  local_path <- download_target(target, download_dir)
  
  state <- set_file_state(state, target_file_id, remote_mtime, local_path)
  write_state(state, state_path)
  
  list(
    changed = TRUE,
    local_path = local_path,
    file_id = target_file_id,
    modified_time = remote_mtime,
    reason = ifelse(is.na(prev_mtime), "first_time", "modified_time_updated"),
    state_path = as.character(state_path)
  )
}


