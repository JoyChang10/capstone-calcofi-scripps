generate_config <- function(csv_path) {
  library(readr)
  library(yaml)
  library(tools)
  
  df <- read_csv(csv_path, n_max = 100)
  cols <- names(df)
  
  find_col <- function(patterns) {
    for (p in patterns) {
      match <- cols[grepl(p, cols, ignore.case = TRUE)]
      if (length(match) > 0) return(match[1])
    }
    return(NA)
  }
  
  latitude_col  <- find_col(c("^lat$", "latitude"))
  longitude_col <- find_col(c("^lon$", "longitude"))
  cruise_col    <- find_col(c("cruise", "s_c"))
  line_col      <- find_col(c("line", "s_l"))
  station_col   <- find_col(c("station", "s_s"))
  date_col      <- find_col(c("date", "time"))
  year_col      <- find_col(c("^year$"))
  season_col    <- find_col(c("season"))
  
  
  numeric_cols <- cols[sapply(df, is.numeric)]
  
  exclude_patterns <- c(
    "lat", "lon", 
    "depth", "temp", "sal",
    "line", "station", "row"
  )
  
  pivot_candidates <- numeric_cols[
    !grepl(paste(exclude_patterns, collapse = "|"),
           numeric_cols,
           ignore.case = TRUE)
  ]
  
  pivot_config <- NULL
  
  if (length(pivot_candidates) > 5) {
    pivot_config <- list(
      cols = pivot_candidates,
      names_to = "taxon",
      values_to = "abundance"
    )
  }

  
  if (!is.na(date_col)) {
    time_config <- list(
      type = "date",
      column = date_col
    )
  } else {
    time_config <- list(
      type = "seasonal",
      year_col = year_col,
      season_col = season_col
    )
  }
  
  # dataset name
  
  dataset_name <- file_path_sans_ext(basename(csv_path))
  
  # config
  
  config <- list(
    dataset_name = dataset_name,
    
    columns = list(
      latitude = latitude_col,
      longitude = longitude_col,
      s_c = cruise_col,
      s_l = line_col,
      s_s = station_col
    ),
    
    pivot = pivot_config,
    time = time_config
  )
  

  output_dir <- "config"
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_path <- file.path(output_dir, paste0(dataset_name, ".yaml"))
  
  write_yaml(config, output_path)
  
  cat("✅ Config generated at:", output_path, "\n")
}

generate_config("data/BTEDB_Abundances.csv")