# R/state.R
# Stores and manages global user selections / filter state

init_state <- function(config, data_result) {
  yr <- data_result$years
  sp <- data_result$species

  # Resolve default species from config, fallback to top 3
  default_species <- config$defaults$species
  valid_defaults  <- intersect(default_species, sp)
  if (length(valid_defaults) == 0) valid_defaults <- sp[1:min(3, length(sp))]

  shiny::reactiveValues(
    # Time filters
    year_min   = max(yr[1], config$defaults$year_min, na.rm = TRUE),
    year_max   = min(yr[2], config$defaults$year_max, na.rm = TRUE),

    # Species
    selected_species = valid_defaults,

    # Season
    selected_seasons = config$defaults$season,

    # Plot controls
    aggregation = config$defaults$aggregation,
    smooth      = config$defaults$smooth,

    # UI metadata
    last_updated = Sys.time()
  )
}

# Sync state from filter inputs
update_state <- function(state, input) {
  if (!is.null(input$year_range)) {
    state$year_min <- input$year_range[1]
    state$year_max <- input$year_range[2]
  }
  if (!is.null(input$species_select)) {
    state$selected_species <- input$species_select
  }
  if (!is.null(input$season_check)) {
    state$selected_seasons <- input$season_check
  }
  if (!is.null(input$aggregation)) {
    state$aggregation <- input$aggregation
  }
  if (!is.null(input$smooth)) {
    state$smooth <- input$smooth
  }
}
