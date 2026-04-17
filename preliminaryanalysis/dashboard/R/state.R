# R/state.R
# Stores and manages global user selections / filter state

init_state <- function(config, data_result) {
  yr <- data_result$years
  sp <- data_result$species
  
  default_species <- config$defaults$species
  valid_defaults  <- intersect(default_species, sp)
  if (length(valid_defaults) == 0) valid_defaults <- sp[1:min(3, length(sp))]
  
  shiny::reactiveValues(
    year_min         = max(yr[1], config$defaults$year_min, na.rm = TRUE),
    year_max         = min(yr[2], config$defaults$year_max, na.rm = TRUE),
    selected_species = valid_defaults,
    selected_seasons = config$defaults$season,
    aggregation      = config$defaults$aggregation,
    smooth           = config$defaults$smooth,
    filter_mode      = "species",
    habitat_submode  = "habitat_type",
    habitat_select   = c("pelagic", "benthic"),
    grpname_select   = c("coastal", "oceanic", "coastal-oceanic"),
    last_updated     = Sys.time()
  )
}

update_state <- function(state, input) {
  if (!is.null(input$year_range)) {
    state$year_min <- input$year_range[1]
    state$year_max <- input$year_range[2]
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
  if (!is.null(input$filter_mode)) {
    state$filter_mode <- input$filter_mode
  }
  if (!is.null(input$habitat_submode)) {
    state$habitat_submode <- input$habitat_submode
  }
  if (!is.null(input$habitat_select)) {
    state$habitat_select <- input$habitat_select
  }
  if (!is.null(input$grpname_select)) {
    state$grpname_select <- input$grpname_select
  }
}