# app.R — Entry point
# Loads config, sources modules, builds UI and server

# Dependencies 
library(shiny)
library(duckdb)
library(DBI)
library(ggplot2)
library(plotly)
library(scales)
library(RColorBrewer)
library(shinycssloaders)
library(glue)
library(yaml)
library(tools)

# Source modules
source("R/data.R")
source("R/state.R")
source("R/filters.R")
source("R/plots.R")
source("R/exports.R")
source("R/ui.R")

# Config 
config <- yaml::read_yaml("config.yml")

# Initial data load
initial_data <- load_data(config)
if (!is.null(initial_data$error)) {
  message("⚠ Data load error: ", initial_data$error)
}

# UI 
ui <- build_ui(config, initial_data)

# Server
server <- function(input, output, session) {

  # Reactive data (seeded with already-loaded initial_data — no double load)
  data_rv <- make_data_reactive(config, session, initial_data)

  # Manual refresh
  shiny::observeEvent(input$btn_refresh, {
    data_rv(load_data(config))
  })

  # App state (user selections) — use initial_data to avoid reactive context error
  state <- init_state(config, initial_data)

  # Keep state in sync with inputs
  shiny::observe({
    update_state(state, input)
  })

  # Render filter UI dynamically
  output$filter_ui <- shiny::renderUI({
    build_filter_ui(config, data_rv())
  })

  # Species shortcut links
  shiny::observeEvent(input$select_all_species, {
    all_sp <- data_rv()$species
    shiny::updateSelectizeInput(session, "species_select", selected = all_sp)
  })

  shiny::observeEvent(input$deselect_all_species, {
    shiny::updateSelectizeInput(session, "species_select", selected = character(0))
  })

  shiny::observeEvent(input$select_top_species, {
    df <- data_rv()$data
    if (is.null(df)) return()
    top5 <- names(sort(tapply(df$abundance, df$taxon, sum, na.rm = TRUE), decreasing = TRUE))[1:5]
    shiny::updateSelectizeInput(session, "species_select", selected = top5)
  })

  # Filtered row count (fast SQL COUNT — no need to pull all rows) 
  filtered_count <- shiny::reactive({
    shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)
    tryCatch({
      con <- get_con(resolve_db_path(config))
      tbl <- config$data$table
      seasons_str <- paste0("'", state$selected_seasons, "'", collapse = ", ")
      species_str <- paste0("'", state$selected_species, "'", collapse = ", ")
      sql <- paste0(
        "SELECT COUNT(*) FROM ", tbl,
        " WHERE year BETWEEN ", state$year_min, " AND ", state$year_max,
        "   AND season IN (", seasons_str, ")",
        "   AND taxon  IN (", species_str, ")",
        "   AND abundance IS NOT NULL"
      )
      DBI::dbGetQuery(con, sql)[[1]]
    }, error = function(e) NA_integer_)
  })

  # filtered_data is kept only for the exports module (CSV/PDF)
  # It is NOT called by the plot module — plots use query_aggregated directly
  filtered_data <- shiny::reactive({
    apply_filters(data_rv()$data, state)
  })

  #  Status outputs 
  output$last_updated_text <- shiny::renderText({
    ts <- data_rv()$timestamp
    paste("Updated", format(ts, "%H:%M:%S"))
  })

  output$record_count <- shiny::renderText({
    n     <- filtered_count()
    total <- data_rv()$n_rows
    paste0(format(n, big.mark = ","), " / ", format(total, big.mark = ","), " rows")
  })

  output$data_status_badge <- shiny::renderUI({
    dr <- data_rv()
    if (!is.null(dr$error)) {
      shiny::span(class = "badge-error",   "● Error")
    } else if (is.null(dr$data)) {
      shiny::span(class = "badge-loading", "● Loading")
    } else {
      shiny::span(class = "badge-ok",      "● Live")
    }
  })

  #  Plot modules
  abundanceTimeServer("abundance_time", filtered_data, state, config)
}

#  Run 
shiny::shinyApp(ui, server)
