# app.R — Entry point
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
library(dplyr)
library(leaflet)

source("R/data.R")
source("R/state.R")
source("R/filters.R")
source("R/plots.R")
source("R/exports.R")
source("R/ui.R")

config <- yaml::read_yaml("config.yml")

initial_data <- load_data(config)
if (!is.null(initial_data$error)) message("Data load error: ", initial_data$error)

habitat_lookup <- readr::read_csv(
  "csvs/species_habitat_categories2.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(species_clean = tolower(gsub("\\.", "_", species))) |>
  dplyr::distinct(species_clean, .keep_all = TRUE)

ui <- build_ui(config, initial_data, habitat_lookup)

server <- function(input, output, session) {
  
  data_rv <- make_data_reactive(config, session, initial_data)
  shiny::observeEvent(input$btn_refresh, { data_rv(load_data(config)) })
  
  state <- init_state(config, initial_data)
  shiny::observe({ update_state(state, input) })
  
  # Resolve species based on filter mode
  shiny::observe({
    mode <- input$filter_mode %||% "species"
    
    if (mode == "habitat") {
      submode <- input$habitat_submode %||% "habitat_type"
      
      if (submode == "habitat_type") {
        hab <- if (!is.null(input$habitat_select) && length(input$habitat_select) > 0)
          input$habitat_select else c("pelagic", "benthic")
        matched <- habitat_lookup |>
          dplyr::filter(tolower(trimws(habitat)) %in% tolower(hab)) |>
          dplyr::pull(species_clean)
        
      } else {
        grp <- if (!is.null(input$grpname_select) && length(input$grpname_select) > 0)
          input$grpname_select else c("coastal", "oceanic", "coastal-oceanic")
        matched <- habitat_lookup |>
          dplyr::filter(tolower(trimws(GRPname)) %in% tolower(grp)) |>
          dplyr::pull(species_clean)
      }
      
      state$selected_species <- intersect(matched, data_rv()$species)
      
    } else {
      state$selected_species <- input$species_select %||% character(0)
    }
  })
  
  output$filter_ui <- shiny::renderUI({
    build_filter_ui(config, data_rv(), habitat_lookup)
  })
  
  shiny::observeEvent(input$select_all_species, {
    shiny::updateSelectizeInput(session, "species_select", selected = data_rv()$species)
  })
  shiny::observeEvent(input$deselect_all_species, {
    shiny::updateSelectizeInput(session, "species_select", selected = character(0))
  })
  all_periods <- c("1951–1976", "1977–1998", "1999–2014", "2015–present")
  shiny::observeEvent(input$select_all_periods, {
    shiny::updateCheckboxGroupInput(session, "period_check", selected = all_periods)
  })
  shiny::observeEvent(input$deselect_all_periods, {
    shiny::updateCheckboxGroupInput(session, "period_check", selected = character(0))
  })

  shiny::observeEvent(input$select_top_species, {
    df <- data_rv()$data
    if (is.null(df)) return()
    top5 <- names(sort(tapply(df$abundance, df$taxon, sum, na.rm = TRUE),
                       decreasing = TRUE))[1:5]
    shiny::updateSelectizeInput(session, "species_select", selected = top5)
  })
  
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
  
  filtered_data <- shiny::reactive({ apply_filters(data_rv()$data, state) })
  
  output$last_updated_text <- shiny::renderText({
    paste("Updated", format(data_rv()$timestamp, "%H:%M:%S"))
  })
  
  output$record_count <- shiny::renderText({
    n <- filtered_count(); total <- data_rv()$n_rows
    paste0(format(n, big.mark = ","), " / ", format(total, big.mark = ","), " rows")
  })
  
  output$data_status_badge <- shiny::renderUI({
    dr <- data_rv()
    if (!is.null(dr$error))    shiny::span(class = "badge-error",   "● Error")
    else if (is.null(dr$data)) shiny::span(class = "badge-loading", "● Loading")
    else                       shiny::span(class = "badge-ok",      "● Live")
  })
  
  abundanceTimeServer("abundance_time", filtered_data, state, config, habitat_lookup)
  corrHeatmapServer("corr_heatmap",     filtered_data, state, config, habitat_lookup)
  meanVarServer("mean_var",             filtered_data, state, config, habitat_lookup)
  abundanceBarServer("abundance_bar",   filtered_data, state, config, habitat_lookup)
  temporalTrendsServer("temporal_trends",          state, config)
  timeSeriesServer("time_series",                  state, config)
  habitatTimeSeriesServer("habitat_time_series",   state, config, habitat_lookup)
  spatialMapServer("spatial_map",                  state, config)
}

shiny::shinyApp(ui, server)