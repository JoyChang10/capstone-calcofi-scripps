# R/ui.R
# Builds the full UI layout
# Called by app.R with config and initial data

build_ui <- function(config, data_result, habitat_lookup = NULL) {
  shiny::fluidPage(
    title = config$app$title,
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "styles.css"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),

    # ── Top bar ─────────────────────────────────────────────────────────────
    shiny::div(
      class = "topbar",
      shiny::div(
        class = "topbar-left",
        shiny::span(class = "topbar-icon", "🐟"),
        shiny::h1(config$app$title, class = "topbar-title")
      ),
      shiny::div(
        class = "topbar-right",
        shiny::div(
          class = "data-badge",
          shiny::uiOutput("data_status_badge")
        )
      )
    ),

    # ── Main layout ──────────────────────────────────────────────────────────
    shiny::div(
      class = "main-layout",

      # Sidebar
      shiny::div(
        class = "sidebar",
        shiny::div(
          class = "sidebar-inner",
          shiny::div(
            class = "sidebar-header",
            shiny::h4("Filters", class = "sidebar-title"),
            shiny::div(
              class = "record-count",
              shiny::textOutput("record_count", inline = TRUE)
            )
          ),
          shiny::uiOutput("filter_ui"),
          shiny::hr(),
          exportsUI("exports"),
          shiny::p(
            shiny::icon("circle-info", style = "font-size: 0.75em;"),
            " Chrome must be installed to include the Spatial Distribution map in PDF exports.",
            style = "font-size: 0.72em; color: #999; font-style: italic; margin-top: 4px; line-height: 1.4;"
          )
        )
      ),

      # Content area
      shiny::div(
        style = "width: 100%; padding-top: 4px;",
        shiny::tabsetPanel(
          shiny::tabPanel("Abundance Through Time",
                          shiny::br(),
                          abundanceTimeUI("abundance_time")
          ),
          shiny::tabPanel("Correlation Heatmap",
                          shiny::br(),
                          corrHeatmapUI("corr_heatmap")
          ),
          shiny::tabPanel("Stability & Variability",
                          shiny::br(),
                          meanVarUI("mean_var")
          ),
          shiny::tabPanel("Abundance Bar Chart",
                          shiny::br(),
                          abundanceBarUI("abundance_bar")
          ),
          shiny::tabPanel("Temporal Trends",
                          shiny::br(),
                          temporalTrendsUI("temporal_trends")
          ),
          shiny::tabPanel("Time Series by Period",
                          shiny::br(),
                          timeSeriesUI("time_series")
          ),
          shiny::tabPanel("Spatial Distribution",
                          shiny::br(),
                          spatialMapUI("spatial_map")
          ),
          shiny::tabPanel("Habitat Reference",
                          shiny::br(),
                          shiny::div(
                            class = "habitat-ref-panel",
                            shiny::div(
                              style = "margin-bottom: 12px;",
                              shiny::h4("Species Habitat Reference",
                                        style = "margin: 0 0 4px 0; color: #cdd9e5;"),
                              shiny::p("Reference table of all species and their associated habitat classifications.",
                                       style = "margin: 0; font-size: 0.85em; color: #8bafc8;")
                            ),
                            DT::DTOutput("habitat_ref_table")
                          )
          )
        )
      )
    )
  )
}
