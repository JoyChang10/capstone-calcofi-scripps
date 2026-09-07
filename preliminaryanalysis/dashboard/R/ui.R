# R/ui.R
# Builds the full UI layout
# Called by app.R with config and initial data

build_ui <- function(config, data_result, habitat_lookup = NULL) {
  shiny::fluidPage(
    title = config$app$title,
    shiny::tags$head(
      shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      shiny::tags$link(rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;500;600;700;800&display=swap"),
      shiny::tags$link(rel = "stylesheet", href = "styles.css?v=6"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),

    # ── Top bar ─────────────────────────────────────────────────────────────
    shiny::div(
      class = "topbar",
      shiny::div(
        class = "topbar-left",
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
          shiny::uiOutput("filter_ui")
        )
      ),

      # Content area
      shiny::div(
        class = "content",
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
          shiny::tabPanel("Time Series",
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
                                        style = "margin: 0 0 4px 0;"),
                              shiny::p("Reference table of all species and their associated habitat classifications.",
                                       style = "margin: 0; font-size: 0.85em; color: var(--text-muted);")
                            ),
                            DT::DTOutput("habitat_ref_table")
                          )
          )
        )
      )
    )
  )
}
