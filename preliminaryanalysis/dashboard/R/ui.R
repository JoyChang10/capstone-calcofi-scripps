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
          shiny::uiOutput("filter_ui")
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
          )
        )
      )
    )
  )
}
