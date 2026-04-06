# R/ui.R
# Builds the full UI layout
# Called by app.R with config and initial data

build_ui <- function(config, data_result) {
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
        class = "content",
        shiny::div(
          class = "plots-grid",
          abundanceTimeUI("abundance_time")
        )
      )
    )
  )
}
