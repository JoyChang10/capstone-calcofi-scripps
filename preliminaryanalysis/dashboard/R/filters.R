# R/filters.R
# Builds sidebar filter UI from config.yml + raw data
# Applies user-selected filters to dataset

# ── UI Builder ──────────────────────────────────────────────────────────────

build_filter_ui <- function(config, data_result) {
  if (is.null(data_result$data)) {
    return(shiny::div(class = "filter-error", "No data available"))
  }

  df      <- data_result$data
  defs    <- config$defaults
  filters <- config$filters

  shiny::tagList(

    # ── Refresh ────────────────────────────────────────────────────────────
    if (isTRUE(config$data$enable_manual_refresh)) {
      shiny::div(
        class = "filter-section",
        shiny::actionButton(
          "btn_refresh", "↻  Refresh Data",
          class = "btn-refresh btn-block"
        ),
        shiny::div(
          class = "last-updated",
          shiny::textOutput("last_updated_text", inline = TRUE)
        )
      )
    },

    shiny::hr(class = "filter-divider"),

    # ── Year Range ─────────────────────────────────────────────────────────
    shiny::div(
      class = "filter-section",
      shiny::div(class = "filter-label", filters$year_range$label),
      shiny::sliderInput(
        inputId = "year_range",
        label   = NULL,
        min     = data_result$years[1],
        max     = data_result$years[2],
        value   = c(
          max(data_result$years[1], defs$year_min),
          min(data_result$years[2], defs$year_max)
        ),
        step    = 1,
        sep     = "",
        ticks   = FALSE
      )
    ),

    shiny::hr(class = "filter-divider"),

    # ── Species ────────────────────────────────────────────────────────────
    shiny::div(
      class = "filter-section",
      shiny::div(
        class = "filter-label-row",
        shiny::div(class = "filter-label", filters$species$label),
        shiny::div(
          class = "filter-label-actions",
          shiny::actionLink("select_all_species",   "All"),
          shiny::span(" · "),
          shiny::actionLink("deselect_all_species", "None"),
          shiny::span(" · "),
          shiny::actionLink("select_top_species",   "Top 5")
        )
      ),
      shiny::selectizeInput(
        inputId  = "species_select",
        label    = NULL,
        choices  = stats::setNames(
          data_result$species,
          tools::toTitleCase(gsub("_", " ", data_result$species))
        ),
        selected = intersect(defs$species, data_result$species),
        multiple = TRUE,
        options  = list(
          placeholder    = "Select species…",
          maxItems       = 15,
          plugins        = list("remove_button"),
          closeAfterSelect = FALSE
        )
      )
    )
  )
}

# ── Data Filtering ───────────────────────────────────────────────────────────

apply_filters <- function(raw_data, state) {
  if (is.null(raw_data)) return(NULL)

  df <- raw_data

  # Year range
  df <- df[df$year >= state$year_min & df$year <= state$year_max, ]

  # Season
  if (length(state$selected_seasons) > 0) {
    df <- df[as.character(df$season) %in% state$selected_seasons, ]
  }

  # Species
  if (length(state$selected_species) > 0) {
    df <- df[df$taxon %in% state$selected_species, ]
  }

  df
}
