# R/filters.R

build_filter_ui <- function(config, data_result, habitat_lookup = NULL) {
  if (is.null(data_result$data)) {
    return(shiny::div(class = "filter-error", "No data available"))
  }
  
  df      <- data_result$data
  defs    <- config$defaults
  filters <- config$filters
  
  habitat_choices <- c("pelagic", "benthic")
  grpname_choices <- c("coastal", "oceanic", "coastal-oceanic")
  
  shiny::tagList(
    
    # ── Refresh ──────────────────────────────────────────────────────────
    if (isTRUE(config$data$enable_manual_refresh)) {
      shiny::div(
        class = "filter-section",
        shiny::actionButton("btn_refresh", "↻  Refresh Data",
                            class = "btn-refresh btn-block"),
        shiny::div(class = "last-updated",
                   shiny::textOutput("last_updated_text", inline = TRUE))
      )
    },
    
    shiny::hr(class = "filter-divider"),
    
    # ── Year Range ────────────────────────────────────────────────────────
    shiny::div(
      class = "filter-section",
      shiny::div(class = "filter-label", filters$year_range$label),
      shiny::sliderInput(
        inputId = "year_range", label = NULL,
        min   = data_result$years[1],
        max   = data_result$years[2],
        value = c(max(data_result$years[1], defs$year_min),
                  min(data_result$years[2], defs$year_max)),
        step = 1, sep = "", ticks = FALSE
      )
    ),
    
    shiny::hr(class = "filter-divider"),
    
    # ── Filter Mode ───────────────────────────────────────────────────────
    shiny::div(
      class = "filter-section",
      shiny::div(class = "filter-label", "Filter Mode"),
      shiny::radioButtons(
        inputId  = "filter_mode", label = NULL,
        choices  = c("By Species" = "species", "By Habitat" = "habitat"),
        selected = "species", inline = TRUE
      )
    ),
    
    shiny::hr(class = "filter-divider"),
    
    # ── Species panel (always in DOM, toggled by JS) ──────────────────────
    shiny::div(
      id    = "species_panel",
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
        inputId  = "species_select", label = NULL,
        choices  = stats::setNames(data_result$species,
                                   tools::toTitleCase(gsub("_", " ", data_result$species))),
        selected = intersect(defs$species, data_result$species),
        multiple = TRUE,
        options  = list(placeholder = "Select species…", maxItems = NULL,
                        plugins = list("remove_button"), closeAfterSelect = FALSE)
      )
    ),
    
    # ── Habitat panel (always in DOM, toggled by JS) ──────────────────────
    shiny::div(
      id = "habitat_panel",
      
      shiny::div(
        class = "filter-section",
        shiny::div(class = "filter-label", "Filter By"),
        shiny::radioButtons(
          inputId  = "habitat_submode", label = NULL,
          choices  = c("Habitat Type" = "habitat_type", "Group" = "group"),
          selected = "habitat_type", inline = TRUE
        )
      ),
      
      shiny::hr(class = "filter-divider"),
      
      shiny::div(
        id    = "habitat_type_panel",
        class = "filter-section",
        shiny::div(class = "filter-label", "Habitat Type"),
        shiny::checkboxGroupInput(
          inputId  = "habitat_select", label = NULL,
          choices  = habitat_choices,
          selected = habitat_choices
        )
      ),
      
      shiny::div(
        id    = "grpname_panel",
        class = "filter-section",
        shiny::div(class = "filter-label", "Group"),
        shiny::checkboxGroupInput(
          inputId  = "grpname_select", label = NULL,
          choices  = grpname_choices,
          selected = grpname_choices
        )
      )
    ),
    
    # ── JS toggles ────────────────────────────────────────────────────────
    shiny::tags$script(shiny::HTML("
      function applyFilterMode(mode) {
        if (mode === 'habitat') {
          $('#species_panel').hide();
          $('#habitat_panel').show();
        } else {
          $('#species_panel').show();
          $('#habitat_panel').hide();
        }
      }

      function applyHabitatSubmode(submode) {
        if (submode === 'habitat_type') {
          $('#habitat_type_panel').show();
          $('#grpname_panel').hide();
        } else {
          $('#habitat_type_panel').hide();
          $('#grpname_panel').show();
        }
      }

      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'filter_mode')     applyFilterMode(e.value);
        if (e.name === 'habitat_submode') applyHabitatSubmode(e.value);
      });

      $(document).ready(function() {
        $('#habitat_panel').hide();
        $('#grpname_panel').hide();
      });
    "))
  )
}

apply_filters <- function(raw_data, state) {
  if (is.null(raw_data)) return(NULL)
  df <- raw_data
  df <- df[df$year >= state$year_min & df$year <= state$year_max, ]
  if (length(state$selected_seasons) > 0)
    df <- df[as.character(df$season) %in% state$selected_seasons, ]
  if (length(state$selected_species) > 0)
    df <- df[df$taxon %in% state$selected_species, ]
  df
}