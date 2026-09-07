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

    # ── Header ───────────────────────────────────────────────────────────
    shiny::div(class = "sb-header",
      shiny::div(
        shiny::h4("Filters", class = "sb-title"),
        shiny::div(
          shiny::textOutput("record_count", inline = TRUE),
          class = "sb-count"
        )
      )
    ),

    # ── Refresh ───────────────────────────────────────────────────────────
    if (isTRUE(config$data$enable_manual_refresh)) {
      shiny::div(class = "sb-refresh",
        shiny::actionButton("btn_refresh", "\u21bb  Refresh Data", class = "btn-refresh"),
        shiny::div(class = "last-updated", shiny::textOutput("last_updated_text", inline = TRUE))
      )
    },

    # ── Gray spacer ───────────────────────────────────────────────────────
    shiny::div(style = "height: 3px; background: var(--bg);"),

    # ── YEAR RANGE accordion card ─────────────────────────────────────────
    shiny::tags$details(
      open = NA,
      class = "acc-card",
      shiny::tags$summary(
        class = "acc-summary",
        "Year Range",
        shiny::tags$span(class = "acc-chev", "\u25be")
      ),
      shiny::div(class = "acc-body",
        shiny::sliderInput(
          inputId = "year_range", label = NULL,
          min   = data_result$years[1],
          max   = data_result$years[2],
          value = c(max(data_result$years[1], defs$year_min),
                    min(data_result$years[2], defs$year_max)),
          step = 1, sep = "", ticks = FALSE
        )
      )
    ),

    # ── SEASON accordion card (shown only on Temporal Trends tab) ─────────
    shiny::tags$details(
      open = NA,
      class = "acc-card",
      id = "season_card",
      style = "display:none;",
      shiny::tags$summary(
        class = "acc-summary",
        "Season",
        shiny::tags$span(class = "acc-chev", "\u25be")
      ),
      shiny::div(class = "acc-body",
        shiny::checkboxGroupInput(
          inputId  = "season_check", label = NULL,
          choices  = c("Spring" = "spring", "Summer" = "summer",
                       "Fall"   = "fall",   "Winter" = "winter"),
          selected = config$defaults$season,
          inline   = TRUE
        )
      )
    ),

    # ── TIME PERIOD accordion card (shown only on Time Series by Period tab)
    shiny::tags$details(
      open = NA,
      class = "acc-card",
      id = "period_card",
      style = "display:none;",
      shiny::tags$summary(
        class = "acc-summary",
        "Time Period",
        shiny::tags$span(class = "acc-chev", "\u25be")
      ),
      shiny::div(class = "acc-body",
        shiny::div(class = "filter-label-row",
          shiny::div(class = "filter-label-actions",
            shiny::actionLink("select_all_periods",   "All"),
            shiny::span(" \u00b7 "),
            shiny::actionLink("deselect_all_periods", "None")
          )
        ),
        shiny::checkboxGroupInput(
          inputId  = "period_check", label = NULL,
          choices  = c("1951\u20131976", "1977\u20131998",
                       "1999\u20132014", "2015\u2013present"),
          selected = c("1951\u20131976", "1977\u20131998",
                       "1999\u20132014", "2015\u2013present")
        )
      )
    ),

    # ── SPECIES / HABITAT accordion card ─────────────────────────────────
    shiny::tags$details(
      open = NA,
      class = "acc-card",
      shiny::tags$summary(
        class = "acc-summary",
        "Data Type",
        shiny::tags$span(class = "acc-chev", "\u25be")
      ),
      shiny::div(class = "acc-body",

        # Filter mode pills
        shiny::div(class = "mode-pills",
          shiny::tags$label(
            class = "mode-pill mode-pill-active",
            id = "pill-species",
            shiny::tags$input(type = "radio", name = "filter_mode_ui",
                              value = "species", checked = NA,
                              onclick = "Shiny.setInputValue('filter_mode', 'species'); updatePills('species');"),
            "By Species"
          ),
          shiny::tags$label(
            class = "mode-pill",
            id = "pill-habitat",
            shiny::tags$input(type = "radio", name = "filter_mode_ui",
                              value = "habitat",
                              onclick = "Shiny.setInputValue('filter_mode', 'habitat'); updatePills('habitat');"),
            "By Habitat"
          )
        ),

        # Species panel (toggled by JS)
        shiny::div(id = "species_panel", style = "margin-top:12px;",
          shiny::div(class = "filter-label-row",
            shiny::div(class = "filter-label", filters$species$label),
            shiny::div(class = "filter-label-actions",
              shiny::actionLink("select_all_species",   "All"),
              shiny::span(" \u00b7 "),
              shiny::actionLink("deselect_all_species", "None"),
              shiny::span(" \u00b7 "),
              shiny::actionLink("select_top_species",   "Top 5")
            )
          ),
          shiny::selectizeInput(
            inputId  = "species_select", label = NULL,
            choices  = stats::setNames(data_result$species,
                                       tools::toTitleCase(gsub("_", " ", data_result$species))),
            selected = intersect(defs$species, data_result$species),
            multiple = TRUE,
            options  = list(placeholder = "Select species\u2026", maxItems = NULL,
                            plugins = list("remove_button"), closeAfterSelect = FALSE)
          )
        ),

        # Habitat panel (toggled by JS)
        shiny::div(id = "habitat_panel",
          shiny::div(style = "margin-top:12px;",
            shiny::div(class = "filter-label", "Filter By"),
            shiny::radioButtons(
              inputId  = "habitat_submode", label = NULL,
              choices  = c("Habitat Type" = "habitat_type", "Group" = "group"),
              selected = "habitat_type", inline = TRUE
            )
          ),
          shiny::div(class = "acc-divider"),
          shiny::div(id = "habitat_type_panel", class = "filter-section",
            shiny::div(class = "filter-label", "Habitat Type"),
            shiny::checkboxGroupInput(
              inputId  = "habitat_select", label = NULL,
              choices  = habitat_choices,
              selected = habitat_choices
            )
          ),
          shiny::div(id = "grpname_panel", class = "filter-section",
            shiny::div(class = "filter-label", "Group"),
            shiny::checkboxGroupInput(
              inputId  = "grpname_select", label = NULL,
              choices  = grpname_choices,
              selected = grpname_choices
            )
          )
        )
      )
    ),

    # ── EXPORT accordion card ─────────────────────────────────────────────
    shiny::tags$details(
      open = NA,
      class = "acc-card",
      shiny::tags$summary(
        class = "acc-summary",
        "Export",
        shiny::tags$span(class = "acc-chev", "\u25be")
      ),
      shiny::div(class = "acc-body",
        exportsUI("exports"),
        shiny::p(
          shiny::icon("circle-info", style = "font-size: 0.75em;"),
          " Chrome must be installed to include the Spatial Distribution map in PDF exports.",
          style = "font-size: 0.72em; color: var(--text-faint); font-style: italic; margin-top: 8px; line-height: 1.4;"
        )
      )
    ),

    # ── JS ────────────────────────────────────────────────────────────────
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

      function updatePills(mode) {
        document.getElementById('pill-species').className = 'mode-pill' + (mode === 'species' ? ' mode-pill-active' : '');
        document.getElementById('pill-habitat').className = 'mode-pill' + (mode === 'habitat' ? ' mode-pill-active' : '');
      }

      function updateConditionalCards() {
        var active = $('.nav-tabs li.active a').text().trim();
        $('#season_card').toggle(active === 'Temporal Trends');
        $('#period_card').toggle(active === 'Time Series by Period');
      }

      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'filter_mode')     applyFilterMode(e.value);
        if (e.name === 'habitat_submode') applyHabitatSubmode(e.value);
      });

      $(document).on('click', '.nav-tabs li a', function() {
        setTimeout(updateConditionalCards, 50);
      });

      $(document).ready(function() {
        $('#habitat_panel').hide();
        $('#grpname_panel').hide();
        updateConditionalCards();
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
