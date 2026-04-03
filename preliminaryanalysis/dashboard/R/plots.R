# R/plots.R
# ── Plot Modules ─────────────────────────────────────────────────────────────
# Each section defines one plot module: UI + server logic
# Currently implemented: Abundance Through Time
# Additional modules stubbed for future use

# ── Helpers ──────────────────────────────────────────────────────────────────

agg_fn <- function(method) {
  switch(method,
    "mean"   = mean,
    "median" = median,
    "sum"    = sum,
    "max"    = max,
    mean
  )
}

agg_label <- function(method) {
  switch(method,
    "mean"   = "Mean Abundance",
    "median" = "Median Abundance",
    "sum"    = "Total Abundance",
    "max"    = "Max Abundance",
    "Mean Abundance"
  )
}

# Species colour palette (consistent across plots)
make_species_palette <- function(species, palette = "Set2") {
  n   <- length(species)
  if (n == 0) return(character(0))
  pal <- if (n <= 8) RColorBrewer::brewer.pal(max(3, n), palette)[seq_len(n)]
         else grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, palette))(n)
  stats::setNames(pal, species)
}

pretty_taxon <- function(x) tools::toTitleCase(gsub("_", " ", x))

# ── Module 1: Abundance Through Time ─────────────────────────────────────────

#' UI component
abundanceTimeUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plot-card",
    shiny::div(
      class = "plot-header",
      shiny::div(
        class = "plot-title-row",
        shiny::h3("Abundance Through Time", class = "plot-title"),
        shiny::div(
          class = "plot-controls-inline",
          shiny::downloadButton(ns("dl_plot"),  "", icon = shiny::icon("download"),
                                class = "btn-icon", title = "Download plot"),
          shiny::downloadButton(ns("dl_data"),  "", icon = shiny::icon("table"),
                                class = "btn-icon", title = "Download filtered data")
        )
      ),
      shiny::div(
        class = "plot-subtitle",
        shiny::textOutput(ns("plot_subtitle"), inline = TRUE)
      )
    ),
    shiny::div(
      class = "plot-body",
      plotly::plotlyOutput(ns("plot"), height = "460px") |>
        shinycssloaders::withSpinner(type = 6, color = "#2E86AB", size = 0.6)
    ),
    shiny::div(
      class = "plot-footer",
      shiny::uiOutput(ns("no_data_msg"))
    )
  )
}

#' Server component
abundanceTimeServer <- function(id, filtered_data, state, config) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Aggregated data — pushed into DuckDB, not computed in R ────────────
    plot_data <- shiny::reactive({
      shiny::req(
        length(state$selected_species) > 0,
        length(state$selected_seasons) > 0
      )
      query_aggregated(
        config,
        year_min   = state$year_min,
        year_max   = state$year_max,
        seasons    = state$selected_seasons,
        species    = state$selected_species,
        agg_method = state$aggregation
      )
    })

    # ── Subtitle ────────────────────────────────────────────────────────────
    output$plot_subtitle <- shiny::renderText({
      pd <- plot_data()
      sp <- length(unique(pd$taxon))
      yr <- if (nrow(pd) > 0) range(pd$year, na.rm = TRUE) else c(state$year_min, state$year_max)
      glue::glue("{sp} species · {yr[1]}–{yr[2]} · {agg_label(state$aggregation)} per year")
    })

    # ── No-data message ─────────────────────────────────────────────────────
    output$no_data_msg <- shiny::renderUI({
      pd <- plot_data()
      if (is.null(pd) || nrow(pd) == 0) {
        shiny::div(class = "no-data-msg",
          shiny::icon("circle-exclamation"),
          " No data matches the current filters. Try broadening your selection."
        )
      }
    })

    # ── Main plot ───────────────────────────────────────────────────────────
    output$plot <- plotly::renderPlotly({
      pd <- plot_data()
      shiny::req(nrow(pd) > 0)

      species_list <- sort(unique(pd$taxon))
      pal          <- make_species_palette(species_list, config$plots$abundance_time$color_palette)

      p <- ggplot2::ggplot(pd, ggplot2::aes(
        x     = year,
        y     = abundance,
        color = taxon_display,
        group = taxon_display,
        text  = paste0(
          "<b>", taxon_display, "</b><br>",
          "Year: ", year, "<br>",
          agg_label(state$aggregation), ": ",
          formatC(abundance, format = "f", digits = 1, big.mark = ",")
        )
      )) +
        ggplot2::geom_line(linewidth = 0.9, alpha = 0.85) +
        ggplot2::geom_point(size = 1.8, alpha = 0.9) +
        ggplot2::scale_color_manual(
          values = stats::setNames(pal, pretty_taxon(names(pal))),
          name   = "Species"
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::label_comma(),
          expand = ggplot2::expansion(mult = c(0.02, 0.08))
        ) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        ggplot2::labs(
          x = config$plots$abundance_time$x_label,
          y = agg_label(state$aggregation)
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.title         = ggplot2::element_text(size = 11, color = "#555"),
          legend.title       = ggplot2::element_text(size = 11, face = "bold"),
          legend.text        = ggplot2::element_text(size = 10),
          plot.background    = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background   = ggplot2::element_rect(fill = "transparent", color = NA)
        )

      # Optional smoother
      if (isTRUE(state$smooth) && length(species_list) <= 8) {
        p <- p + ggplot2::geom_smooth(
          method  = "loess", formula = y ~ x,
          se      = FALSE, linewidth = 1.5, alpha = 0.5,
          linetype = "dashed"
        )
      }

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(
            orientation = "v",
            x = 1.02, y = 0.98,
            bgcolor     = "rgba(255,255,255,0.85)",
            bordercolor = "#ddd",
            borderwidth = 1
          ),
          margin = list(l = 60, r = 160, t = 20, b = 60),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(
            format   = "png",
            filename = "abundance_through_time",
            width    = 1200,
            height   = 600
          )
        )
    })

    # ── Downloads ───────────────────────────────────────────────────────────
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("abundance_time_", Sys.Date(), ".png"),
      content  = function(file) {
        pd  <- plot_data()
        shiny::req(nrow(pd) > 0)
        species_list <- sort(unique(pd$taxon))
        pal          <- make_species_palette(species_list,
                          config$plots$abundance_time$color_palette)

        p <- ggplot2::ggplot(pd, ggplot2::aes(
          x = year, y = abundance,
          color = taxon_display, group = taxon_display
        )) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::geom_point(size = 2) +
          ggplot2::scale_color_manual(
            values = stats::setNames(pal, pretty_taxon(names(pal))),
            name = "Species"
          ) +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(
            title = "Ichthyoplankton Abundance Through Time",
            x = "Year", y = agg_label(state$aggregation)
          ) +
          ggplot2::theme_minimal(base_size = 14)

        ggplot2::ggsave(file, p, width = 12, height = 6, dpi = 150, bg = "white")
      }
    )

    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("filtered_data_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(plot_data(), file, row.names = FALSE)
      }
    )
  })
}

# ── Module 2–5: Stubs (future plots) ─────────────────────────────────────────
# Uncomment and implement as needed:
#
# spatialDistributionUI     <- function(id) { ... }
# spatialDistributionServer <- function(id, filtered_data, state, config) { ... }
#
# seasonalPatternUI     <- function(id) { ... }
# seasonalPatternServer <- function(id, filtered_data, state, config) { ... }
#
# speciesCompositionUI     <- function(id) { ... }
# speciesCompositionServer <- function(id, filtered_data, state, config) { ... }
#
# correlationUI     <- function(id) { ... }
# correlationServer <- function(id, filtered_data, state, config) { ... }
