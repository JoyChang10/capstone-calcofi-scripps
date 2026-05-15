# R/exports.R
# Handles dashboard-level exports:
#   - Full filtered dataset as CSV
#   - Summary table as CSV
#   - PDF report (plot & summary stats)

# ── UI Component ─────────────────────────────────────────────────────────────

exportsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "export-panel",
    shiny::h5("Export", class = "export-title"),
    shiny::div(
      class = "export-buttons",
      shiny::downloadButton(ns("dl_csv"),     "Filtered CSV",    class = "btn-export"),
      shiny::downloadButton(ns("dl_summary"), "Summary CSV",     class = "btn-export"),
      shiny::downloadButton(ns("dl_pdf"),     "PDF Report",      class = "btn-export btn-export-pdf")
    )
  )
}

# ── Server Component ──────────────────────────────────────────────────────────

exportsServer <- function(id, filtered_data, state, config, habitat_lookup = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Full filtered CSV
    output$dl_csv <- shiny::downloadHandler(
      filename = function() paste0("ichthyo_filtered_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(filtered_data(), file, row.names = FALSE)
      }
    )

    # Aggregated summary CSV
    output$dl_summary <- shiny::downloadHandler(
      filename = function() paste0("ichthyo_summary_", Sys.Date(), ".csv"),
      content  = function(file) {
        df <- filtered_data()
        shiny::req(!is.null(df) && nrow(df) > 0)

        fn <- switch(state$aggregation,
          "mean"   = mean,
          "median" = median,
          "sum"    = sum,
          "max"    = max,
          mean
        )

        summary_df <- stats::aggregate(
          abundance ~ year + taxon + season,
          data = df,
          FUN  = fn,
          na.rm = TRUE
        )
        summary_df$taxon <- tools::toTitleCase(gsub("_", " ", summary_df$taxon))
        names(summary_df)[names(summary_df) == "abundance"] <- paste0(state$aggregation, "_abundance")

        utils::write.csv(summary_df, file, row.names = FALSE)
      }
    )

    # PDF report
    output$dl_pdf <- shiny::downloadHandler(
      filename = function() paste0("calcofi_report_", Sys.Date(), ".pdf"),
      content  = function(file) {
        plots <- build_all_ggplots(state, config, habitat_lookup)

        grDevices::pdf(file, width = 11, height = 8.5)

        # Title page
        graphics::plot.new()
        graphics::text(0.5, 0.88, "CalCOFI Ichthyoplankton Dashboard",
                       cex = 2, font = 2, adj = 0.5)
        graphics::text(0.5, 0.78, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
                       cex = 1.1, adj = 0.5, col = "#444444")
        graphics::text(0.5, 0.68, paste("Year range:", state$year_min, "–", state$year_max),
                       cex = 1, adj = 0.5)
        graphics::text(0.5, 0.61, paste("Seasons:", paste(tools::toTitleCase(state$selected_seasons), collapse = ", ")),
                       cex = 1, adj = 0.5)
        graphics::text(0.5, 0.54, paste("Species selected:", length(state$selected_species)),
                       cex = 1, adj = 0.5)
        graphics::text(0.5, 0.47, paste("Aggregation:", tools::toTitleCase(state$aggregation)),
                       cex = 1, adj = 0.5)
        graphics::text(0.5, 0.34, paste("Plots included:", length(plots)),
                       cex = 0.95, adj = 0.5, col = "#666666")
        graphics::text(0.5, 0.27, "Spatial Distribution map included on final page (requires Chrome).",
                       cex = 0.9, adj = 0.5, col = "#888888", font = 3)

        # One ggplot per page
        for (p in plots) print(p)

        # Spatial map page via webshot2
        map_widget <- tryCatch(build_spatial_leaflet(state, config), error = function(e) NULL)
        if (!is.null(map_widget)) {
          tmp_html <- tempfile(fileext = ".html")
          tmp_png  <- tempfile(fileext = ".png")
          tryCatch({
            htmlwidgets::saveWidget(map_widget, tmp_html, selfcontained = TRUE)
            webshot2::webshot(tmp_html, tmp_png, vwidth = 1100, vheight = 850, delay = 2)
            if (file.exists(tmp_png)) {
              img <- png::readPNG(tmp_png)
              grid::grid.newpage()
              grid::pushViewport(grid::viewport(
                layout = grid::grid.layout(2, 1, heights = grid::unit(c(0.07, 0.93), "npc"))
              ))
              grid::pushViewport(grid::viewport(layout.pos.row = 1))
              grid::grid.text("Spatial Distribution",
                              gp = grid::gpar(fontsize = 15, fontface = "bold"))
              grid::popViewport()
              grid::pushViewport(grid::viewport(layout.pos.row = 2))
              grid::grid.raster(img, width = 1, height = 1)
              grid::popViewport()
            }
          }, error = function(e) {
            grid::grid.newpage()
            grid::grid.text(
              paste0("Spatial Distribution map could not be generated.\n",
                     "Chrome must be installed and accessible to capture this plot.\n\n",
                     "Error: ", conditionMessage(e)),
              gp = grid::gpar(fontsize = 12, col = "#666666"), just = "centre"
            )
          })
        }

        grDevices::dev.off()
      }
    )
  })
}
