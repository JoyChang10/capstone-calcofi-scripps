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

exportsServer <- function(id, filtered_data, state, config) {
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
      filename = function() paste0("ichthyo_report_", Sys.Date(), ".pdf"),
      content  = function(file) {
        df <- filtered_data()
        shiny::req(!is.null(df) && nrow(df) > 0)

        fn <- switch(state$aggregation,
          "mean"   = mean,   "median" = median,
          "sum"    = sum,    "max"    = max, mean
        )

        pd <- stats::aggregate(
          abundance ~ year + taxon,
          data = df, FUN = fn, na.rm = TRUE
        )
        pd$taxon_display <- tools::toTitleCase(gsub("_", " ", pd$taxon))

        grDevices::pdf(file, width = 11, height = 8.5)

        # Title page info
        graphics::plot.new()
        graphics::text(0.5, 0.9,  "Ichthyoplankton Abundance Dashboard",
                       cex = 1.8, font = 2, adj = 0.5)
        graphics::text(0.5, 0.82, paste("Report generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
                       cex = 1,   adj = 0.5)
        graphics::text(0.5, 0.74, paste("Year range:", state$year_min, "–", state$year_max),
                       cex = 1,   adj = 0.5)
        graphics::text(0.5, 0.68, paste("Seasons:", paste(state$selected_seasons, collapse = ", ")),
                       cex = 1,   adj = 0.5)
        graphics::text(0.5, 0.62, paste("Species:", length(state$selected_species)),
                       cex = 1,   adj = 0.5)
        graphics::text(0.5, 0.56, paste("Aggregation:", state$aggregation),
                       cex = 1,   adj = 0.5)

        # Abundance plot
        sp_list <- sort(unique(pd$taxon))
        colours <- grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(min(8, length(sp_list)), "Set2")
        )(length(sp_list))

        graphics::plot.new()
        graphics::plot.window(
          xlim = range(pd$year),
          ylim = c(0, max(pd$abundance, na.rm = TRUE) * 1.1)
        )
        graphics::axis(1); graphics::axis(2, las = 1)
        graphics::title(
          main = "Abundance Through Time",
          xlab = "Year",
          ylab = paste(tools::toTitleCase(state$aggregation), "Abundance")
        )
        graphics::box()

        for (i in seq_along(sp_list)) {
          sub <- pd[pd$taxon == sp_list[i], ]
          sub <- sub[order(sub$year), ]
          graphics::lines(sub$year, sub$abundance, col = colours[i], lwd = 2)
          graphics::points(sub$year, sub$abundance, col = colours[i], pch = 19, cex = 0.7)
        }
        graphics::legend("topright",
          legend = tools::toTitleCase(gsub("_", " ", sp_list)),
          col    = colours, lwd = 2, pch = 19, cex = 0.75,
          bty    = "n"
        )

        grDevices::dev.off()
      }
    )
  })
}
