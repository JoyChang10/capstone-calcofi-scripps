# R/plots.R
# ── Plot Modules ─────────────────────────────────────────────────────────────

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

make_species_palette <- function(species, palette = "Set2") {
  n   <- length(species)
  if (n == 0) return(character(0))
  pal <- if (n <= 8) RColorBrewer::brewer.pal(max(3, n), palette)[seq_len(n)]
  else grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, palette))(n)
  stats::setNames(pal, species)
}

pretty_taxon <- function(x) tools::toTitleCase(gsub("_", " ", x))

# ── Habitat aggregation helper ────────────────────────────────────────────────
# When in habitat mode, fetch all selected species and collapse into group labels
query_habitat_aggregated <- function(config, year_min, year_max, seasons,
                                     species, habitat_lookup,
                                     habitat_select, grpname_select) {
  # Get raw yearly totals per species
  pd <- query_aggregated(
    config,
    year_min   = year_min,
    year_max   = year_max,
    seasons    = seasons,
    species    = species,
    agg_method = "sum"
  )
  
  if (nrow(pd) == 0) return(pd)
  
  # Join with habitat lookup to get group labels
  lookup_clean <- habitat_lookup |>
    dplyr::filter(
      trimws(habitat) %in% habitat_select,
      trimws(GRPname) %in% grpname_select
    ) |>
    dplyr::select(species_clean, habitat, GRPname) |>
    dplyr::distinct(species_clean, .keep_all = TRUE)
  
  pd_joined <- pd |>
    dplyr::left_join(lookup_clean, by = c("taxon" = "species_clean")) |>
    dplyr::mutate(
      group_label = paste0(
        tools::toTitleCase(trimws(habitat)), " - ",
        tools::toTitleCase(trimws(GRPname))
      )
    )
  
  # Sum abundance across all species within each group per year
  pd_grouped <- pd_joined |>
    dplyr::group_by(year, group_label) |>
    dplyr::summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop") |>
    dplyr::rename(taxon_display = group_label) |>
    dplyr::mutate(taxon = taxon_display)
  
  pd_grouped
}

# ── Plot 1: Abundance Through Time ────────────────────────────────────────────

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
          shiny::downloadButton(ns("dl_plot"), "", icon = shiny::icon("download"),
                                class = "btn-icon", title = "Download plot"),
          shiny::downloadButton(ns("dl_data"), "", icon = shiny::icon("table"),
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
    shiny::div(class = "plot-footer", shiny::uiOutput(ns("no_data_msg")))
  )
}

abundanceTimeServer <- function(id, filtered_data, state, config, habitat_lookup = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    plot_data <- shiny::reactive({
      shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)
      
      if (!is.null(state$filter_mode) && state$filter_mode == "habitat") {
        query_habitat_aggregated(
          config,
          year_min       = state$year_min,
          year_max       = state$year_max,
          seasons        = state$selected_seasons,
          species        = state$selected_species,
          habitat_lookup = habitat_lookup,
          habitat_select = state$habitat_select,
          grpname_select = state$grpname_select
        )
      } else {
        query_aggregated(
          config,
          year_min   = state$year_min,
          year_max   = state$year_max,
          seasons    = state$selected_seasons,
          species    = state$selected_species,
          agg_method = state$aggregation
        )
      }
    })
    
    output$plot_subtitle <- shiny::renderText({
      pd <- plot_data()
      grp <- length(unique(pd$taxon_display))
      yr  <- if (nrow(pd) > 0) range(pd$year, na.rm = TRUE) else c(state$year_min, state$year_max)
      if (!is.null(state$filter_mode) && state$filter_mode == "habitat") {
        glue::glue("{grp} habitat groups · {yr[1]}–{yr[2]} · Total Abundance per year")
      } else {
        glue::glue("{grp} species · {yr[1]}–{yr[2]} · {agg_label(state$aggregation)} per year")
      }
    })
    
    output$no_data_msg <- shiny::renderUI({
      pd <- plot_data()
      if (is.null(pd) || nrow(pd) == 0)
        shiny::div(class = "no-data-msg", shiny::icon("circle-exclamation"),
                   " No data matches the current filters. Try broadening your selection.")
    })
    
    output$plot <- plotly::renderPlotly({
      pd <- plot_data()
      shiny::req(nrow(pd) > 0)
      
      group_list <- sort(unique(pd$taxon_display))
      pal        <- make_species_palette(
        sort(unique(pd$taxon)),
        config$plots$abundance_time$color_palette
      )
      pal_display <- stats::setNames(pal, sort(unique(pd$taxon_display)))
      
      y_label <- if (!is.null(state$filter_mode) && state$filter_mode == "habitat")
        "Total Abundance" else agg_label(state$aggregation)
      
      p <- ggplot2::ggplot(pd, ggplot2::aes(
        x     = year,
        y     = abundance,
        color = taxon_display,
        group = taxon_display,
        text  = paste0(
          "<b>", taxon_display, "</b><br>",
          "Year: ", year, "<br>",
          y_label, ": ",
          formatC(abundance, format = "f", digits = 1, big.mark = ",")
        )
      )) +
        ggplot2::geom_line(linewidth = 0.9, alpha = 0.85) +
        ggplot2::geom_point(size = 1.8, alpha = 0.9) +
        ggplot2::scale_color_manual(values = pal_display, name = if (!is.null(state$filter_mode) && state$filter_mode == "habitat") "Habitat Group" else "Species") +
        ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                    expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        ggplot2::labs(x = "Year", y = y_label) +
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
      
      if (isTRUE(state$smooth) && length(group_list) <= 8) {
        p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                      se = FALSE, linewidth = 1.5, alpha = 0.5,
                                      linetype = "dashed")
      }
      
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(orientation = "v", x = 1.02, y = 0.98,
                        bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ddd", borderwidth = 1),
          margin = list(l = 60, r = 160, t = 20, b = 60),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(format = "png", filename = "abundance_through_time",
                                      width = 1200, height = 600)
        )
    })
    
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("abundance_time_", Sys.Date(), ".png"),
      content  = function(file) {
        pd <- plot_data(); shiny::req(nrow(pd) > 0)
        pal <- make_species_palette(sort(unique(pd$taxon)), config$plots$abundance_time$color_palette)
        pal_display <- stats::setNames(pal, sort(unique(pd$taxon_display)))
        p <- ggplot2::ggplot(pd, ggplot2::aes(x = year, y = abundance,
                                              color = taxon_display, group = taxon_display)) +
          ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
          ggplot2::scale_color_manual(values = pal_display, name = "Group") +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(title = "Ichthyoplankton Abundance Through Time", x = "Year", y = "Abundance") +
          ggplot2::theme_minimal(base_size = 14)
        ggplot2::ggsave(file, p, width = 12, height = 6, dpi = 150, bg = "white")
      }
    )
    
    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("filtered_data_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(plot_data(), file, row.names = FALSE)
    )
  })
}

# ── Plot 2: Species Correlation Heatmap ───────────────────────────────────────

corrHeatmapUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plot-card",
    shiny::div(
      class = "plot-header",
      shiny::div(
        class = "plot-title-row",
        shiny::h3("Species Correlation Heatmap", class = "plot-title"),
        shiny::div(
          class = "plot-controls-inline",
          shiny::downloadButton(ns("dl_plot"), "", icon = shiny::icon("download"),
                                class = "btn-icon", title = "Download plot"),
          shiny::downloadButton(ns("dl_data"), "", icon = shiny::icon("table"),
                                class = "btn-icon", title = "Download correlation matrix")
        )
      ),
      shiny::div(class = "plot-subtitle", shiny::textOutput(ns("plot_subtitle"), inline = TRUE))
    ),
    shiny::div(
      class = "plot-body",
      plotly::plotlyOutput(ns("plot"), height = "460px") |>
        shinycssloaders::withSpinner(type = 6, color = "#2E86AB", size = 0.6)
    ),
    shiny::div(class = "plot-footer", shiny::uiOutput(ns("no_data_msg")))
  )
}

corrHeatmapServer <- function(id, filtered_data, state, config, habitat_lookup = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    corr_data <- shiny::reactive({
      shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)
      
      if (!is.null(state$filter_mode) && state$filter_mode == "habitat") {
        pd <- query_habitat_aggregated(
          config,
          year_min       = state$year_min,
          year_max       = state$year_max,
          seasons        = state$selected_seasons,
          species        = state$selected_species,
          habitat_lookup = habitat_lookup,
          habitat_select = state$habitat_select,
          grpname_select = state$grpname_select
        )
      } else {
        pd <- query_aggregated(
          config,
          year_min   = state$year_min,
          year_max   = state$year_max,
          seasons    = state$selected_seasons,
          species    = state$selected_species,
          agg_method = state$aggregation
        )
      }
      
      shiny::req(nrow(pd) > 0)
      
      wide <- tryCatch(
        tidyr::pivot_wider(pd[, c("year", "taxon_display", "abundance")],
                           names_from = "taxon_display", values_from = "abundance",
                           values_fn = mean),
        error = function(e) NULL
      )
      shiny::req(!is.null(wide), ncol(wide) >= 3)
      species_mat <- wide[, -which(names(wide) == "year"), drop = FALSE]
      shiny::req(nrow(species_mat) >= 3)
      corr_mat <- cor(species_mat, use = "pairwise.complete.obs")
      corr_mat
    })
    
    output$plot_subtitle <- shiny::renderText({
      cm <- corr_data()
      yr <- c(state$year_min, state$year_max)
      label <- if (!is.null(state$filter_mode) && state$filter_mode == "habitat")
        "habitat groups" else "species"
      glue::glue("{ncol(cm)} {label} · {yr[1]}–{yr[2]} · Correlation of Total Abundance")
    })
    
    output$no_data_msg <- shiny::renderUI({
      cm <- tryCatch(corr_data(), error = function(e) NULL)
      if (is.null(cm))
        shiny::div(class = "no-data-msg", shiny::icon("circle-exclamation"),
                   " Need at least 2 groups with overlapping years to compute correlations.")
    })
    
    output$plot <- plotly::renderPlotly({
      cm <- corr_data(); shiny::req(!is.null(cm))
      n_sp <- ncol(cm); sp_nm <- colnames(cm)
      corr_long <- reshape2::melt(cm, varnames = c("Var1", "Var2"), value.name = "Correlation")
      corr_long$hover_text <- paste0("<b>", corr_long$Var1, "</b> × <b>", corr_long$Var2,
                                     "</b><br>Pearson r = ", round(corr_long$Correlation, 3))
      plotly::plot_ly(
        x = sp_nm, y = sp_nm, z = cm, type = "heatmap", zmin = -1, zmax = 1,
        colorscale = list(list(0, "#2166ac"), list(0.25, "#92c5de"), list(0.5, "#f7f7f7"),
                          list(0.75, "#f4a582"), list(1, "#b2182b")),
        colorbar = list(title = "r", tickvals = c(-1, -0.5, 0, 0.5, 1),
                        ticktext = c("-1", "-0.5", "0", "0.5", "1"),
                        len = 0.75, thickness = 14, outlinewidth = 0),
        text = matrix(corr_long$hover_text, nrow = n_sp, ncol = n_sp),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          xaxis = list(title = "", tickangle = -40, tickfont = list(size = 11),
                       showgrid = FALSE, fixedrange = TRUE),
          yaxis = list(title = "", tickfont = list(size = 11), showgrid = FALSE,
                       autorange = "reversed", fixedrange = TRUE),
          margin = list(l = 140, r = 80, t = 20, b = 140),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "zoom2d", "pan2d"),
          toImageButtonOptions = list(format = "png", filename = "correlation_heatmap",
                                      width = 900, height = 800)
        )
    })
    
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("correlation_heatmap_", Sys.Date(), ".png"),
      content  = function(file) {
        cm <- corr_data(); shiny::req(!is.null(cm))
        corr_long <- reshape2::melt(cm, varnames = c("Var1", "Var2"), value.name = "Correlation")
        p <- ggplot2::ggplot(corr_long, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
          ggplot2::geom_tile(color = "white", linewidth = 0.4) +
          ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
                                        midpoint = 0, limits = c(-1, 1), name = "Pearson r") +
          ggplot2::coord_fixed() +
          ggplot2::labs(title = glue::glue("Correlation Heatmap · {state$year_min}–{state$year_max}")) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(axis.title = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                         panel.grid = ggplot2::element_blank())
        ggplot2::ggsave(file, p, width = 9, height = 8, dpi = 150, bg = "white")
      }
    )
    
    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("correlation_matrix_", Sys.Date(), ".csv"),
      content  = function(file) {
        cm <- corr_data(); shiny::req(!is.null(cm))
        utils::write.csv(as.data.frame(cm), file)
      }
    )
  })
}

# ── Plot 3: Mean vs Variance ──────────────────────────────────────────────────

meanVarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plot-card",
    shiny::div(
      class = "plot-header",
      shiny::div(
        class = "plot-title-row",
        shiny::h3("Stability & Variability", class = "plot-title"),
        shiny::div(
          class = "plot-controls-inline",
          shiny::downloadButton(ns("dl_plot"), "", icon = shiny::icon("download"),
                                class = "btn-icon", title = "Download plot"),
          shiny::downloadButton(ns("dl_data"), "", icon = shiny::icon("table"),
                                class = "btn-icon", title = "Download data")
        )
      ),
      shiny::div(class = "plot-subtitle", shiny::textOutput(ns("plot_subtitle"), inline = TRUE))
    ),
    shiny::div(
      class = "plot-body",
      plotly::plotlyOutput(ns("plot"), height = "460px") |>
        shinycssloaders::withSpinner(type = 6, color = "#2E86AB", size = 0.6)
    ),
    shiny::div(class = "plot-footer", shiny::uiOutput(ns("no_data_msg")))
  )
}

meanVarServer <- function(id, filtered_data, state, config, habitat_lookup = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    plot_data <- shiny::reactive({
      shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)
      
      if (!is.null(state$filter_mode) && state$filter_mode == "habitat") {
        pd <- query_habitat_aggregated(
          config,
          year_min       = state$year_min,
          year_max       = state$year_max,
          seasons        = state$selected_seasons,
          species        = state$selected_species,
          habitat_lookup = habitat_lookup,
          habitat_select = state$habitat_select,
          grpname_select = state$grpname_select
        )
      } else {
        pd <- query_aggregated(
          config,
          year_min   = state$year_min,
          year_max   = state$year_max,
          seasons    = state$selected_seasons,
          species    = state$selected_species,
          agg_method = state$aggregation
        )
      }
      
      shiny::req(nrow(pd) > 0)
      
      pd |>
        dplyr::group_by(taxon_display) |>
        dplyr::summarise(
          Mean_Abundance = mean(abundance, na.rm = TRUE),
          Variance       = var(abundance, na.rm = TRUE),
          .groups        = "drop"
        ) |>
        dplyr::filter(is.finite(Mean_Abundance), is.finite(Variance))
    })
    
    output$plot_subtitle <- shiny::renderText({
      pd <- plot_data()
      yr <- c(state$year_min, state$year_max)
      label <- if (!is.null(state$filter_mode) && state$filter_mode == "habitat")
        "habitat groups" else "species"
      glue::glue("{nrow(pd)} {label} · {yr[1]}–{yr[2]} · Mean vs Variance of Total Abundance")
    })
    
    output$no_data_msg <- shiny::renderUI({
      pd <- tryCatch(plot_data(), error = function(e) NULL)
      if (is.null(pd) || nrow(pd) == 0)
        shiny::div(class = "no-data-msg", shiny::icon("circle-exclamation"),
                   " No data matches the current filters. Try broadening your selection.")
    })
    
    output$plot <- plotly::renderPlotly({
      pd <- plot_data(); shiny::req(nrow(pd) > 0)
      pal <- make_species_palette(sort(unique(pd$taxon_display)),
                                  config$plots$abundance_time$color_palette)
      p <- ggplot2::ggplot(pd, ggplot2::aes(
        x = Mean_Abundance, y = Variance, color = taxon_display,
        text = paste0("<b>", taxon_display, "</b><br>",
                      "Mean Abundance: ", formatC(Mean_Abundance, format = "f", digits = 2, big.mark = ","), "<br>",
                      "Variance: ", formatC(Variance, format = "f", digits = 2, big.mark = ","))
      )) +
        ggplot2::geom_point(size = 4, alpha = 0.88) +
        ggplot2::scale_color_manual(values = pal,
                                    name = if (!is.null(state$filter_mode) && state$filter_mode == "habitat") "Habitat Group" else "Species") +
        ggplot2::scale_x_continuous(labels = scales::label_comma()) +
        ggplot2::scale_y_continuous(labels = scales::label_comma()) +
        ggplot2::labs(x = "Mean Abundance", y = "Variance") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          axis.title       = ggplot2::element_text(size = 11, color = "#555"),
          legend.title     = ggplot2::element_text(size = 11, face = "bold"),
          legend.text      = ggplot2::element_text(size = 10),
          plot.background  = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
        )
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(orientation = "v", x = 1.02, y = 0.98,
                        bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ddd", borderwidth = 1),
          margin = list(l = 60, r = 160, t = 20, b = 60),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(format = "png", filename = "mean_vs_variance",
                                      width = 1200, height = 600)
        )
    })
    
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("mean_vs_variance_", Sys.Date(), ".png"),
      content  = function(file) {
        pd <- plot_data(); shiny::req(nrow(pd) > 0)
        pal <- make_species_palette(sort(unique(pd$taxon_display)),
                                    config$plots$abundance_time$color_palette)
        p <- ggplot2::ggplot(pd, ggplot2::aes(x = Mean_Abundance, y = Variance,
                                              color = taxon_display)) +
          ggplot2::geom_point(size = 4, alpha = 0.88) +
          ggplot2::scale_color_manual(values = pal, name = "Group") +
          ggplot2::scale_x_continuous(labels = scales::label_comma()) +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(title = "Stability & Variability", x = "Mean Abundance", y = "Variance") +
          ggplot2::theme_minimal(base_size = 13)
        ggplot2::ggsave(file, p, width = 12, height = 6, dpi = 150, bg = "white")
      }
    )
    
    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("mean_variance_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(plot_data(), file, row.names = FALSE)
    )
  })
}

# ── Plot 4: Abundance Bar Chart ───────────────────────────────────────────────

abundanceBarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plot-card",
    shiny::div(
      class = "plot-header",
      shiny::div(
        class = "plot-title-row",
        shiny::h3("Total Abundance by Species", class = "plot-title"),
        shiny::div(
          class = "plot-controls-inline",
          shiny::downloadButton(ns("dl_plot"), "", icon = shiny::icon("download"),
                                class = "btn-icon", title = "Download plot"),
          shiny::downloadButton(ns("dl_data"), "", icon = shiny::icon("table"),
                                class = "btn-icon", title = "Download data")
        )
      ),
      shiny::div(class = "plot-subtitle", shiny::textOutput(ns("plot_subtitle"), inline = TRUE))
    ),
    shiny::div(
      class = "plot-body",
      plotly::plotlyOutput(ns("plot"), height = "460px") |>
        shinycssloaders::withSpinner(type = 6, color = "#2E86AB", size = 0.6)
    ),
    shiny::div(class = "plot-footer", shiny::uiOutput(ns("no_data_msg")))
  )
}

abundanceBarServer <- function(id, filtered_data, state, config, habitat_lookup = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    plot_data <- shiny::reactive({
      shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)
      
      if (!is.null(state$filter_mode) && state$filter_mode == "habitat") {
        pd <- query_habitat_aggregated(
          config,
          year_min       = state$year_min,
          year_max       = state$year_max,
          seasons        = state$selected_seasons,
          species        = state$selected_species,
          habitat_lookup = habitat_lookup,
          habitat_select = state$habitat_select,
          grpname_select = state$grpname_select
        )
        # Sum across all years per group
        pd |>
          dplyr::group_by(taxon_display) |>
          dplyr::summarise(total_abundance = sum(abundance, na.rm = TRUE), .groups = "drop") |>
          dplyr::arrange(dplyr::desc(total_abundance))
      } else {
        pd <- query_aggregated(
          config,
          year_min   = state$year_min,
          year_max   = state$year_max,
          seasons    = state$selected_seasons,
          species    = state$selected_species,
          agg_method = "sum"
        )
        pd |>
          dplyr::group_by(taxon, taxon_display) |>
          dplyr::summarise(total_abundance = sum(abundance, na.rm = TRUE), .groups = "drop") |>
          dplyr::arrange(dplyr::desc(total_abundance))
      }
    })
    
    output$plot_subtitle <- shiny::renderText({
      pd <- plot_data()
      yr <- c(state$year_min, state$year_max)
      label <- if (!is.null(state$filter_mode) && state$filter_mode == "habitat")
        "habitat groups" else "species"
      glue::glue("{nrow(pd)} {label} · {yr[1]}–{yr[2]} · Total Abundance")
    })
    
    output$no_data_msg <- shiny::renderUI({
      pd <- plot_data()
      if (is.null(pd) || nrow(pd) == 0)
        shiny::div(class = "no-data-msg", shiny::icon("circle-exclamation"),
                   " No data matches the current filters. Try broadening your selection.")
    })
    
    output$plot <- plotly::renderPlotly({
      pd <- plot_data(); shiny::req(nrow(pd) > 0)
      pd$taxon_display <- factor(pd$taxon_display, levels = pd$taxon_display)
      pal <- make_species_palette(as.character(pd$taxon_display),
                                  config$plots$abundance_time$color_palette)
      p <- ggplot2::ggplot(pd, ggplot2::aes(
        x = taxon_display, y = total_abundance, fill = taxon_display,
        text = paste0("<b>", taxon_display, "</b><br>Total Abundance: ",
                      formatC(total_abundance, format = "f", digits = 1, big.mark = ","))
      )) +
        ggplot2::geom_col(alpha = 0.88, width = 0.7) +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                    expand = ggplot2::expansion(mult = c(0, 0.08))) +
        ggplot2::labs(x = if (!is.null(state$filter_mode) && state$filter_mode == "habitat")
          "Habitat Group" else "Species",
          y = "Total Abundance") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
          axis.title         = ggplot2::element_text(size = 11, color = "#555"),
          legend.position    = "none",
          plot.background    = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background   = ggplot2::element_rect(fill = "transparent", color = NA)
        )
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          margin = list(l = 60, r = 40, t = 20, b = 160),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(format = "png", filename = "abundance_bar",
                                      width = 1200, height = 600)
        )
    })
    
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("abundance_bar_", Sys.Date(), ".png"),
      content  = function(file) {
        pd <- plot_data(); shiny::req(nrow(pd) > 0)
        pal <- make_species_palette(as.character(pd$taxon_display),
                                    config$plots$abundance_time$color_palette)
        p <- ggplot2::ggplot(pd, ggplot2::aes(x = taxon_display, y = total_abundance,
                                              fill = taxon_display)) +
          ggplot2::geom_col(alpha = 0.88, width = 0.7) +
          ggplot2::scale_fill_manual(values = pal) +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(title = "Total Abundance", x = "", y = "Total Abundance") +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                         legend.position = "none")
        ggplot2::ggsave(file, p, width = 12, height = 6, dpi = 150, bg = "white")
      }
    )
    
    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("abundance_bar_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(plot_data(), file, row.names = FALSE)
    )
  })
}

# ── Plot 5: Temporal Trends by Season ────────────────────────────────────────

temporalTrendsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plot-card",
    shiny::div(
      class = "plot-header",
      shiny::div(
        class = "plot-title-row",
        shiny::h3("Temporal Trends by Season", class = "plot-title"),
        shiny::div(
          class = "plot-controls-inline",
          shiny::downloadButton(ns("dl_plot"), "", icon = shiny::icon("download"),
                                class = "btn-icon", title = "Download plot"),
          shiny::downloadButton(ns("dl_data"), "", icon = shiny::icon("table"),
                                class = "btn-icon", title = "Download data")
        )
      ),
      shiny::div(
        class = "plot-controls-inline",
        shiny::radioButtons(ns("metric"), label = "Show:",
                            choices  = c("Mean" = "mean", "Median" = "median"),
                            selected = "mean", inline = TRUE)
      ),
      shiny::div(class = "plot-subtitle", shiny::textOutput(ns("plot_subtitle"), inline = TRUE))
    ),
    shiny::div(
      class = "plot-body",
      plotly::plotlyOutput(ns("plot"), height = "460px") |>
        shinycssloaders::withSpinner(type = 6, color = "#2E86AB", size = 0.6)
    ),
    shiny::div(class = "plot-footer", shiny::uiOutput(ns("no_data_msg")))
  )
}

temporalTrendsServer <- function(id, state, config) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_data <- shiny::reactive({
      shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)
      query_temporal_trends(
        config,
        year_min = state$year_min,
        year_max = state$year_max,
        seasons  = state$selected_seasons,
        species  = state$selected_species
      )
    })

    output$plot_subtitle <- shiny::renderText({
      yr <- c(state$year_min, state$year_max)
      glue::glue("{yr[1]}–{yr[2]} · Total abundance across selected species, broken down by season")
    })

    output$no_data_msg <- shiny::renderUI({
      pd <- tryCatch(plot_data(), error = function(e) NULL)
      if (is.null(pd) || nrow(pd) == 0)
        shiny::div(class = "no-data-msg", shiny::icon("circle-exclamation"),
                   " No data matches the current filters.")
    })

    output$plot <- plotly::renderPlotly({
      pd <- plot_data(); shiny::req(nrow(pd) > 0)

      y_col   <- if (input$metric == "mean") "mean_abundance" else "median_abundance"
      y_label <- if (input$metric == "mean") "Mean Total Abundance" else "Median Total Abundance"

      pd$y_val        <- pd[[y_col]]
      pd$season_label <- tools::toTitleCase(as.character(pd$season))

      season_colors <- c(Spring = "#4DAF4A", Summer = "#FF7F00",
                         Fall   = "#E41A1C", Winter = "#377EB8")

      p <- ggplot2::ggplot(pd, ggplot2::aes(
        x     = year,
        y     = y_val,
        color = season_label,
        group = season_label,
        text  = paste0(
          "<b>", season_label, "</b><br>",
          "Year: ", year, "<br>",
          y_label, ": ", formatC(y_val, format = "f", digits = 1, big.mark = ",")
        )
      )) +
        ggplot2::geom_line(linewidth = 0.9, alpha = 0.85) +
        ggplot2::geom_point(size = 1.8, alpha = 0.9) +
        ggplot2::scale_color_manual(values = season_colors, name = "Season") +
        ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                    expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        ggplot2::labs(x = "Year", y = y_label) +
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

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(orientation = "v", x = 1.02, y = 0.98,
                        bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ddd", borderwidth = 1),
          margin = list(l = 60, r = 160, t = 20, b = 60),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(format = "png", filename = "temporal_trends",
                                      width = 1200, height = 600)
        )
    })

    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("temporal_trends_", Sys.Date(), ".png"),
      content  = function(file) {
        pd <- plot_data(); shiny::req(nrow(pd) > 0)
        y_col   <- if (input$metric == "mean") "mean_abundance" else "median_abundance"
        y_label <- if (input$metric == "mean") "Mean Total Abundance" else "Median Total Abundance"
        pd$y_val        <- pd[[y_col]]
        pd$season_label <- tools::toTitleCase(as.character(pd$season))
        season_colors   <- c(Spring = "#4DAF4A", Summer = "#FF7F00", Fall = "#E41A1C", Winter = "#377EB8")
        p <- ggplot2::ggplot(pd, ggplot2::aes(x = year, y = y_val,
                                               color = season_label, group = season_label)) +
          ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
          ggplot2::scale_color_manual(values = season_colors, name = "Season") +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(title = paste("Temporal Trends —", y_label), x = "Year", y = y_label) +
          ggplot2::theme_minimal(base_size = 14)
        ggplot2::ggsave(file, p, width = 12, height = 6, dpi = 150, bg = "white")
      }
    )

    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("temporal_trends_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(plot_data(), file, row.names = FALSE)
    )
  })
}

# ── Plot 6: Time Series by Historical Period ──────────────────────────────────

timeSeriesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plot-card",
    shiny::div(
      class = "plot-header",
      shiny::div(
        class = "plot-title-row",
        shiny::h3("Time Series by Period", class = "plot-title"),
        shiny::div(
          class = "plot-controls-inline",
          shiny::downloadButton(ns("dl_plot"), "", icon = shiny::icon("download"),
                                class = "btn-icon", title = "Download plot"),
          shiny::downloadButton(ns("dl_data"), "", icon = shiny::icon("table"),
                                class = "btn-icon", title = "Download data")
        )
      ),
      shiny::div(class = "plot-subtitle", shiny::textOutput(ns("plot_subtitle"), inline = TRUE))
    ),
    shiny::div(
      class = "plot-body",
      plotly::plotlyOutput(ns("plot"), height = "460px") |>
        shinycssloaders::withSpinner(type = 6, color = "#2E86AB", size = 0.6)
    ),
    shiny::div(class = "plot-footer", shiny::uiOutput(ns("no_data_msg")))
  )
}

timeSeriesServer <- function(id, state, config) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_data <- shiny::reactive({
      shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)

      pd <- query_aggregated(
        config,
        year_min   = state$year_min,
        year_max   = state$year_max,
        seasons    = state$selected_seasons,
        species    = state$selected_species,
        agg_method = state$aggregation
      )
      shiny::req(nrow(pd) > 0)

      pd |>
        dplyr::mutate(period = dplyr::case_when(
          year >= 1951 & year <= 1976 ~ "1951–1976",
          year >  1976 & year <= 1998 ~ "1977–1998",
          year >  1998 & year <= 2014 ~ "1999–2014",
          year >  2014               ~ "2015–present",
          TRUE ~ NA_character_
        )) |>
        dplyr::filter(!is.na(period), period %in% state$selected_periods)
    })

    output$plot_subtitle <- shiny::renderText({
      pd <- plot_data()
      sp <- length(unique(pd$taxon_display))
      glue::glue("{sp} species · {state$year_min}–{state$year_max} · {agg_label(state$aggregation)} faceted by period")
    })

    output$no_data_msg <- shiny::renderUI({
      pd <- tryCatch(plot_data(), error = function(e) NULL)
      if (is.null(pd) || nrow(pd) == 0)
        shiny::div(class = "no-data-msg", shiny::icon("circle-exclamation"),
                   " No data matches the current filters.")
    })

    output$plot <- plotly::renderPlotly({
      pd <- plot_data(); shiny::req(nrow(pd) > 0)

      period_order <- c("1951–1976", "1977–1998", "1999–2014", "2015–present")
      pd$period    <- factor(pd$period, levels = period_order)

      pal         <- make_species_palette(sort(unique(pd$taxon)), config$plots$abundance_time$color_palette)
      pal_display <- stats::setNames(pal, sort(unique(pd$taxon_display)))

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
        ggplot2::facet_wrap(~ period, scales = "free_x") +
        ggplot2::scale_color_manual(values = pal_display, name = "Species") +
        ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                    expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
        ggplot2::labs(x = "Year", y = agg_label(state$aggregation)) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          strip.text         = ggplot2::element_text(size = 11, face = "bold"),
          axis.title         = ggplot2::element_text(size = 11, color = "#555"),
          legend.title       = ggplot2::element_text(size = 11, face = "bold"),
          legend.text        = ggplot2::element_text(size = 10),
          plot.background    = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background   = ggplot2::element_rect(fill = "transparent", color = NA)
        )

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(orientation = "v", x = 1.02, y = 0.98,
                        bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ddd", borderwidth = 1),
          margin = list(l = 60, r = 160, t = 20, b = 60),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(format = "png", filename = "time_series_period",
                                      width = 1400, height = 700)
        )
    })

    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("time_series_period_", Sys.Date(), ".png"),
      content  = function(file) {
        pd <- plot_data(); shiny::req(nrow(pd) > 0)
        pal         <- make_species_palette(sort(unique(pd$taxon)), config$plots$abundance_time$color_palette)
        pal_display <- stats::setNames(pal, sort(unique(pd$taxon_display)))
        pd$period   <- factor(pd$period, levels = c("1951–1976", "1977–1998",
                                                      "1999–2014", "2015–present"))
        p <- ggplot2::ggplot(pd, ggplot2::aes(x = year, y = abundance,
                                               color = taxon_display, group = taxon_display)) +
          ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
          ggplot2::facet_wrap(~ period, scales = "free_x") +
          ggplot2::scale_color_manual(values = pal_display, name = "Species") +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(title = "Time Series by Period", x = "Year", y = agg_label(state$aggregation)) +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
        ggplot2::ggsave(file, p, width = 14, height = 7, dpi = 150, bg = "white")
      }
    )

    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("time_series_period_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(plot_data(), file, row.names = FALSE)
    )
  })
}

# ── Plot 7: Habitat Time Series by Period ─────────────────────────────────────

habitatTimeSeriesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plot-card",
    shiny::div(
      class = "plot-header",
      shiny::div(
        class = "plot-title-row",
        shiny::h3("Habitat Time Series by Period", class = "plot-title"),
        shiny::div(
          class = "plot-controls-inline",
          shiny::downloadButton(ns("dl_plot"), "", icon = shiny::icon("download"),
                                class = "btn-icon", title = "Download plot"),
          shiny::downloadButton(ns("dl_data"), "", icon = shiny::icon("table"),
                                class = "btn-icon", title = "Download data")
        )
      ),
      shiny::div(class = "plot-subtitle", shiny::textOutput(ns("plot_subtitle"), inline = TRUE))
    ),
    shiny::div(
      class = "plot-body",
      plotly::plotlyOutput(ns("plot"), height = "460px") |>
        shinycssloaders::withSpinner(type = 6, color = "#2E86AB", size = 0.6)
    ),
    shiny::div(class = "plot-footer", shiny::uiOutput(ns("no_data_msg")))
  )
}

habitatTimeSeriesServer <- function(id, state, config, habitat_lookup) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_data <- shiny::reactive({
      shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)

      pd <- query_aggregated(
        config,
        year_min   = state$year_min,
        year_max   = state$year_max,
        seasons    = state$selected_seasons,
        species    = state$selected_species,
        agg_method = "mean"
      )
      shiny::req(nrow(pd) > 0)

      pd |>
        dplyr::left_join(
          habitat_lookup |> dplyr::select(species_clean, habitat),
          by = c("taxon" = "species_clean")
        ) |>
        dplyr::filter(!is.na(habitat)) |>
        dplyr::group_by(year, habitat) |>
        dplyr::summarise(mean_abundance = mean(abundance, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
          habitat = tools::toTitleCase(trimws(habitat)),
          period  = dplyr::case_when(
            year >= 1951 & year <= 1976 ~ "1951–1976",
            year >  1976 & year <= 1998 ~ "1977–1998",
            year >  1998 & year <= 2014 ~ "1999–2014",
            year >  2014               ~ "2015–present",
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::filter(!is.na(period), period %in% state$selected_periods)
    })

    output$plot_subtitle <- shiny::renderText({
      pd <- plot_data()
      hab <- length(unique(pd$habitat))
      glue::glue("{hab} habitat types · {state$year_min}–{state$year_max} · Mean abundance by habitat and period")
    })

    output$no_data_msg <- shiny::renderUI({
      pd <- tryCatch(plot_data(), error = function(e) NULL)
      if (is.null(pd) || nrow(pd) == 0)
        shiny::div(class = "no-data-msg", shiny::icon("circle-exclamation"),
                   " No habitat data available. Check that selected species have habitat assignments.")
    })

    output$plot <- plotly::renderPlotly({
      pd <- plot_data(); shiny::req(nrow(pd) > 0)

      period_order <- c("1951–1976", "1977–1998", "1999–2014", "2015–present")
      pd$period    <- factor(pd$period, levels = period_order)

      habitat_colors <- c(Pelagic = "#2E86AB", Benthic = "#E84855",
                          "Coastal-Oceanic" = "#F9A825", Other = "#888888")

      p <- ggplot2::ggplot(pd, ggplot2::aes(
        x     = year,
        y     = mean_abundance,
        color = habitat,
        group = habitat,
        text  = paste0(
          "<b>", habitat, "</b><br>",
          "Year: ", year, "<br>",
          "Mean Abundance: ", formatC(mean_abundance, format = "f", digits = 1, big.mark = ",")
        )
      )) +
        ggplot2::geom_line(linewidth = 0.9, alpha = 0.85) +
        ggplot2::geom_point(size = 1.8, alpha = 0.9) +
        ggplot2::facet_wrap(~ period, scales = "free_x") +
        ggplot2::scale_color_manual(values = habitat_colors, name = "Habitat") +
        ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                    expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
        ggplot2::labs(x = "Year", y = "Mean Abundance") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          strip.text         = ggplot2::element_text(size = 11, face = "bold"),
          axis.title         = ggplot2::element_text(size = 11, color = "#555"),
          legend.title       = ggplot2::element_text(size = 11, face = "bold"),
          legend.text        = ggplot2::element_text(size = 10),
          plot.background    = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.background   = ggplot2::element_rect(fill = "transparent", color = NA)
        )

      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          legend = list(orientation = "v", x = 1.02, y = 0.98,
                        bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ddd", borderwidth = 1),
          margin = list(l = 60, r = 160, t = 20, b = 60),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(format = "png", filename = "habitat_time_series",
                                      width = 1400, height = 700)
        )
    })

    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("habitat_time_series_", Sys.Date(), ".png"),
      content  = function(file) {
        pd <- plot_data(); shiny::req(nrow(pd) > 0)
        pd$period <- factor(pd$period, levels = c("1951–1976", "1977–1998",
                                                    "1999–2014", "2015–present"))
        habitat_colors <- c(Pelagic = "#2E86AB", Benthic = "#E84855",
                            "Coastal-Oceanic" = "#F9A825", Other = "#888888")
        p <- ggplot2::ggplot(pd, ggplot2::aes(x = year, y = mean_abundance,
                                               color = habitat, group = habitat)) +
          ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2) +
          ggplot2::facet_wrap(~ period, scales = "free_x") +
          ggplot2::scale_color_manual(values = habitat_colors, name = "Habitat") +
          ggplot2::scale_y_continuous(labels = scales::label_comma()) +
          ggplot2::labs(title = "Habitat Time Series by Period", x = "Year", y = "Mean Abundance") +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
        ggplot2::ggsave(file, p, width = 14, height = 7, dpi = 150, bg = "white")
      }
    )

    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("habitat_time_series_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(plot_data(), file, row.names = FALSE)
    )
  })
}

# ── Plot 8: Spatial Distribution Map ─────────────────────────────────────────

spatialMapUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plot-card",
    shiny::div(
      class = "plot-header",
      shiny::div(
        class = "plot-title-row",
        shiny::h3("Spatial Distribution", class = "plot-title"),
        shiny::div(
          class = "plot-controls-inline",
          shiny::downloadButton(ns("dl_data"), "", icon = shiny::icon("table"),
                                class = "btn-icon", title = "Download station data")
        )
      ),
      shiny::div(
        class = "plot-controls-inline",
        shiny::radioButtons(ns("map_mode"), label = "Color by:",
                            choices  = c("CalCOFI Line" = "line", "Dominant Species" = "species"),
                            selected = "line", inline = TRUE)
      ),
      shiny::div(class = "plot-subtitle", shiny::textOutput(ns("plot_subtitle"), inline = TRUE))
    ),
    shiny::div(
      class = "plot-body",
      leaflet::leafletOutput(ns("map"), height = "460px") |>
        shinycssloaders::withSpinner(type = 6, color = "#2E86AB", size = 0.6)
    ),
    shiny::div(class = "plot-footer", shiny::uiOutput(ns("no_data_msg")))
  )
}

spatialMapServer <- function(id, state, config) {
  shiny::moduleServer(id, function(input, output, session) {

    raw_data <- shiny::reactive({
      shiny::req(length(state$selected_species) > 0, length(state$selected_seasons) > 0)
      query_spatial(
        config,
        year_min = state$year_min,
        year_max = state$year_max,
        seasons  = state$selected_seasons,
        species  = state$selected_species
      )
    })

    station_data <- shiny::reactive({
      pd <- raw_data(); shiny::req(nrow(pd) > 0)
      pd |>
        dplyr::group_by(latitude, longitude, s_l) |>
        dplyr::summarise(
          total_abundance  = sum(total_abundance, na.rm = TRUE),
          dominant_species = taxon[which.max(total_abundance)],
          .groups          = "drop"
        ) |>
        dplyr::mutate(
          size_scaled      = 4 + (total_abundance / max(total_abundance, na.rm = TRUE)) * 12,
          dominant_display = pretty_taxon(dominant_species)
        )
    })

    output$plot_subtitle <- shiny::renderText({
      pd <- station_data()
      glue::glue("{nrow(pd)} stations · {state$year_min}–{state$year_max}")
    })

    output$no_data_msg <- shiny::renderUI({
      pd <- tryCatch(station_data(), error = function(e) NULL)
      if (is.null(pd) || nrow(pd) == 0)
        shiny::div(class = "no-data-msg", shiny::icon("circle-exclamation"),
                   " No spatial data found. The database may not contain lat/lon for these records.")
    })

    output$map <- leaflet::renderLeaflet({
      pd <- station_data(); shiny::req(nrow(pd) > 0)

      base <- leaflet::leaflet(pd) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

      if (input$map_mode == "line") {
        pal <- leaflet::colorNumeric("viridis", domain = pd$s_l, na.color = "#808080")
        base |>
          leaflet::addCircleMarkers(
            lng         = ~longitude,
            lat         = ~latitude,
            radius      = ~size_scaled,
            fillColor   = ~pal(s_l),
            color       = ~pal(s_l),
            fillOpacity = 0.75,
            stroke      = FALSE,
            popup       = ~paste0(
              "<b>CalCOFI Line:</b> ", round(s_l, 1), "<br>",
              "<b>Total Abundance:</b> ",
              formatC(total_abundance, format = "f", digits = 0, big.mark = ","), "<br>",
              "<b>Dominant Species:</b> ", dominant_display
            )
          ) |>
          leaflet::addLegend(position = "bottomright", pal = pal, values = ~s_l,
                             title = "CalCOFI Line", opacity = 0.8)

      } else {
        top_sp <- names(sort(table(pd$dominant_display), decreasing = TRUE))[
          1:min(8, length(unique(pd$dominant_display)))
        ]
        pd <- pd |>
          dplyr::mutate(dom_grp = ifelse(dominant_display %in% top_sp,
                                         dominant_display, "Other"))
        all_grps <- sort(unique(pd$dom_grp))
        n_grps   <- length(all_grps)
        pal      <- leaflet::colorFactor(
          palette = if (n_grps <= 8) RColorBrewer::brewer.pal(max(3, n_grps), "Set2")[seq_len(n_grps)]
                    else grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_grps),
          domain  = all_grps
        )
        base |>
          leaflet::addCircleMarkers(
            data        = pd,
            lng         = ~longitude,
            lat         = ~latitude,
            radius      = ~size_scaled,
            fillColor   = ~pal(dom_grp),
            color       = ~pal(dom_grp),
            fillOpacity = 0.75,
            stroke      = FALSE,
            popup       = ~paste0(
              "<b>Dominant Species:</b> ", dom_grp, "<br>",
              "<b>CalCOFI Line:</b> ", round(s_l, 1), "<br>",
              "<b>Total Abundance:</b> ",
              formatC(total_abundance, format = "f", digits = 0, big.mark = ",")
            )
          ) |>
          leaflet::addLegend(position = "bottomright", pal = pal, values = pd$dom_grp,
                             title = "Dominant Species", opacity = 0.8)
      }
    })

    output$dl_data <- shiny::downloadHandler(
      filename = function() paste0("spatial_data_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(station_data(), file, row.names = FALSE)
    )
  })
}