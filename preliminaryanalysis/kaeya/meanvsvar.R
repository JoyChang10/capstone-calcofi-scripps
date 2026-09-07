suppressPackageStartupMessages({
  library(tidyverse)
  library(plotly)
})

# ---- user inputs ----
DATA_PATH <- "/Users/kaeya/capstonerep/capstone-calcofi-scripps/visualizations/preliminarydata.csv"
START_COL <- "Argentina.sialis"
END_COL   <- "Teleostei"

# ---- read + subset species ----
data <- read_csv(DATA_PATH, show_col_types = FALSE)

species_df <- data %>%
  select(all_of(START_COL):all_of(END_COL))

# ---- compute mean + variance per species ----
stats_df <- tibble(
  Species = names(species_df),
  Mean_Abundance = map_dbl(species_df, ~ mean(.x, na.rm = TRUE)),
  Variance       = map_dbl(species_df, ~ var(.x,  na.rm = TRUE))
) %>%
  filter(is.finite(Mean_Abundance), is.finite(Variance))

stats_df[stats_df$Species == "Merluccius.productus", ]

# interactive plot (just for easier internal analysis)

p <- plot_ly(
  data = stats_df,
  x = ~Mean_Abundance,
  y = ~Variance,
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.8),
  text = ~Species,
  hovertemplate = paste(
    "<b>Species:</b> %{text}<br>",
    "<b>Mean Abundance:</b> %{x:.3f}<br>",
    "<b>Variance:</b> %{y:.3f}<extra></extra>"
  )
)

p <- p %>%
  layout(
    title = "Stability & Variability",
    xaxis = list(title = "Mean Abundance", type = "log"),
    yaxis = list(title = "Variance", type = "log")
  )

p

