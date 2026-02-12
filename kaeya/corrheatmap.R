suppressPackageStartupMessages({
  library(tidyverse)
  library(reshape2)   # for melt()
  library(pheatmap)   # optional clustered heatmap
})

# ---- user inputs ----
DATA_PATH <- "/Users/kaeya/capstonerep/capstone-calcofi-scripps/visualizations/preliminarydata.csv"
START_COL <- "Argentina.sialis"
END_COL   <- "Teleostei"

TOP_N_PAIRS <- 10      # how many top correlated pairs to use
USE_CLUSTERED_HEATMAP <- FALSE  # TRUE -> pheatmap, FALSE -> ggplot heatmap

# ---- read data ----
data <- read_csv(DATA_PATH, show_col_types = FALSE)


if (!START_COL %in% names(data)) stop(paste("START_COL not found:", START_COL))
if (!END_COL %in% names(data))   stop(paste("END_COL not found:", END_COL))

# ---- subset species columns (like Python: data.loc[:, START:END]) ----
species_df <- data %>%
  select(all_of(START_COL):all_of(END_COL))

# ---- correlation matrix ----
corr_mat <- cor(species_df, use = "pairwise.complete.obs")

# ---- ranked correlations 
corr_ranked <- corr_mat %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  ) %>%
  filter(!is.na(Correlation)) %>%
  filter(Var1 != Var2) %>%
  mutate(
    pair = map2_chr(Var1, Var2, ~ paste(sort(c(.x, .y)), collapse = "_"))
  ) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair) %>%
  arrange(desc(abs(Correlation)))

# print top correlations
print(head(corr_ranked, 30))

# ---- pick top N pairs and build reduced matrix ----
top_pairs <- corr_ranked %>% slice_head(n = TOP_N_PAIRS)
top_species <- unique(c(top_pairs$Var1, top_pairs$Var2))

corr_top <- corr_mat[top_species, top_species, drop = FALSE]

# ---- heatmap ----
if (USE_CLUSTERED_HEATMAP) {
  
  pheatmap(
    corr_top,
    color = colorRampPalette(c("blue", "white", "red"))(100),
    breaks = seq(-1, 1, length.out = 101),
    clustering_distance_rows = "correlation",
    clustering_distance_cols = "correlation",
    main = paste0("Top Species Correlation Heatmap (Top ", TOP_N_PAIRS, " pairs)")
  )
  
} else {
  
  corr_long <- reshape2::melt(corr_top)
  
  ggplot(corr_long, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    coord_fixed() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0("Heatmap of Highest-Correlation Species (Top ", TOP_N_PAIRS, " pairs)")
    )
}

