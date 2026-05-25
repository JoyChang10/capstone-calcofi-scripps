library(DBI)
library(duckdb)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# ---- load duckdb ----
DUCKDB_PATH <- "/Users/kaeya/capstonerep/capstone-calcofi-scripps/preliminaryanalysis/kaeya/prototype.duckdb"
TABLE_NAME <- "ichthyoplankton_observations"

con <- dbConnect(duckdb::duckdb(), dbdir = DUCKDB_PATH, read_only = TRUE)

print(dbGetQuery(con, paste("SELECT MIN(year), MAX(year) FROM", TABLE_NAME, "WHERE year <= 1998")))
print(dbGetQuery(con, paste("SELECT MIN(year), MAX(year) FROM", TABLE_NAME)))

df <- dbGetQuery(con, paste("SELECT taxon, abundance FROM", TABLE_NAME, "WHERE year <= 1998")) %>%
  group_by(taxon) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  arrange(desc(total_abundance))

write.csv(df, "~/Desktop/current_data_ordered_by_abundance.csv", row.names = FALSE)
dbDisconnect(con)

View("~/Desktop/current_data_ordered_by_abundance.csv")

# ---- load csv ----
csv <- read_csv("/Users/kaeya/capstonerep/capstone-calcofi-scripps/preliminaryanalysis/kaeya/taxon_data.csv")

# ---- normalize both to match format ----
# csv has e.g. "Engraulis mordax" -> convert to "engraulis_mordax"
csv_taxa <- csv %>%
  mutate(taxon_normalized = tolower(str_replace_all(Taxon, " ", "_"))) %>%
  pull(taxon_normalized)

db_taxa <- unique(tolower(df$taxon))

# ---- find csv species NOT in duckdb ----
missing <- csv %>%
  mutate(taxon_normalized = tolower(str_replace_all(Taxon, " ", "_"))) %>%
  filter(!taxon_normalized %in% db_taxa) %>%
  select(Rank, Taxon, Total) %>%
  arrange(desc(Total))

print(missing)
View(missing)
View(df)

write.csv(missing, "Desktop/missing.csv", row.names = FALSE)

# 301 species in atlas
# 101 in our data
# 238 are not in our data
# 63 overlap

# -----plot graph------

top10 <- csv %>%
  slice_max(Total, n = 10) %>%
  mutate(
    taxon_normalized = tolower(gsub(" ", "_", Taxon)),
    Taxon = factor(Taxon, levels = Taxon[order(Total)])
  ) %>%
  rename(csv_total = Total)

# ---- aggregate duckdb totals (using df already loaded) ----
db_totals <- df %>%
  group_by(taxon) %>%
  summarise(db_total = sum(total_abundance, na.rm = TRUE))

# ---- join ----
combined <- top10 %>%
  left_join(db_totals, by = c("taxon_normalized" = "taxon")) %>%
  pivot_longer(cols = c(csv_total, db_total), names_to = "source", values_to = "total") %>%
  mutate(source = recode(source, csv_total = "Atlas", db_total = "New Data"))

# ---- plot ----
ggplot(combined, aes(x = total, y = Taxon, fill = source)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c("Atlas" = "#0096c7", "New Data" = "#f77f00")) +
  labs(
    title = "Top 10 Species",
    x = "Total Abundance", y = NULL, fill = "Source"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50", size = 10),
    axis.text.y = element_text(face = "italic"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

