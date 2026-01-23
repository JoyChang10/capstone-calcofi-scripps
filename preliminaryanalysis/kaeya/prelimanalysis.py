import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path


DATA_PATH = Path(__file__).resolve().parent.parent / "preliminarydata.csv"
data = pd.read_csv(DATA_PATH)

print(data.head())
print(data.columns)

subset = data.loc[:, "Argentina.sialis":'Teleostei']
corr_matrix = subset.corr()

#----------------------RANKED CORRELATION----------------------------

corr_ranked = (
    corr_matrix
    .where(~corr_matrix.isna())        # safety
    .stack()                           # matrix → long form
    .reset_index()
)

corr_ranked.columns = ['Var1', 'Var2', 'Correlation']
corr_ranked = corr_ranked[corr_ranked['Var1'] != corr_ranked['Var2']]
corr_ranked['pair'] = corr_ranked.apply(

    lambda row: tuple(sorted([row['Var1'], row['Var2']])),
    axis=1
)
corr_ranked = corr_ranked.drop_duplicates(subset='pair').drop(columns='pair')

corr_ranked = corr_ranked.sort_values(
    by='Correlation',
    key=abs,
    ascending=False
)

print(corr_ranked)


#----------------------FISH ABUNDANCE BY LATITUDE----------------------------

meta_cols = {
    "", "unique.code", "S_C", "S_SC", "S_L", "S_S",
    "longitude", "latitude", "year", "season"
}

species_cols = [
    c for c in data.columns
    if c not in meta_cols and pd.api.types.is_numeric_dtype(data[c])
]

#Total abundance per station 
data["total_abundance"] = data[species_cols].fillna(0).sum(axis=1)

# Sort by latitude 
df_sorted = data.sort_values("latitude")

plt.figure(figsize=(8, 5))
plt.scatter(df_sorted["latitude"], df_sorted["total_abundance"])
plt.xlabel("Latitude")
plt.ylabel("Total Fish Abundance")
plt.title("Total Fish Abundance vs Latitude")
plt.tight_layout()
OUT_PATH = Path(__file__).resolve().parent / "total_abundance_vs_latitude.png"
plt.savefig(OUT_PATH, dpi=300, bbox_inches="tight")
plt.show()


# changes by season
# changes over the time period
# correlation with other animal levels
