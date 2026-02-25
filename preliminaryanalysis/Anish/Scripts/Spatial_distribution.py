import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import geopandas as gpd
import contextily as ctx
from pathlib import Path


data = pd.read_csv(r"C:\ANISH-OLDLENOVO\Anish_School\CalCofi_capstone\capstone-calcofi-scripps\preliminaryanalysis\preliminarydata.csv")

meta_cols = {
    "", "unique.code", "S_C", "S_SC", "S_L", "S_S",
    "longitude", "latitude", "year", "season"
}

species_cols = [
    c for c in data.columns
    if c not in meta_cols
    and not c.startswith("Unnamed")
    and pd.api.types.is_numeric_dtype(data[c])
]


data["total_abundance"] = data[species_cols].fillna(0).sum(axis=1)

data["dominant_species"] = data[species_cols].idxmax(axis=1)
sizes = data["total_abundance"] / data["total_abundance"].max()
sizes = sizes * 200  # scale factor for visibility


gdf = gpd.GeoDataFrame(data, geometry = gpd.points_from_xy(data.longitude, data.latitude), 
                       crs="EPSG:4326")

gdf = gdf.to_crs(epsg =3857)


fig,ax = plt.subplots(figsize=(10,8))

scatter = ax.scatter(
    gdf.geometry.x,
    gdf.geometry.y,
    c=data["S_L"],
    s=sizes,
    cmap="viridis",
    alpha=0.7
)

ctx.add_basemap(ax, source=ctx.providers.CartoDB.Positron)

cbar = plt.colorbar(scatter, ax=ax)
cbar.set_label("CalCOFI Line (North to South)")

ax.set_title("Spatial Distribution of Standardized Fish Abundance")
ax.set_axis_off()

plt.tight_layout()
output_path = Path(__file__).parent / "spatial_distribution_abundance_python.png"
plt.savefig(output_path, dpi=300, bbox_inches="tight")

print(f"Figure saved to {output_path}")

plt.close()


#Below use same type of graph as above but now we identify top 10 dominant species on the map
species_totals = data[species_cols].sum()
top10_species = species_totals.sort_values(ascending=False).head(10).index

data["dominant_species"] = data[species_cols].idxmax(axis=1)

data["dominant_grouped"] = data["dominant_species"].where(
    data["dominant_species"].isin(top10_species),
    other="Other"
)

gdf = gpd.GeoDataFrame(data,geometry=gpd.points_from_xy(data.longitude, data.latitude), crs="EPSG:4326")

gdf = gdf.to_crs(epsg=3857)

sizes = gdf["total_abundance"] / gdf["total_abundance"].max()
sizes = sizes * 200

codes = gdf["dominant_grouped"].astype("category").cat.codes
categories = gdf["dominant_grouped"].astype("category").cat.categories

fig2, ax2 = plt.subplots(figsize=(10,8))

scatter2 = ax2.scatter(
    gdf.geometry.x,
    gdf.geometry.y,
    c=codes,
    s=sizes,
    cmap="tab20",
    alpha=0.8
)

ctx.add_basemap(ax2, source=ctx.providers.CartoDB.Positron)

handles = []
for i, species in enumerate(categories):
    handles.append(
        plt.Line2D([], [], marker='o', linestyle='',
                   color=plt.cm.tab20(i / len(categories)),
                   markersize=6,
                   label=species)
    )

ax2.legend(handles=handles,
           title="Dominant Species (Top 10)",
           bbox_to_anchor=(1.05, 1),
           loc="upper left",
           fontsize=8)

ax2.set_title("Spatial Distribution of Dominant Species")
ax2.set_axis_off()

plt.tight_layout()

output_path2 = Path(__file__).parent / "dominant_species_map_python.png"
plt.savefig(output_path2, dpi=300, bbox_inches="tight")
plt.close()

print(f"Dominant species map saved to {output_path2}")