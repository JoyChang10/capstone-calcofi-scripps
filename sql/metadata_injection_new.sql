-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

COMMENT ON TABLE ichthyoplankton_observations IS
'{
  "name_long": "CalCOFI ichthyoplankton observations",
  "description_md": "Long-format ichthyoplankton observation table derived from CalCOFI survey data. Each row represents one taxon observed for one sampling event.",
  "primary_key": ["unique_code", "taxon"],
  "visualization_hint": "tabular"
}';

COMMENT ON COLUMN ichthyoplankton_observations.unique_code IS
'{
  "name_long": "unique code",
  "units": "identifier",
  "description_md": "Sampling event identifier. Together with taxon, this forms the composite primary key."
}';

COMMENT ON COLUMN ichthyoplankton_observations.s_c IS
'{
  "name_long": "cruise code",
  "units": "yymm",
  "description_md": "Cruise identifier encoding the survey year and month."
}';

COMMENT ON COLUMN ichthyoplankton_observations.s_sc IS
'{
  "name_long": "ship code",
  "units": "text",
  "description_md": "Ship code associated with the survey or processing record."
}';

COMMENT ON COLUMN ichthyoplankton_observations.s_l IS
'{
  "name_long": "CalCOFI line",
  "units": "decimal index",
  "description_md": "CalCOFI sampling line, representing an offshore transect."
}';

COMMENT ON COLUMN ichthyoplankton_observations.s_s IS
'{
  "name_long": "CalCOFI station",
  "units": "decimal index",
  "description_md": "CalCOFI station along a line, with lower station values generally closer to shore."
}';

COMMENT ON COLUMN ichthyoplankton_observations.latitude IS
'{
  "name_long": "latitude",
  "units": "decimal degrees",
  "description_md": "Latitude of the sampling location in WGS84."
}';

COMMENT ON COLUMN ichthyoplankton_observations.longitude IS
'{
  "name_long": "longitude",
  "units": "decimal degrees",
  "description_md": "Longitude of the sampling location in WGS84."
}';

COMMENT ON COLUMN ichthyoplankton_observations.year IS
'{
  "name_long": "year",
  "units": "yyyy",
  "description_md": "Calendar year in which the sample was collected."
}';

COMMENT ON COLUMN ichthyoplankton_observations.season IS
'{
  "name_long": "season",
  "units": "text",
  "description_md": "Season assigned to the cruise or sampling event."
}';

COMMENT ON COLUMN ichthyoplankton_observations.taxon IS
'{
  "name_long": "taxon name",
  "units": "scientific name",
  "description_md": "Scientific name of the observed organism in standardized long format."
}';

COMMENT ON COLUMN ichthyoplankton_observations.abundance IS
'{
  "name_long": "abundance",
  "units": "standardized density",
  "description_md": "Standardized abundance value computed as (raw count * standard haul factor) / proportion sorted."
}';

COMMENT ON COLUMN ichthyoplankton_observations.worms_id IS
'{
  "name_long": "WoRMS AphiaID",
  "units": "identifier",
  "description_md": "Unique taxonomic identifier from the World Register of Marine Species."
}';

-- =========================
-- Bottle data variables
-- =========================

COMMENT ON COLUMN ichthyoplankton_observations.depth_m IS
'{
  "name_long": "sampling depth",
  "units": "meters",
  "description_md": "Depth at which the water sample was collected."
}';

COMMENT ON COLUMN ichthyoplankton_observations.t_degc IS
'{
  "name_long": "water temperature",
  "units": "°C",
  "description_md": "Seawater temperature measured from bottle sample."
}';

COMMENT ON COLUMN ichthyoplankton_observations.salnty IS
'{
  "name_long": "salinity",
  "units": "Practical Salinity Units",
  "description_md": "Seawater salinity measured using the Practical Salinity Scale (PSS-78)."
}';

COMMENT ON COLUMN ichthyoplankton_observations.o2ml_l IS
'{
  "name_long": "dissolved oxygen",
  "units": "mL/L",
  "description_md": "Concentration of dissolved oxygen in seawater."
}';

COMMENT ON COLUMN ichthyoplankton_observations.s_theta IS
'{
  "name_long": "potential density",
  "units": "kg/m³",
  "description_md": "Potential density anomaly (sigma-theta) derived from temperature and salinity."
}';

COMMENT ON COLUMN ichthyoplankton_observations.o2sat IS
'{
  "name_long": "oxygen saturation",
  "units": "percent",
  "description_md": "Percent saturation of dissolved oxygen relative to equilibrium concentration."
}';

COMMENT ON COLUMN ichthyoplankton_observations.oxy_umol_kg IS
'{
  "name_long": "dissolved oxygen concentration",
  "units": "µmol/kg",
  "description_md": "Dissolved oxygen concentration expressed in micromoles per kilogram."
}';

COMMENT ON COLUMN ichthyoplankton_observations.chlor_a IS
'{
  "name_long": "chlorophyll-a concentration",
  "units": "µg/L",
  "description_md": "Chlorophyll-a concentration used as a proxy for phytoplankton biomass."
}';

COMMENT ON COLUMN ichthyoplankton_observations.phaeop IS
'{
  "name_long": "phaeopigment concentration",
  "units": "µg/L",
  "description_md": "Concentration of phaeopigments derived from chlorophyll degradation products."
}';

-- =========================
-- Nutrient concentrations
-- =========================

COMMENT ON COLUMN ichthyoplankton_observations.po4um IS
'{
  "name_long": "phosphate concentration",
  "units": "µmol/L",
  "description_md": "Dissolved phosphate concentration in seawater."
}';

COMMENT ON COLUMN ichthyoplankton_observations.sio3um IS
'{
  "name_long": "silicate concentration",
  "units": "µmol/L",
  "description_md": "Dissolved silicate concentration in seawater."
}';

COMMENT ON COLUMN ichthyoplankton_observations.no2um IS
'{
  "name_long": "nitrite concentration",
  "units": "µmol/L",
  "description_md": "Dissolved nitrite concentration in seawater."
}';

COMMENT ON COLUMN ichthyoplankton_observations.no3um IS
'{
  "name_long": "nitrate concentration",
  "units": "µmol/L",
  "description_md": "Dissolved nitrate concentration in seawater."
}';

COMMENT ON COLUMN ichthyoplankton_observations.nh3um IS
'{
  "name_long": "ammonia concentration",
  "units": "µmol/L",
  "description_md": "Dissolved ammonia concentration in seawater."
}';

-- =========================
-- Location / station info
-- =========================

COMMENT ON COLUMN ichthyoplankton_observations.sta_id IS
'{
  "name_long": "station identifier",
  "units": "text",
  "description_md": "Identifier for the CalCOFI sampling station."
}';

COMMENT ON COLUMN ichthyoplankton_observations.distance IS
'{
  "name_long": "distance from coast",
  "units": "nautical miles",
  "description_md": "Distance of the sampling station from the coastline."
}';

COMMENT ON COLUMN ichthyoplankton_observations.bottom_d IS
'{
  "name_long": "bottom depth",
  "units": "meters",
  "description_md": "Seafloor depth at the sampling station."
}';