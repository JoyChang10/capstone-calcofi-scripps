-- JSON keys for documentation
COMMENT ON COLUMN ichthyoplankton_observations.unique_code IS 
'{"name_long": "unique code", 
  "units": "id", 
  "description_md": "primary key identifier for the sampling event and taxon combination"}';

COMMENT ON COLUMN ichthyoplankton_observations.s_c IS 
'{"name_long": "cruise code", 
  "units": "yymm", 
  "description_md": "the year and month of the survey"}';

COMMENT ON COLUMN ichthyoplankton_observations.s_sc IS 
'{"name_long": "ship code", 
  "units": "alpha", 
  "description_md": "ship code for identifying the specific processing batch or sample type"}';

COMMENT ON COLUMN ichthyoplankton_observations.s_l IS 
'{"name_long": "CalCOFI line", 
  "units": "decimal index", 
  "description_md": "CalCOFI lines that are transects running roughly perpendicular from shore"}';

COMMENT ON COLUMN ichthyoplankton_observations.s_s IS 
'{"name_long": "CalCOFI station", 
  "units": "decimal index", 
  "description_md": "CalCOFI stations that run inshore to offshore, with the lower values being closer to shore"}';

COMMENT ON COLUMN ichthyoplankton_observations.latitude IS 
'{"name_long": "latitude", 
  "units": "decimal degrees", 
  "description_md": "North-South coordinate in WGS84"}';

COMMENT ON COLUMN ichthyoplankton_observations.longitude IS 
'{"name_long": "longitude", 
  "units": "decimal degrees", 
  "description_md": "East-West coordinate in WGS84"}';

COMMENT ON COLUMN ichthyoplankton_observations.year IS 
'{"name_long": "year", 
  "units": "yyyy"", 
  "description_md": "year in which the sample was collected"}';

COMMENT ON COLUMN ichthyoplankton_observations.season IS 
'{"name_long": "season", 
  "units": "text", 
  "description_md": "season assigned to the cruise"}';

COMMENT ON COLUMN ichthyoplankton_observations.taxon IS 
'{"name_long": "taxon name", 
  "units": "scientific name", 
  "description_md": "scientific name of the organism, standardized via `janitor::clean_names()`."}';

COMMENT ON COLUMN ichthyoplankton_observations.abundance IS 
'{"name_long": "abundance", 
  "units": "density", 
  "description_md": "density of the taxon=(raw count*standard haul factor)/proportion sorted. Haul accounts for the area that was sampled in the water column"}';

COMMENT ON COLUMN ichthyoplankton_observations.worms_id IS 
'{"name_long": "WoRMS AphiaID", 
  "units": "Integer", 
  "description_md": "unique identifier from the [World Register of Marine Species](https://www.marinespecies.org/)."}';