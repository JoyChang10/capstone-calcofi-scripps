CREATE TABLE IF NOT EXISTS survey_header_map (
  raw_name TEXT PRIMARY KEY,
  std_name TEXT NOT NULL,
  scope    TEXT NOT NULL DEFAULT 'metadata', -- metadata | long_fields
  notes    TEXT
);

-- Metadata mappings
-- EDIT the raw_name values to match your actual CSV headers.
INSERT OR IGNORE INTO survey_header_map (raw_name, std_name, scope, notes) VALUES
  ('UniqueCode', 'unique_code', 'metadata', 'Sampling event identifier'),
  ('unique_code', 'unique_code', 'metadata', 'Already standardized'),
  ('Latitude', 'latitude', 'metadata', 'Decimal degrees'),
  ('Lat',      'latitude', 'metadata', 'Alternate header'),
  ('latitude', 'latitude', 'metadata', 'Already standardized'),
  ('Longitude', 'longitude', 'metadata', 'Decimal degrees'),
  ('Lon',       'longitude', 'metadata', 'Alternate header'),
  ('longitude', 'longitude', 'metadata', 'Already standardized'),
  ('Year', 'year', 'metadata', 'Calendar year'),
  ('year', 'year', 'metadata', 'Already standardized'),
  ('Season', 'season', 'metadata', 'Season label'),
  ('season', 'season', 'metadata', 'Already standardized'),
  ('S_C',  's_c',  'metadata', 'Cruise id'),
  ('S_SC', 's_sc', 'metadata', 'Ship code'),
  ('S_L',  's_l',  'metadata', 'CalCOFI line'),
  ('S_S',  's_s',  'metadata', 'CalCOFI station')

