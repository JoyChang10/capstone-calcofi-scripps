CREATE TABLE ichthyoplankton_observations (
    unique_code TEXT,
    s_c         TEXT,    -- cruise id
    s_sc        TEXT,    -- ship code
    s_l         DOUBLE,  -- CalCOFI line
    s_s         DOUBLE,  -- CalCOFI station
    latitude    DOUBLE,
    longitude   DOUBLE,
    year        INTEGER,
    season      TEXT,
    taxon       TEXT,    -- species name
    abundance   DOUBLE,  -- standardized abundances = (raw count*standard haul factor)/proportion
    worms_id    INTEGER,  -- WoRMS AphiaID
    PRIMARY KEY (unique_code, taxon)
);