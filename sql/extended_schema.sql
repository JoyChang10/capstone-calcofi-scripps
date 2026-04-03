CREATE OR REPLACE TABLE ichthyoplankton_observations (

    -- CORE IDENTIFIERS 
    unique_code  TEXT,
    s_c          TEXT,      -- cruise id
    s_sc         TEXT,      -- ship code
    s_l          DOUBLE,    -- CalCOFI line
    s_s          DOUBLE,    -- CalCOFI station
    latitude     DOUBLE,
    longitude    DOUBLE,
    year         INTEGER,
    season       TEXT,
    taxon        TEXT,      -- species name
    abundance    DOUBLE,    -- standardized abundances = (raw count*standard haul factor)/proportion
    worms_id     INTEGER,   -- WoRMS AphiaID

    -- BOTTLE DATA (Bottle_Field_Descriptions.csv) 
    cst_cnt      INTEGER,   -- cast count, consecutively numbered
    btl_cnt      INTEGER,   -- bottle count, consecutively numbered
    depth_id     TEXT,      -- depth identifier
    depth_m      DOUBLE,    -- bottle depth (meters)
    t_degc       DOUBLE,    -- water temperature (C)
    salnty       DOUBLE,    -- salinity (Practical Salinity Scale 1978)
    o2ml_l       DOUBLE,    -- dissolved oxygen (mL/L)
    s_theta      DOUBLE,    -- potential density sigma theta (kg/m)
    o2sat        DOUBLE,    -- oxygen percent saturation
    oxy_umol_kg  DOUBLE,    -- oxygen (micromoles/kg)
    btl_num      INTEGER,   -- niskin bottle number
    rec_ind      TEXT,      -- record indicator / quality code
    t_prec       DOUBLE,    -- temperature precision
    t_qual       TEXT,      -- temperature quality code
    s_prec       DOUBLE,    -- salinity precision
    s_qual       TEXT,      -- salinity quality code
    p_qual       TEXT,      -- pressure quality code
    o_qual       TEXT,      -- oxygen quality code
    s_thetaq     TEXT,      -- potential density quality code
    o2satq       TEXT,      -- oxygen saturation quality code
    chlor_a      DOUBLE,    -- chlorophyll-a (µg/L)
    chl_qua      TEXT,      -- chlorophyll quality code
    phaeop       DOUBLE,    -- phaeopigment (µg/L)
    pha_qua      TEXT,      -- phaeopigment quality code
    po4um        DOUBLE,    -- phosphate (µmol/L)
    po4q         TEXT,      -- phosphate quality code
    sio3um       DOUBLE,    -- silicate (µmol/L)
    sio3qu       TEXT,      -- silicate quality code
    no2um        DOUBLE,    -- nitrite (µmol/L)
    no2q         TEXT,      -- nitrite quality code
    no3um        DOUBLE,    -- nitrate (µmol/L)
    no3q         TEXT,      -- nitrate quality code
    nh3um        DOUBLE,    -- ammonia (µmol/L)
    nh3q         TEXT,      -- ammonia quality code
    c14as1       DOUBLE,    -- 14C assimilation replicate 1 (mgC/m3/half light day)
    c14a1p       DOUBLE,    -- precision of 14C assimilation replicate 1
    c14a1q       TEXT,      -- quality code
    c14as2       DOUBLE,    -- 14C assimilation replicate 2 (mgC/m3/half light day)
    c14a2p       DOUBLE,    -- precision of 14C assimilation replicate 2
    c14a2q       TEXT,      -- quality code
    dark_as      DOUBLE,    -- 14C dark/control bottle assimilation
    dark_ap      DOUBLE,    -- precision of dark bottle assimilation
    dark_aq      TEXT,      -- quality code
    mean_as      DOUBLE,    -- mean 14C assimilation replicates 1 and 2
    mean_ap      DOUBLE,    -- precision of mean 14C assimilation
    mean_aq      TEXT,      -- quality code
    inc_tim      TEXT,      -- elapsed incubation time
    light_p      DOUBLE,    -- light intensity of incubation tubes (%)
    r_depth      DOUBLE,    -- reported depth from pressure (meters)
    r_temp       DOUBLE,    -- reported potential temperature (C)
    r_sal        DOUBLE,    -- reported salinity
    r_dynht      DOUBLE,    -- reported dynamic height (dynamic meters)
    r_nuts       DOUBLE,    -- reported ammonium concentration (µmol/L)
    r_oxy_umol   DOUBLE,    -- reported oxygen (µmol/kg)
    dic1         DOUBLE,    -- dissolved inorganic carbon replicate 1 (µmol/kg)
    dic2         DOUBLE,    -- dissolved inorganic carbon replicate 2 (µmol/kg)
    ta1          DOUBLE,    -- total alkalinity replicate 1 (µmol/kg)
    ta2          DOUBLE,    -- total alkalinity replicate 2 (µmol/kg)
    ph1          DOUBLE,    -- pH replicate 1
    ph2          DOUBLE,    -- pH replicate 2
    dic_quality  TEXT,      -- DIC quality comment

    -- CAST DATA (Cast_Field_Descriptions.csv) 
    cast_cnt     INTEGER,   -- cast count
    cruise_id    TEXT,      -- cruise identifier
    cruise       TEXT,      -- cruise name
    cruz_sta     TEXT,      -- cruise name and station
    cast_id      TEXT,      -- cast identifier
    sta_id       TEXT,      -- line and station
    quarter      INTEGER,   -- quarter of the year
    sta_code     TEXT,      -- station designation code
    distance     DOUBLE,    -- nautical miles from coast
    obs_date     TEXT,      -- date (month day year)
    month        INTEGER,   -- month
    julian_date  DOUBLE,    -- OA date (days since Dec 30 1899)
    julian_day   INTEGER,   -- julian day of year
    obs_time     TEXT,      -- time UTC CTD reached terminal depth
    lat_dec      DOUBLE,    -- observed latitude (decimal degrees)
    lon_dec      DOUBLE,    -- observed longitude (decimal degrees)
    rpt_line     DOUBLE,    -- reported line number
    st_line      DOUBLE,    -- nearest standard line
    ac_line      DOUBLE,    -- calculated actual line
    rpt_sta      DOUBLE,    -- reported station number
    st_station   DOUBLE,    -- nearest standard station number
    ac_sta       DOUBLE,    -- calculated actual station
    bottom_d     DOUBLE,    -- bottom depth (meters)
    ship_name    TEXT,      -- ship name
    ship_code    TEXT,      -- ship NODC code
    data_type    TEXT,      -- data type
    order_occ    INTEGER,   -- order station was occupied
    event_num    INTEGER,   -- event number
    cruz_leg     TEXT,      -- cruise leg
    orig_sta_id  TEXT,      -- original reported station id
    cruz_num     TEXT,      -- cruise designation
    int_chl      DOUBLE,    -- integrated chlorophyll (mg chl/m2/half light day)
    int_c14      DOUBLE,    -- integrated primary productivity (mgC/m2/half light day)
    inc_str      TEXT,      -- incubation start time (PST)
    inc_end      TEXT,      -- incubation end time (PST)
    pst_lan      TEXT,      -- local apparent noon (PST)
    civil_t      TEXT,      -- civil twilight time (PST)
    timezone     TEXT,      -- time zone
    wave_dir     DOUBLE,    -- wave direction (degrees)
    wave_ht      DOUBLE,    -- wave height (feet)
    wave_prd     DOUBLE,    -- wave period (seconds)
    wind_dir     DOUBLE,    -- wind direction (degrees)
    wind_spd     DOUBLE,    -- wind speed (knots)
    barometer    DOUBLE,    -- barometric pressure (millibars)
    dry_t        DOUBLE,    -- dry air temperature (C)
    wet_t        DOUBLE,    -- wet air temperature (C)
    wea          TEXT,      -- WMO weather code
    cloud_typ    TEXT,      -- WMO cloud type code
    cloud_amt    TEXT,      -- WMO cloud amount code (oktas)
    visibility   TEXT,      -- WMO visibility code
    secchi       DOUBLE,    -- secchi disk depth (meters)
    forel_u      TEXT,      -- forel-ule water color scale

    PRIMARY KEY (unique_code, taxon)
);