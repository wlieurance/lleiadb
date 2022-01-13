CREATE SCHEMA IF NOT EXISTS eco;
--
-- level -1, db tables
--
DROP TABLE IF EXISTS eco.db CASCADE;
CREATE TABLE eco.db (
  dbkey VARCHAR(255) PRIMARY KEY,
  dbpath VARCHAR,
  description VARCHAR,
  md5hash VARCHAR(32) NOT NULL 
);

--
-- Level 0, no foreign key (except db)
--
DROP TABLE IF EXISTS eco.site CASCADE;
CREATE TABLE eco.site (
    sitekey character varying(20) PRIMARY KEY,
    siteid character varying(40) NOT NULL,
    site_name character varying(70),
    ownership character varying(50),
    source character varying(100),
    source_type character varying(4),
    contact_name character varying(75),
    notes text,
    dbkey VARCHAR(255),
    FOREIGN KEY (dbkey) REFERENCES eco.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Level 1, references lvl 0
--
DROP TABLE IF EXISTS eco.point CASCADE;
CREATE TABLE eco.point (
    plotkey character varying(20) PRIMARY KEY,
    plotid character varying(30) NOT NULL,
    survey integer NOT NULL,
    establish_date timestamp with time zone NOT NULL,
    state character varying(2),
    county character varying(3),
    mlra character varying(4),
    own character varying(3),
    landform_major character varying(50),
    landform_minor character varying(50),
    vertical_slope_shape character varying(7),
    horizontal_slope_shape character varying(7),
    slope_percent numeric(4,1),
    slope_length integer,
    aspect integer,
    latitude numeric(10,8) CHECK (latitude BETWEEN -90 AND 90),
    longitude numeric(11,8) CHECK (longitude BETWEEN -180 AND 180),
    elevation_m numeric(7,3) CHECK (elevation_m BETWEEN -413 AND 8848),
    nogps character varying(12),
    tz character varying(30),
    geom geometry(PointZ, 4326),
    sitekey character varying(20) NOT NULL,
    FOREIGN KEY (sitekey) REFERENCES eco.site(sitekey) ON UPDATE CASCADE ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS point_geom_gix ON eco.point USING gist (geom);

--
-- Level 2, references lvl 0,1
--
DROP TABLE IF EXISTS eco.disturbance CASCADE;
CREATE TABLE eco.disturbance (
    plotkey character varying(20) NOT NULL,
    survey_date timestamp with time zone NOT NULL,
    pastpres character varying(9) NOT NULL,
    cultivation boolean,
    hayed boolean, 
    mowing boolean,
    hay_removal boolean,
    heavy_machinery boolean,
    seedbed_preparation boolean,
    livestock_tanks boolean,
    livestock_heavy_use boolean,
    livestock_grazing boolean,
    graze_category integer, 
    insects boolean,
    small_rodents boolean,
    non_rodent_animals boolean,
    wildlife_grazing boolean,
    mining_equipment_operations boolean,
    recreation_foot_traffic boolean,
    recreation_vehicles_bikes boolean,
    livestock_walkways boolean,
    roads_dirt boolean,
    roads_gravel boolean,
    roads_paved boolean,
    drainage boolean,
    underground_utilities boolean,
    overhead_transmission_lines boolean,
    construction boolean,
    water_ponding boolean,
    soil_deposition_water boolean,
    soil_deposition_wind boolean,
    water boolean,
    wind boolean,
    transported_fill boolean,
    wildfire boolean,
    prescribed_fire boolean,
    fire_fighting_operations boolean,
    brush_management_chemical boolean,
    brush_management_mechanical boolean,
    brush_management_biological boolean,
    other boolean,
    other_desc character varying(255),
    notes text,
    PRIMARY KEY (plotkey, survey_date, pastpres),
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.esfsg_meta CASCADE;
CREATE TABLE eco.esfsg_meta (
    reckey character varying(20) NOT NULL,
    survey_date timestamp with time zone,
    observer character varying(50),
    notes text,
    plotkey character varying(20) NOT NULL,
    PRIMARY KEY (reckey),
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.note CASCADE;
CREATE TABLE eco.note (
    reckey character varying(20) PRIMARY KEY,
    note_date timestamp with time zone NOT NULL,
    recorder character varying(50), 
    note_type character varying(20),
    note text,
    plotkey character varying(20) NOT NULL,
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.plantcensus_meta CASCADE;
CREATE TABLE eco.plantcensus_meta (
    reckey character varying(20) PRIMARY KEY,
    survey_date timestamp with time zone NOT NULL,
    observer character varying(50),
    survey_size_m2 numeric(6,1) CHECK(survey_size_m2 >= 0),
    notes text,
    plotkey character varying(20) NOT NULL,
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.production_meta CASCADE;
CREATE TABLE eco.production_meta (
    reckey character varying(20) PRIMARY KEY,
    survey_date timestamp with time zone NOT NULL,
    observer character varying(50),
    subplot_no integer,
    notes text,
    plotkey character varying(20) NOT NULL,
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.rangehealth_meta CASCADE;
CREATE TABLE eco.rangehealth_meta (
    reckey character varying(20) PRIMARY KEY,
    survey_date timestamp with time zone NOT NULL,
    observer character varying(50),
    ecoid_std character varying(10), 
    refsheet_src character varying(50), 
    refsheet_datepub date, 
    refsheet_author character varying(50), 
    refsheet_dategot date, 
    indicator_wgt_method character varying(50), 
    weight_src character varying(50), 
    rep_criteria text,
    composition_base character varying(10),
    plotkey character varying(20) NOT NULL,
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.soil_meta CASCADE;
CREATE TABLE eco.soil_meta (
    reckey character varying(20) PRIMARY KEY,
    survey_date timestamp with time zone NOT NULL,
    observer character varying(100),  -- in case of multiple observers 100 instead of 50
    pit_desc text,
    area_symbol character varying(5), 
    mapunit_symbol character varying(6), 
    taxon_name text, 
    series_name character varying(50), 
    component_name character varying(50), 
    component_key character varying(8), 
    soil_confidence_rating smallint,
    notes text,
    longitude numeric(11,8) CHECK (longitude BETWEEN -180 AND 180), 
    latitude numeric(10,8) CHECK (latitude BETWEEN -90 AND 90), 
    elevation_m numeric(7,3) CHECK (elevation_m BETWEEN -413 AND 8848), 
    geom geometry(PointZ, 4326),
    plotkey character varying(20) NOT NULL,
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.soilstability_meta CASCADE;
CREATE TABLE eco.soilstability_meta (
    reckey character varying(20) PRIMARY KEY,
    survey_date timestamp with time zone NOT NULL, 
    observer character varying(50), 
    rectype character varying(20), 
    notes text, 
    plotkey character varying(20) NOT NULL,
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.transect CASCADE;
CREATE TABLE eco.transect (
    linekey character varying(20) PRIMARY KEY,
    establish_date timestamp with time zone NOT NULL,
    lineid character varying(15) NOT NULL,
    azimuth integer,
    azimuth_type character varying(10),
    transect_length integer CHECK (transect_length > 0),
    transect_units character varying(2) CHECK(transect_units IN ('m', 'ft')),
    latitude_start numeric(10, 8) CHECK (latitude_start BETWEEN -90 AND 90),
    longitude_start numeric(11, 8) CHECK (longitude_start BETWEEN -180 AND 180),
    elevation_start numeric(7,3) CHECK (elevation_start BETWEEN -413 AND 8848),
    latitude_end numeric(10, 8) CHECK (latitude_end BETWEEN -90 AND 90),
    longitude_end numeric(11, 8) CHECK (longitude_end BETWEEN -180 AND 180),
    elevation_end numeric(7,3) CHECK (elevation_end BETWEEN -413 AND 8848),
    geom geometry(LineStringZ, 4326),
    plotkey character varying(20) NOT NULL,
    FOREIGN KEY (plotkey) REFERENCES eco.point(plotkey) ON UPDATE CASCADE ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS transect_geom_gix ON eco.transect USING gist (geom);
    
--
-- Level 3, references lvl 0,1,2
--
DROP TABLE IF EXISTS eco.esfsg CASCADE;
CREATE TABLE eco.esfsg (
    reckey character varying(20) NOT NULL,
    ecoid_std character varying(20),
    ecotype character varying(1),
    econame character varying(255),
    area_pct double precision CHECK(area_pct BETWEEN 0 AND 1),
    ecorank integer,
    esd_state character varying(100),
    state_community character varying(100),
    community_desc TEXT,
    PRIMARY KEY (reckey, ecoid_std),
    FOREIGN KEY (reckey) REFERENCES eco.esfsg_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.gap_meta CASCADE;
CREATE TABLE eco.gap_meta (
    reckey character varying(20) PRIMARY KEY,
    survey_date timestamp with time zone NOT NULL,
    observer character varying(50),
    gapmin_cm numeric(4,1) CHECK (gapmin_cm >= 0),
    gap_types character varying(20),
    canopy_stops_gap character varying(30),
    canopy_no_gap boolean,
    basal_stops_gap character varying(30),
    basal_no_gap boolean,
    notes text,
    linekey character varying(20),
    FOREIGN KEY (linekey) REFERENCES eco.transect(linekey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.pintercept_meta CASCADE;
CREATE TABLE eco.pintercept_meta (
    reckey character varying(20) PRIMARY KEY,
    survey_date timestamp with time zone NOT NULL,
    transect_length double precision,
    transect_interval double precision,
    transect_units character varying (5),
    observer character varying(50),
    notes text,
    linekey character varying(20) NOT NULL,
    FOREIGN KEY (linekey) REFERENCES eco.transect(linekey) ON UPDATE CASCADE ON DELETE CASCADE
);


DROP TABLE IF EXISTS eco.plantcensus CASCADE;
CREATE TABLE eco.plantcensus (
    reckey character varying(20) NOT NULL,
    seq_no integer NOT NULL,
    species_code character varying(7) NOT NULL,
    notes text,
    PRIMARY KEY (reckey, species_code),
    FOREIGN KEY (reckey) REFERENCES eco.plantcensus_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS idx_plantcensus_species_code ON eco.plantcensus USING btree (species_code);

DROP TABLE IF EXISTS eco.plantdensity_meta CASCADE;
CREATE TABLE eco.plantdensity_meta (
    reckey character varying(20) PRIMARY KEY,
    survey_date timestamp with time zone NOT NULL,
    observer character varying(50),
    subplot_no smallint,
    notes text,
    species_searched text,
    linekey character varying(20) NOT NULL,
    FOREIGN KEY (linekey) REFERENCES eco.transect(linekey) ON UPDATE CASCADE ON DELETE CASCADE
);
DROP TABLE IF EXISTS eco.production_species CASCADE;
CREATE TABLE eco.production_species (
    reckey character varying(20) NOT NULL,
    species_code character varying(7), 
    subsize_m2 numeric(9,4) CHECK (subsize_m2 > 0), 
    unit_wgt_g double precision CHECK (unit_wgt_g > 0), 
    adjust_airdrywgt double precision CHECK (adjust_airdrywgt BETWEEN 0 AND 1), 
    adjust_utilization double precision CHECK (adjust_utilization BETWEEN 0 AND 1), 
    adjust_growth double precision CHECK (adjust_growth BETWEEN 0 AND 1), 
    adjust_climate double precision CHECK (adjust_climate BETWEEN 0.1 AND 2.0),
    PRIMARY KEY (reckey, species_code),
    FOREIGN KEY (reckey) REFERENCES eco.production_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.production_subplot CASCADE;
CREATE TABLE eco.production_subplot (
    reckey character varying(20) NOT NULL, 
    subplot smallint, 
    subloc text, 
    expanded boolean, 
    not_sampled boolean,
    PRIMARY KEY (reckey, subplot),
    FOREIGN KEY (reckey) REFERENCES eco.production_meta ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.rangehealth CASCADE;
CREATE TABLE eco.rangehealth (
    reckey character varying(20) NOT NULL, 
    seq_no smallint, 
    rate_abbr character varying(7), 
    rate_type character varying(1), 
    rating smallint, 
    note text,
    PRIMARY KEY (reckey, seq_no),
    FOREIGN KEY (reckey) REFERENCES eco.rangehealth_meta ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.soil CASCADE;
-- pages references from NRCS Field Book for Describing and Sampling Soils v3
CREATE TABLE eco.soil (
    reckey character varying(20) NOT NULL, 
    seq_no smallint NOT NULL, 
    depth_upper_cm numeric(5,1) CHECK(depth_upper_cm >=0), 
    depth_lower_cm numeric(5,1) CHECK(depth_lower_cm >= depth_upper_cm),
    
    -- Horizon and Layer Designations, 2-2    
    horizon_lbl character varying(10), 
    
    -- Soil Color, 2-8
    color_hue character varying(6), 
    color_value double precision, 
    color_chroma double precision, 
    color_measure_type character varying(5),
    
    -- Concentrations, 2-20
    conc_gr_co3_pct double precision CHECK (conc_gr_co3_pct BETWEEN 0 AND 1), 
    
    -- Pedogenic Carbonate Stages (Discussion), 2-28
    conc_carbonate_stage smallint, 
    
    -- Ped and Void Surface Features, 2-32
    film_clay boolean,
    
    -- Soil Texture, 2-36    
    texture character varying(4), 
    texture_modifier character varying(4), 
    texture_gypsic boolean,
    
    -- Rock and Other Fragments, 2-46    
    frag_total_pct double precision CHECK (frag_total_pct BETWEEN 0 AND 1), 
    frag_petrocalcic boolean,
    
    -- (Soil) Structure, 2-52    
    struct_type1 character varying(3), 
    struct_grade1 smallint, 
    struct_size1 character varying(2), 
    struct_verb character varying(10), 
    struct_type2 character varying(3), 
    struct_grade2 smallint, 
    struct_size2 character varying(2),
    
    -- Consistence - Rupture Resistance, 2-62    
    rupture_resist character varying(2),
    
    -- Roots, 2-70
    root_size character varying(2), 
    root_qty character varying(14), 
    pore_size character varying(2), 
    pore_qty character varying(14),
    
    -- Chemical Response, 2-85    
    chem_ph double precision CHECK (chem_ph BETWEEN 0 AND 14), 
    chem_effer_class character varying(2), 
    chem_caco3_equiv_pct double precision CHECK (chem_caco3_equiv_pct BETWEEN 0 AND 1), 
    chem_gypsum_pct double precision CHECK (chem_gypsum_pct BETWEEN 0 AND 1), 
    
    -- Chemical Response - Salinity Class, 2-89
    chem_ec double precision CHECK (chem_ec >= 0), 
    chem_sar double precision CHECK (chem_sar >= 0),
     
    notes text,
    PRIMARY KEY (reckey, seq_no),
    FOREIGN KEY (reckey) REFERENCES eco.soil_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.soilstability CASCADE;
CREATE TABLE eco.soilstability (
    reckey character varying(20) NOT NULL, 
    box_no smallint NOT NULL, 
    cell smallint NOT NULL, 
    lineid character varying(15), 
    pos double precision, 
    veg character varying(2), 
    rating smallint, 
    hydrophobic boolean,
    PRIMARY KEY (reckey, box_no, cell),
    FOREIGN KEY (reckey) REFERENCES eco.soilstability_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Level 4, references lvl 0,1,2,3
--
DROP TABLE IF EXISTS eco.esfsg_detail CASCADE;
CREATE TABLE eco.esfsg_detail (
    linekey character varying(20) NOT NULL,
    reckey character varying(20) NOT NULL,
    seq_no integer NOT NULL,
    ecoid_std character varying(10),
    start_mark double precision,
    end_mark double precision,
    PRIMARY KEY (linekey, reckey, seq_no),
    FOREIGN KEY (linekey) REFERENCES eco.transect(linekey) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (reckey, ecoid_std) REFERENCES eco.esfsg(reckey, ecoid_std) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.gap CASCADE;
CREATE TABLE eco.gap (
    reckey character varying(20) NOT NULL,
    rectype character varying(1),
    seqno smallint,
    gap_start_cm integer CHECK (gap_start_cm >= 0),
    gap_end_cm integer CHECK (gap_end_cm >= 0),
    CHECK (gap_start_cm != gap_end_cm),
    PRIMARY KEY (reckey, rectype, seqno),
    FOREIGN KEY (reckey) REFERENCES eco.gap_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.pintercept CASCADE;
CREATE TABLE eco.pintercept (
    reckey character varying(20) NOT NULL,
    mark numeric(4,1) NOT NULL,
    hit_order smallint,
    hit_type character varying(2),
    hit_sub character varying(2),
    hit character varying(9),
    height_cm numeric(6,1), -- tallest tree in NA = 11607 cm
    dead boolean,
    PRIMARY KEY (reckey, mark, hit_type, hit_order),
    FOREIGN KEY (reckey) REFERENCES eco.pintercept_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.plantdensity_class CASCADE;
CREATE TABLE eco.plantdensity_class (
    reckey character varying(20) NOT NULL,
    class_no smallint, 
    class_lbl text,
    PRIMARY KEY (reckey, class_no),
    FOREIGN KEY (reckey) REFERENCES eco.plantdensity_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.plantdensity_subplot CASCADE;
CREATE TABLE eco.plantdensity_subplot (
    reckey character varying(20) NOT NULL,
    subid character varying(15), 
    subplot integer,
    subsize_m2 numeric(7, 2) CHECK (subsize_m2 > 0),
    PRIMARY KEY (reckey, subid),
    FOREIGN KEY (reckey) REFERENCES eco.plantdensity_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.production CASCADE;
CREATE TABLE eco.production (
    reckey character varying(20) NOT NULL,
    species_code character varying(7) NOT NULL,
    subplot integer NOT NULL,
    units double precision,
    trace boolean,
    clipped_wgt_g double precision CHECK (clipped_wgt_g > 0),
    PRIMARY KEY (reckey, species_code, subplot),
    FOREIGN KEY (reckey, species_code) REFERENCES eco.production_species(reckey, species_code) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (reckey, subplot) REFERENCES eco.production_subplot(reckey, subplot) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.shrubshape CASCADE;
CREATE TABLE eco.shrubshape (
    reckey character varying(20) NOT NULL, 
    mark numeric(4,1) NOT NULL,
    species_code character varying(7), 
    shape character varying(1),
    PRIMARY KEY (reckey, mark),
    FOREIGN KEY (reckey) REFERENCES eco.pintercept_meta(reckey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS eco.soil_component CASCADE;
CREATE TABLE eco.soil_component (
    reckey character varying(20) NOT NULL, 
    seq_no smallint NOT NULL, 
    analysis character varying(10) NOT NULL, 
    component character varying(4), 
    vol_pct double precision CHECK (vol_pct BETWEEN 0 and 1), 
    frag_roundness character varying(2), 
    frac_vfine double precision, 
    frac_fine double precision,
    frac_med double precision, 
    frac_coarse double precision,
    frac_vcoarse double precision,
    CHECK (coalesce(frac_vfine, 0) + coalesce(frac_fine, 0) + coalesce(frac_med, 0) +
           coalesce(frac_coarse, 0) + coalesce(frac_vcoarse, 0) BETWEEN 0 AND 1),
    PRIMARY KEY (reckey, seq_no, analysis, component),
    FOREIGN KEY (reckey, seq_no) REFERENCES eco.soil(reckey, seq_no) ON UPDATE CASCADE ON DELETE CASCADE
);


--
-- Level 5, references lvl 0,1,2,3,4
--

DROP TABLE IF EXISTS eco.plantdensity CASCADE;
CREATE TABLE eco.plantdensity (
    reckey character varying(20) NOT NULL,
    subid character varying(15),
    species_code character varying(7), 
    class_no smallint,
    total integer CHECK (total >= 0),
    PRIMARY KEY (reckey, subid, species_code, class_no),
    FOREIGN KEY (reckey, subid) REFERENCES eco.plantdensity_subplot(reckey, subid) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (reckey, class_no) REFERENCES eco.plantdensity_class(reckey, class_no) ON UPDATE CASCADE ON DELETE CASCADE
);
