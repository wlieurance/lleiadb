CREATE SCHEMA IF NOT EXISTS lmf;

-- Custom tables are more pared down for LMF than with DIMA, as the LMF database distributed is no a collection database and
-- does not suffer the same problems collection databases often do. 
DROP TABLE IF EXISTS lmf.db CASCADE;
CREATE TABLE lmf.db (
  dbkey VARCHAR(255) PRIMARY KEY,
  dbpath VARCHAR,
  description VARCHAR,
  md5hash VARCHAR(32) NOT NULL 
);

--
-- Level 0 tables,  No Foreign Key except db
--

-- Table: STATENM
DROP TABLE IF EXISTS lmf."STATENM" CASCADE;
CREATE TABLE lmf."STATENM" (
    "STATE" integer,
    "STABBR" varchar(2),
    "STATENM" varchar(19),
    PRIMARY KEY ("STATE")
);


--
-- Level 1 tables,  No Foreign Key except lvl 0
--


-- Table: COUNTYNM
DROP TABLE IF EXISTS lmf."COUNTYNM" CASCADE;
CREATE TABLE lmf."COUNTYNM"
(
    "STATE" integer,
    "COUNTY" integer,
    "COUNTYNM" varchar(25),
    PRIMARY KEY ("STATE", "COUNTY"),
    FOREIGN KEY ("STATE") REFERENCES lmf."STATENM"("STATE")
    ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Level 2 tables,  No Foreign Key except lvl 0,1
--

-- Table: POINT
DROP TABLE IF EXISTS lmf."POINT" CASCADE;
CREATE TABLE lmf."POINT" (
    dbkey varchar(255),  -- added for upload mgmt
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "MLRA" varchar(4),
    "OWN" varchar(3),
    "LANDUSE" integer,
    "VERTICAL_SLOPE_SHAPE" varchar(7),
    "HORIZONTAL_SLOPE_SHAPE" varchar(7),
    "SLOPE_PERCENT" double precision,
    "SSAID" varchar(5),
    "MUSYM" varchar(6),
    "COMPONENT_NAME" varchar(100),
    "COMPONENT_ID" varchar(15),
    "ECO_SITE_1992" varchar(10),
    "FULL_ESD_PLOT" varchar(1),
    "APPARENT_TREND" varchar(6),
    "BASAL_GAPS_NESW" varchar(1),
    "CANOPY_GAPS_NESW" varchar(1),
    "BASAL_GAPS_NWSE" varchar(1),
    "CANOPY_GAPS_NWSE" varchar(1),
    "PLOT_SIZE_HERB" varchar(4),
    "TOT_ANN_PROD" integer,
    "SIMILARITY_INDEX" double precision,
    "PTSTATUS" varchar(2),
    "SOIL_CONFIDENCE_RATING" integer,
    "SLOPE_LENGTH" integer,
    "SLOPE_ASPECT" varchar(2),
    "BIOMASS_PLOT_SIZE_HERB" varchar(4),
    "BIOMASS_PLOT_METHOD" varchar(10),
    "BIOMASS_WOODY_METHOD" varchar(9),
    "GRAZING_USE" integer,
    "HAYED" varchar(1),
    "OPT_SSI_PASTURE" varchar(1),
    "OPT_DWR_PASTURE" varchar(1),
    "OPT_DWR_RANGE" varchar(1),
    "SAGE_EXISTS" varchar(1),
    "PERENNIAL_CANOPY_GAPS_NESW" varchar(1),
    "PERENNIAL_CANOPY_GAPS_NWSE" varchar(1),
    "COMPONENT_HISTORICAL" varchar(100),
    "COMPONENT_CHOSEN" varchar(15),
    "COMPONENT_POPULATED" varchar(1),
    "LANDUSE_SOURCE" varchar(8),
    "GAPS_DIFFERENT_NESW" varchar(5),
    "GAPS_DIFFERENT_NWSE" varchar(5),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("STATE", "COUNTY") REFERENCES lmf."COUNTYNM"("STATE", "COUNTY") ON UPDATE CASCADE,
    FOREIGN KEY (dbkey) REFERENCES lmf.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);


--
-- Level 3 tables,  No Foreign Key except lvl 0,1,2
--

-- Table: CONCERN
DROP TABLE IF EXISTS lmf."CONCERN" CASCADE;
CREATE TABLE lmf."CONCERN" (   
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "EROSION_SHEET_AND_RILL" varchar(1),
    "EROSION_WIND" varchar(1),
    "EROSION_CLASSIC_GULLY" varchar(1),
    "EROSION_STREAMBANK" varchar(1),
    "EROSION_SHORELINE" varchar(1),
    "EROSION_MASS_MOVEMENT" varchar(1),
    "ORGANIC_MATTER_DEPLETION" varchar(1),
    "COMPACTION" varchar(1),
    "CONTAMINANTS_SALTS" varchar(1),
    "CONTAMINANTS_WASTES_ORGANICS" varchar(1),
    "DAMAGE_FROM_SOIL_DEPOSITION" varchar(1),
    "EXCESSIVE_RUNOFF_FLOODING_POND" varchar(1),
    "REDUCED_STORAGE_BY_SEDIMENT" varchar(1),
    "INSUFFICIENT_FLOWS" varchar(1),
    "EXCESSIVE_NUTRIENTS_ORGANICS" varchar(1),
    "EXCESSIVE_SEDIMENT_TURBIDITY" varchar(1),
    "PLANT_NOT_ADAPTED_OR_SUITED" varchar(1),
    "PRODUCTIVITY_HEALTH_AND_VIGOR" varchar(1),
    "NOXIOUS_AND_INVASIVE_PLANTS" varchar(1),
    "FORAGE_QUALITY_PALATABILITY" varchar(1),
    "WILDFIRE_HAZARD" varchar(1),
    "INADEQUATE_STOCK_WATER" varchar(1),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);



-- Table: DISTURBANCE
DROP TABLE IF EXISTS lmf."DISTURBANCE" CASCADE;
CREATE TABLE lmf."DISTURBANCE" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "PASTPRES" varchar(9),
    "CULTIVATION" varchar(1),
    "MOWING" varchar(1),
    "HAY_REMOVAL" varchar(1),
    "HEAVY_MACHINERY" varchar(1),
    "SEEDBED_PREPARATION" varchar(1),
    "LIVESTOCK_TANKS" varchar(1),
    "LIVESTOCK_HEAVY_USE" varchar(1),
    "LIVESTOCK_GRAZING" varchar(1),
    "INSECTS" varchar(1),
    "SMALL_RODENTS" varchar(1),
    "NON_RODENT_ANIMALS" varchar(1),
    "WILDLIFE_GRAZING" varchar(1),
    "MINING_EQUIPMENT_OPERATIONS" varchar(1),
    "RECREATION_FOOT_TRAFFIC" varchar(1),
    "RECREATION_VEHICLES_BIKES" varchar(1),
    "LIVESTOCK_WALKWAYS" varchar(1),
    "ROADS_DIRT" varchar(1),
    "ROADS_GRAVEL" varchar(1),
    "ROADS_PAVED" varchar(1),
    "DRAINAGE" varchar(1),
    "UNDERGROUND_UTILITIES" varchar(1),
    "OVERHEAD_TRANSMISSION_LINES" varchar(1),
    "CONSTRUCTION" varchar(1),
    "WATER_PONDING" varchar(1),
    "SOIL_DEPOSITION_WATER" varchar(1),
    "SOIL_DEPOSITION_WIND" varchar(1),
    "WATER" varchar(1),
    "WIND" varchar(1),
    "TRANSPORTED_FILL" varchar(1),
    "WILDFIRE" varchar(1),
    "PRESCRIBED_FIRE" varchar(1),
    "FIRE_FIGHTING_OPERATIONS" varchar(1),
    "BRUSH_MANAGEMENT_CHEMICAL" varchar(1),
    "BRUSH_MANAGEMENT_MECHANICAL" varchar(1),
    "BRUSH_MANAGEMENT_BIOLOGICAL" varchar(1),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT", "PASTPRES"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: ESFSG
DROP TABLE IF EXISTS lmf."ESFSG" CASCADE;
CREATE TABLE lmf."ESFSG" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "SEQNUM" integer,
    "COVERAGE" varchar(4),
    "START_MARK" integer,
    "END_MARK" integer,
    "ESFSG_STATE" varchar(2),
    "ESFSG_MLRA" varchar(4),
    "ESFSG_SITE" varchar(4),
    "ESFSG_NAME" varchar(88),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT", "SEQNUM"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
    
);


-- Table: GINTERCEPT
DROP TABLE IF EXISTS lmf."GINTERCEPT" CASCADE;
CREATE TABLE lmf."GINTERCEPT" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(10),
    "POINT" integer,
    "TRANSECT" varchar(6),
    "GAP_TYPE" varchar(8),
    "SEQNUM" integer,
    "START_GAP" double precision,
    "END_GAP" double precision,
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT", "TRANSECT", "GAP_TYPE", "SEQNUM"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: GPS
DROP TABLE IF EXISTS lmf."GPS" CASCADE;
CREATE TABLE lmf."GPS" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "NOGPS" varchar(12),
    "CAPDATE" timestamp without time zone,
    "ELEVATION" integer,
    "FIELD_VISIT" varchar(12),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: PASTUREHEIGHTS
DROP TABLE IF EXISTS lmf."PASTUREHEIGHTS" CASCADE;
CREATE TABLE lmf."PASTUREHEIGHTS" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "TRANSECT" varchar(4),
    "DISTANCE" integer,
    "HPLANT" varchar(7),
    "HEIGHT" varchar(8),
    "WPLANT" varchar(7),
    "WHEIGHT" varchar(8),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT", "TRANSECT", "DISTANCE"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: PINTERCEPT
DROP TABLE IF EXISTS lmf."PINTERCEPT" CASCADE;
CREATE TABLE lmf."PINTERCEPT" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "TRANSECT" varchar(6),
    "MARK" integer,
    "HIT1" varchar(7),
    "HIT2" varchar(7),
    "HIT3" varchar(7),
    "HIT4" varchar(7),
    "HIT5" varchar(7),
    "HIT6" varchar(7),
    "BASAL" varchar(7),
    "NONSOIL" varchar(6),
    "SAGEBRUSH_SHAPE" integer,
    "SAGEBRUSH_SPP" varchar(8),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT", "TRANSECT", "MARK"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: PLANTCENSUS
DROP TABLE IF EXISTS lmf."PLANTCENSUS" CASCADE;
CREATE TABLE lmf."PLANTCENSUS" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(8),
    "POINT" integer,
    "SEQNUM" integer,
    "CPLANT" varchar(8),
    "DENSITY" integer,
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT", "SEQNUM"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: POINTCOORDINATES
DROP TABLE IF EXISTS lmf."POINTCOORDINATES" CASCADE;
CREATE TABLE lmf."POINTCOORDINATES" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "TARGET_LATITUDE" double precision,
    "TARGET_LONGITUDE" double precision,
    "FIELD_LATITUDE" double precision,
    "FIELD_LONGITUDE" double precision,
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: POINTWEIGHT
DROP TABLE IF EXISTS lmf."POINTWEIGHT" CASCADE;
CREATE TABLE lmf."POINTWEIGHT" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "WEIGHT" double precision,
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: PRACTICE
DROP TABLE IF EXISTS lmf."PRACTICE" CASCADE;
CREATE TABLE lmf."PRACTICE" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(255),
    "POINT" integer,
    "P314" varchar(1),
    "N314" varchar(1),
    "P338" varchar(1),
    "N338" varchar(1),
    "P378" varchar(1),
    "N378" varchar(1),
    "P380" varchar(1),
    "N380" varchar(1),
    "P381" varchar(1),
    "N381" varchar(1),
    "P382" varchar(1),
    "N382" varchar(1),
    "P390" varchar(1),
    "N390" varchar(1),
    "P393" varchar(1),
    "N393" varchar(1),
    "P449" varchar(1),
    "N449" varchar(1),
    "P512" varchar(1),
    "N512" varchar(1),
    "P516" varchar(1),
    "N516" varchar(1),
    "P528" varchar(1),
    "N528" varchar(1),
    "P548" varchar(1),
    "N548" varchar(1),
    "P550" varchar(1),
    "N550" varchar(1),
    "P574" varchar(1),
    "N574" varchar(1),
    "P575" varchar(1),
    "N575" varchar(1),
    "P590" varchar(1),
    "N590" varchar(1),
    "P595" varchar(1),
    "N595" varchar(1),
    "P614" varchar(1),
    "N614" varchar(1),
    "P642" varchar(1),
    "N642" varchar(1),
    "P648" varchar(1),
    "N648" varchar(1),
    "P666" varchar(1),
    "N666" varchar(1),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: PTNOTE
DROP TABLE IF EXISTS lmf."PTNOTE" CASCADE;
CREATE TABLE lmf."PTNOTE" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "PTSECTION" varchar(20),
    "PTNOTE" varchar,
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT", "PTSECTION"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: RANGEHEALTH
DROP TABLE IF EXISTS lmf."RANGEHEALTH" CASCADE;
CREATE TABLE lmf."RANGEHEALTH" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "RILLS" varchar(2),
    "WATER_FLOW_PATTERNS" varchar(2),
    "PEDESTALS_TERRACETTES" varchar(2),
    "BARE_GROUND" varchar(2),
    "GULLIES" varchar(2),
    "WIND_SCOURED_AREAS" varchar(2),
    "LITTER_MOVEMENT" varchar(2),
    "SOIL_SURF_RESIS_EROSION" varchar(2),
    "SOIL_SURFACE_LOSS_DEG" varchar(2),
    "INFILTRATION_RUNOFF" varchar(2),
    "COMPACTION_LAYER" varchar(2),
    "FUNC_STRUCT_GROUPS" varchar(2),
    "PLANT_MORTALITY_DEC" varchar(2),
    "LITTER_AMOUNT" varchar(2),
    "ANNUAL_PRODUCTION" varchar(2),
    "INVASIVE_PLANTS" varchar(2),
    "REPROD_CAPABILITY_PEREN" varchar(2),
    "SOILSITE_STABILITY" varchar(2),
    "BIOTIC_INTEGRITY" varchar(2),
    "HYDROLOGIC_FUNCTION" varchar(2),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: RHSUMMARY
DROP TABLE IF EXISTS lmf."RHSUMMARY" CASCADE;
CREATE TABLE lmf."RHSUMMARY" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "SOILSITE_EXTREME" integer,
    "SOILSITE_MODERATE_TO_EXTREME" integer,
    "SOILSITE_MODERATE" integer,
    "SOILSITE_SLIGHT_TO_MODERATE" integer,
    "SOILSITE_NONE_TO_SLIGHT" integer,
    "BIOTIC_EXTREME" integer,
    "BIOTIC_MODERATE_TO_EXTREME" integer,
    "BIOTIC_MODERATE" integer,
    "BIOTIC_SLIGHT_TO_MODERATE" integer,
    "BIOTIC_NONE_TO_SLIGHT" integer,
    "HYDROLOGIC_EXTREME" integer,
    "HYDROLOGIC_MODERATE_TO_EXTREME" integer,
    "HYDROLOGIC_MODERATE" integer,
    "HYDROLOGIC_SLIGHT_TO_MODERATE" integer,
    "HYDROLOGIC_NONE_TO_SLIGHT" integer,
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: SOILDISAG
DROP TABLE IF EXISTS lmf."SOILDISAG" CASCADE;
CREATE TABLE lmf."SOILDISAG" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "VEG1" varchar(2),
    "VEG2" varchar(2),
    "VEG3" varchar(2),
    "VEG4" varchar(2),
    "VEG5" varchar(2),
    "VEG6" varchar(2),
    "VEG7" varchar(2),
    "VEG8" varchar(2),
    "VEG9" varchar(2),
    "VEG10" varchar(2),
    "VEG11" varchar(2),
    "VEG12" varchar(2),
    "VEG13" varchar(2),
    "VEG14" varchar(2),
    "VEG15" varchar(2),
    "VEG16" varchar(2),
    "VEG17" varchar(2),
    "VEG18" varchar(2),
    "STABILITY1" integer,
    "STABILITY2" integer,
    "STABILITY3" integer,
    "STABILITY4" integer,
    "STABILITY5" integer,
    "STABILITY6" integer,
    "STABILITY7" integer,
    "STABILITY8" integer,
    "STABILITY9" integer,
    "STABILITY10" integer,
    "STABILITY11" integer,
    "STABILITY12" integer,
    "STABILITY13" integer,
    "STABILITY14" integer,
    "STABILITY15" integer,
    "STABILITY16" integer,
    "STABILITY17" integer,
    "STABILITY18" integer,
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);


-- Table: SOILHORIZON
DROP TABLE IF EXISTS lmf."SOILHORIZON" CASCADE;
CREATE TABLE lmf."SOILHORIZON" (
    "SURVEY" integer,
    "STATE" integer,
    "COUNTY" integer,
    "PSU" varchar(7),
    "POINT" integer,
    "SEQNUM" integer,
    "DEPTH" integer,
    "HORIZON_TEXTURE" varchar(4),
    "TEXTURE_MODIFIER" varchar(4),
    "EFFERVESCENCE_CLASS" varchar(2),
    "UNUSUAL_FEATURES" varchar(250),
    PRIMARY KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT", "SEQNUM"),
    FOREIGN KEY ("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    REFERENCES lmf."POINT"("SURVEY", "STATE", "COUNTY", "PSU", "POINT") 
    ON UPDATE CASCADE ON DELETE CASCADE
);
