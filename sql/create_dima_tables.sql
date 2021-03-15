/* Schema created from DIMA 5.5 template. */

CREATE SCHEMA IF NOT EXISTS dima;

--Custom items added for use in combining multiple DIMA's
--These tables and associated triggers act as intermediary shims that allow the foreign key cascading delete of all data withing a certain
--database which has been uploaded.  Primary/foreign key constraints still function normally, that is that primary keys
--cannot be violated and duplicate inserts will either be ignored or updated instead of inserted. 
DROP TABLE IF EXISTS dima.db CASCADE;
CREATE TABLE dima.db (
  dbkey VARCHAR(255) PRIMARY KEY,
  dbpath VARCHAR,
  description VARCHAR,
  md5hash VARCHAR(32) NOT NULL 
);

DROP TABLE IF EXISTS dima.db_site CASCADE;
CREATE TABLE dima.db_site (
  dbkey VARCHAR(255) NOT NULL,
  "SiteKey" VARCHAR(20),
  PRIMARY KEY (dbkey, "SiteKey"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS dima.db_plot CASCADE;
CREATE TABLE dima.db_plot (
  dbkey VARCHAR(255) NOT NULL,
  "PlotKey" VARCHAR(20),
  PRIMARY KEY (dbkey, "PlotKey"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TABLE IF EXISTS dima.db_line CASCADE;
CREATE TABLE dima.db_line (
  dbkey VARCHAR(255) NOT NULL,
  "LineKey" VARCHAR(20),
  PRIMARY KEY (dbkey, "LineKey"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Level 0 tables,  No Foreign Key except db
-- 

--
-- Species
--

--
-- Table structure for table 'tblSpecies'
--

-- TODO create a species base and species delta table using tblSpecies as a base
DROP TABLE IF EXISTS dima."tblSpecies" CASCADE;

CREATE TABLE dima."tblSpecies" (
  dbkey VARCHAR(255) NOT NULL,
  "SpeciesCode" VARCHAR(255) NOT NULL, 
  "ScientificName" VARCHAR(255), 
  "CommonName" VARCHAR(255), 
  "Family" VARCHAR(255), 
  "SortSeq" INTEGER DEFAULT 99, 
  "synonymOf" VARCHAR(255), 
  "GrowthHabitCode" VARCHAR(20), 
  "Duration" VARCHAR(20), 
  "Stabilizing" BOOLEAN, 
  "Invasive" BOOLEAN, 
  "Group" VARCHAR(30), 
  PRIMARY KEY (dbkey, "SpeciesCode"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Table structure for table 'tblSpeciesGeneric'
--

DROP TABLE IF EXISTS dima."tblSpeciesGeneric" CASCADE;

CREATE TABLE dima."tblSpeciesGeneric" (
  dbkey VARCHAR(255),
  "SpeciesCode" VARCHAR(255) NOT NULL, 
  "ScientificName" VARCHAR(255), 
  "CommonName" VARCHAR(255), 
  "Family" VARCHAR(255), 
  "SortSeq" INTEGER, 
  "synonymOf" VARCHAR(255), 
  "GrowthHabitCode" VARCHAR(255), 
  "Duration" VARCHAR(255), 
  "Stabilizing" BOOLEAN, 
  "Group" VARCHAR(255), 
  "DateModified" TIMESTAMP, 
  "DateFound" TIMESTAMP, 
  "PlotFirstFound" VARCHAR(255), 
  "PotentialGenus" VARCHAR(255), 
  "PotenialSpecies" VARCHAR(255), 
  "PotentialSubspecies" VARCHAR(255), 
  "Comments" VARCHAR(255), 
  "PhotosTaken" BOOLEAN, 
  "PhotoNumbers" VARCHAR(255), 
  "SpecimenCollected" BOOLEAN, 
  "IdentifiedTo" VARCHAR(255), 
  "FinalCode" VARCHAR(255), 
  "ChangedInDIMA" BOOLEAN, 
  "petalsNumber" INTEGER, 
  "petalsColor" VARCHAR(255), 
  "petalsSize" VARCHAR(255), 
  "sepalsNumber" INTEGER, 
  "sepalsColor" VARCHAR(255), 
  "sepalsSize" VARCHAR(255), 
  "stamensNumber" INTEGER, 
  "stamensColor" VARCHAR(255), 
  "stamensSize" VARCHAR(255), 
  "glumesPresent" BOOLEAN, 
  "ligulesPresent" BOOLEAN, 
  PRIMARY KEY (dbkey, "SpeciesCode"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Table structure for table 'UnknownTracking'
--
/*
DROP TABLE IF EXISTS dima."UnknownTracking" CASCADE;

CREATE TABLE dima."UnknownTracking" (  -- no primary key in DIMA
  dbkey VARCHAR(255) NOT NULL, 
  "UnknownCode" VARCHAR(255), 
  "DateFound" TIMESTAMP, 
  "PlotFirstFound" VARCHAR(255), 
  "PotentialGenus" VARCHAR(255), 
  "PotenialSpecies" VARCHAR(255), 
  "PotentialSubspecies" VARCHAR(255), 
  "Comments" TEXT, 
  "PhotosTaken" BOOLEAN, 
  "PhotoNumbers" VARCHAR(255), 
  "SpecimenCollected" BOOLEAN, 
  "IdentifiedTo" VARCHAR(255), 
  "FinalCode" VARCHAR(255), 
  "ChangedInDIMA" BOOLEAN, 
  "petalsNumber" INTEGER, 
  "petalsColor" VARCHAR(255), 
  "petalsSize" VARCHAR(255), 
  "sepalsNumber" INTEGER, 
  "sepalsColor" VARCHAR(255), 
  "sepalsSize" VARCHAR(255), 
  "stamensNumber" INTEGER, 
  "stamensColor" VARCHAR(255), 
  "stamensSize" VARCHAR(255), 
  "glumesPresent" BOOLEAN, 
  "ligulesPresent" BOOLEAN,
  PRIMARY KEY (dbkey, "UnknownCode"),  --added
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);
*/


--
-- Table structure for table 'tblSpeciesGroups'
--

DROP TABLE IF EXISTS dima."tblSpeciesGroups" CASCADE;

CREATE TABLE dima."tblSpeciesGroups" (
  dbkey VARCHAR(255) NOT NULL, 
  "RecKey" VARCHAR(50) NOT NULL, 
  "GroupName" VARCHAR(30), 
  "GroupDesc" VARCHAR(255), 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Table structure for table 'tblSpeciesGrowthHabit'
--

DROP TABLE IF EXISTS dima."tblSpeciesGrowthHabit" CASCADE;

CREATE TABLE dima."tblSpeciesGrowthHabit" (
  dbkey VARCHAR(255) NOT NULL, 
  "Code" VARCHAR(20) NOT NULL, 
  "GrowthHabit" VARCHAR(10), 
  "GrowthHabitSub" VARCHAR(20), 
  PRIMARY KEY ("Code"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);


--
-- non-species
--


--
-- Table structure for table 'tblApplicationConstants'
--

DROP TABLE IF EXISTS dima."tblApplicationConstants" CASCADE;

CREATE TABLE dima."tblApplicationConstants" (
  dbkey VARCHAR(255) PRIMARY KEY,  --custom field
  "dbVersion" VARCHAR(10), 
  "dbDate" VARCHAR(10), 
  "ShowPlantLen" BOOLEAN DEFAULT E'0', 
  "dbMode" INTEGER DEFAULT 0,  --1 = "Quantitative Forms only"; 2 = "Qualitative Forms only"; 3 = "Both"
  "ShowStartupScreen" BOOLEAN, 
  "LPIValidateDetails" BOOLEAN,  --Controls whether we do the detailed data validation checks on each LPI detail
  "LPISelectedCodeToTop" BOOLEAN,  --Controls 'Quick' data-entry form - do selected codes go to the top of their list?
  "DirectEntryOfSpecies" BOOLEAN,  --Affects LPI (Main), Production, and Belt forms
  "DirectEntryOfSubPlotSize" BOOLEAN,  --Production form only
  "ModifyEcolSitesWeightings" BOOLEAN, 
  "ModifyRangelandSupportTables" BOOLEAN, 
  "DataEntry" VARCHAR(50),  --1 = "Keyboard/Mouse"; 2 = "Touch-Screen"
  "MostRecentPlot" VARCHAR(50), 
  "PlotCustom1Desc" VARCHAR(50), 
  "PlotCustom2Desc" VARCHAR(50), 
  "PlotCustom3Desc" VARCHAR(50), 
  "PlotCustom4Desc" VARCHAR(50), 
  "PlotCustomLookupLabel1" VARCHAR(50), 
  "PlotCustomLookupLabel2" VARCHAR(50), 
  "PlotCustomLookupLabel3" VARCHAR(10), 
  "PlotCustomTextLabel1" VARCHAR(50), 
  "PlotCustomTextLabel2" VARCHAR(50), 
  "ESDCustom1Desc" VARCHAR(50), 
  "ESDCustom2Desc" VARCHAR(50), 
  "PhotosDirectory" VARCHAR(255), 
  "GenericAG" BOOLEAN, 
  "GenericPG" BOOLEAN, 
  "GenericAF" BOOLEAN, 
  "GenericPF" BOOLEAN, 
  "GenericSH" BOOLEAN, 
  "GenericTR" BOOLEAN, 
  "numGenericSets" INTEGER DEFAULT 10, 
  "CountyNotes" VARCHAR(255), 
  "SpeciesNotes" VARCHAR(255), 
  "PDFPath" VARCHAR(255), 
  "ReportsPath" VARCHAR(255), 
  "NRCSdownloadURL" VARCHAR(255), 
  "ESDdownloadURL" VARCHAR(255), 
  "MLRAdownloadURL" VARCHAR(255), 
  "ShowLPI" BOOLEAN, 
  "ShowGap" BOOLEAN, 
  "ShowSoil" BOOLEAN, 
  "ShowProd" BOOLEAN, 
  "ShowCompact" BOOLEAN, 
  "ShowInfiltration" BOOLEAN, 
  "ShowVegStruct" BOOLEAN, 
  "ShowQual" BOOLEAN, 
  "ShowTreeDen" BOOLEAN, 
  "ShowRiparSurv" BOOLEAN,  --Riparian Channel Vegetation Survey
  "ShowRiparPro" BOOLEAN,  --Riparian Channel and Gully Profile
  "ShowSpecRich" BOOLEAN,  --Species Richness
  "ShowDryWt" BOOLEAN,  --Dry Weight Rank
  "ShowOcularCov" BOOLEAN,  --Ocular Cover Estimate
  "ShowPlantDen" BOOLEAN,  --Plant Density
  "ShowLIC" BOOLEAN,  --Line Intercept Continuous
  "ShowESD" BOOLEAN, 
  "ShowSurfFuel" BOOLEAN, 
  "ShowCanopyGap" BOOLEAN, 
  "ShowNestedFreq" BOOLEAN, 
  "ShowPTFrame" BOOLEAN, 
  "GPSMode" VARCHAR(10), 
  "GPSPort" VARCHAR(10), 
  "GPSBaud" VARCHAR(15), 
  "GPSDatum" VARCHAR(255), 
  "GPSGrid" VARCHAR(255), 
  "GPSToolsDLLName" VARCHAR(50), 
  "GPSToolsDLLFolder" TEXT, 
  "QuickSpeciesTop" TEXT, 
  "QuickSpeciesLower1" TEXT, 
  "QuickSpeciesLower2" TEXT, 
  "QuickSpeciesLower3" TEXT, 
  "QuickSpeciesLower4" TEXT, 
  "QuickSpeciesSurface" TEXT, 
  "BuriedSpeciesCodes" VARCHAR(50), 
  "DefaultHemisphere" VARCHAR(1), 
  "DefaultGPSCoordSys" VARCHAR(50), 
  "DefaultDatum" VARCHAR(50), 
  "DefaultSoilPedonDepthUOM" VARCHAR(50), 
  "ShowPastCond" BOOLEAN,  --Pasture Condition
  "ShowBSNE" BOOLEAN,   --Dust collection boxes
  "ShowUtil" BOOLEAN DEFAULT E'0',  --Utilization 
  "ShowPlotMgt" BOOLEAN DEFAULT E'0',  --Plot Management
  "ShowSageGrouse" BOOLEAN DEFAULT E'0', 
  "QuickSpeciesLower5" TEXT, 
  "QuickSpeciesLower6" TEXT, 
  "QuickSpeciesLower7" TEXT, 
  "QuickSpeciesWoody" TEXT, 
  "QuickSpeciesHerb" TEXT, 
  "OfficeVersion" VARCHAR(255), 
  "tempLineLength" FLOAT NULL DEFAULT 0, 
  "genericStartNum" VARCHAR(5), 
  "QuickSpeciesLowerHerb" TEXT, 
  "GenericSU" BOOLEAN,
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Table structure for table 'tblCounty'
--

DROP TABLE IF EXISTS dima."tblCounty" CASCADE;

CREATE TABLE dima."tblCounty" (
  "State" VARCHAR(255) NOT NULL, 
  "County" VARCHAR(255) NOT NULL, 
  "BLM County Code" VARCHAR(255), 
  "FIPS" VARCHAR(255), 
  PRIMARY KEY ("State", "County")
);

--
-- Table structure for table 'tblEcolSites'
--

DROP TABLE IF EXISTS dima."tblEcolSites" CASCADE;

CREATE TABLE dima."tblEcolSites" (  --no primary key in DIMA
  dbkey VARCHAR(255) NOT NULL,
  "EcolSite" VARCHAR(50), 
  "SiteName" VARCHAR(255), 
  "DateModified" TIMESTAMP, 
  "DateComplete" DATE, 
  "SSS1" FLOAT NULL DEFAULT 1, 
  "HF1" FLOAT NULL DEFAULT 1, 
  "BI1" FLOAT NULL DEFAULT 0, 
  "SSS2" FLOAT NULL DEFAULT 1, 
  "HF2" FLOAT NULL DEFAULT 1, 
  "BI2" FLOAT NULL DEFAULT 0, 
  "SSS3" FLOAT NULL DEFAULT 1, 
  "HF3" FLOAT NULL DEFAULT 1, 
  "BI3" FLOAT NULL DEFAULT 0, 
  "SSS4" FLOAT NULL DEFAULT 1, 
  "HF4" FLOAT NULL DEFAULT 1, 
  "BI4" FLOAT NULL DEFAULT 0, 
  "SSS5" FLOAT NULL DEFAULT 1, 
  "HF5" FLOAT NULL DEFAULT 1, 
  "BI5" FLOAT NULL DEFAULT 0, 
  "SSS6" FLOAT NULL DEFAULT 1, 
  "HF6" FLOAT NULL DEFAULT 0, 
  "BI6" FLOAT NULL DEFAULT 0, 
  "SSS7" FLOAT NULL DEFAULT 1, 
  "HF7" FLOAT NULL DEFAULT 0, 
  "BI7" FLOAT NULL DEFAULT 0, 
  "SSS8" FLOAT NULL DEFAULT 1, 
  "HF8" FLOAT NULL DEFAULT 1, 
  "BI8" FLOAT NULL DEFAULT 1, 
  "SSS9" FLOAT NULL DEFAULT 1, 
  "HF9" FLOAT NULL DEFAULT 1, 
  "BI9" FLOAT NULL DEFAULT 1, 
  "SSS10" FLOAT NULL DEFAULT 0, 
  "HF10" FLOAT NULL DEFAULT 1, 
  "BI10" FLOAT NULL DEFAULT 0, 
  "SSS11" FLOAT NULL DEFAULT 1, 
  "HF11" FLOAT NULL DEFAULT 1, 
  "BI11" FLOAT NULL DEFAULT 1, 
  "SSS12" FLOAT NULL DEFAULT 0, 
  "HF12" FLOAT NULL DEFAULT 0, 
  "BI12" FLOAT NULL DEFAULT 1, 
  "SSS13" FLOAT NULL DEFAULT 0, 
  "HF13" FLOAT NULL DEFAULT 0, 
  "BI13" FLOAT NULL DEFAULT 1, 
  "SSS14" FLOAT NULL DEFAULT 0, 
  "HF14" FLOAT NULL DEFAULT 1, 
  "BI14" FLOAT NULL DEFAULT 1, 
  "SSS15" FLOAT NULL DEFAULT 0, 
  "HF15" FLOAT NULL DEFAULT 0, 
  "BI15" FLOAT NULL DEFAULT 1, 
  "SSS16" FLOAT NULL DEFAULT 0, 
  "HF16" FLOAT NULL DEFAULT 0, 
  "BI16" FLOAT NULL DEFAULT 1, 
  "SSS17" FLOAT NULL DEFAULT 0, 
  "HF17" FLOAT NULL DEFAULT 0, 
  "BI17" FLOAT NULL DEFAULT 1, 
  "SSS18" FLOAT NULL DEFAULT 0, 
  "HF18" FLOAT NULL DEFAULT 0, 
  "BI18" FLOAT NULL DEFAULT 0, 
  "SSS19" FLOAT NULL DEFAULT 0, 
  "HF19" FLOAT NULL DEFAULT 0, 
  "BI19" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("EcolSite"),  -- added
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Table structure for table 'tblGISDatums'
--

DROP TABLE IF EXISTS dima."tblGISDatums" CASCADE;

CREATE TABLE dima."tblGISDatums" (
  "DatumCode" VARCHAR(255) NOT NULL, 
  "DatumDesc" VARCHAR(255), 
  "Axis" DOUBLE PRECISION NULL, 
  "Flattening" DOUBLE PRECISION NULL, 
  "DX" DOUBLE PRECISION NULL, 
  "DY" DOUBLE PRECISION NULL, 
  "DZ" DOUBLE PRECISION NULL, 
  UNIQUE ("DatumDesc"), 
  PRIMARY KEY ("DatumCode")
);

--
-- Table structure for table 'tblKMLFields'
--

DROP TABLE IF EXISTS dima."tblKMLFields" CASCADE;

CREATE TABLE dima."tblKMLFields" (  --no primary key in DIMA
  "SortSeq" INTEGER DEFAULT 0, 
  "FieldDesc" VARCHAR(50), 
  "FieldName" VARCHAR(50), 
  "ShowIt" BOOLEAN DEFAULT E'0',
  PRIMARY KEY ("FieldName")  --added
);

--
-- Table structure for table 'tblLowerCanopy'
--

DROP TABLE IF EXISTS dima."tblLowerCanopy" CASCADE;

CREATE TABLE dima."tblLowerCanopy" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "CommonName" VARCHAR(255), 
  "SortSeq" INTEGER DEFAULT 0, 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblLPILowerCodes'
--

DROP TABLE IF EXISTS dima."tblLPILowerCodes" CASCADE;

CREATE TABLE dima."tblLPILowerCodes" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "SortSeq" INTEGER DEFAULT 0, 
  "Selected" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblLPIMeasures'
--

DROP TABLE IF EXISTS dima."tblLPIMeasures" CASCADE;

CREATE TABLE dima."tblLPIMeasures" (  --no primary key in DIMA
  "Measure" SMALLINT NOT NULL DEFAULT 0, 
  "SortSeq" SMALLINT DEFAULT 0, 
  "SpacingInterval" FLOAT NULL DEFAULT 0, 
  "SpacingIntervalType" VARCHAR(50),
  PRIMARY KEY ("Measure", "SpacingInterval", "SpacingIntervalType")  --added
);

--
-- Table structure for table 'tblLPIOtherCodes'
--

DROP TABLE IF EXISTS dima."tblLPIOtherCodes" CASCADE;

CREATE TABLE dima."tblLPIOtherCodes" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "SortSeq" INTEGER DEFAULT 0, 
  "Selected" BOOLEAN DEFAULT E'0', 
  "Category" VARCHAR(5),  --H = Herbaceous
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblMaintBedrock'
--

DROP TABLE IF EXISTS dima."tblMaintBedrock" CASCADE;

CREATE TABLE dima."tblMaintBedrock" (
  "PDPNASIS" VARCHAR(255) NOT NULL, 
  "Kind" VARCHAR(255), 
  "SpecificKind" VARCHAR(255), 
  "PDPCode" VARCHAR(255), 
  PRIMARY KEY ("PDPNASIS")
);

--
-- Table structure for table 'tblMaintCarbonateStage'
--

DROP TABLE IF EXISTS dima."tblMaintCarbonateStage" CASCADE;

CREATE TABLE dima."tblMaintCarbonateStage" (
  "Stage" DOUBLE PRECISION NOT NULL, 
  "Gravelly" VARCHAR(255), 
  "Nongravelly" VARCHAR(255), 
  PRIMARY KEY ("Stage")
);

--
-- Table structure for table 'tblMaintDKClass'
--

DROP TABLE IF EXISTS dima."tblMaintDKClass" CASCADE;

CREATE TABLE dima."tblMaintDKClass" (
  "DKClass" VARCHAR(255) NOT NULL, 
  "DKDesc" VARCHAR(255), 
  "DKRange" VARCHAR(255), 
  "DKmid" VARCHAR(255), 
  "SortSeq" INTEGER, 
  PRIMARY KEY ("DKClass")
);

--
-- Table structure for table 'tblMaintErosionPatternClass'
--

DROP TABLE IF EXISTS dima."tblMaintErosionPatternClass" CASCADE;

CREATE TABLE dima."tblMaintErosionPatternClass" (
  "Class" VARCHAR(255) NOT NULL, 
  "Description" VARCHAR(255), 
  PRIMARY KEY ("Class")
);

--
-- Table structure for table 'tblMaintESDFragmentTypes'
--

DROP TABLE IF EXISTS dima."tblMaintESDFragmentTypes" CASCADE;

CREATE TABLE dima."tblMaintESDFragmentTypes" (
  "Id" SERIAL NOT NULL, 
  "Abbrev" VARCHAR(50), 
  "Definition" VARCHAR(50), 
  PRIMARY KEY ("Id")
);

--
-- Table structure for table 'tblMaintESDRupture'
--

DROP TABLE IF EXISTS dima."tblMaintESDRupture" CASCADE;

CREATE TABLE dima."tblMaintESDRupture" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "DryMoistBoth" VARCHAR(255), 
  "SortSeq" SMALLINT DEFAULT 0, 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblMaintGeomorphComp'
--

DROP TABLE IF EXISTS dima."tblMaintGeomorphComp" CASCADE;

CREATE TABLE dima."tblMaintGeomorphComp" (
  "Code" VARCHAR(255) NOT NULL, 
  "Landform" VARCHAR(255), 
  "Geomorphic_component" VARCHAR(255), 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblMaintHorizons'
--

DROP TABLE IF EXISTS dima."tblMaintHorizons" CASCADE;

CREATE TABLE dima."tblMaintHorizons" (  --no primary key in DIMA
  "Horizon" VARCHAR(255), 
  "SortSeq" INTEGER,
  PRIMARY KEY ("Horizon") --added
);

--
-- Table structure for table 'tblMaintLandform'
--

DROP TABLE IF EXISTS dima."tblMaintLandform" CASCADE;

CREATE TABLE dima."tblMaintLandform" (
  "Landform" VARCHAR(50) NOT NULL, 
  PRIMARY KEY ("Landform")
);


--
-- Table structure for table 'tblMaintMinerologyClasses'
--

DROP TABLE IF EXISTS dima."tblMaintMinerologyClasses" CASCADE;

CREATE TABLE dima."tblMaintMinerologyClasses" (
  "Class" VARCHAR(255) NOT NULL, 
  PRIMARY KEY ("Class")
);

--
-- Table structure for table 'tblMaintNASIS'
--

DROP TABLE IF EXISTS dima."tblMaintNASIS" CASCADE;

CREATE TABLE dima."tblMaintNASIS" (
  "NASIS" VARCHAR(255) NOT NULL, 
  "Class" VARCHAR(255), 
  "Granular" VARCHAR(255), 
  "Platy" VARCHAR(255), 
  "Columnar/Prismatic/Wedge" VARCHAR(255), 
  "Angular/Subangular Blocky" VARCHAR(255), 
  "SortSeq" SMALLINT, 
  PRIMARY KEY ("NASIS")
);

--
-- Table structure for table 'tblMaintNearestPerennial'
--

DROP TABLE IF EXISTS dima."tblMaintNearestPerennial" CASCADE;

CREATE TABLE dima."tblMaintNearestPerennial" (
  "SortSeq" VARCHAR(5) NOT NULL, 
  "DescriptionEnglish" VARCHAR(12), 
  "AvgFactorEnglish" FLOAT NULL DEFAULT 0, 
  "DescriptionMetric" VARCHAR(12), 
  "AvgFactorMetric" FLOAT NULL DEFAULT 0, 
  PRIMARY KEY ("SortSeq")
);

--
-- Table structure for table 'tblMaintParentMaterial'
--

DROP TABLE IF EXISTS dima."tblMaintParentMaterial" CASCADE;

CREATE TABLE dima."tblMaintParentMaterial" (
  "NASISCode" VARCHAR(255) NOT NULL, 
  "Kind" VARCHAR(255), 
  "SpecificKind" VARCHAR(255), 
  "PDPCode" VARCHAR(255), 
  PRIMARY KEY ("NASISCode")
);


--
-- Table structure for table 'tblMaintParticleSizes'
--

DROP TABLE IF EXISTS dima."tblMaintParticleSizes" CASCADE;

CREATE TABLE dima."tblMaintParticleSizes" (
  "ParticleSize" VARCHAR(255) NOT NULL, 
  PRIMARY KEY ("ParticleSize")
);

--
-- Table structure for table 'tblMaintPlotTags'
--

DROP TABLE IF EXISTS dima."tblMaintPlotTags" CASCADE;

CREATE TABLE dima."tblMaintPlotTags" (  --no primary key in DIMA
  "Tag" VARCHAR(50), 
  "Baseline" BOOLEAN DEFAULT E'0',
  PRIMARY KEY ("Tag") --added
);

--
-- Table structure for table 'tblMaintPosition'
--

DROP TABLE IF EXISTS dima."tblMaintPosition" CASCADE;

CREATE TABLE dima."tblMaintPosition" (
  "SortSeq" SMALLINT NOT NULL DEFAULT 0, 
  "Position" VARCHAR(50), 
  PRIMARY KEY ("SortSeq")
);

--
-- Table structure for table 'tblMaintQualIndicators'
--

DROP TABLE IF EXISTS dima."tblMaintQualIndicators" CASCADE;

CREATE TABLE dima."tblMaintQualIndicators" (
  "Seq" SMALLINT NOT NULL, 
  "Description" VARCHAR(255), 
  "SSS" BOOLEAN, 
  "HF" BOOLEAN, 
  "BI" BOOLEAN, 
  PRIMARY KEY ("Seq")
);

--
-- Table structure for table 'tblMaintQualRatings'
--

DROP TABLE IF EXISTS dima."tblMaintQualRatings" CASCADE;

CREATE TABLE dima."tblMaintQualRatings" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(255), 
  "Rating" SMALLINT DEFAULT 0, 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblMaintResourceRetentionClasses'
--

DROP TABLE IF EXISTS dima."tblMaintResourceRetentionClasses" CASCADE;

CREATE TABLE dima."tblMaintResourceRetentionClasses" (
  "Class" INTEGER NOT NULL DEFAULT 0, 
  "Description" VARCHAR(255),  
  PRIMARY KEY ("Class")
);

--
-- Table structure for table 'tblMaintSlopeShape'
--

DROP TABLE IF EXISTS dima."tblMaintSlopeShape" CASCADE;

CREATE TABLE dima."tblMaintSlopeShape" (
  "SlopeSymbol" VARCHAR(10) NOT NULL, 
  "VerticalShape" VARCHAR(50), 
  "HorizontalShape" VARCHAR(50), 
  "SortSeq" SMALLINT DEFAULT 0, 
  PRIMARY KEY ("SlopeSymbol")
);

--
-- Table structure for table 'tblMaintSoilRedistributionClass'
--

DROP TABLE IF EXISTS dima."tblMaintSoilRedistributionClass" CASCADE;

CREATE TABLE dima."tblMaintSoilRedistributionClass" (
  "Class" VARCHAR(5) NOT NULL, 
  "Description" VARCHAR(255), 
  PRIMARY KEY ("Class")
);

--
-- Table structure for table 'tblMaintSoilStability'
--

DROP TABLE IF EXISTS dima."tblMaintSoilStability" CASCADE;

CREATE TABLE dima."tblMaintSoilStability" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "CommonName" VARCHAR(255), 
  "SortSeq" SMALLINT DEFAULT 0, 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblMaintSoilTempClasses'
--

DROP TABLE IF EXISTS dima."tblMaintSoilTempClasses" CASCADE;

CREATE TABLE dima."tblMaintSoilTempClasses" (
  "Class" VARCHAR(255) NOT NULL, 
  PRIMARY KEY ("Class")
);

--
-- Table structure for table 'tblMaintSoilTexture'
--

DROP TABLE IF EXISTS dima."tblMaintSoilTexture" CASCADE;

CREATE TABLE dima."tblMaintSoilTexture" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "SortSeq" SMALLINT DEFAULT 0, 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblMaintStructureShapes'
--

DROP TABLE IF EXISTS dima."tblMaintStructureShapes" CASCADE;

CREATE TABLE dima."tblMaintStructureShapes" (
  "Abbrev" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "SortSeq" INTEGER NOT NULL DEFAULT 0,  
  UNIQUE ("Description"), 
  UNIQUE ("SortSeq"),
  PRIMARY KEY ("Abbrev") 
);

--
-- Table structure for table 'tblMaintSurfaceSoilProperties'
--

DROP TABLE IF EXISTS dima."tblMaintSurfaceSoilProperties" CASCADE;

CREATE TABLE dima."tblMaintSurfaceSoilProperties" (
  "Abbrev" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(255), 
  "Sort" INTEGER DEFAULT 0, 
  PRIMARY KEY ("Abbrev")
);

--
-- Table structure for table 'tblMethods'
--

DROP TABLE IF EXISTS dima."tblMethods" CASCADE;

CREATE TABLE dima."tblMethods" (
  "Code" VARCHAR(255) NOT NULL, 
  "SortSeq" VARCHAR(50), 
  "PageNo" SMALLINT, 
  "Desc" VARCHAR(255), 
  "ShowLine" VARCHAR(50), 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblNoneSpecies'
--

DROP TABLE IF EXISTS dima."tblNoneSpecies" CASCADE;

CREATE TABLE dima."tblNoneSpecies" (
  "SpeciesCode" VARCHAR(10) NOT NULL,  --this table consists of one record, whose code is ' NONE'.  It is required for display of Top Canopy codes in the LPI form
  "ScientificName" VARCHAR(255), 
  "CommonName" VARCHAR(255), 
  "SortSeq" INTEGER DEFAULT 0, 
  PRIMARY KEY ("SpeciesCode")
);

--
-- Table structure for table 'tblOwnership'
--

DROP TABLE IF EXISTS dima."tblOwnership" CASCADE;

CREATE TABLE dima."tblOwnership" (
  "Code" VARCHAR(255) NOT NULL, 
  "Description" VARCHAR(255), 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblPDFs'
--

DROP TABLE IF EXISTS dima."tblPDFs" CASCADE;

CREATE TABLE dima."tblPDFs" (
  "SortSeq" INTEGER NOT NULL DEFAULT 0, 
  "FileKey" VARCHAR(10), 
  "FileDesc" VARCHAR(250), 
  "PDFName" VARCHAR(255), 
  "filePath" VARCHAR(255), 
  "ShortFileName" VARCHAR(255) DEFAULT E'0', 
  UNIQUE ("FileKey"), 
  PRIMARY KEY ("SortSeq")
);


--
-- Table structure for table 'tblPeople'
--

DROP TABLE IF EXISTS dima."tblPeople" CASCADE;

CREATE TABLE dima."tblPeople" (
  dbkey VARCHAR(255) NOT NULL,
  "FullName" VARCHAR(50) NOT NULL, 
  "Organization" VARCHAR(50), 
  "Address" VARCHAR(255), 
  "PhoneNbr" VARCHAR(50), 
  "Email" VARCHAR(50), 
  "Recorder" BOOLEAN DEFAULT E'0', 
  "Observer" BOOLEAN DEFAULT E'0', 
  "LandManager" BOOLEAN DEFAULT E'0', 
  "Designer" BOOLEAN DEFAULT E'0', 
  "FieldCrewLeader" BOOLEAN DEFAULT E'0', 
  "DataEntry" BOOLEAN DEFAULT E'0', 
  "DataErrorChecking" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("FullName"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Table structure for table 'tblPhotos'
--

DROP TABLE IF EXISTS dima."tblPhotos" CASCADE;

CREATE TABLE dima."tblPhotos" (
  dbkey VARCHAR(255) NOT NULL,
  "RecKey" VARCHAR(50), 
  "PhotoKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "PhotoDesc" VARCHAR(150), 
  "RecType" VARCHAR(8),  --1 = 'Plot' or 2 = 'Line'
  "PhotoDate" DATE, 
  "LocationType" VARCHAR(50), --1 = free-form description, 2 = file stored on this copmputer, 3 = file stored on another computer
  "LocationInfo" VARCHAR(250), 
  PRIMARY KEY ("PhotoKey"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Table structure for table 'tblPlotCustomLookup1'
--

DROP TABLE IF EXISTS dima."tblPlotCustomLookup1";

CREATE TABLE dima."tblPlotCustomLookup1" (
  "CustomCode" VARCHAR(50) NOT NULL, 
  "CustomDesc1" VARCHAR(50), 
  "CustomDesc2" VARCHAR(50), 
  PRIMARY KEY ("CustomCode")
);

--
-- Table structure for table 'tblPlotCustomLookup2'
--

DROP TABLE IF EXISTS dima."tblPlotCustomLookup2";

CREATE TABLE dima."tblPlotCustomLookup2" (
  "CustomCode" VARCHAR(50) NOT NULL, 
  "CustomDesc1" VARCHAR(50), 
  "CustomDesc2" VARCHAR(50), 
  PRIMARY KEY ("CustomCode")
);

--
-- Table structure for table 'tblPlotCustomLookup3'
--

DROP TABLE IF EXISTS dima."tblPlotCustomLookup3";

CREATE TABLE dima."tblPlotCustomLookup3" (
  "CustomCode" VARCHAR(50) NOT NULL, 
  "CustomDesc1" VARCHAR(50), 
  "CustomDesc2" VARCHAR(50),  
  PRIMARY KEY ("CustomCode")
);

--
-- Table structure for table 'tblSageThresholds'
--

DROP TABLE IF EXISTS dima."tblSageThresholds" CASCADE;

CREATE TABLE dima."tblSageThresholds" (  --no primary key in DIMA
  "SGKey" INTEGER DEFAULT 0, 
  "SG_Form" VARCHAR(5), 
  "SortSeq" INTEGER DEFAULT 0, 
  "Desc" VARCHAR(255), 
  "Suitable" VARCHAR(255), 
  "Marginal" VARCHAR(255), 
  "Unsuitable" VARCHAR(255),
  PRIMARY KEY ("SGKey")  --added
);

--
-- Table structure for table 'tblSoilSurface'
--

DROP TABLE IF EXISTS dima."tblSoilSurface" CASCADE;

CREATE TABLE dima."tblSoilSurface" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "CommonName" VARCHAR(255), 
  "SortSeq" INTEGER DEFAULT 0, 
  "Selected" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblSoilSurfaceOcular'
--

DROP TABLE IF EXISTS dima."tblSoilSurfaceOcular" CASCADE;

CREATE TABLE dima."tblSoilSurfaceOcular" (
  "Code" VARCHAR(50) NOT NULL, 
  "Description" VARCHAR(50), 
  "CommonName" VARCHAR(255), 
  "SortSeq" INTEGER DEFAULT 0, 
  PRIMARY KEY ("Code")
);

--
-- Table structure for table 'tblStateMLRAs'
--

DROP TABLE IF EXISTS dima."tblStateMLRAs" CASCADE;

CREATE TABLE dima."tblStateMLRAs" (  --no primary key in DIMA
  "ST" VARCHAR(255), 
  "MLRA" VARCHAR(255), 
  "CRA" VARCHAR(255),
  PRIMARY KEY ("ST", "MLRA", "CRA")  --added
);

--
-- Level 1, FK to Level 0
--

--
-- Table structure for table 'tblSites'
--

DROP TABLE IF EXISTS dima."tblSites" CASCADE;

CREATE TABLE dima."tblSites" (
  "SiteKey" VARCHAR(20) NOT NULL,   --Computed YYMMDDHHMMSS + random number < 9999
  "DateModified" TIMESTAMP, 
  "SiteID" VARCHAR(40), 
  "SiteName" VARCHAR(70), 
  "Ownership" VARCHAR(50), 
  "ContactName" VARCHAR(75), 
  "MgtObject" TEXT, 
  "MonObject" TEXT, 
  "Notes" TEXT, 
  "uploaded" BOOLEAN DEFAULT E'0',   --to SQL Server
  PRIMARY KEY ("SiteKey") 
  -- tblOwnership not present in Terradat data. Disabling foreign key for now
  -- FOREIGN KEY ("Ownership") REFERENCES dima."tblOwnership"("Code") ON UPDATE CASCADE
);

--
-- Level 2, FK to Level 0,1
--

--
-- Table structure for table 'tblPlots'
--

DROP TABLE IF EXISTS dima."tblPlots" CASCADE;

CREATE TABLE dima."tblPlots" (
  "SiteKey" VARCHAR(20), 
  "PlotKey" VARCHAR(20) NOT NULL,  --Computed YYMMDDHHMMSS + random number < 9999
  "DateModified" TIMESTAMP, 
  "PlotID" VARCHAR(30), 
  "genericPlot" BOOLEAN DEFAULT E'0', 
  "EstablishDate" DATE, 
  "State" VARCHAR(10), 
  "County" VARCHAR(50), 
  "Directions" TEXT, 
  "AvgPrecip" FLOAT NULL DEFAULT 0, 
  "AvgPrecipUOM" VARCHAR(5) DEFAULT E'0', 
  "EcolSite" VARCHAR(50),  --this is the full Ecological Site number (i.e. R025XY019NV)
  "EcolSiteMLRA" VARCHAR(5),  --i.e. R025
  "EcolSiteSubMLRA" VARCHAR(5),  --i.e XY
  "EcolSiteNum" VARCHAR(5),  --i.e. 019
  "EcolSiteState" VARCHAR(5),  --i.e. NV
  "Soil" VARCHAR(50), 
  "ParentMaterial" VARCHAR(50), 
  "Slope" FLOAT NULL DEFAULT 0, 
  "Aspect" VARCHAR(5), 
  "ESD_SlopeShape" VARCHAR(50), 
  "LandscapeType" VARCHAR(50), 
  "LandscapeTypeSecondary" VARCHAR(50), 
  "MgtUnit" VARCHAR(50), 
  "GPSCoordSys" VARCHAR(50), 
  "Datum" VARCHAR(10), 
  "Zone" VARCHAR(5), 
  "Easting" DOUBLE PRECISION NULL DEFAULT 0, 
  "Northing" DOUBLE PRECISION NULL DEFAULT 0, 
  "Elevation" FLOAT NULL DEFAULT 0, 
  "ElevationType" SMALLINT DEFAULT 0, 
  "RecentWeatherPast12" VARCHAR(10), 
  "RecentWeatherPrevious12" VARCHAR(10), 
  "DisturbWildfire" BOOLEAN, 
  "DisturbRodents" BOOLEAN, 
  "DisturbMammals" BOOLEAN, 
  "DisturbWater" BOOLEAN, 
  "DisturbWind" BOOLEAN, 
  "DisturbWaterSoilDep" BOOLEAN, 
  "DisturbWindSoilDep" BOOLEAN, 
  "DisturbUndgroundUtils" BOOLEAN, 
  "DisturbOverhdTransLines" BOOLEAN, 
  "DisturbOther" BOOLEAN, 
  "DisturbOtherDesc" VARCHAR(50), 
  "WildlifeUse" VARCHAR(250), 
  "MgtHistory" VARCHAR(255), 
  "OffsiteInfluences" VARCHAR(255), 
  "Comments" VARCHAR(255), 
  "SpeciesList" TEXT, 
  "DensityList" TEXT, 
  "MapUnitComponent" VARCHAR(255), 
  "SoilPhase" VARCHAR(255),  
  "ESD_MLRA" VARCHAR(50), 
  "ESD_CRA" VARCHAR(15),  
  "ESD_Region" VARCHAR(50), 
  "ESD_Investigators" VARCHAR(255), 
  "ESD_Bedrock" VARCHAR(50), 
  "ESD_MajorLandform" VARCHAR(255), 
  "ESD_ComponentLandform" VARCHAR(50), 
  "HillslopeType" VARCHAR(50), 
  "ESD_GeomorphicComp" VARCHAR(8), 
  "ESD_RunIn_RunOff" VARCHAR(50), 
  "ESD_SlopeComplexity" VARCHAR(255),  
  "ESD_LitterClass" VARCHAR(50), 
  "ESD_BiologicalCrustClass" VARCHAR(50),  
  "ESD_Series" VARCHAR(50), 
  "ESD_ParticleSizeClass" VARCHAR(50), 
  "ESD_Mineralogy" VARCHAR(50), 
  "ESD_SoilTempRegime" VARCHAR(50), 
  "ESD_DepthClass" VARCHAR(50), 
  "ESD_Subgroup" VARCHAR(50), 
  "ESD_Greatgroup" VARCHAR(50), 
  "ESD_Reaction" VARCHAR(50), 
  "ESD_SoilMoistureRegime" VARCHAR(50), 
  "ESD_CationExchangeActivityClass" VARCHAR(50), 
  "ESD_Epipedon" VARCHAR(50), 
  "ESD_Subsurface_features" VARCHAR(250), 
  "ESD_Depth_to_root_horizon" FLOAT NULL, 
  "ESD_Type_root_horizon" VARCHAR(50), 
  "ESD_Horizon_notes" VARCHAR(250), 
  "ESD_TrackingID" VARCHAR(50), 
  "EcolSite_Assoc1" VARCHAR(50), 
  "EcolSite_Assoc2" VARCHAR(50), 
  "EcolSite_Assoc3" VARCHAR(50), 
  "EcolSite_Similar1" VARCHAR(50), 
  "EcolSite_Similar2" VARCHAR(50), 
  "EcolSite_Similar3" VARCHAR(50), 
  "EcolSite_Notes" VARCHAR(255), 
  "EcolSite_Lookup1" VARCHAR(100), 
  "EcolSite_Lookup2" VARCHAR(100), 
  "EcolSite_Lookup3" VARCHAR(100), 
  "EcolSite_Text1" VARCHAR(100), 
  "EcolSite_Text2" VARCHAR(100), 
  "EcolSite_Text3" VARCHAR(100), 
  "ESD_RecentWeatherPast12" VARCHAR(10), 
  "ESD_ErosionPatternClass" VARCHAR(5),  
  "Longitude" DOUBLE PRECISION NULL DEFAULT 0,  --primary Longitude for the Plot
  "Latitude" DOUBLE PRECISION NULL DEFAULT 0,  --primary Latitude for the Plot
  "CoordLabel1" VARCHAR(20),  --label for supplemental coordinates 1
  "CoordDistance1" VARCHAR(20),  --Distance for supplemental coordinates 1
  "Longitude1" DOUBLE PRECISION NULL DEFAULT 0, 
  "Latitude1" DOUBLE PRECISION NULL DEFAULT 0, 
  "Easting1" DOUBLE PRECISION NULL DEFAULT 0, 
  "Northing1" DOUBLE PRECISION NULL DEFAULT 0, 
  "CoordLabel2" VARCHAR(20), 
  "CoordDistance2" VARCHAR(20), 
  "Longitude2" DOUBLE PRECISION NULL DEFAULT 0, 
  "Latitude2" DOUBLE PRECISION NULL DEFAULT 0, 
  "Easting2" DOUBLE PRECISION NULL DEFAULT 0, 
  "Northing2" DOUBLE PRECISION NULL DEFAULT 0, 
  "CoordLabel3" VARCHAR(20), 
  "CoordDistance3" VARCHAR(20), 
  "Longitude3" DOUBLE PRECISION NULL DEFAULT 0, 
  "Latitude3" DOUBLE PRECISION NULL DEFAULT 0, 
  "Easting3" DOUBLE PRECISION NULL DEFAULT 0, 
  "Northing3" DOUBLE PRECISION NULL DEFAULT 0, 
  PRIMARY KEY ("PlotKey"),
  FOREIGN KEY ("SiteKey") REFERENCES dima."tblSites"("SiteKey") ON DELETE CASCADE ON UPDATE CASCADE--,
  -- Neither tblCounty nor tblEcosites exist in terradat data. Disabling for now...
  --FOREIGN KEY ("State", "County") REFERENCES dima."tblCounty"("State", "County") ON UPDATE CASCADE,
  --FOREIGN KEY ("EcolSite") REFERENCES dima."tblEcolSites"("EcolSite") ON UPDATE CASCADE
);

--
-- Level 3, FK to 0,1,2
--

--
-- Table structure for table 'tblBSNE_Stack'
--

DROP TABLE IF EXISTS dima."tblBSNE_Stack" CASCADE;

CREATE TABLE dima."tblBSNE_Stack" (
  dbkey VARCHAR(255) NOT NULL,  --custom field
  "PlotKey" VARCHAR(50), 
  "StackID" VARCHAR(50) NOT NULL, 
  "DateEstablished" DATE, 
  "Location" VARCHAR(100),  --to help them identify it
  "Notes" TEXT, 
  "ItemType" VARCHAR(1),  --T = TrapID; M = Stack
  "trapOpeningArea" FLOAT NULL, 
  PRIMARY KEY ("StackID"), 
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblCompactHeader'
--

DROP TABLE IF EXISTS dima."tblCompactHeader" CASCADE;

CREATE TABLE dima."tblCompactHeader" (
  dbkey VARCHAR(255) NOT NULL,  -- custom field
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(25) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(8) DEFAULT E'0', 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "HammerHeight" INTEGER DEFAULT 0,  --cm or in
  "HammerHeightUOM" VARCHAR(5), 
  "NbrDepth" SMALLINT DEFAULT 0, 
  "CompactPenUsed" BOOLEAN, 
  "CompactPenReps" SMALLINT DEFAULT 0, 
  "SoilMoistSurf" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist5cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist10cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist15cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist20cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist25cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist30cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist35cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist40cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "CommentStrike" VARCHAR(255), 
  "StrikeNCTotals" VARCHAR(255) DEFAULT E'0', 
  "StrikeVegTotals" VARCHAR(255) DEFAULT E'0', 
  "StrikeAllTotals" VARCHAR(255) DEFAULT E'0', 
  "StrikeRatioTotals" VARCHAR(255) DEFAULT E'0', 
  "PenNCTotals" VARCHAR(255) DEFAULT E'0', 
  "PenVegTotals" VARCHAR(255) DEFAULT E'0', 
  "PenAllTotals" VARCHAR(255) DEFAULT E'0', 
  "CommentPen" VARCHAR(255), 
  PRIMARY KEY ("RecKey"), 
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblDKHeader'
-- Method: Medium and High Intensity Ecological Inventory
-- Tab: Plant Composition
-- Form: Domin-Krajina and Line-Point Intercept Summary Data 
--

DROP TABLE IF EXISTS dima."tblDKHeader" CASCADE;

CREATE TABLE dima."tblDKHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5), 
  "FormDate" DATE, 
  "Observers" VARCHAR(255), 
  "All_PGs_DK_CoverClass" VARCHAR(50), 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("All_PGs_DK_CoverClass") REFERENCES dima."tblMaintDKClass"("DKClass") ON UPDATE CASCADE,
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblESDDominantPerennialHeight'
--

DROP TABLE IF EXISTS dima."tblESDDominantPerennialHeight" CASCADE;

CREATE TABLE dima."tblESDDominantPerennialHeight" (  --no primary key in DIMA
  dbkey VARCHAR(255) NOT NULL,
  "PlotKey" VARCHAR(50), 
  "Species" VARCHAR(50), 
  "Avg_height" INTEGER, 
  "Units" VARCHAR(50), 
  "Grazed/Browsed?" BOOLEAN, 
  "Notes" VARCHAR(50),
  PRIMARY KEY ("PlotKey", "Species"),  --added
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblESDWaypoints'
--

DROP TABLE IF EXISTS dima."tblESDWaypoints" CASCADE;

CREATE TABLE dima."tblESDWaypoints" (
  dbkey VARCHAR(255) NOT NULL,
  "PlotKey" VARCHAR(20) NOT NULL, 
  "PointKey" VARCHAR(20) NOT NULL DEFAULT E'0',  --Computed YYMMDDHHMMSS + random number < 9999
  "ObservationDate" DATE, 
  "Position" VARCHAR(30), 
  "Northing" DOUBLE PRECISION NULL DEFAULT 0, 
  "Easting" DOUBLE PRECISION NULL DEFAULT 0, 
  "Elevation" FLOAT NULL DEFAULT 0, 
  "ElevationType" SMALLINT DEFAULT 0, 
  "SoilMapUnit" VARCHAR(50), 
  "CommunityDescription" VARCHAR(255), 
  "EcolSiteID" VARCHAR(50), 
  "EcolSiteName" VARCHAR(100), 
  "EcolSiteState" VARCHAR(100), 
  "PlantSpecies1" VARCHAR(255), 
  "Notes" VARCHAR(255), 
  "Custom1" VARCHAR(250), 
  "Custom2" VARCHAR(250), 
  "PhotoLink" VARCHAR(255), 
  "TrackingID" VARCHAR(50), 
  "Slope" FLOAT NULL DEFAULT 0, 
  "Aspect" VARCHAR(5), 
  "PedodermClass" VARCHAR(8), 
  "ESD_ResourceRetentionClass" SMALLINT, 
  "ESD_bareGapPatchSize" INTEGER DEFAULT 0, 
  "ESD_grassPatchSize" INTEGER DEFAULT 0, 
  "ESD_ErosionPatternClass" VARCHAR(255), 
  "PctCov1" FLOAT NULL DEFAULT 0, 
  "Height1" INTEGER DEFAULT 0, 
  "PlantSpecies2" VARCHAR(255), 
  "PctCov2" FLOAT NULL DEFAULT 0, 
  "Height2" INTEGER DEFAULT 0, 
  "PlantSpecies3" VARCHAR(255), 
  "PctCov3" FLOAT NULL DEFAULT 0, 
  "Height3" INTEGER DEFAULT 0, 
  "ESD_SoilRedistributionClass" VARCHAR(255), 
  "PedodermMod" VARCHAR(8),  --changed from 'BuriedPedoderm'
  "Latitude" DOUBLE PRECISION NULL DEFAULT 0, 
  "Longitude" DOUBLE PRECISION NULL DEFAULT 0, 
  PRIMARY KEY ("PointKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblInfiltrationHeader'
--

DROP TABLE IF EXISTS dima."tblInfiltrationHeader" CASCADE;

CREATE TABLE dima."tblInfiltrationHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(25) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "NbrDepth" SMALLINT DEFAULT 0, 
  "SoilMoistSurf" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist5cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist10cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist15cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist20cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist25cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist30cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist35cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "SoilMoist40cm" VARCHAR(1),  --D=Dry;M=Moist;W=Wet
  "RingInsertionDepth" FLOAT NULL DEFAULT 0,  --Mandatory
  "RingDiameter" FLOAT NULL DEFAULT 0, 
  "BottleDiameter" FLOAT NULL DEFAULT 0, 
  "RingArea" FLOAT NULL DEFAULT 0, 
  "BottleArea" FLOAT NULL DEFAULT 0, 
  "CorrectionFactor" FLOAT NULL DEFAULT 0, 
  "Comment" VARCHAR(255), 
  "Averages" VARCHAR(255) DEFAULT E'0', 
  PRIMARY KEY ("RecKey"), 
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblLines'
--

DROP TABLE IF EXISTS dima."tblLines" CASCADE;

CREATE TABLE dima."tblLines" (
  "PlotKey" VARCHAR(20), 
  "LineKey" VARCHAR(20) NOT NULL,  --Computed YYMMDDHHMMSS + random number < 9999
  "DateModified" TIMESTAMP, 
  "LineID" VARCHAR(15), 
  "Azimuth" INTEGER DEFAULT 0, 
  "ElevationType" SMALLINT DEFAULT 0,  --1 = meters;2 = feet
  "NorthType" INTEGER DEFAULT 1,  --1 = Magnetic; 2 = True
  "NorthingStart" DOUBLE PRECISION NULL DEFAULT 0, 
  "EastingStart" DOUBLE PRECISION NULL DEFAULT 0, 
  "ElevationStart" FLOAT NULL DEFAULT 0, 
  "NorthingEnd" DOUBLE PRECISION NULL DEFAULT 0, 
  "EastingEnd" DOUBLE PRECISION NULL DEFAULT 0, 
  "ElevationEnd" FLOAT NULL DEFAULT 0, 
  "LatitudeStart" DOUBLE PRECISION NULL DEFAULT 0, 
  "LongitudeStart" DOUBLE PRECISION NULL DEFAULT 0, 
  "LatitudeEnd" DOUBLE PRECISION NULL DEFAULT 0, 
  "LongitudeEnd" DOUBLE PRECISION NULL DEFAULT 0, 
  PRIMARY KEY ("LineKey"),
  --FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPastCondHeader'
--

DROP TABLE IF EXISTS dima."tblPastCondHeader" CASCADE;

CREATE TABLE dima."tblPastCondHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(25) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormDate" DATE, 
  "FormType" VARCHAR(8), 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "Degradation" SMALLINT, 
  "DegradationComment" VARCHAR(255), 
  "GrazingIntensity" SMALLINT, 
  "GrazingIntensityComment" VARCHAR(255), 
  "VegetationCond" SMALLINT, 
  "VegetationCondComment" VARCHAR(255), 
  "SoilMovement" SMALLINT, 
  "SoilMovementComment" VARCHAR(255), 
  "SoilErosion" SMALLINT, 
  "SoilErosionComment" VARCHAR(255), 
  "SoilRedistribution" VARCHAR(5), 
  "SoilRedistributionComment" VARCHAR(255), 
  "RillsAndGulliesComment" VARCHAR(255), 
  "DegradationFactorComment" VARCHAR(255), 
  "RangelandConditionComment" TEXT, 
  PRIMARY KEY ("RecKey"), 
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlantLenHeader'
--

DROP TABLE IF EXISTS dima."tblPlantLenHeader" CASCADE;

CREATE TABLE dima."tblPlantLenHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(25) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "DataYear" VARCHAR(10), 
  PRIMARY KEY ("RecKey"), 
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlantProdHeader'
--

DROP TABLE IF EXISTS dima."tblPlantProdHeader" CASCADE;

CREATE TABLE dima."tblPlantProdHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5), 
  "FormDate" DATE, 
  "LineKey" VARCHAR(50),  --optional
  "Measure" SMALLINT DEFAULT 0,  --metric vs english - informational only - does not affect computations
  "LineLengthAmount" INTEGER DEFAULT 0,  --informational only - does not affect computations
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "numSubPlots" SMALLINT DEFAULT 0, 
  "SubPlotLocs" VARCHAR(200), 
  "Notes" TEXT, 
  "TotalProd" FLOAT NULL DEFAULT 0,  --lbs/acre
  "SubPlot1Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot2Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot3Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot4Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot5Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot6Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot7Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot8Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot9Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot10Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot11Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot12Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot13Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot14Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot15Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot16Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot17Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot18Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot19Exp" BOOLEAN DEFAULT E'0', 
  "SubPlot20Exp" BOOLEAN DEFAULT E'0', 
  "TotalProdHectare" FLOAT NULL DEFAULT 0,  --kg/hectare
  "SubPlot1NotSamp" BOOLEAN DEFAULT E'0',  --SubPlot Not Sampled
  "SubPlot2NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot3NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot4NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot5NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot6NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot7NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot8NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot9NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot10NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot11NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot12NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot13NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot14NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot15NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot16NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot17NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot18NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot19NotSamp" BOOLEAN DEFAULT E'0', 
  "SubPlot20NotSamp" BOOLEAN DEFAULT E'0', 
  "TotNotSamp" INTEGER, 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlotFormDefaults'
--

DROP TABLE IF EXISTS dima."tblPlotFormDefaults" CASCADE;

CREATE TABLE dima."tblPlotFormDefaults" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(20) NOT NULL,  --Computed YYMMDDHHMMSS + random number < 9999
  "LineLength" FLOAT NULL DEFAULT 0, 
  "LineLengthType" SMALLINT DEFAULT 0, 
  "LPIHeightOption" VARCHAR(15), 
  "LPIHeightUOM" VARCHAR(5), 
  "LPIShowCheckbox" BOOLEAN, 
  "LPICheckboxLabel" VARCHAR(20), 
  "LPISpacingInterval" FLOAT NULL DEFAULT 0, 
  "LPISpacingType" VARCHAR(5), 
  "GapMin" FLOAT NULL DEFAULT 0, 
  "GapData" VARCHAR(5), 
  "VegStructDataFormat" SMALLINT,  --1 = 0's and 1's; 2 = Percents
  "VegStructPctsMethod" INTEGER,  --1=A only; 2 = A and B
  "EveryNth_num" VARCHAR(5),
  "PTFrameNumDrops" INTEGER DEFAULT 0, 
  "PTFrameDataType" VARCHAR(20), 
  "LPILayerHeights" BOOLEAN, 
  "LPIWoodyHerbHeights" BOOLEAN, 
  "SoilStabSubSurface" SMALLINT DEFAULT 0,  --1 = surface only; 2 = surface/subsurface
  "SoilStabTimeInterval" SMALLINT DEFAULT 0,  --1 = 15 sec; 2 = 30 sec
  "SoilStabDataEntryLayout" SMALLINT DEFAULT 0,  --1 = vertical; 2 = horizontal
  "ProdNbrSubPlots" VARCHAR(5), 
  "ProdSubPlotMethod" SMALLINT DEFAULT 0, 
  "ProdSpacingInt" INTEGER DEFAULT 0, 
  "ProdStartingPos" SMALLINT DEFAULT 0, 
  "CompactNbrLines" SMALLINT DEFAULT 0, 
  "CompactNbrSamples" SMALLINT DEFAULT 0, 
  "CompactNbr1stPos" FLOAT NULL DEFAULT 0, 
  "CompactNbrSpacing" FLOAT NULL DEFAULT 0, 
  "CompactNbrDepth" SMALLINT DEFAULT 0, 
  "CompactPenUsed" BOOLEAN, 
  "CompactPenReps" SMALLINT DEFAULT 0, 
  "InfiltrationRingDiam" FLOAT NULL DEFAULT 0, 
  "InfiltrationBottleDiam" FLOAT NULL DEFAULT 0, 
  "VegStructMethod" SMALLINT DEFAULT 0, 
  "VegStructSamples" SMALLINT DEFAULT 0, 
  "VegStructSpacing" SMALLINT DEFAULT 0, 
  "VegStruct1stPos" SMALLINT DEFAULT 0, 
  "TreeDenNumSubplots" SMALLINT DEFAULT 0, 
  "TreeDenSubPlotRadius" FLOAT NULL DEFAULT 0, 
  "TreeDenSubPlotMeasure" VARCHAR(5), 
  "TreeDenDiameterMeasure" VARCHAR(5), 
  "TreeDenHeightMeasure" VARCHAR(5), 
  "TreeDenTapeType" SMALLINT DEFAULT 0,  --1=diameter; 2=standard tape
  "RiparSurvLineLength" INTEGER DEFAULT 0, 
  "RiparSurvLineLengthType" SMALLINT DEFAULT 0, 
  "RiparSurvSpacingInterval" INTEGER DEFAULT 0, 
  "RiparSurvSpacingType" VARCHAR(5), 
  "RiparSurvHeightOption" VARCHAR(15), 
  "RiparSurvHeightUOM" VARCHAR(5), 
  "RiparProStringLength" FLOAT NULL DEFAULT 0, 
  "RiparProStringLengthType" SMALLINT DEFAULT 0, 
  "RiparProSpacingInterval" INTEGER DEFAULT 0, 
  "SpecRichMethod" INTEGER DEFAULT 0, 
  "SpecRichMeasure" INTEGER DEFAULT 0, 
  "SpecRichNbrSubPlots" SMALLINT DEFAULT 0, 
  "SpecRich1Container" BOOLEAN,  --if yes, all other plots are contained in this one
  "SpecRich1Shape" SMALLINT DEFAULT 0,  --1 = Rectangluar; 2 = circle
  "SpecRich1Dim1" FLOAT NULL DEFAULT 0,  --if Rectangle, one of the sides; if circle, the radius
  "SpecRich1Dim2" FLOAT NULL DEFAULT 0,  --if Rectangle, the other side
  "SpecRich1Area" FLOAT NULL DEFAULT 0, 
  "SpecRich2Container" BOOLEAN, 
  "SpecRich2Shape" SMALLINT DEFAULT 0, 
  "SpecRich2Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich2Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich2Area" FLOAT NULL DEFAULT 0, 
  "SpecRich3Container" BOOLEAN, 
  "SpecRich3Shape" SMALLINT DEFAULT 0, 
  "SpecRich3Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich3Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich3Area" FLOAT NULL DEFAULT 0, 
  "SpecRich4Container" BOOLEAN, 
  "SpecRich4Shape" SMALLINT DEFAULT 0, 
  "SpecRich4Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich4Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich4Area" FLOAT NULL DEFAULT 0, 
  "SpecRich5Container" BOOLEAN, 
  "SpecRich5Shape" SMALLINT DEFAULT 0, 
  "SpecRich5Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich5Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich5Area" FLOAT NULL DEFAULT 0, 
  "SpecRich6Container" BOOLEAN, 
  "SpecRich6Shape" SMALLINT DEFAULT 0, 
  "SpecRich6Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich6Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich6Area" FLOAT NULL DEFAULT 0, 
  "DryWtNumQuadrats" SMALLINT DEFAULT 0, 
  "DryWtSize" FLOAT NULL DEFAULT 0, 
  "DryWtUOM" FLOAT NULL DEFAULT 0, 
  "DryWtCompYieldYesNo" BOOLEAN DEFAULT E'0', 
  "DryWtCompYieldNbr" INTEGER DEFAULT 0, 
  "DryWtCompYieldSize" FLOAT NULL DEFAULT 0, 
  "DryWtCompYieldUOM" FLOAT NULL DEFAULT 0, 
  "InfiltrationNbrLines" SMALLINT DEFAULT 0, 
  "InfiltrationNbrSamples" SMALLINT DEFAULT 0, 
  "InfiltrationNbr1stPos" SMALLINT DEFAULT 0, 
  "InfiltrationNbrSpacing" SMALLINT DEFAULT 0, 
  "InfiltrationNbrDepth" SMALLINT DEFAULT 0, 
  "PlantDenNumQuadrats" SMALLINT DEFAULT 0, 
  "PlantDenClass1" VARCHAR(20), 
  "PlantDenClass2" VARCHAR(20), 
  "PlantDenClass3" VARCHAR(20), 
  "PlantDenClass4" VARCHAR(20), 
  "PlantDenClass5" VARCHAR(20), 
  "PlantDenClass6" VARCHAR(20), 
  "PlantDenClass7" VARCHAR(20), 
  "PlantDenClass8" VARCHAR(20), 
  "PlantDenClass9" VARCHAR(20), 
  "LICShowCheckbox" BOOLEAN DEFAULT E'0', 
  "LICCheckboxLabel" VARCHAR(20) DEFAULT E'0', 
  "LICHeightUOM" VARCHAR(5), 
  "LPIHeightNoneOption" BOOLEAN, 
  "LPIStartPos" FLOAT NULL DEFAULT 0, 
  "LICPositionUOM" VARCHAR(5), 
  "LPISpecialLowerCodes" BOOLEAN,  --if Yes, LPI form will include S, W, and GR as valid Lower slot choices
  "LPIRapidMode" BOOLEAN,  --if Yes, LPI form will disable Lower slots
  "CanopyGapShowCheckbox" BOOLEAN DEFAULT E'0', 
  "CanopyGapCheckboxLabel" VARCHAR(20) DEFAULT E'0', 
  "CanopyGapPositionUOM" VARCHAR(5), 
  "CanopyGapMinLength" FLOAT NULL DEFAULT 0, 
  "NestedFreqLineLength" INTEGER DEFAULT 0, 
  "NestedFreqLineLengthType" SMALLINT DEFAULT 0, 
  "NestedFreqSpacingInterval" INTEGER DEFAULT 0, 
  "NestedFreqSpacingType" VARCHAR(5), 
  "NestedFreqMidOpt" BOOLEAN, 
  "LPIShowShrubShape" BOOLEAN DEFAULT E'0', 
  "SoilStabLimitedVeg" BOOLEAN DEFAULT E'0', 
  "NestedFreqStartAtZero" BOOLEAN DEFAULT E'0', 
  "LPIPaceCount" SMALLINT DEFAULT 0, 
  "ProductionPaceCount" SMALLINT DEFAULT 0, 
  "UtilizationPaceCount" SMALLINT DEFAULT 0, 
  "LPILowerHerbHeight" BOOLEAN,  --if Yes, Woody/Heb fields also show
  "GapPerennialsCanopy" BOOLEAN, 
  "GapAnnualGrassesCanopy" BOOLEAN, 
  "GapAnnualForbsCanopy" BOOLEAN, 
  "GapOtherCanopy" BOOLEAN, 
  "GapPerennialsBasal" BOOLEAN, 
  "GapAnnualGrassesBasal" BOOLEAN, 
  "GapAnnualForbsBasal" BOOLEAN, 
  "GapOtherBasal" BOOLEAN, 
  PRIMARY KEY ("PlotKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlotHistory'
--

DROP TABLE IF EXISTS dima."tblPlotHistory" CASCADE;

CREATE TABLE dima."tblPlotHistory" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(20), 
  "RecKey" VARCHAR(20) NOT NULL,  --P = Plot; E = ESD Tier2
  "RecType" VARCHAR(1), 
  "DateRecorded" TIMESTAMP, 
  "DateModified" TIMESTAMP, 
  "PlotCustom1Data" VARCHAR(255), 
  "PlotCustom2Data" VARCHAR(255), 
  "PlotCustom3Data" VARCHAR(255), 
  "PlotCustom4Data" VARCHAR(255), 
  "ESD_PctGrassCover" DOUBLE PRECISION NULL, 
  "ESD_ResourceRetentionClass" SMALLINT, 
  "ESD_bareGapPatchSize" INTEGER DEFAULT 0, 
  "ESD_grassPatchSize" INTEGER DEFAULT 0, 
  "ESD_SoilRedistributionClass" VARCHAR(5), 
  "ESD_ResourceRetentionNotes" VARCHAR(255), 
  "ESD_Pedoderm_Class" VARCHAR(8), 
  "ESD_PedodermMod" VARCHAR(8), 
  "ESD_SurfaceNotes" TEXT, 
  "ESD_Compaction" BOOLEAN, 
  "ESD_SubsurfaceNotes" VARCHAR(50), 
  "ESD_StateWithinEcologicalSite" VARCHAR(100), 
  "ESD_CommunityWithinState" VARCHAR(100), 
  "ESD_CommunityDescription" VARCHAR(255), 
  "ESD_Root_density" VARCHAR(50), 
  "ESD_Root_Depth" INTEGER DEFAULT 0, 
  "ESD_Root_Notes" VARCHAR(250), 
  "Observers" VARCHAR(255), 
  "Methods" VARCHAR(255), 
  "DataEntry" VARCHAR(255), 
  "DataEntryDate" DATE, 
  "ErrorCheck" VARCHAR(255), 
  "ErrorCheckDate" DATE, 
  "RecentWeatherPast12" VARCHAR(10), 
  "RecentWeatherPrevious12" VARCHAR(10), 
  "PrecipUOM" VARCHAR(2), 
  "PrecipPast12" VARCHAR(4), 
  "PrecipPrevious12" VARCHAR(4), 
  "DataSource" VARCHAR(255), 
  "Photo1Num" VARCHAR(10), 
  "Photo2Num" VARCHAR(10), 
  "Photo3Num" VARCHAR(10), 
  "Photo4Num" VARCHAR(10), 
  "Photo5Num" VARCHAR(10), 
  "Photo6Num" VARCHAR(10), 
  "Photo1Desc" VARCHAR(50), 
  "Photo2Desc" VARCHAR(50), 
  "Photo3Desc" VARCHAR(50), 
  "Photo4Desc" VARCHAR(50), 
  "Photo5Desc" VARCHAR(50), 
  "Photo6Desc" VARCHAR(50), 
  "Rills" VARCHAR(1), 
  "Gullies" VARCHAR(1), 
  "Pedestals" VARCHAR(1), 
  "Deposition" VARCHAR(1), 
  "WaterFlow" VARCHAR(1), 
  "SheetErosion" VARCHAR(1), 
  "Other" VARCHAR(1), 
  "MgtHistory" TEXT, 
  "Wildlife" TEXT, 
  "Livestock" TEXT, 
  "OffSite" TEXT, 
  "Disturbances" TEXT, 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlotMgtHeader'
--

DROP TABLE IF EXISTS dima."tblPlotMgtHeader" CASCADE;

CREATE TABLE dima."tblPlotMgtHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL,  --C = Cropland; R = Rangeland
  "FormType" VARCHAR(10), 
  "DateModified" TIMESTAMP, 
  "RecType" VARCHAR(1), 
  "YearsManaged" VARCHAR(255), 
  "YearsGrazed" VARCHAR(255), 
  "CashCoverFrequency" VARCHAR(255), 
  "CropPlanted" VARCHAR(255), 
  "Improvements" VARCHAR(255), 
  "Notes" TEXT, 
  "FormDate" DATE,  --unused, but needs to be defined, for some processes
  PRIMARY KEY ("RecKey"), 
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlotNotes'
--

DROP TABLE IF EXISTS dima."tblPlotNotes" CASCADE;

CREATE TABLE dima."tblPlotNotes" (
  dbkey VARCHAR(255) NOT NULL, 
  "CommentID" VARCHAR(50) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "NoteDate" DATE, 
  "Recorder" VARCHAR(50), 
  "Note" TEXT,  --this field must always be the last one in this table - see Excel Export
  PRIMARY KEY ("CommentID"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlotSpecies'
--

DROP TABLE IF EXISTS dima."tblPlotSpecies" CASCADE;

CREATE TABLE dima."tblPlotSpecies" (  --no primary key in DIMA
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "SpecType" VARCHAR(1) NOT NULL, 
  "SortSeq" FLOAT NULL DEFAULT 0, 
  "Species" VARCHAR(50) NOT NULL,
  PRIMARY KEY ("PlotKey", "SpecType", "Species"), --added
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlotTags'
--

DROP TABLE IF EXISTS dima."tblPlotTags" CASCADE;

CREATE TABLE dima."tblPlotTags" (  --no primary key in DIMA
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(20),  --Computed YYMMDDHHMMSS + random number < 9999
  "Tag" VARCHAR(50),
  PRIMARY KEY ("PlotKey", "Tag"),  --added
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPTFrameHeader'
--

DROP TABLE IF EXISTS dima."tblPTFrameHeader" CASCADE;

CREATE TABLE dima."tblPTFrameHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(25) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Position" VARCHAR(12), 
  "PositionDesc" VARCHAR(255), 
  "DataType" VARCHAR(50),  --Basal only or Top/Bottom
  "Comment" TEXT, 
  "PctSurfCover" INTEGER DEFAULT 0, 
  "PctTopCover" INTEGER DEFAULT 0, 
  "FormType" VARCHAR(8),  --PtFrame = PointFrame; SP = SamplePoint
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblQualHeader'
--

DROP TABLE IF EXISTS dima."tblQualHeader" CASCADE;

CREATE TABLE dima."tblQualHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(25) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "EcolSite" VARCHAR(50),  --this is the full Ecological Site number (i.e. R025XY019NV)
  "EcolSiteMLRA" VARCHAR(5),  --i.e. R025
  "EcolSiteSubMLRA" VARCHAR(5),  --i.e XY
  "EcolSiteNum" VARCHAR(5),  --i.e. 019
  "EcolSiteState" VARCHAR(5),  --i.e. NV
  "RefSheetType" SMALLINT DEFAULT 0,  --1=New; 2=Existing - downloaded from NRCS; 3=Existing - obtained from other source
  "RefSheetDate" DATE, 
  "RefSheetAuthor" VARCHAR(100), 
  "DateDownloaded" DATE, 
  "AttrEvalMethod" SMALLINT DEFAULT 0, 
  "WeightsSource" VARCHAR(50), 
  "AerialPhoto" VARCHAR(80), 
  "SitePhotoTaken" BOOLEAN, 
  "EvalAreaSize" VARCHAR(20), 
  "RepCriteria" VARCHAR(255), 
  "CompositionBase" SMALLINT DEFAULT 0, 
  "SumSSSWt" FLOAT NULL DEFAULT 0, 
  "SumSSSVxW" FLOAT NULL DEFAULT 0, 
  "SumHFWt" FLOAT NULL DEFAULT 0, 
  "SumHFVxW" FLOAT NULL DEFAULT 0, 
  "SumBIWt" FLOAT NULL DEFAULT 0, 
  "SumBIVxW" FLOAT NULL DEFAULT 0, 
  "AvgSSSVxW" FLOAT NULL DEFAULT 0, 
  "AvgHFVxW" FLOAT NULL DEFAULT 0, 
  "AvgBIVxW" FLOAT NULL DEFAULT 0, 
  "SSSVxWRatingCalc" VARCHAR(5), 
  "HFVxWRatingCalc" VARCHAR(5), 
  "BIVxWRatingCalc" VARCHAR(5), 
  "SSSVxWRatingFinal" VARCHAR(5), 
  "HFVxWRatingFinal" VARCHAR(5), 
  "BIVxWRatingFinal" VARCHAR(5), 
  "CommentSSS" VARCHAR(255), 
  "CommentHF" VARCHAR(255), 
  "CommentBI" VARCHAR(255), 
  PRIMARY KEY ("RecKey"), 
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSageRange'
--

DROP TABLE IF EXISTS dima."tblSageRange" CASCADE;

CREATE TABLE dima."tblSageRange" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "FormDate" DATE, 
  "DateModified" TIMESTAMP, 
  "Evaluators" VARCHAR(255), 
  "Population" VARCHAR(255), 
  "HomeRange" VARCHAR(255), 
  "LandCover" VARCHAR(255), 
  "SiteType" VARCHAR(25), 
  "Notes" TEXT, 
  PRIMARY KEY ("RecKey"), 
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSoilPits'
--

DROP TABLE IF EXISTS dima."tblSoilPits" CASCADE;

CREATE TABLE dima."tblSoilPits" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(20) NOT NULL, 
  "SoilKey" VARCHAR(20) NOT NULL,   --Computed YYMMDDHHMMSS + random number < 9999
  "Observer" VARCHAR(255), 
  "PitDesc" VARCHAR(30), 
  "SoilDepthUpper" VARCHAR(50), 
  "SoilDepthLower" VARCHAR(50), 
  "SurfTexture" VARCHAR(150), 
  "RockFragments" INTEGER DEFAULT 0, 
  "SurfEffer" VARCHAR(50), 
  "ColorDry" VARCHAR(50), 
  "ColorMoist" VARCHAR(50), 
  "DepthMeasure" VARCHAR(50), 
  "Northing" DOUBLE PRECISION NULL DEFAULT 0, 
  "Easting" DOUBLE PRECISION NULL DEFAULT 0, 
  "Elevation" FLOAT NULL DEFAULT 0, 
  "ElevationType" SMALLINT DEFAULT 0, 
  "Notes" VARCHAR(255), 
  "Latitude" DOUBLE PRECISION NULL DEFAULT 0, 
  "Longitude" DOUBLE PRECISION NULL DEFAULT 0, 
  "DateRecorded" DATE, 
  PRIMARY KEY ("SoilKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSoilStabHeader'
--

DROP TABLE IF EXISTS dima."tblSoilStabHeader" CASCADE;

CREATE TABLE dima."tblSoilStabHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5), 
  "FormDate" DATE, 
  "LineKey" VARCHAR(50),  --this is not ever used - needed for 'Union' Query!
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "SoilStabSubSurface" SMALLINT DEFAULT 0,  --1 = surface only; 2 = surface/subsurface
  "SoilStabTimeInterval" SMALLINT DEFAULT 0,  --1 = 15 seconds; 2 = 30 seconds
  "SoilStabDataEntryLayout" SMALLINT DEFAULT 0,  --1 = vertical; 2 = horizontal
  "AllAvgSurf" FLOAT NULL DEFAULT 0, 
  "AllAvgSub" FLOAT NULL DEFAULT 0, 
  "ProAvgSurf" FLOAT NULL DEFAULT 0, 
  "ProAvgSub" FLOAT NULL DEFAULT 0, 
  "NCAvgSurf" FLOAT NULL DEFAULT 0, 
  "NCAvgSub" FLOAT NULL DEFAULT 0, 
  "NoVegAvgSurf" FLOAT NULL DEFAULT 0, 
  "NoVegAvgSub" FLOAT NULL DEFAULT 0, 
  "PctSurf6" FLOAT NULL DEFAULT 0, 
  "PctSub6" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  "SoilStabLimitedVeg" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblTreeDenHeader'
--

DROP TABLE IF EXISTS dima."tblTreeDenHeader" CASCADE;

CREATE TABLE dima."tblTreeDenHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(25) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(8) DEFAULT E'0', 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Notes" VARCHAR(255), 
  "LineLengthAmount" VARCHAR(50), 
  "SubPlotRadius" FLOAT NULL DEFAULT 0, 
  "SubPlotMeasure" VARCHAR(5), 
  "DiameterMeasure" VARCHAR(5), 
  "HeightMeasure" VARCHAR(5), 
  "TapeType" SMALLINT DEFAULT 0,  --1=diameter; 2=standard tape
  "numSubplots" SMALLINT DEFAULT 0, 
  "Sub1Desc" VARCHAR(20), 
  "Sub2Desc" VARCHAR(20), 
  "Sub3Desc" VARCHAR(20), 
  "Sub4Desc" VARCHAR(20), 
  "PlotArea" FLOAT NULL DEFAULT 0, 
  "totTrees" SMALLINT DEFAULT 0, 
  "totSaplings" SMALLINT DEFAULT 0, 
  "denTrees" FLOAT NULL DEFAULT 0, 
  "denSaplings" FLOAT NULL DEFAULT 0, 
  "denTreesAcre" FLOAT NULL DEFAULT 0, 
  "denSaplingsAcre" FLOAT NULL DEFAULT 0, 
  "totSeedlings" SMALLINT DEFAULT 0, 
  "denSeedlings" FLOAT NULL DEFAULT 0, 
  "denSeedlingsAcre" FLOAT NULL DEFAULT 0, 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblUtilHeader'
--

DROP TABLE IF EXISTS dima."tblUtilHeader" CASCADE;

CREATE TABLE dima."tblUtilHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "PlotKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5) DEFAULT E'0', 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Method" SMALLINT,  --1 = Use Pattern Mapping; 2 = Transect
  "Location" VARCHAR(25), 
  "PatternMappingNbr" VARCHAR(10), 
  "LengthInPaces" INTEGER, 
  "PaceInterval" SMALLINT, 
  "Comments" TEXT, 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("PlotKey") REFERENCES dima."tblPlots"("PlotKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "PlotKey") REFERENCES dima.db_plot(dbkey, "PlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Level 4, FK to 0,1,2,3
--

--
-- Table structure for table 'tblBSNE_Box'
--

DROP TABLE IF EXISTS dima."tblBSNE_Box" CASCADE;

CREATE TABLE dima."tblBSNE_Box" (
  "BoxID" VARCHAR(50) NOT NULL, 
  "StackID" VARCHAR(50), 
  "Height" FLOAT NULL,  --cm
  "DateEstablished" DATE, 
  "DateModified" TIMESTAMP, 
  "Description" VARCHAR(50),  --Top, Bottom, etc
  "openingSize" VARCHAR(20), 
  "Notes" TEXT, 
  "processMethod" VARCHAR(25), 
  "ovenTemp" INTEGER, 
  "BoxType" SMALLINT,  --1 = Rotating; 2 = Static
  "azimuth" VARCHAR(255), 
  "SamplerType" VARCHAR(25), 
  "InletArea" FLOAT NULL DEFAULT 0, 
  PRIMARY KEY ("BoxID"), 
  FOREIGN KEY ("StackID") REFERENCES dima."tblBSNE_Stack"("StackID") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblBSNE_TrapCollection'
--

DROP TABLE IF EXISTS dima."tblBSNE_TrapCollection" CASCADE;

CREATE TABLE dima."tblBSNE_TrapCollection" (
  "RecKey" VARCHAR(255) NOT NULL, 
  "StackID" VARCHAR(50),  --Relates to a specific TrapID
  "DateModified" TIMESTAMP, 
  "collectDate" DATE, 
  "Collector" VARCHAR(30), 
  "labTech" VARCHAR(30), 
  "beakerNbr" VARCHAR(20), 
  "emptyWeight" FLOAT NULL, 
  "recordedWeight" FLOAT NULL, 
  "sedimentWeight" FLOAT NULL,  --computed
  "daysExposed" INTEGER,  --computed
  "sedimentGperDay" FLOAT NULL,  --computed - grams/day
  "sedimentArchived" BOOLEAN DEFAULT E'0', 
  "Notes" TEXT, 
  "sedimentGperDayByInlet" FLOAT NULL,  --computed - grams/day divided by Inlet Area
  "SeqNo" SMALLINT,  --for use when they have multiple collections on the same day
  "SampleCompromised" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("StackID") REFERENCES dima."tblBSNE_Stack"("StackID") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblCanopyGapHeader'
--

DROP TABLE IF EXISTS dima."tblCanopyGapHeader" CASCADE;

CREATE TABLE dima."tblCanopyGapHeader" (
  dbkey VARCHAR(255) NOT NULL,  --custom field
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(10) DEFAULT E'0', 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Direction" VARCHAR(10), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" INTEGER DEFAULT 0, 
  "ShowCheckbox" BOOLEAN, 
  "CheckboxLabel" VARCHAR(20), 
  "totCovPct" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  "DataEntryDirection" INTEGER,  --1 = start at 0 position; 2 = start at end of Line
  "PositionUOM" VARCHAR(5), 
  "MinLength" FLOAT NULL DEFAULT 0,  --no Gap OR PLANT can be < than this
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblCompactDetail'
--

DROP TABLE IF EXISTS dima."tblCompactDetail" CASCADE;

CREATE TABLE dima."tblCompactDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(25), 
  "Line" VARCHAR(50), 
  "Pos" FLOAT NULL DEFAULT 0, 
  "VegClass" VARCHAR(5), 
  "Strikes5cm" VARCHAR(50) DEFAULT E'0', 
  "Strikes10cm" VARCHAR(50) DEFAULT E'0', 
  "Strikes15cm" VARCHAR(50) DEFAULT E'0', 
  "Strikes20cm" VARCHAR(50) DEFAULT E'0', 
  "Strikes25cm" VARCHAR(50) DEFAULT E'0', 
  "Strikes30cm" VARCHAR(50) DEFAULT E'0', 
  "Strikes35cm" VARCHAR(50) DEFAULT E'0', 
  "Strikes40cm" VARCHAR(50) DEFAULT E'0', 
  "PenFootUsed" BOOLEAN DEFAULT E'0', 
  "PenRep1" VARCHAR(5), 
  "PenRep2" VARCHAR(5), 
  "PenRep3" VARCHAR(5), 
  "PenRep4" VARCHAR(5), 
  "PenRep5" VARCHAR(5),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblCompactHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblDKDetail'
-- 

DROP TABLE IF EXISTS dima."tblDKDetail" CASCADE;

CREATE TABLE dima."tblDKDetail" (  -- no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "SpeciesSymbol" VARCHAR(255), 
  "DKClass" VARCHAR(255), 
  "LPIPercentCanopy" FLOAT NULL, 
  "LPIPercentBasal" FLOAT NULL, 
  "Production" VARCHAR(50), 
  "Notes" VARCHAR(255), 
  "CoverMidPoint" VARCHAR(5), 
  "CanopyCoverPoints" INTEGER, 
  "CanopyCoverPercent" INTEGER, 
  "BareGroundPoints" INTEGER, 
  "BareGroundPercent" INTEGER, 
  "BasalCoverPoints" INTEGER, 
  "BasalCoverPercent" INTEGER,
  PRIMARY KEY ("RecKey", "SpeciesSymbol", "DKClass"),  -- added PK based on table and form inspection
  FOREIGN KEY ("RecKey") REFERENCES dima."tblDKHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblDryWtHeader'
--

DROP TABLE IF EXISTS dima."tblDryWtHeader" CASCADE;

CREATE TABLE dima."tblDryWtHeader" (
  dbkey VARCHAR(255) NOT NULL,  --custom field
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(8) DEFAULT E'0', 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "DryWtNumQuadrats" SMALLINT DEFAULT 0, 
  "DryWtSize" FLOAT NULL DEFAULT 0, 
  "DryWtUOM" FLOAT NULL DEFAULT 0, 
  "Notes" VARCHAR(255), 
  "NearestPerennialSum" FLOAT NULL DEFAULT 0, 
  "NearestPerennialAvg" FLOAT NULL DEFAULT 0, 
  "GreenWeight" FLOAT NULL DEFAULT 0, 
  "TotProduction" FLOAT NULL DEFAULT 0, 
  "CompletedQuadrats" INTEGER DEFAULT 0, 
  "DryWtCompYieldYesNo" BOOLEAN DEFAULT E'0', 
  "DryWtCompYieldNbr" INTEGER DEFAULT 0, 
  "DryWtCompYieldSize" FLOAT NULL DEFAULT 0, 
  "DryWtCompYieldUOM" FLOAT NULL DEFAULT 0, 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblESDRockFragments'
--

DROP TABLE IF EXISTS dima."tblESDRockFragments" CASCADE;

CREATE TABLE dima."tblESDRockFragments" (  --no primary key in DIMA
  "RecKey" VARCHAR(25), 
  "SurfPedon" VARCHAR(1),  --S = Surface; P = Pedon
  "Type" VARCHAR(30), 
  "VolumeClass" VARCHAR(30), 
  "SurfaceCoverPct" VARCHAR(50), 
  "Notes" VARCHAR(255),
  PRIMARY KEY ("RecKey", "SurfPedon", "Type"),  -- added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblPlotHistory"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblGapHeader'
--

DROP TABLE IF EXISTS dima."tblGapHeader" CASCADE;

CREATE TABLE dima."tblGapHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5), 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Direction" VARCHAR(10), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" FLOAT NULL DEFAULT 0, 
  "GapMin" FLOAT NULL DEFAULT 0, 
  "GapData" VARCHAR(5),  --1 = Both; 2 = Canopy Only; 3=Basal only
  "PerennialsCanopy" BOOLEAN, 
  "AnnualGrassesCanopy" BOOLEAN, 
  "AnnualForbsCanopy" BOOLEAN, 
  "OtherCanopy" BOOLEAN, 
  "sumCanCat1" FLOAT NULL DEFAULT 0, 
  "sumCanCat2" FLOAT NULL DEFAULT 0, 
  "sumCanCat3" FLOAT NULL DEFAULT 0, 
  "sumCanCat4" FLOAT NULL DEFAULT 0, 
  "pctCanCat1" FLOAT NULL DEFAULT 0, 
  "pctCanCat2" FLOAT NULL DEFAULT 0, 
  "pctCanCat3" FLOAT NULL DEFAULT 0, 
  "pctCanCat4" FLOAT NULL DEFAULT 0, 
  "sumBasCat1" FLOAT NULL DEFAULT 0, 
  "sumBasCat2" FLOAT NULL DEFAULT 0, 
  "sumBasCat3" FLOAT NULL DEFAULT 0, 
  "sumBasCat4" FLOAT NULL DEFAULT 0, 
  "pctBasCat1" FLOAT NULL DEFAULT 0, 
  "pctBasCat2" FLOAT NULL DEFAULT 0, 
  "pctBasCat3" FLOAT NULL DEFAULT 0, 
  "pctBasCat4" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  "NoCanopyGaps" BOOLEAN DEFAULT E'0', 
  "NoBasalGaps" BOOLEAN DEFAULT E'0', 
  "PerennialsBasal" BOOLEAN,  --Basal
  "AnnualGrassesBasal" BOOLEAN,  --Basal
  "AnnualForbsBasal" BOOLEAN,  --Basal
  "OtherBasal" BOOLEAN,  --Basal
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblInfiltrationDetail'
--

DROP TABLE IF EXISTS dima."tblInfiltrationDetail" CASCADE;

CREATE TABLE dima."tblInfiltrationDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(25), 
  "Line" SMALLINT DEFAULT 1, 
  "Pos" SMALLINT DEFAULT 0, 
  "VegClass" VARCHAR(5), 
  "BeginTime" TIMESTAMP, 
  "EndTime" TIMESTAMP, 
  "Distance" FLOAT NULL DEFAULT 0, 
  "TotMin" FLOAT NULL DEFAULT 0, 
  "TotHr" FLOAT NULL DEFAULT 0, 
  "BottleRate" FLOAT NULL DEFAULT 0, 
  "InfilRate" FLOAT NULL DEFAULT 0, 
  "Notes" VARCHAR(255),
  PRIMARY KEY ("RecKey", "Line", "Pos"), --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblInfiltrationHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblLICHeader'
--

DROP TABLE IF EXISTS dima."tblLICHeader" CASCADE;

CREATE TABLE dima."tblLICHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5), 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Direction" VARCHAR(10), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" INTEGER DEFAULT 0, 
  "HeightUOM" VARCHAR(5), 
  "ShowCheckbox" BOOLEAN, 
  "CheckboxLabel" VARCHAR(20), 
  "totCovPct" FLOAT NULL DEFAULT 0, 
  "AvgHeightAll" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  "MinNonInterceptGap" VARCHAR(8), 
  "MinLengthCanopySeg" VARCHAR(8), 
  "MaxPctNonCanopy" VARCHAR(8), 
  "PositionUOM" VARCHAR(5), 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblLPIHeader'
--

DROP TABLE IF EXISTS dima."tblLPIHeader" CASCADE;

CREATE TABLE dima."tblLPIHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5), 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Direction" VARCHAR(10), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" FLOAT NULL DEFAULT 0, 
  "SpacingIntervalAmount" FLOAT NULL DEFAULT 0, 
  "SpacingType" VARCHAR(5), 
  "HeightOption" VARCHAR(15), 
  "HeightUOM" VARCHAR(5), 
  "ShowCheckbox" BOOLEAN, 
  "CheckboxLabel" VARCHAR(20), 
  "numCanopy" INTEGER DEFAULT 0, 
  "numBare" INTEGER DEFAULT 0, 
  "numBasal" INTEGER DEFAULT 0, 
  "pctCanopy" FLOAT NULL DEFAULT 0, 
  "pctBare" FLOAT NULL DEFAULT 0, 
  "pctBasal" FLOAT NULL DEFAULT 0, 
  "pctCheckedPlants" FLOAT NULL DEFAULT 0, 
  "pctCheckedTopPlants" FLOAT NULL DEFAULT 0, 
  "pctCheckedTopPoints" FLOAT NULL DEFAULT 0, 
  "LayerHeights" BOOLEAN, 
  "WoodyHerbHeights" BOOLEAN, 
  "LowerHerbHeight" BOOLEAN DEFAULT E'0', 
  "pctCheckedSoil" FLOAT NULL DEFAULT 0, 
  "avgHeightTop" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  "avgHeightLower1" FLOAT NULL DEFAULT 0, 
  "avgHeightLower2" FLOAT NULL DEFAULT 0, 
  "avgHeightLower3" FLOAT NULL DEFAULT 0, 
  "avgHeightLower4" FLOAT NULL DEFAULT 0, 
  "avgHeightSurface" FLOAT NULL DEFAULT 0, 
  "numGrCovTotal" INTEGER DEFAULT 0, 
  "numGrCovBetween" INTEGER DEFAULT 0, 
  "numGrCovUnder" INTEGER DEFAULT 0, 
  "numLitterTotal" INTEGER DEFAULT 0, 
  "numLitterBetween" INTEGER DEFAULT 0, 
  "numLitterUnder" INTEGER DEFAULT 0, 
  "pctGrCovTotal" FLOAT NULL DEFAULT 0, 
  "pctGrCovBetween" FLOAT NULL DEFAULT 0, 
  "pctGrCovUnder" FLOAT NULL DEFAULT 0, 
  "pctLitterTotal" FLOAT NULL DEFAULT 0, 
  "pctLitterBetween" FLOAT NULL DEFAULT 0, 
  "pctLitterUnder" FLOAT NULL DEFAULT 0, 
  "HeightNoneOption" BOOLEAN, 
  "avgHeightWoody" FLOAT NULL DEFAULT 0, 
  "avgHeightHerb" FLOAT NULL DEFAULT 0, 
  "ShowShrubShape" BOOLEAN DEFAULT E'0', 
  "RapidMode" BOOLEAN DEFAULT E'0', 
  "avgHeightLower5" FLOAT NULL DEFAULT 0, 
  "avgHeightLower6" FLOAT NULL DEFAULT 0, 
  "avgHeightLower7" FLOAT NULL DEFAULT 0, 
  "HAF_Other" VARCHAR(8), 
  "avgHeightLowerHerb" FLOAT NULL DEFAULT 0, 
  "EveryNth_num" VARCHAR(5), 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblNestedFreqHeader'
--

DROP TABLE IF EXISTS dima."tblNestedFreqHeader" CASCADE;

CREATE TABLE dima."tblNestedFreqHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(10) DEFAULT E'0', 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" INTEGER DEFAULT 0, 
  "SpacingIntervalAmount" INTEGER DEFAULT 0, 
  "SpacingType" VARCHAR(5), 
  "MidOpt" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE 
);

--
-- Table structure for table 'tblOcularCovHeader'
--

DROP TABLE IF EXISTS dima."tblOcularCovHeader" CASCADE;

CREATE TABLE dima."tblOcularCovHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(10), 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "TotProduction" FLOAT NULL DEFAULT 0, 
  "Direction" VARCHAR(10), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" INTEGER DEFAULT 0, 
  "GapMin" INTEGER DEFAULT 0, 
  "GapData" VARCHAR(5),  --1 = Both; 2 = Canopy Only; 3=Basal only
  "Perennials" BOOLEAN, 
  "AnnualGrasses" BOOLEAN, 
  "AnnualForbs" BOOLEAN, 
  "Other" BOOLEAN, 
  "Notes" TEXT, 
  "numQuadrats" SMALLINT DEFAULT 0, 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlantDenHeader'
--

DROP TABLE IF EXISTS dima."tblPlantDenHeader" CASCADE;

CREATE TABLE dima."tblPlantDenHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5), 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Direction" VARCHAR(10), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" INTEGER DEFAULT 0, 
  "numQuadrats" INTEGER DEFAULT 0, 
  "Class1total" INTEGER DEFAULT 0, 
  "Class2total" INTEGER DEFAULT 0, 
  "Class3total" INTEGER DEFAULT 0, 
  "Class4total" INTEGER DEFAULT 0, 
  "Class5total" INTEGER DEFAULT 0, 
  "Class6total" INTEGER DEFAULT 0, 
  "Class7total" INTEGER DEFAULT 0, 
  "Class8total" INTEGER DEFAULT 0, 
  "Class9total" INTEGER DEFAULT 0, 
  "Class1density" FLOAT NULL DEFAULT 0, 
  "Class2density" FLOAT NULL DEFAULT 0, 
  "Class3density" FLOAT NULL DEFAULT 0, 
  "Class4density" FLOAT NULL DEFAULT 0, 
  "Class5density" FLOAT NULL DEFAULT 0, 
  "Class6density" FLOAT NULL DEFAULT 0, 
  "Class7density" FLOAT NULL DEFAULT 0, 
  "Class8density" FLOAT NULL DEFAULT 0, 
  "Class9density" FLOAT NULL DEFAULT 0, 
  "AllClassesTotal" INTEGER DEFAULT 0, 
  "AllClassesDensity" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  "SpeciesSearchedFor" VARCHAR(255), 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlantLenDetail'
--

DROP TABLE IF EXISTS dima."tblPlantLenDetail" CASCADE;

CREATE TABLE dima."tblPlantLenDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(25), 
  "Species" VARCHAR(25), 
  "Length" FLOAT NULL, 
  "NotMeasured" BOOLEAN DEFAULT E'0',
  PRIMARY KEY ("RecKey", "Species"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblPlantLenHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlantProdDetail'
--

DROP TABLE IF EXISTS dima."tblPlantProdDetail" CASCADE;

CREATE TABLE dima."tblPlantProdDetail" (
  "RecKey" VARCHAR(50), 
  "DetailKey" VARCHAR(50) NOT NULL, 
  "SpeciesCode" VARCHAR(50), 
  "SubPlotSize" FLOAT NULL DEFAULT 0, 
  "SubPlotUOM" VARCHAR(8), 
  "SizeCF" FLOAT NULL DEFAULT 0, 
  "WtMeas" VARCHAR(5) DEFAULT E'0', 
  "WtMethod" VARCHAR(15) DEFAULT E'0', 
  "Sub1Wt" VARCHAR(8), 
  "Sub2Wt" VARCHAR(8), 
  "Sub3Wt" VARCHAR(8), 
  "Sub4Wt" VARCHAR(8), 
  "Sub5Wt" VARCHAR(8), 
  "Sub6Wt" VARCHAR(8), 
  "Sub7Wt" VARCHAR(8), 
  "Sub8Wt" VARCHAR(8), 
  "Sub9Wt" VARCHAR(8), 
  "Sub10Wt" VARCHAR(8), 
  "Sub11Wt" VARCHAR(8), 
  "Sub12Wt" VARCHAR(8), 
  "Sub13Wt" VARCHAR(8), 
  "Sub14Wt" VARCHAR(8), 
  "Sub15Wt" VARCHAR(8), 
  "Sub16Wt" VARCHAR(8), 
  "Sub17Wt" VARCHAR(8), 
  "Sub18Wt" VARCHAR(8), 
  "Sub19Wt" VARCHAR(8), 
  "Sub20Wt" VARCHAR(8), 
  "Sub1Clip" BOOLEAN DEFAULT E'0', 
  "Sub2Clip" BOOLEAN DEFAULT E'0', 
  "Sub3Clip" BOOLEAN DEFAULT E'0', 
  "Sub4Clip" BOOLEAN DEFAULT E'0', 
  "Sub5Clip" BOOLEAN DEFAULT E'0', 
  "Sub6Clip" BOOLEAN DEFAULT E'0', 
  "Sub7Clip" BOOLEAN DEFAULT E'0', 
  "Sub8Clip" BOOLEAN DEFAULT E'0', 
  "Sub9Clip" BOOLEAN DEFAULT E'0', 
  "Sub10Clip" BOOLEAN DEFAULT E'0', 
  "Sub11Clip" BOOLEAN DEFAULT E'0', 
  "Sub12Clip" BOOLEAN DEFAULT E'0', 
  "Sub13Clip" BOOLEAN DEFAULT E'0', 
  "Sub14Clip" BOOLEAN DEFAULT E'0', 
  "Sub15Clip" BOOLEAN DEFAULT E'0', 
  "Sub16Clip" BOOLEAN DEFAULT E'0', 
  "Sub17Clip" BOOLEAN DEFAULT E'0', 
  "Sub18Clip" BOOLEAN DEFAULT E'0', 
  "Sub19Clip" BOOLEAN DEFAULT E'0', 
  "Sub20Clip" BOOLEAN DEFAULT E'0', 
  "TotalWtUnits" FLOAT NULL DEFAULT 0, 
  "WtUnitWt" FLOAT NULL DEFAULT 0, 
  "ClipWt1" FLOAT NULL DEFAULT 0, 
  "ClipWt2" FLOAT NULL DEFAULT 0, 
  "ClipWt3" FLOAT NULL DEFAULT 0, 
  "ClipWt4" FLOAT NULL DEFAULT 0, 
  "ClipWt5" FLOAT NULL DEFAULT 0, 
  "ClipWt6" FLOAT NULL DEFAULT 0, 
  "ClipWt7" FLOAT NULL DEFAULT 0, 
  "ClipWt8" FLOAT NULL DEFAULT 0, 
  "ClipWt9" FLOAT NULL DEFAULT 0, 
  "ClipWt10" FLOAT NULL DEFAULT 0, 
  "ClipWt11" FLOAT NULL DEFAULT 0, 
  "ClipWt12" FLOAT NULL DEFAULT 0, 
  "ClipWt13" FLOAT NULL DEFAULT 0, 
  "ClipWt14" FLOAT NULL DEFAULT 0, 
  "ClipWt15" FLOAT NULL DEFAULT 0, 
  "ClipWt16" FLOAT NULL DEFAULT 0, 
  "ClipWt17" FLOAT NULL DEFAULT 0, 
  "ClipWt18" FLOAT NULL DEFAULT 0, 
  "ClipWt19" FLOAT NULL DEFAULT 0, 
  "ClipWt20" FLOAT NULL DEFAULT 0, 
  "ADWAdj" VARCHAR(5) DEFAULT E'1', 
  "UtilAdj" VARCHAR(5) DEFAULT E'1', 
  "GwthAdj" VARCHAR(5) DEFAULT E'1', 
  "WthrAdj" VARCHAR(5) DEFAULT E'1', 
  "ClippedEstWt" FLOAT NULL DEFAULT 0, 
  "ClippedClipWt" FLOAT NULL DEFAULT 0, 
  "ClipEstCF" FLOAT NULL DEFAULT 0, 
  "TotalWt" FLOAT NULL DEFAULT 0, 
  "TotalWtHectare" FLOAT NULL DEFAULT 0, 
  PRIMARY KEY ("DetailKey"), 
  FOREIGN KEY ("RecKey") REFERENCES dima."tblPlantProdHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlotMgtDetail'
--

DROP TABLE IF EXISTS dima."tblPlotMgtDetail" CASCADE;

CREATE TABLE dima."tblPlotMgtDetail" (
  "RecKey" VARCHAR(20), 
  "MgtKey" VARCHAR(20) NOT NULL, 
  "DateRecorded" TIMESTAMP, 
  "Names" VARCHAR(255), 
  "Crop" VARCHAR(255), 
  "Activity" VARCHAR(255), 
  "Tillage" VARCHAR(255), 
  "RandomRoughness" FLOAT NULL,  --mm
  "RidgeHeight" FLOAT NULL,  --cm
  "RidgeSpacing" FLOAT NULL,  --cm
  "RidgeAspect" FLOAT NULL, 
  "RidgeHeightStdDev" FLOAT NULL, 
  "RidgeSpacingStdDev" FLOAT NULL, 
  "RidgeAspectStdDev" FLOAT NULL DEFAULT 0, 
  "IrrigateMethod" VARCHAR(25), 
  "IrrigateAmount" FLOAT NULL,  --cm/hr
  "Herb_Pesticide" FLOAT NULL,  --/ac
  "Fertilizer" VARCHAR(255),  --/ac
  "EstYield" VARCHAR(255),  --lbs/ac
  "StockingRate" VARCHAR(255), 
  "PastureSize" VARCHAR(255), 
  "Livestock" VARCHAR(255), 
  "Duration" INTEGER,  --months
  "PastureImprovement" VARCHAR(255), 
  "Notes" TEXT, 
  PRIMARY KEY ("MgtKey"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblPlotMgtHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPTFrameDetail'
--

DROP TABLE IF EXISTS dima."tblPTFrameDetail" CASCADE;

CREATE TABLE dima."tblPTFrameDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(25), 
  "Cell" SMALLINT DEFAULT 0, 
  "SurfCover" VARCHAR(12) DEFAULT E'0', 
  "TopCover" VARCHAR(12) DEFAULT E'0',
  PRIMARY KEY ("RecKey", "Cell"), --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblPTFrameHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblQualDetail'
--

DROP TABLE IF EXISTS dima."tblQualDetail" CASCADE;

CREATE TABLE dima."tblQualDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(25), 
  "Seq" SMALLINT DEFAULT 0, 
  "Rating" SMALLINT DEFAULT 0, 
  "SSSWt" FLOAT NULL DEFAULT 0, 
  "SSSVxW" FLOAT NULL DEFAULT 0, 
  "HFWt" FLOAT NULL DEFAULT 0, 
  "HFVxW" FLOAT NULL DEFAULT 0, 
  "BIWt" FLOAT NULL DEFAULT 0, 
  "BIVxW" FLOAT NULL DEFAULT 0, 
  "Comment" TEXT,
  PRIMARY KEY ("RecKey", "Seq"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblQualHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblRiparProHeader'
--

DROP TABLE IF EXISTS dima."tblRiparProHeader" CASCADE;

CREATE TABLE dima."tblRiparProHeader" (  --no primary key in DIMA
  dbkey VARCHAR(255) NOT NULL, 
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50), 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(8), 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "LineStartSide" VARCHAR(5), 
  "StringLength" FLOAT NULL DEFAULT 0, 
  "StringLengthType" SMALLINT DEFAULT 0, 
  "SpacingInterval" INTEGER DEFAULT 0, 
  "bankAngle0Vert" FLOAT NULL DEFAULT 0, 
  "bankAngle0Horiz" FLOAT NULL DEFAULT 0, 
  "bankAngle0Pct" FLOAT NULL DEFAULT 0, 
  "bankAngleNon0Vert" FLOAT NULL DEFAULT 0, 
  "bankAngleNon0Horiz" FLOAT NULL DEFAULT 0, 
  "bankAngleNon0Pct" FLOAT NULL DEFAULT 0, 
  "width" FLOAT NULL DEFAULT 0, 
  "Depth" FLOAT NULL DEFAULT 0, 
  "widthDepthRatio" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  PRIMARY KEY ("RecKey"),  --added
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblRiparSurvHeader'
--

DROP TABLE IF EXISTS dima."tblRiparSurvHeader" CASCADE;

CREATE TABLE dima."tblRiparSurvHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(5), 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Direction" VARCHAR(20), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" INTEGER DEFAULT 0, 
  "SpacingIntervalAmount" INTEGER DEFAULT 0, 
  "SpacingType" VARCHAR(5), 
  "HeightOption" VARCHAR(15), 
  "HeightUOM" VARCHAR(5), 
  "numCanopy" INTEGER DEFAULT 0, 
  "numBare" INTEGER DEFAULT 0, 
  "numBasal" INTEGER DEFAULT 0, 
  "pctCanopy" FLOAT NULL DEFAULT 0, 
  "pctBare" FLOAT NULL DEFAULT 0, 
  "pctBasal" FLOAT NULL DEFAULT 0, 
  "numStabilizing" INTEGER DEFAULT 0, 
  "numWoody" INTEGER DEFAULT 0, 
  "numTotCanopy" INTEGER DEFAULT 0, 
  "pctStabilizing" FLOAT NULL DEFAULT 0, 
  "pctWoody" FLOAT NULL DEFAULT 0, 
  "ratioStabilizing" FLOAT NULL DEFAULT 0, 
  "avgHeight" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSageEval'
--

DROP TABLE IF EXISTS dima."tblSageEval" CASCADE;

CREATE TABLE dima."tblSageEval" (
  "RecKey" VARCHAR(25), 
  "EvalKey" VARCHAR(25) NOT NULL, 
  "FormType" VARCHAR(10), 
  "EvalDate" DATE, 
  "Evaluators" VARCHAR(255), 
  "LekID" VARCHAR(25),  --S-2 only
  "RelatedLeks" VARCHAR(255),  --S-3 only
  "RelatedLeksID" VARCHAR(255),  --S-3 only
  "LekStatus" VARCHAR(20),  --S-2
  "SagebrushCovAvailability" VARCHAR(15),  --S-2
  "DetrimentalLandProx" VARCHAR(15),  --S-2
  "TreeProx" VARCHAR(15),  --S-2
  "SiteSuitability" VARCHAR(15), 
  "SagebrushCanCover" VARCHAR(15),  --S-3
  "SagebrushHeight" VARCHAR(15),  --S-3
  "SagebrushShape" VARCHAR(15),  --S-3
  "PerenGrassHeight" VARCHAR(15),  --S-3
  "PerenForbHeight" VARCHAR(15),  --S-3
  "PerenGrassCover" VARCHAR(15),  --S-3
  "PerenForbCover" VARCHAR(15),  --S-3
  "PrefForbAvail" VARCHAR(15),  --S-3
  "AnthropogenicNoise" TEXT,  --S-2
  "Rationale" TEXT, 
  "LimitedSuitability" VARCHAR(15),  --S-3/S-4
  "DroughtCondition" VARCHAR(25),  --S-3/S-4/S-5
  "RiparianSiteType" VARCHAR(25),  --S-5
  "RiparianStability" VARCHAR(15),  --S-5
  "SagebrushAvailability" VARCHAR(50),  --S-5
  "Notes" TEXT, 
  "GPSFile" VARCHAR(50),  --S-2
  "dspSagebrushCover" VARCHAR(10), 
  "dspSagebrushHeight" VARCHAR(10), 
  "dspSpreading" VARCHAR(10), 
  "dspColumnar" VARCHAR(10), 
  "dspPerennialGrassHeight" VARCHAR(15), 
  "dspPerennialForbHeight" VARCHAR(15), 
  "dspPerennialGrassCover" VARCHAR(15), 
  "dspPerennialForbCover" VARCHAR(15), 
  "dspPerennialGrassForbCover" VARCHAR(15), 
  "dspPreferredForbSpecies" VARCHAR(15), 
  "datesUsed" VARCHAR(255), 
  PRIMARY KEY ("EvalKey"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblSageRange"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSageLek'
--

DROP TABLE IF EXISTS dima."tblSageLek" CASCADE;

CREATE TABLE dima."tblSageLek" (
  "RecKey" VARCHAR(25), 
  "LekID" VARCHAR(25) NOT NULL, 
  "DateEstablished" DATE, 
  "LekDesc" VARCHAR(255), 
  "Easting" DOUBLE PRECISION NULL DEFAULT 0, 
  "Northing" DOUBLE PRECISION NULL DEFAULT 0, 
  "LineKey" VARCHAR(255),  --related transect
  "Notes" VARCHAR(255), 
  "Longitude" DOUBLE PRECISION NULL DEFAULT 0,  --decimal degrees
  "Latitude" DOUBLE PRECISION NULL DEFAULT 0,  --decimal degrees
  "CoordinatesProtected" BOOLEAN DEFAULT E'0',  --if TRUE, coordinate data is not required,
  PRIMARY KEY ("LekID"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblSageRange"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSoilPitHorizons'
--

DROP TABLE IF EXISTS dima."tblSoilPitHorizons" CASCADE;

CREATE TABLE dima."tblSoilPitHorizons" (
  "SoilKey" VARCHAR(20) NOT NULL, 
  "HorizonKey" VARCHAR(20) NOT NULL,  --Computed YYMMDDHHMMSS + random number < 9999
  "HorizonDepthUpper" VARCHAR(50), 
  "DepthMeasure" VARCHAR(5),  --'cm' or 'in'
  "Texture" VARCHAR(150), 
  "RockFragments" FLOAT NULL DEFAULT 0, 
  "Effer" VARCHAR(50), 
  "HorizonColorDry" VARCHAR(50), 
  "HorizonColorMoist" VARCHAR(50), 
  "StructGrade" VARCHAR(50), 
  "StructShape" VARCHAR(50), 
  "Nomenclature" VARCHAR(50), 
  "HorizonDepthLower" VARCHAR(50) DEFAULT E'0', 
  "ESD_Horizon" VARCHAR(10), 
  "ESD_HorizonModifier" VARCHAR(15), 
  "ESD_FragVolPct" VARCHAR(50), 
  "ESD_FragmentType" VARCHAR(255), 
  "ESD_PetrocalcicRubble" BOOLEAN, 
  "ESD_Gypsic" BOOLEAN, 
  "ESD_PctClay" FLOAT NULL, 
  "ESD_Hue" VARCHAR(50), 
  "ESD_Value" VARCHAR(50), 
  "ESD_Chroma" DOUBLE PRECISION NULL, 
  "ESD_Color" VARCHAR(5), 
  "ESD_Grade" VARCHAR(50), 
  "ESD_Size" VARCHAR(50), 
  "ESD_Structure" VARCHAR(50), 
  "ESD_StructQual" VARCHAR(25), 
  "ESD_Grade2" VARCHAR(50), 
  "ESD_Size2" VARCHAR(50), 
  "ESD_Structure2" VARCHAR(50), 
  "ESD_RuptureResistance" VARCHAR(50), 
  "ESD_ClayFilm" BOOLEAN, 
  "ESD_CarbonateStage" SMALLINT, 
  "ESD_CaCO3EquivPct" FLOAT NULL, 
  "ESD_EC" FLOAT NULL, 
  "ESD_pH" FLOAT NULL, 
  "ESD_GypsumPct" FLOAT NULL, 
  "ESD_NAabsorptionRatio" FLOAT NULL, 
  "ESD_Notes" VARCHAR(255), 
  "ESD_PSAPctSand" FLOAT NULL, 
  "ESD_PSAPctSilt" FLOAT NULL, 
  "ESD_PSAPctClay" FLOAT NULL, 
  "ESD_GravelClassPctFine" FLOAT NULL, 
  "ESD_GravelClassPctMed" FLOAT NULL, 
  "ESD_GravelClassPctCoarse" FLOAT NULL, 
  "ESD_GravelCarbonateCoatPct" FLOAT NULL, 
  "ESD_FragmentRoundness" VARCHAR(5), 
  "ESD_RootSize" VARCHAR(5), 
  "ESD_RootQty" VARCHAR(15), 
  "ESD_PoresSize" VARCHAR(5), 
  "ESD_PoresQty" VARCHAR(15), 
  "ESD_SandFractPctVeryFine" FLOAT NULL, 
  "ESD_SandFractPctFine" FLOAT NULL, 
  "ESD_SandFractPctMed" FLOAT NULL, 
  "ESD_SandFractPctCoarse" FLOAT NULL, 
  "ESD_SandFractPctVeryCoarse" FLOAT NULL, 
  "ESD_FragVolPct2" VARCHAR(50), 
  "ESD_FragmentType2" VARCHAR(255), 
  "ESD_FragVolPct3" VARCHAR(50), 
  "ESD_FragmentType3" VARCHAR(255), 
  "ESD_PctSand" FLOAT NULL, 
  "ESD_LabGravelPctFine" FLOAT NULL, 
  "ESD_LabGravelPctMed" FLOAT NULL, 
  "ESD_LabGravelPctCoarse" FLOAT NULL, 
  "HorizonDepthUpperNum" FLOAT NULL DEFAULT 0,  --automatically generated - needed to sort properly
  PRIMARY KEY ("HorizonKey"),
  FOREIGN KEY ("SoilKey") REFERENCES dima."tblSoilPits"("SoilKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSoilStabDetail'
--

DROP TABLE IF EXISTS dima."tblSoilStabDetail" CASCADE;

CREATE TABLE dima."tblSoilStabDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "BoxNum" SMALLINT DEFAULT 0,   --1,2 or 3
  "Line1" VARCHAR(10), 
  "Line2" VARCHAR(10), 
  "Line3" VARCHAR(10), 
  "Line4" VARCHAR(10), 
  "Line5" VARCHAR(10), 
  "Line6" VARCHAR(10), 
  "Pos1" VARCHAR(5) DEFAULT E'0', 
  "Pos2" VARCHAR(5) DEFAULT E'0', 
  "Pos3" VARCHAR(5) DEFAULT E'0', 
  "Pos4" VARCHAR(5) DEFAULT E'0', 
  "Pos5" VARCHAR(5) DEFAULT E'0', 
  "Pos6" VARCHAR(5) DEFAULT E'0', 
  "Pos7" VARCHAR(5) DEFAULT E'0', 
  "Pos8" VARCHAR(5) DEFAULT E'0', 
  "Pos9" VARCHAR(5) DEFAULT E'0', 
  "Pos10" VARCHAR(5) DEFAULT E'0', 
  "Pos11" VARCHAR(5) DEFAULT E'0', 
  "Pos12" VARCHAR(5) DEFAULT E'0', 
  "Pos13" VARCHAR(5) DEFAULT E'0', 
  "Pos14" VARCHAR(5) DEFAULT E'0', 
  "Pos15" VARCHAR(5) DEFAULT E'0', 
  "Pos16" VARCHAR(5) DEFAULT E'0', 
  "Pos17" VARCHAR(5) DEFAULT E'0', 
  "Pos18" VARCHAR(5) DEFAULT E'0', 
  "Veg1" VARCHAR(5), 
  "Veg2" VARCHAR(5), 
  "Veg3" VARCHAR(5), 
  "Veg4" VARCHAR(5), 
  "Veg5" VARCHAR(5), 
  "Veg6" VARCHAR(5), 
  "Veg7" VARCHAR(5), 
  "Veg8" VARCHAR(5), 
  "Veg9" VARCHAR(5), 
  "Veg10" VARCHAR(5), 
  "Veg11" VARCHAR(5), 
  "Veg12" VARCHAR(5), 
  "Veg13" VARCHAR(5), 
  "Veg14" VARCHAR(5), 
  "Veg15" VARCHAR(5), 
  "Veg16" VARCHAR(5), 
  "Veg17" VARCHAR(5), 
  "Veg18" VARCHAR(5), 
  "Rating1" VARCHAR(1), 
  "Rating2" VARCHAR(1), 
  "Rating3" VARCHAR(1), 
  "Rating4" VARCHAR(1), 
  "Rating5" VARCHAR(1), 
  "Rating6" VARCHAR(1), 
  "Rating7" VARCHAR(1), 
  "Rating8" VARCHAR(1), 
  "Rating9" VARCHAR(1), 
  "Rating10" VARCHAR(1), 
  "Rating11" VARCHAR(1), 
  "Rating12" VARCHAR(1), 
  "Rating13" VARCHAR(1), 
  "Rating14" VARCHAR(1), 
  "Rating15" VARCHAR(1), 
  "Rating16" VARCHAR(1), 
  "Rating17" VARCHAR(1), 
  "Rating18" VARCHAR(1), 
  "Hydro1" BOOLEAN DEFAULT E'0', 
  "Hydro2" BOOLEAN DEFAULT E'0', 
  "Hydro3" BOOLEAN DEFAULT E'0', 
  "Hydro4" BOOLEAN DEFAULT E'0', 
  "Hydro5" BOOLEAN DEFAULT E'0', 
  "Hydro6" BOOLEAN DEFAULT E'0', 
  "Hydro7" BOOLEAN DEFAULT E'0', 
  "Hydro8" BOOLEAN DEFAULT E'0', 
  "Hydro9" BOOLEAN DEFAULT E'0', 
  "Hydro10" BOOLEAN DEFAULT E'0', 
  "Hydro11" BOOLEAN DEFAULT E'0', 
  "Hydro12" BOOLEAN DEFAULT E'0', 
  "Hydro13" BOOLEAN DEFAULT E'0', 
  "Hydro14" BOOLEAN DEFAULT E'0', 
  "Hydro15" BOOLEAN DEFAULT E'0', 
  "Hydro16" BOOLEAN DEFAULT E'0', 
  "Hydro17" BOOLEAN DEFAULT E'0', 
  "Hydro18" BOOLEAN DEFAULT E'0', 
  "In1" VARCHAR(5),  --The module 'modCalcSoilStab' knows the field index of this item.  If fields are added or deleted ABOVE me, that module must be changed.
  "In2" VARCHAR(5), 
  "In3" VARCHAR(5), 
  "In4" VARCHAR(5), 
  "In5" VARCHAR(5), 
  "In6" VARCHAR(5), 
  "In7" VARCHAR(5), 
  "In8" VARCHAR(5), 
  "In9" VARCHAR(5), 
  "In10" VARCHAR(5), 
  "In11" VARCHAR(5), 
  "In12" VARCHAR(5), 
  "In13" VARCHAR(5), 
  "In14" VARCHAR(5), 
  "In15" VARCHAR(5), 
  "In16" VARCHAR(5), 
  "In17" VARCHAR(5), 
  "In18" VARCHAR(5), 
  "Dip1" VARCHAR(5), 
  "Dip2" VARCHAR(5), 
  "Dip3" VARCHAR(5), 
  "Dip4" VARCHAR(5), 
  "Dip5" VARCHAR(5), 
  "Dip6" VARCHAR(5), 
  "Dip7" VARCHAR(5), 
  "Dip8" VARCHAR(5), 
  "Dip9" VARCHAR(5), 
  "Dip10" VARCHAR(5), 
  "Dip11" VARCHAR(5), 
  "Dip12" VARCHAR(5), 
  "Dip13" VARCHAR(5), 
  "Dip14" VARCHAR(5), 
  "Dip15" VARCHAR(5), 
  "Dip16" VARCHAR(5), 
  "Dip17" VARCHAR(5), 
  "Dip18" VARCHAR(5),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblSoilStabHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);
-- Creating partial indices as BoxNum has no NOT NULL constraint, but RecKey with BoxNum should be unique
CREATE UNIQUE INDEX "tblSoilStabDetail_unique_boxnull" ON dima."tblSoilStabDetail" ("RecKey") WHERE "BoxNum" IS NULL; 
CREATE UNIQUE INDEX "tblSoilStabDetail_unique_boxnotnull" ON dima."tblSoilStabDetail" ("RecKey", "BoxNum") WHERE "BoxNum" IS NOT NULL;

--
-- Table structure for table 'tblSoilStabSubtotal'
--

DROP TABLE IF EXISTS dima."tblSoilStabSubtotal" CASCADE;

CREATE TABLE dima."tblSoilStabSubtotal" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "LineNum" VARCHAR(50), 
  "AllSurf" FLOAT NULL DEFAULT 0, 
  "AllSub" FLOAT NULL DEFAULT 0, 
  "ProSurf" FLOAT NULL DEFAULT 0, 
  "ProSub" FLOAT NULL DEFAULT 0, 
  "NCSurf" FLOAT NULL DEFAULT 0, 
  "NCSub" FLOAT NULL DEFAULT 0, 
  "NoVegSurf" FLOAT NULL DEFAULT 0, 
  "NoVegSub" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("RecKey", "LineNum"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblSoilStabHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSpecRichHeader'
--

DROP TABLE IF EXISTS dima."tblSpecRichHeader" CASCADE;

CREATE TABLE dima."tblSpecRichHeader" (
  dbkey VARCHAR(255) NOT NULL,
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "FormType" VARCHAR(8) DEFAULT E'0', 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "SpecRichMethod" SMALLINT DEFAULT 0, 
  "SpecRichMeasure" SMALLINT DEFAULT 0,  --Metric vs. English
  "SpecRichNbrSubPlots" SMALLINT DEFAULT 0, 
  "SpecRich1Container" BOOLEAN,  --if yes, all other plots are contained in this one
  "SpecRich1Shape" SMALLINT DEFAULT 0,  --1 = Rectangluar; 2 = circle
  "SpecRich1Dim1" FLOAT NULL DEFAULT 0,  --if Rectangle, one of the sides; if circle, the radius
  "SpecRich1Dim2" FLOAT NULL DEFAULT 0,  --if Rectangle, the other side
  "SpecRich1Area" FLOAT NULL DEFAULT 0, 
  "SpecRich2Container" BOOLEAN, 
  "SpecRich2Shape" SMALLINT DEFAULT 0, 
  "SpecRich2Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich2Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich2Area" FLOAT NULL DEFAULT 0, 
  "SpecRich3Container" BOOLEAN, 
  "SpecRich3Shape" SMALLINT DEFAULT 0, 
  "SpecRich3Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich3Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich3Area" FLOAT NULL DEFAULT 0, 
  "SpecRich4Container" BOOLEAN, 
  "SpecRich4Shape" SMALLINT DEFAULT 0, 
  "SpecRich4Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich4Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich4Area" FLOAT NULL DEFAULT 0, 
  "SpecRich5Container" BOOLEAN, 
  "SpecRich5Shape" SMALLINT DEFAULT 0, 
  "SpecRich5Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich5Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich5Area" FLOAT NULL DEFAULT 0, 
  "SpecRich6Container" BOOLEAN, 
  "SpecRich6Shape" SMALLINT DEFAULT 0, 
  "SpecRich6Dim1" FLOAT NULL DEFAULT 0, 
  "SpecRich6Dim2" FLOAT NULL DEFAULT 0, 
  "SpecRich6Area" FLOAT NULL DEFAULT 0, 
  "Notes" TEXT, 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblTreeDenDetail'
--

DROP TABLE IF EXISTS dima."tblTreeDenDetail" CASCADE;

CREATE TABLE dima."tblTreeDenDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(25), 
  "SubPlot" VARCHAR(15), 
  "SpeciesCode" VARCHAR(50), 
  "DBH" FLOAT NULL DEFAULT 0, 
  "DRC" FLOAT NULL DEFAULT 0, 
  "Height" FLOAT NULL DEFAULT 0, 
  "chkboxDead" BOOLEAN DEFAULT E'0',
  PRIMARY KEY ("RecKey", "SubPlot", "SpeciesCode"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblTreeDenHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblUtilTransect'
--

DROP TABLE IF EXISTS dima."tblUtilTransect" CASCADE;

CREATE TABLE dima."tblUtilTransect" (
  "RecKey" VARCHAR(50), 
  "SubPlotKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "Location" VARCHAR(25), 
  "Northing" DOUBLE PRECISION NULL DEFAULT 0, 
  "Easting" DOUBLE PRECISION NULL DEFAULT 0, 
  "Latitude" DOUBLE PRECISION NULL DEFAULT 0, 
  "Longitude" DOUBLE PRECISION NULL DEFAULT 0, 
  "Comments" TEXT, 
  PRIMARY KEY ("SubPlotKey"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblUtilHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblVegStructHeader'
--

DROP TABLE IF EXISTS dima."tblVegStructHeader" CASCADE;

CREATE TABLE dima."tblVegStructHeader" (
  dbkey VARCHAR(255) NOT NULL, 
  "LineKey" VARCHAR(50), 
  "RecKey" VARCHAR(50) NOT NULL, 
  "DateModified" TIMESTAMP, 
  "VegStructMethod" SMALLINT DEFAULT 0, 
  "FormDate" DATE, 
  "Observer" VARCHAR(50), 
  "Recorder" VARCHAR(50), 
  "DataEntry" VARCHAR(50), 
  "DataErrorChecking" VARCHAR(50), 
  "Measure" SMALLINT DEFAULT 0, 
  "LineLengthAmount" INTEGER DEFAULT 0, 
  "Notes" TEXT, 
  "NbrSamples" SMALLINT DEFAULT 0, 
  "Positions" VARCHAR(255), 
  "maxHeight1" FLOAT NULL DEFAULT 0, 
  "maxHeight2" FLOAT NULL DEFAULT 0, 
  "maxHeight3" FLOAT NULL DEFAULT 0, 
  "maxHeight4" FLOAT NULL DEFAULT 0, 
  "maxHeight5" FLOAT NULL DEFAULT 0, 
  "maxHeight6" FLOAT NULL DEFAULT 0, 
  "maxHeight7" FLOAT NULL DEFAULT 0, 
  "maxHeight8" FLOAT NULL DEFAULT 0, 
  "maxHeight9" FLOAT NULL DEFAULT 0, 
  "maxHeight10" FLOAT NULL DEFAULT 0, 
  "maxHeight11" FLOAT NULL DEFAULT 0, 
  "maxHeight12" FLOAT NULL DEFAULT 0, 
  "maxHeight13" FLOAT NULL DEFAULT 0, 
  "maxHeight14" FLOAT NULL DEFAULT 0, 
  "DataFormat" SMALLINT,  --1 = 0's and 1's; 2 = Percents
  "PctsMethod" SMALLINT,  --1 = A only; 2 = A and B
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("LineKey") REFERENCES dima."tblLines"("LineKey") ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (dbkey, "LineKey") REFERENCES dima.db_line(dbkey, "LineKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Level 5, FK to 0,1,2,3,4
--

--
-- Table structure for table 'tblBSNE_BoxCollection'
--

DROP TABLE IF EXISTS dima."tblBSNE_BoxCollection" CASCADE;

CREATE TABLE dima."tblBSNE_BoxCollection" (
  "RecKey" VARCHAR(255) NOT NULL, 
  "BoxID" VARCHAR(50),  --key of the specific box
  "DateModified" TIMESTAMP, 
  "collectDate" DATE, 
  "Collector" VARCHAR(30), 
  "labTech" VARCHAR(30), 
  "beakerNbr" VARCHAR(20), 
  "emptyWeight" FLOAT NULL, 
  "recordedWeight" FLOAT NULL, 
  "sedimentWeight" FLOAT NULL,  --computed
  "daysExposed" INTEGER,  --computed
  "sedimentGperDay" FLOAT NULL,  --computed - grams/day
  "sedimentArchived" BOOLEAN DEFAULT E'0', 
  "Notes" TEXT, 
  "sedimentGperDayByInlet" FLOAT NULL,  --computed - grams/day divided by Inlet Area
  "SeqNo" SMALLINT,  --for use when they have multiple collections on the same day
  "SampleCompromised" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("RecKey"),
  FOREIGN KEY ("BoxID") REFERENCES dima."tblBSNE_Box"("BoxID") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblCanopyGapDetail'
--

DROP TABLE IF EXISTS dima."tblCanopyGapDetail" CASCADE;

CREATE TABLE dima."tblCanopyGapDetail" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "Species" VARCHAR(50) NOT NULL, 
  "StartPos" FLOAT NOT NULL, 
  "Length" FLOAT NULL, 
  "Chkbox" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("RecKey", "Species", "StartPos"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblCanopyGapHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblCanopyGapSpecies'
--

DROP TABLE IF EXISTS dima."tblCanopyGapSpecies" CASCADE;

CREATE TABLE dima."tblCanopyGapSpecies" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "Species" VARCHAR(50) NOT NULL, 
  "CovLength" FLOAT NULL,  --sum of all lengths
  "CovPct" FLOAT NULL,  --sum of all lengths / line-length
  "PctComp" FLOAT NULL DEFAULT 0,  --CovPct / total CovPct all Species
  "PctChecked" FLOAT NULL DEFAULT 0, 
  "LengthChecked" FLOAT NULL DEFAULT 0, 
  PRIMARY KEY ("RecKey", "Species"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblCanopyGapHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblDryWtCompYield'
--

DROP TABLE IF EXISTS dima."tblDryWtCompYield" CASCADE;

CREATE TABLE dima."tblDryWtCompYield" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "SubPlot" INTEGER DEFAULT 0, 
  "ClippedOrEst" VARCHAR(5), 
  "GreenWt" FLOAT NULL DEFAULT 0, 
  "GreenWtLbsAcre" FLOAT NULL DEFAULT 0, 
  "AirDryWt" FLOAT NULL DEFAULT 0, 
  "AirDryWtLbsAcre" FLOAT NULL DEFAULT 0, 
  "PctAirDry" FLOAT NULL DEFAULT 0, 
  "PctUngrazed" FLOAT NULL DEFAULT 100, 
  "PctGrowthComp" FLOAT NULL DEFAULT 100, 
  "PctNormalClimate" FLOAT NULL DEFAULT 100, 
  "AirDryWtAdjust" FLOAT NULL DEFAULT 0, 
  "ReconFactor" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("RecKey", "SubPlot"), -- based on table/form inspection
  FOREIGN KEY ("RecKey") REFERENCES dima."tblDryWtHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblDryWtDetail'
--

DROP TABLE IF EXISTS dima."tblDryWtDetail" CASCADE;

CREATE TABLE dima."tblDryWtDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "QuadratID" INTEGER DEFAULT 0, 
  "SpeciesRank1" VARCHAR(35), 
  "SpeciesRank2" VARCHAR(35), 
  "SpeciesRank3" VARCHAR(35), 
  "BasalCover1" VARCHAR(30), 
  "NearestPerennial" VARCHAR(5), 
  "BasalCover2" VARCHAR(30), 
  "BasalCover3" VARCHAR(30), 
  "BasalCover4" VARCHAR(30), 
  "BasalCover5" VARCHAR(30), 
  "CompYieldSubPlot" INTEGER,
  PRIMARY KEY ("RecKey", "QuadratID"),  -- based on table/form inspection
  FOREIGN KEY ("RecKey") REFERENCES dima."tblDryWtHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblDryWtSpecies'
--

DROP TABLE IF EXISTS dima."tblDryWtSpecies" CASCADE;

CREATE TABLE dima."tblDryWtSpecies" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "SpeciesCode" VARCHAR(50) DEFAULT E'0', 
  "TallyRank1" INTEGER DEFAULT 0, 
  "TallyRank2" INTEGER DEFAULT 0, 
  "TallyRank3" INTEGER DEFAULT 0, 
  "Weighted" INTEGER DEFAULT 0, 
  "PctComp" FLOAT NULL DEFAULT 0, 
  "lbsPerAcre" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("RecKey", "SpeciesCode"),  -- based on table/form inspection
  FOREIGN KEY ("RecKey") REFERENCES dima."tblDryWtHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblGapDetail'
--

DROP TABLE IF EXISTS dima."tblGapDetail" CASCADE;

CREATE TABLE dima."tblGapDetail" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "SeqNo" SERIAL NOT NULL, 
  "RecType" VARCHAR(1),  --C = Canopy; B = Basal
  "GapStart" VARCHAR(50) DEFAULT E'0', 
  "GapEnd" VARCHAR(50) DEFAULT E'0', 
  "Gap" VARCHAR(50) DEFAULT E'0', 
  PRIMARY KEY ("RecKey", "SeqNo"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblGapHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblLICDetail'
--

DROP TABLE IF EXISTS dima."tblLICDetail" CASCADE;

CREATE TABLE dima."tblLICDetail" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "Species" VARCHAR(50) NOT NULL, 
  "StartPos" FLOAT NOT NULL, 
  "EndPos" FLOAT NULL, 
  "Length" FLOAT NULL, 
  "Height" FLOAT NULL, 
  "Chkbox" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("RecKey", "Species", "StartPos"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblLICHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblLICSpecies'
--

DROP TABLE IF EXISTS dima."tblLICSpecies" CASCADE;

CREATE TABLE dima."tblLICSpecies" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "Species" VARCHAR(50) NOT NULL, 
  "CovLength" FLOAT NULL,  --sum of all lengths
  "CovPct" FLOAT NULL,  --sum of all lengths / line-length
  "AvgHeight" FLOAT NULL, 
  "PctComp" FLOAT NULL DEFAULT 0,  --CovPct / total CovPct all Species
  "PctChecked" FLOAT NULL DEFAULT 0, 
  "LengthChecked" FLOAT NULL DEFAULT 0, 
  PRIMARY KEY ("RecKey", "Species"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblLICHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblLPIDetail'
--

DROP TABLE IF EXISTS dima."tblLPIDetail" CASCADE;

CREATE TABLE dima."tblLPIDetail" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "PointLoc" FLOAT NOT NULL,  --the actual position along the transect
  "PointNbr" INTEGER,  --a sequential number starting from 1 - used for checking to see if Height is required (every other, every 5th, etc)
  "TopCanopy" VARCHAR(50) DEFAULT E'0', 
  "Lower1" VARCHAR(50) DEFAULT E'0', 
  "Lower2" VARCHAR(50) DEFAULT E'0', 
  "Lower3" VARCHAR(50) DEFAULT E'0', 
  "Lower4" VARCHAR(50) DEFAULT E'0', 
  "SoilSurface" VARCHAR(50) DEFAULT E'0', 
  "HeightTop" VARCHAR(12) DEFAULT E'0', 
  "ChkboxTop" BOOLEAN DEFAULT E'0', 
  "ChkboxLower1" BOOLEAN DEFAULT E'0', 
  "ChkboxLower2" BOOLEAN DEFAULT E'0', 
  "ChkboxLower3" BOOLEAN DEFAULT E'0', 
  "ChkboxLower4" BOOLEAN DEFAULT E'0', 
  "ChkboxSoil" BOOLEAN DEFAULT E'0', 
  "HeightLower1" VARCHAR(12) DEFAULT E'0', 
  "HeightLower2" VARCHAR(12) DEFAULT E'0', 
  "HeightLower3" VARCHAR(12) DEFAULT E'0', 
  "HeightLower4" VARCHAR(12) DEFAULT E'0', 
  "HeightSurface" VARCHAR(12) DEFAULT E'0', 
  "HeightWoody" VARCHAR(12) DEFAULT E'0', 
  "HeightHerbaceous" VARCHAR(12) DEFAULT E'0', 
  "ShrubShape" VARCHAR(5), 
  "SpeciesWoody" VARCHAR(12) DEFAULT E'0', 
  "SpeciesHerbaceous" VARCHAR(12) DEFAULT E'0', 
  "ChkboxWoody" BOOLEAN DEFAULT E'0', 
  "ChkboxHerbaceous" BOOLEAN DEFAULT E'0', 
  "Lower5" VARCHAR(50) DEFAULT E'0', 
  "Lower6" VARCHAR(50) DEFAULT E'0', 
  "Lower7" VARCHAR(50) DEFAULT E'0', 
  "ChkboxLower5" BOOLEAN DEFAULT E'0', 
  "ChkboxLower6" BOOLEAN DEFAULT E'0', 
  "ChkboxLower7" BOOLEAN DEFAULT E'0', 
  "HeightLower5" VARCHAR(12) DEFAULT E'0', 
  "HeightLower6" VARCHAR(12) DEFAULT E'0', 
  "HeightLower7" VARCHAR(12) DEFAULT E'0', 
  "SpeciesLowerHerb" VARCHAR(12) DEFAULT E'0', 
  "HeightLowerHerb" VARCHAR(12) DEFAULT E'0', 
  "ChkboxLowerHerb" BOOLEAN DEFAULT E'0', 
  PRIMARY KEY ("RecKey", "PointLoc"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblLPIHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblLPISpecies'
--

DROP TABLE IF EXISTS dima."tblLPISpecies" CASCADE;

CREATE TABLE dima."tblLPISpecies" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "Type" VARCHAR(5),  --C = Canopy; B = Basal; H = Height
  "SpeciesCode" VARCHAR(50) DEFAULT E'0', 
  "Data" FLOAT NULL DEFAULT 0, 
  "avgHeightTop" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightLower1" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightLower2" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightLower3" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightLower4" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightSurface" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightWoody" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightHerb" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightLower5" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightLower6" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightLower7" FLOAT NULL DEFAULT 0,  --type H only
  "avgHeightLowerHerb" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("RecKey", "Type", "SpeciesCode"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblLPIHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblNestedFreqDetail'
--

DROP TABLE IF EXISTS dima."tblNestedFreqDetail" CASCADE;

CREATE TABLE dima."tblNestedFreqDetail" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "PointNbr" INTEGER NOT NULL,  --a sequential number starting from 1 - used for checking to see if Height is required (every other, every 5th, etc)
  "PointLoc" FLOAT NULL,  --the actual position along the transect
  "Canopy1" VARCHAR(50) DEFAULT E'0', 
  "Canopy2" VARCHAR(50) DEFAULT E'0', 
  "Canopy3" VARCHAR(50) DEFAULT E'0', 
  "Canopy4" VARCHAR(50) DEFAULT E'0', 
  "Mid1" VARCHAR(50) DEFAULT E'0', 
  "Mid2" VARCHAR(50) DEFAULT E'0', 
  "Mid3" VARCHAR(50) DEFAULT E'0', 
  "Mid4" VARCHAR(50) DEFAULT E'0', 
  "Ground1" VARCHAR(50) DEFAULT E'0', 
  "Ground2" VARCHAR(50) DEFAULT E'0', 
  "Ground3" VARCHAR(50) DEFAULT E'0', 
  "Ground4" VARCHAR(50) DEFAULT E'0', 
  PRIMARY KEY ("RecKey", "PointNbr"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblNestedFreqHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblNestedFreqSpeciesDetail'
--

DROP TABLE IF EXISTS dima."tblNestedFreqSpeciesDetail" CASCADE;

CREATE TABLE dima."tblNestedFreqSpeciesDetail" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "PointNbr" INTEGER NOT NULL,  --a sequential number starting from 1 - used for checking to see if Height is required (every other, every 5th, etc)
  "Species" VARCHAR(20) NOT NULL, 
  "Freq" INTEGER, 
  PRIMARY KEY ("RecKey", "PointNbr", "Species"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblNestedFreqHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE 
);

--
-- Table structure for table 'tblNestedFreqSpeciesSummary'
--

DROP TABLE IF EXISTS dima."tblNestedFreqSpeciesSummary" CASCADE;

CREATE TABLE dima."tblNestedFreqSpeciesSummary" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "Species" VARCHAR(20) NOT NULL, 
  "Hits1" INTEGER, 
  "Hits2" INTEGER, 
  "Hits3" INTEGER, 
  "Hits4" INTEGER, 
  PRIMARY KEY ("RecKey", "Species"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblNestedFreqHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE 
);

--
-- Table structure for table 'tblOcularCovDetail'
--

DROP TABLE IF EXISTS dima."tblOcularCovDetail" CASCADE;

CREATE TABLE dima."tblOcularCovDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(50) NOT NULL, 
  "SpeciesCode" VARCHAR(30) NOT NULL, 
  "Foliar" BOOLEAN DEFAULT E'0', 
  "Canopy" BOOLEAN DEFAULT E'0', 
  "DKClass" VARCHAR(255), 
  "CoverMidPoint" VARCHAR(5), 
  "Production" VARCHAR(50), 
  "Notes" VARCHAR(255), 
  "Basal" BOOLEAN DEFAULT E'0', 
  "PlantType" VARCHAR(50), 
  "ProductionCheck" BOOLEAN DEFAULT E'0', 
  "pctCanopy" INTEGER, 
  "pctComposition" INTEGER, 
  "Frequency" INTEGER, 
  "SoilSurf" VARCHAR(5), 
  "quadrat" SMALLINT DEFAULT 0,
  PRIMARY KEY ("RecKey", "SpeciesCode"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblOcularCovHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlantDenQuads'
--

DROP TABLE IF EXISTS dima."tblPlantDenQuads" CASCADE;

CREATE TABLE dima."tblPlantDenQuads" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "Quadrat" INTEGER DEFAULT 0, 
  "Description" VARCHAR(255),
  PRIMARY KEY ("RecKey", "Quadrat"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblPlantDenHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlantDenSpecies'
--

DROP TABLE IF EXISTS dima."tblPlantDenSpecies" CASCADE;

CREATE TABLE dima."tblPlantDenSpecies" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "SpeciesCode" VARCHAR(50), 
  "SubQuadSize" FLOAT NULL DEFAULT 0, 
  "SubQuadSizeUOM" VARCHAR(8), 
  "Class1total" INTEGER DEFAULT 0, 
  "Class2total" INTEGER DEFAULT 0, 
  "Class3total" INTEGER DEFAULT 0, 
  "Class4total" INTEGER DEFAULT 0, 
  "Class5total" INTEGER DEFAULT 0, 
  "Class6total" INTEGER DEFAULT 0, 
  "Class7total" INTEGER DEFAULT 0, 
  "Class8total" INTEGER DEFAULT 0, 
  "Class9total" INTEGER DEFAULT 0, 
  "Class1density" FLOAT NULL DEFAULT 0, 
  "Class2density" FLOAT NULL DEFAULT 0, 
  "Class3density" FLOAT NULL DEFAULT 0, 
  "Class4density" FLOAT NULL DEFAULT 0, 
  "Class5density" FLOAT NULL DEFAULT 0, 
  "Class6density" FLOAT NULL DEFAULT 0, 
  "Class7density" FLOAT NULL DEFAULT 0, 
  "Class8density" FLOAT NULL DEFAULT 0, 
  "Class9density" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("RecKey", "SpeciesCode"), --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblPlantDenHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblPlotMgtCropData'
--

DROP TABLE IF EXISTS dima."tblPlotMgtCropData" CASCADE;

CREATE TABLE dima."tblPlotMgtCropData" (
  "RecKey" VARCHAR(255) NOT NULL, 
  "MgtKey" VARCHAR(20), 
  "DataType" VARCHAR(255), --H=Height; S=Spacing; A=Aspect
  "numData" FLOAT NULL, 
  PRIMARY KEY ("RecKey"), 
  FOREIGN KEY ("MgtKey") REFERENCES dima."tblPlotMgtDetail"("MgtKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblRiparProDetail'
--

DROP TABLE IF EXISTS dima."tblRiparProDetail" CASCADE;

CREATE TABLE dima."tblRiparProDetail" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "PointLoc" FLOAT NOT NULL,  --the actual position along the transect, in eaither CM or inches
  "PointDesc" VARCHAR(20),  --position in meters or feet/inches
  "Depth" FLOAT NULL DEFAULT 0, 
  "Marker" VARCHAR(50) DEFAULT E'0',  --designates both Bank Tops and Bottoms
  PRIMARY KEY ("RecKey", "PointLoc"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblRiparProHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblRiparSurvDetail'
--

DROP TABLE IF EXISTS dima."tblRiparSurvDetail" CASCADE;

CREATE TABLE dima."tblRiparSurvDetail" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "PointLoc" FLOAT NOT NULL,  --the actual position along the transect
  "PointNbr" INTEGER,  --a sequential number starting from 1 - used for checking to see if Height is required (every other, every 5th, etc)
  "TopCanopy" VARCHAR(50) DEFAULT E'0', 
  "Lower1" VARCHAR(50) DEFAULT E'0', 
  "Lower2" VARCHAR(50) DEFAULT E'0', 
  "Lower3" VARCHAR(50) DEFAULT E'0', 
  "Lower4" VARCHAR(50) DEFAULT E'0', 
  "SoilSurface" VARCHAR(50) DEFAULT E'0', 
  "HeightTop" VARCHAR(12) DEFAULT E'0', 
  PRIMARY KEY ("RecKey", "PointLoc"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblRiparSurvHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblRiparSurvSpecies'
--

DROP TABLE IF EXISTS dima."tblRiparSurvSpecies" CASCADE;

CREATE TABLE dima."tblRiparSurvSpecies" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "Type" VARCHAR(5),   --C = Canopy; B = Basal
  "SpeciesCode" VARCHAR(50) DEFAULT E'0', 
  "Data" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("RecKey", "Type", "SpeciesCode"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblRiparSurvHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSpecRichAbundance'
--

DROP TABLE IF EXISTS dima."tblSpecRichAbundance" CASCADE;

CREATE TABLE dima."tblSpecRichAbundance" (
  "RecKey" VARCHAR(50) NOT NULL, 
  "Species" VARCHAR(15) NOT NULL, 
  "Abundance" INTEGER DEFAULT 0,  --1 = Common; 2 = Rare
  PRIMARY KEY ("RecKey", "Species"),
  FOREIGN KEY ("RecKey") REFERENCES dima."tblSpecRichHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblSpecRichDetail'
--

DROP TABLE IF EXISTS dima."tblSpecRichDetail" CASCADE;

CREATE TABLE dima."tblSpecRichDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "subPlotID" SMALLINT DEFAULT 0, 
  "subPlotDesc" VARCHAR(15), 
  "SpeciesCount" SMALLINT DEFAULT 0, 
  "SpeciesList" TEXT,
  FOREIGN KEY ("RecKey") REFERENCES dima."tblSpecRichHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);
-- Creating partial indices as subPlotID has no NOT NULL constraint, but RecKey with subPlotID should be unique
CREATE UNIQUE INDEX "tblSpecRichDetail_unique_spnull" ON dima."tblSpecRichDetail" ("RecKey") WHERE "subPlotID" IS NULL; 
CREATE UNIQUE INDEX "tblSpecRichDetail_unique_spnotnull" ON dima."tblSpecRichDetail" ("RecKey", "subPlotID") WHERE "subPlotID" IS NOT NULL;

--
-- Table structure for table 'tblUtilDetail'
--

DROP TABLE IF EXISTS dima."tblUtilDetail" CASCADE;

CREATE TABLE dima."tblUtilDetail" (  --no primary key in DIMA
  "SubPlotKey" VARCHAR(50), 
  "SpeciesCode" VARCHAR(15), 
  "Freq1" SMALLINT DEFAULT 0, 
  "Freq2" SMALLINT DEFAULT 0, 
  "Freq3" SMALLINT DEFAULT 0, 
  "Freq4" SMALLINT DEFAULT 0, 
  "Freq5" SMALLINT DEFAULT 0, 
  "Freq6" SMALLINT DEFAULT 0, 
  "Comp1" FLOAT NULL DEFAULT 0, 
  "Comp2" FLOAT NULL DEFAULT 0, 
  "Comp3" FLOAT NULL DEFAULT 0, 
  "Comp4" FLOAT NULL DEFAULT 0, 
  "Comp5" FLOAT NULL DEFAULT 0, 
  "Comp6" FLOAT NULL DEFAULT 0, 
  "FreqTotal" INTEGER DEFAULT 0, 
  "CompTotal" FLOAT NULL DEFAULT 0, 
  "AvgUtil" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("SubPlotKey", "SpeciesCode"),  --added
  FOREIGN KEY ("SubPlotKey") REFERENCES dima."tblUtilTransect"("SubPlotKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Table structure for table 'tblVegStructDetail'
--

DROP TABLE IF EXISTS dima."tblVegStructDetail" CASCADE;

CREATE TABLE dima."tblVegStructDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "Segment" SMALLINT DEFAULT 0, 
  "Band" SMALLINT DEFAULT 0, 
  "Height" FLOAT NULL DEFAULT 0, 
  "1A" INTEGER, 
  "1B" INTEGER, 
  "2A" INTEGER, 
  "2B" INTEGER, 
  "3A" INTEGER, 
  "3B" INTEGER, 
  "4A" INTEGER, 
  "4B" INTEGER, 
  "5A" INTEGER, 
  "5B" INTEGER, 
  "6A" INTEGER, 
  "6B" INTEGER, 
  "7A" INTEGER, 
  "7B" INTEGER, 
  "8A" INTEGER, 
  "8B" INTEGER, 
  "9A" INTEGER, 
  "9B" INTEGER, 
  "10A" INTEGER, 
  "10B" INTEGER, 
  "11A" INTEGER, 
  "11B" INTEGER, 
  "12A" INTEGER, 
  "12B" INTEGER, 
  "13A" INTEGER, 
  "13B" INTEGER, 
  "14A" INTEGER, 
  "14B" INTEGER,
  PRIMARY KEY ("RecKey", "Segment", "Band"),  --added
  FOREIGN KEY ("RecKey") REFERENCES dima."tblVegStructHeader"("RecKey") ON DELETE CASCADE ON UPDATE CASCADE
);

--
-- Level 6, FK to 0,1,2,3,4,5
--

--
-- Table structure for table 'tblPlantDenDetail'
--

DROP TABLE IF EXISTS dima."tblPlantDenDetail" CASCADE;

CREATE TABLE dima."tblPlantDenDetail" (  --no primary key in DIMA
  "RecKey" VARCHAR(50), 
  "Quadrat" INTEGER DEFAULT 0, 
  "SpeciesCode" VARCHAR(50), 
  "SubQuadSize" FLOAT NULL DEFAULT 0, 
  "SubQuadSizeUOM" VARCHAR(8), 
  "AreaInHectares" FLOAT NULL DEFAULT 0, 
  "Class1total" INTEGER DEFAULT 0, 
  "Class2total" INTEGER DEFAULT 0, 
  "Class3total" INTEGER DEFAULT 0, 
  "Class4total" INTEGER DEFAULT 0, 
  "Class5total" INTEGER DEFAULT 0, 
  "Class6total" INTEGER DEFAULT 0, 
  "Class7total" INTEGER DEFAULT 0, 
  "Class8total" INTEGER DEFAULT 0, 
  "Class9total" INTEGER DEFAULT 0, 
  "Class1density" FLOAT NULL DEFAULT 0, 
  "Class2density" FLOAT NULL DEFAULT 0, 
  "Class3density" FLOAT NULL DEFAULT 0, 
  "Class4density" FLOAT NULL DEFAULT 0, 
  "Class5density" FLOAT NULL DEFAULT 0, 
  "Class6density" FLOAT NULL DEFAULT 0, 
  "Class7density" FLOAT NULL DEFAULT 0, 
  "Class8density" FLOAT NULL DEFAULT 0, 
  "Class9density" FLOAT NULL DEFAULT 0,
  PRIMARY KEY ("RecKey", "Quadrat", "SpeciesCode"), --added
  FOREIGN KEY ("RecKey", "Quadrat") REFERENCES dima."tblPlantDenQuads"("RecKey", "Quadrat") ON DELETE CASCADE ON UPDATE CASCADE
);