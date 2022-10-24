--
-- LU
--
CREATE TABLE IF NOT EXISTS aim_lotic."LU_Ecoregion" (
    fid serial PRIMARY KEY,	
    "EcoregionLevel3" character varying(255),
    "EcoregionLevel2" character varying(255),
    "EcoregionLevel1" character varying(255),
    "Ecoregion" character varying(255),
    "EcoregionCode" character varying(255),
    "Climate" character varying(255),
    "ClimateCode" character varying(255),
    -- "RecordID" character varying(255),
    geom geometry(MULTIPOLYGON, 4269)
);

CREATE TABLE IF NOT EXISTS aim_lotic."LU_SpeciesMetadata" (
    "CommonName" character varying(255),
    "ScientificName" character varying(255),
    name character varying(255),
    label character varying(255),
    "WoodyHerb" character varying(255),
    "GrowthHabit" character varying(255),
    "Duration" character varying(255),
    image character varying(255),
    "AlwaysNoxious" character varying(255),
    "AlwaysNative" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("CommonName")
);

CREATE TABLE IF NOT EXISTS aim_lotic."LU_StateSpeciesList" (
    "SpeciesState" character varying(255),
    "CommonName" character varying(255),
    "Noxious" character varying(255),
    "Year" integer,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("SpeciesState", "CommonName", "Year"),
    FOREIGN KEY ("CommonName") REFERENCES aim_lotic."LU_SpeciesMetadata" ("CommonName")
);

--
-- EvaluationID
--
CREATE TABLE IF NOT EXISTS aim_lotic."I_Indicators" (
    "Project" character varying(255),
    "BLM_AdminState" character varying(255),
    "District" character varying(255),
    "FieldOffice" character varying(255),
    "EvaluationID" character varying(255),
    "PointID" character varying(255),
    "StreamName" character varying(255),
    "FieldEvalDate" timestamp with time zone,
    "SampledMidLatitude" double precision,
    "SampledMidLongitude" double precision,
    "BottomReachLatitude" double precision,
    "BottomReachLongitude" double precision,
    "TopReachLatitude" double precision,
    "TopReachLongitude" double precision,
    "ProtocolReachLength" double precision,
    "ProtocolType" character varying(255),
    "ProtocolVersion" character varying(255),
    "FieldStatus" character varying(255),
    "PointSelectionType" character varying(255),
    "OriginalDesign" character varying(255),
    "OriginalStratum" character varying(255),
    "StreamOrder" integer,
    "DefaultBenchmarkGroup" character varying(255),
    "PctOverheadCover" double precision,
    "PctBankOverheadCover" double precision,
    "VegComplexity" double precision,
    "VegComplexityWoody" double precision,
    "VegComplexityUnderstoryGround" double precision,
    "PctNoxiousWoodySpecies" integer,
    "PctNativeWoodySpecies" integer,
    "PctNoxiousHerbSpecies" integer,
    "PctSedgeRushSpecies" integer,
    "PctEquisetumSpecies" integer,
    "InvasiveInvertSpecies" character varying(255),
    "ObservedInvertRichness" integer,
    "ExpectedInvertRichness" double precision,
    "OE_Macroinvertebrate" double precision,
    "MMI_Macroinvertebrate" double precision,
    "OE_MMI_ModelUsed" character varying(255),
    "OE_MMI_ModelApplicability" character varying(255),
    "MacroinvertebrateCount" integer,
    "TotalNitrogen" double precision,
    "PredictedTotalNitrogen" double precision,
    "TotalPhosphorous" double precision,
    "PredictedTotalPhosphorous" double precision,
    "SpecificConductance" double precision,
    "PredictedSpecificConductance" double precision,
    "pH" double precision,
    "InstantTemp" double precision,
    "AugTempAvg" double precision,
    "TurbidityAvg" double precision,
    "PoolCount" integer,
    "PctPools" double precision,
    "PoolFreq" double precision,
    "ResPoolDepthAvg" double precision,
    "LgWoodInChanCount" integer,
    "LgWoodInChanFreq" double precision,
    "LgWoodInChanVol" double precision,
    "LgWoodAboveChanCount" integer,
    "LgWoodAboveChanFreq" double precision,
    "LgWoodAboveChanVol" double precision,
    "PctFinesLessThan2mm" double precision,
    "PctFinesLessThan6mm" double precision,
    "D16" integer,
    "D84" integer,
    "D50" integer,
    "GeometricMeanParticleDiam" integer,
    "PctPoolTailFinesLessThan2mm" integer,
    "PctPoolTailFinesLessThan6mm" integer,
    "PctBankCoveredMIM" integer,
    "PctBankStable" integer,
    "PctBankCoveredStableMIM" integer,
    "PctBankCoveredOld" integer,
    "PctBankCoveredStableOld" integer,
    "BankfullHeightAvg" double precision,
    "BenchHeightAvg" double precision,
    "ChannelIncision" double precision,
    "FloodplainConnectivity" double precision,
    "InstreamHabitatComplexity" double precision,
    "BankAngleAvg" double precision,
    "ThalwegDepthCV" double precision,
    "ThalwegDepthAvg" double precision,
    "PctDry" double precision,
    "BankfullWidthAvg" double precision,
    "WettedWidthAvg" double precision,
    "EntrenchmentRiffle1" double precision,
    "EntrenchmentRiffle2" double precision,
    "PctSlope" double precision,
    "Sinuosity" double precision,
    "HumanInfluence" character varying(255),
    "BeaverFlowMod" character varying(255),
    "BeaverSigns" character varying(255),
    "WaterWithdrawals" character varying(255),
    "SideChannels" character varying(255),
    "CoreSubset" character varying(255),
    "Supplementals" character varying(255),
    "GreenlineVegComposition" character varying(255),
    "EvaluationID_OLD" character varying(255),
    -- "RecordID" character varying(255),
    geom geometry(POINT, 4269),
    PRIMARY KEY ("EvaluationID")
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_SampledReaches_W_B" (
    "EvaluationID" character varying(255),
    "PointID" character varying(255),
    "FieldEvalDate" timestamp with time zone,
    "ProtocolType" character varying(255),
    "ProtocolVersion" character varying(255),
    "FieldStatus" character varying(255),
    "PointCoordinatesMoved" character varying(255),
    "DistanceFromDesignCoord" double precision,
    "SampledMidLatitude" double precision,
    "SampledMidLongitude" double precision,
    "AccuracyMidReachCoord" double precision,
    "ElevationMidReachCoord" double precision,
    "TopReachLatitude" double precision,
    "TopReachLongitude" double precision,
    "AccuracyTopReachCoord" double precision,
    "ElevationTopReachCoord" double precision,
    "BottomReachLatitude" double precision,
    "BottomReachLongitude" double precision,
    "AccuracyBottomReachCoord" double precision,
    "ElevationBottomReachCoord" double precision,
    "MonumentLatitude" double precision,
    "MonumentLongitude" double precision,
    "AccuracyMonumentCoord" double precision,
    "ProtocolReachLength" integer,
    "AvgTypicalBankfullWidths" double precision,
    "AvgTypicalWettedWidths" double precision,
    "ThalwegSpacing" double precision,
    "NumThalwegsPerTransect" integer,
    "BeaverFlowMod" character varying(255),
    "BeaverSigns" character varying(255),
    "WaterWithdrawals" character varying(255),
    "WeatherConditions" character varying(255),
    "Project" character varying(254),
    "DataCollectionOrganization" character varying(255),
    "QC_Visit" character varying(255),
    "OfficeEvalDate" timestamp with time zone,
    "AdminState" character varying(255),
    "SampledCOMID" integer,
    -- "RecordID" character varying(255),
    "EvaluationID_OLD" character varying(255),
    geom geometry(POINT, 4269),
    PRIMARY KEY ("EvaluationID"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_NotSampledReaches_W_B" (
    "EvaluationID" character varying(255),
    "PointID" character varying(255),
    "EvaluationLatitude" double precision,
    "EvaluationLongitude" double precision,
    "OfficeEvalDate" timestamp with time zone,
    "FieldEvalDate" timestamp with time zone,
    "EvalStatus" character varying(255),
    "FieldEvalStatus" character varying(255),
    "ReasonNotSampled" character varying(255),
    "NotSampledEvidence1" character varying(255),
    "NotSampledEvidence2" character varying(255),
    "AdminState" character varying(255),
    -- "RecordID" character varying(255),
    "EvaluationID_OLD" character varying(255),
    geom geometry(POINT, 4269),
    UNIQUE ("EvaluationID", "OfficeEvalDate", "FieldEvalDate", "EvalStatus", "FieldEvalStatus")
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_SlopePoolSummary_W" (
    "EvaluationID" character varying(255),
    "SlopeCollected" character varying(255),
    "SlopeStartTransect" character varying(255),
    "SlopeEndTransect" character varying(255),
    "SlopeReachLength" double precision,
    "TotalElevationChangePass1" double precision,
    "TotalElevationChangePass2" double precision,
    "TotalElevationChangePass3" double precision,
    "SlopeMethod" character varying(255),
    "SlopeFlag" character varying(255),
    "PoolsCollected" character varying(255),
    "PoolReachLength" double precision,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_WaterQualityMacroInvert_W_B" (
    "EvaluationID" character varying(255),
    "MacroInvertCollectionDate" timestamp with time zone,
    "MacroInvertCollectionMethod" character varying(255),
    "NetType" character varying(255),
    "NetArea" double precision,
    "NumMacroInvertLocationsSampled" integer,
    "TotalAreaSampled" double precision,
    "NumMacroInvertJars" integer,
    "NAMC_MacroInvertSampleID" integer,
    "CalibrationDate" timestamp with time zone,
    "InstrumentSerialNum" character varying(255),
    "InstrumentModel" character varying(255),
    "SondeDateTimeCollected" timestamp with time zone,
    "SpecificConductance" double precision,
    "TempCorrected" character varying(255),
    "SpecificConductanceFlag" character varying(255),
    "pH" double precision,
    "pHFlag" character varying(255),
    "InstantTemp" double precision,
    "InstantTempFlag" character varying(255),
    "TurbidityReading1" double precision,
    "TurbidityReading2" double precision,
    "TurbidityReading3" double precision,
    "TurbidityFlag" character varying(255),
    "TNTP_DateTimeCollected" timestamp with time zone,
    "TNTP_PreservationStatus" character varying(255),
    "WaterQualityLab" character varying(255),
    "Date_TNTP_Analyzed" timestamp with time zone,
    "TotalNitrogenOriginal" double precision,
    "TotalNitrogenDuplicate" double precision,
    "TotalNitrogenBlank" double precision,
    "TotalNitrogenFlag" character varying(255),
    "TotalPhosphorusOriginal" double precision,
    "TotalPhosphorusDuplicate" double precision,
    "TotalPhosphorusBlank" double precision,
    "TotalPhosphorusFlag" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Riffle
--
CREATE TABLE IF NOT EXISTS aim_lotic."F_FloodproneWidth_W_B" (
    "EvaluationID" character varying(255),
    "Riffle" integer,
    "FloodproneBankfullHeight" double precision,
    "FloodproneMaxWaterDepth" double precision,
    "FloodproneHeight" double precision,
    "FloodproneBankfullWidth" double precision,
    "FloodproneWidth" double precision,
    "FloodproneLatitude" double precision,
    "FloodproneLongitude" double precision,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Riffle"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Pass
--
CREATE TABLE IF NOT EXISTS aim_lotic."F_Slope_W" (
    "EvaluationID" character varying(255),
    "Pass" integer,
    "Shot" integer,
    "StartHeight" double precision,
    "EndHeight" double precision,
    "ElevationChange" double precision,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Pass", "Shot"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- CommonName
--
CREATE TABLE IF NOT EXISTS aim_lotic."I_VegSpeciesFreqOccurrence" (
    "EvaluationID" character varying(255),
    "CommonName" character varying(255),
    "ScientificName" character varying(255),
    "PercentPlotsPresent" integer,
    "Noxious" character varying(255),
    "WoodyHerb" character varying(255),
    -- "RecordID" character varying(255),
    geom geometry(POINT, 4269),
    PRIMARY KEY ("EvaluationID", "CommonName"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY ("CommonName") REFERENCES aim_lotic."LU_SpeciesMetadata" ("CommonName") ON UPDATE CASCADE ON DELETE RESTRICT
);

--
-- PoolNum
--
CREATE TABLE IF NOT EXISTS aim_lotic."F_Pools_W" (
    "EvaluationID" character varying(255),
    "PoolNum" integer,
    "PoolType" character varying(255),
    "PoolLength" double precision,
    "PoolTailDepth" double precision,
    "PoolMaxDepth" double precision,
    "PoolLocation" character varying(255),
    "PoolFlag" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "PoolNum"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_PoolTailFines_W" (
    "EvaluationID" character varying(255),
    "PoolNum" integer,
    "GridNum" integer,
    "PoolTailFinesLessThan2mm" integer,
    "PoolTailFinesLessThan6mm" integer,
    "PoolTailOrgMatterBoulder" integer,
    "PoolTail6To512mm" integer,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "PoolNum", "GridNum"),
    FOREIGN KEY ("EvaluationID", "PoolNum") REFERENCES aim_lotic."F_Pools_W" ("EvaluationID", "PoolNum") ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Transect
--
CREATE TABLE IF NOT EXISTS aim_lotic."F_CanopyCover_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "CanopyDown" integer,
    "CanopyUp" integer,
    "CanopyLeft" integer,
    "CanopyRight" integer,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);


CREATE TABLE IF NOT EXISTS aim_lotic."F_CanopyCover_W" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "CanopyCenterDown" integer,
    "CanopyCenterLeft" integer,
    "CanopyCenterRight" integer,
    "CanopyCenterUp" integer,
    "CanopyLeft" integer,
    "CanopyRight" integer,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_ChannelDimensions_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "WettedWidth" double precision,
    "BarWidth" double precision,
    "BankfullWidth" double precision,
    "BankfullHeight" double precision,
    "BenchHeight" double precision,
    "TransectValleyConstraint" character varying(255),
    "SeeOverBank" character varying(255),
    "DistanceToRiparianVeg" double precision,
    "WettedWidthFlag" character varying(255),
    "BarWidthFlag" character varying(255),
    "BenchHeightFlag" character varying(255),
    "BankfullWidthFlag" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_ChannelDimensions_W" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "WettedWidth" double precision,
    "BarWidth" double precision,
    "TransectStatus" character varying(255),
    "BankfullWidth" double precision,
    "BankfullHeight" double precision,
    "BenchHeight" double precision,
    "SideChannelType" character varying(255),
    "SideChannelLocation" character varying(255),
    "BankfullWidthFlag" character varying(255),
    "BankfullHeightFlag" character varying(255),
    "WettedWidthFlag" character varying(255),
    "BarWidthFlag" character varying(255),
    "BenchHeightFlag" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_FishCover_W_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "FilamentousAlgaeCover" integer,
    "BoulderCover" integer,
    "SmallWoodyDebrisCover" integer,
    "LiveTreeCover" integer,
    "MacrophyteCover" integer,
    "OverhangingVegCover" integer,
    "ArtificialStructureCover" integer,
    "UndercutBankCover" integer,
    "LargeWoodyDebrisCover" integer,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_Littoral_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "DominantLittoralSubstrate" character varying(255),
    "SecondaryLittoralSubstrate" character varying(255),
    "DominantShoreSubstrate" character varying(255),
    "SecondaryShoreSubstrate" character varying(255),
    "LittoralDepth1" double precision,
    "LittoralDepth2" double precision,
    "LittoralDepth3" double precision,
    "LittoralDepth4" double precision,
    "LittoralDepth5" double precision,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- WoodLocation
--
CREATE TABLE IF NOT EXISTS aim_lotic."F_LargeWood_W_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "WoodLocation" character varying(255),
    "LargeDiamLargeLen" integer,
    "LargeDiamMediumLen" integer,
    "LargeDiamCombinedSmallLen" integer,
    "LargeDiamSmallLen" integer,
    "LargeDiamXSmallLen" integer,
    "MediumDiamLargeLen" integer,
    "MediumDiamMediumLen" integer,
    "MediumDiamCombinedSmallLen" integer,
    "MediumDiamSmallLen" integer,
    "MediumDiamXSmallLen" integer,
    "SmallDiamLargeLen" integer,
    "SmallDiamMediumLen" integer,
    "SmallDiamCombinedSmallLen" integer,
    "SmallDiamSmallLen" integer,
    "SmallDiamXSmallLen" integer,
    "XLargeDiamLargeLen" integer,
    "XLargeDiamMediumLen" integer,
    "XLargeDiamCombinedSmallLen" integer,
    "XLargeDiamSmallLen" integer,
    "XLargeDiamXSmallLen" integer,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect", "WoodLocation"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- ParticleNum
--
CREATE TABLE IF NOT EXISTS aim_lotic."F_StreambedParticles_W" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "ParticleNum" integer,
    "StreambedLocation" character varying(255),
    "ParticleSize" double precision,
    "ParticleSizeClassNonMeas" character varying(255),
    "ParticleSizeClass" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect", "ParticleNum"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- ThalwegMeasNum
--
CREATE TABLE IF NOT EXISTS aim_lotic."F_Thalweg_StreambedParticles_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "ThalwegMeasNum" integer,
    "ThalwegDepth" double precision,
    "ThalwegDepthMethod" character varying(255),
    "OffChannelHabitatPresent" character varying(255),
    "SnagPresent" character varying(255),
    "ParticleSizeClass" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect", "ThalwegMeasNum"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_Thalweg_W" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "ThalwegMeasNum" integer,
    "ThalwegDepth" double precision,
    "FlowPresent" character varying(255),
    "SideChannelPresent" character varying(255),
    "BackwaterPresent" character varying(255),
    "BarPresent" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect", "ThalwegMeasNum"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

--
-- Bank
--

-- changed from PRIMARY KEY to UNIQUE in order to match F_Bank_W null values in bank (even though this table doesnt seem to have them)
CREATE TABLE IF NOT EXISTS aim_lotic."F_Bank_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "Bank" character varying(255),
    "BankAngleCategory" character varying(255),
    "BankType" character varying(255),
    "CoveredBankMIM" character varying(255),
    "CoveredBankOld" character varying(255),
    "ErosionalFeature" character varying(255),
    "DataCollectionBank" character varying(255),
    "BankAngleFlag" character varying(255),
    "BankStabilityCoverFlag" character varying(255),
    -- "RecordID" character varying(255),
    UNIQUE ("EvaluationID", "Transect", "Bank"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);
-- has a lot of null Bank values
CREATE TABLE IF NOT EXISTS aim_lotic."F_Bank_W" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "Bank" character varying(255),
    "BankType" character varying(255),
    "CoveredBankMIM" character varying(255),
    "CoveredBankOld" character varying(255),
    "BankBedrockCover" integer,
    "BankCobbleCover" integer,
    "BankLargeWoodCover" integer,
    "BankVegCoverFoliar" integer,
    "BankVegCoverBasal" integer,
    "ErosionalFeature" character varying(255),
    "BankAngle" double precision,
    "BankAngleType" character varying(255),
    "BankAngleFlag" character varying(255),
    "BankStabilityCoverFlag" character varying(255),
    -- "RecordID" character varying(255),
    UNIQUE ("EvaluationID", "Transect", "Bank"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_HumanInfluence_W_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "Bank" character varying(255),
    "Buildings" character varying(255),
    "LoggingOperations" character varying(255),
    "Mining" character varying(255),
    "ParksLawns" character varying(255),
    "PavementClearedLot" character varying(255),
    "Pipes" character varying(255),
    "RoadRailroadCulvert" character varying(255),
    "RowCrops" character varying(255),
    "LandfillTrash" character varying(255),
    "WallDikeRipRap" character varying(255),
    "PastureHayFence" character varying(255),
    "LivestockHorseBurro" character varying(255),
    "HydrologicAlterations" character varying(255),
    "Recreation" character varying(255),
    "InstreamRestoration" character varying(255),
    "PowerlinePipeline" character varying(255),
    "OilGas" character varying(255),
    "Fire" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect", "Bank"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_VegComplexity_W_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "Bank" character varying(255),
    "CanopyVegType" character varying(255),
    "CanopyBigTreeCover" integer,
    "CanopySmallTreeCover" integer,
    "UnderstoryVegType" character varying(255),
    "UnderstoryWoodyCover" integer,
    "UnderstoryNonWoodyCover" integer,
    "GroundNonWoodyCover" integer,
    "GroundWoodyCover" integer,
    "BareGroundCover" integer,
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect", "Bank"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS aim_lotic."F_VegSpecies_W_B" (
    "EvaluationID" character varying(255),
    "Transect" character varying(255),
    "Bank" character varying(255),
    "CommonName" character varying(255),
    -- "RecordID" character varying(255),
    PRIMARY KEY ("EvaluationID", "Transect", "Bank", "CommonName"),
    FOREIGN KEY ("EvaluationID") REFERENCES aim_lotic."I_Indicators" ("EvaluationID") ON UPDATE CASCADE ON DELETE CASCADE
);
