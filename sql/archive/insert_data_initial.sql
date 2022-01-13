-- site: based on DIMA's tblSites as LMF has no site table
/* INSERT from DIMA */
INSERT INTO site (sitekey, site_id, site_name, notes, source, montype)
SELECT SiteKey, SiteID, SiteName, NULL AS notes, 'DataSourceHere' AS source, 'aim' AS montype FROM tblSites
 WHERE SiteKey NOT IN ('999999999','888888888');

/* INSERT FROM LMF */
INSERT INTO site (sitekey, site_id, source, montype)
SELECT sitekey, site_id, source, montype
  FROM (
       SELECT (CAST(state AS NVARCHAR) + CAST(county AS NVARCHAR) + psu) as sitekey, psu AS site_id, 
              'National Landscape Monitoring Framework Database' AS source, 'lmf' AS montype 
         FROM POINT)
       AS a
 GROUP BY sitekey, site_id, source, montype
 ORDER BY sitekey;


-- disturbance: based on LMF disturbance table
ALTER TABLE disturbance DROP COLUMN objectid;
ALTER TABLE disturbance ADD other nvarchar(1) NULL;
ALTER TABLE disturbance ADD other_desc nvarchar(255) NULL;

INSERT INTO disturbance (plotkey, survey, small_rodents, non_rodent_animals, underground_utilities, overhead_transmission_lines,
                         soil_deposition_water, soil_deposition_wind, water, wind, wildfire, other, other_desc)
SELECT plotkey, YEAR(EstablishDate) AS survey,
       CASE WHEN DisturbRodents = 1 THEN 'Y' WHEN DisturbRodents = 0 THEN 'N' ELSE NULL END AS small_rodents,
       CASE WHEN DisturbMammals = 1 THEN 'Y' WHEN DisturbMammals = 0 THEN 'N' ELSE NULL END AS non_rodent_animals,
       CASE WHEN DisturbUndgroundUtils = 1 THEN 'Y' WHEN DisturbUndgroundUtils = 0 THEN 'N' ELSE NULL END AS underground_utilities,
       CASE WHEN DisturbOverhdTransLines = 1 THEN 'Y' WHEN DisturbOverhdTransLines = 0 THEN 'N' ELSE NULL END AS overhead_transmission_lines,
       CASE WHEN DisturbWaterSoilDep = 1 THEN 'Y' WHEN DisturbWaterSoilDep = 0 THEN 'N' ELSE NULL END AS soil_deposition_water,
       CASE WHEN DisturbWindSoilDep = 1 THEN 'Y' WHEN DisturbWindSoilDep = 0 THEN 'N' ELSE NULL END AS soil_deposition_wind,
       CASE WHEN DisturbWater = 1 THEN 'Y' WHEN DisturbWater = 0 THEN 'N' ELSE NULL END AS water,
       CASE WHEN DisturbWind = 1 THEN 'Y' WHEN DisturbWind = 0 THEN 'N' ELSE NULL END AS wind,
       CASE WHEN DisturbWildfire = 1 THEN 'Y' WHEN DisturbWildfire = 0 THEN 'N' ELSE NULL END AS wildfire,
       CASE WHEN DisturbOther = 1 THEN 'Y' WHEN DisturbOther = 0 THEN 'N' ELSE NULL END AS other,
       DisturbOtherDesc AS other_desc
  FROM tblPlots
 WHERE plotkey NOT IN ('999999999','888888888');
 
ALTER TABLE disturbance ADD notes NVARCHAR(MAX);

INSERT INTO disturbance (plotkey, wildfire, other, other_desc, notes)
SELECT plotkey, wildfire, other, other_desc, notes
  FROM (
        SELECT plotkey, 
               CASE WHEN burned_yn IS NOT NULL THEN UPPER(burned_yn) ELSE NULL END AS wildfire,
               CASE WHEN treated_yn IS NOT NULL THEN UPPER(treated_yn) ELSE NULL END AS other,
               CASE WHEN treated_yn = 'y' AND treatment IS NULL THEN 'treated'
                    WHEN treated_yn = 'n' AND treatment IS NULL THEN 'not treated'
                    WHEN treated_yn = 'y' AND treatment IS NOT NULL THEN treatment
                    WHEN treated_yn = 'n' AND treatment IS NOT NULL THEN treatment
                    ELSE NULL END AS other_desc,
               fire_notes AS notes
          FROM unr_plots
       ) AS a
 WHERE COALESCE(wildfire, other, other_desc, notes) IS NOT NULL;
 
ALTER TABLE disturbance ADD survey_date DATETIME2(7);

UPDATE a
   SET a.survey_date = b.establish_date
  FROM disturbance AS a
 INNER JOIN point AS b ON a.plotkey = b.plotkey;

ALTER TABLE disturbance ADD survey_year INT NULL;

UPDATE disturbance SET survey_year = YEAR(survey_date);

ALTER TABLE disturbance DROP COLUMN survey;

--esfsg
ALTER TABLE esfsg DROP COLUMN objectid;
ALTER TABLE esfsg ADD coverage_units NVARCHAR(2);
ALTER TABLE esfsg ADD esd_state NVARCHAR(100);
ALTER TABLE esfsg ADD state_community NVARCHAR(100);
ALTER TABLE esfsg ADD community_desc NVARCHAR(255);

INSERT INTO esfsg (plotkey, survey, seqnum, coverage, coverage_units, start_mark, end_mark, esfsg_state, 
                   esfsg_mlra, esfsg_site, esd_state, state_community, community_desc)
SELECT a.plotkey, YEAR(a.EstablishDate) AS survey, 1 AS seqnum, 'all' AS coverage, 'm' AS coverage_units,
       0 AS start_mark, 0 AS end_mark,
       CASE WHEN a.ecolsite IS NULL OR LOWER(a.ecolsite) LIKE ('%unknown%') THEN NULL
            ELSE SUBSTRING(REPLACE(ecolsite,' ',''),10,2) END AS esfsg_state,
       CASE WHEN a.ecolsite IS NULL OR LOWER(a.ecolsite) LIKE ('%unknown%') THEN NULL
            ELSE SUBSTRING(REPLACE(ecolsite,' ',''),2,4) END AS esfsg_mlra,
       CASE WHEN a.ecolsite IS NULL OR LOWER(a.ecolsite) LIKE ('%unknown%') THEN NULL
            ELSE SUBSTRING(REPLACE(ecolsite,' ',''),6,4) END AS esfsg_site,
       b.ESD_StateWithinEcologicalSite AS esd_state, b.ESD_CommunityWithinState AS state_community, 
       b.ESD_CommunityDescription AS community_desc
  FROM tblPlots AS a
  LEFT JOIN (SELECT * FROM tblPlotHistory WHERE RecType = 'E') AS b ON a.PlotKey = b.PlotKey
 WHERE a.plotkey NOT IN ('999999999','888888888');
 
/* Next line deals with AIM data with duplicate tblPlotHistory records in same year*/
DELETE FROM esfsg WHERE plotkey in ('1506081021305389','1505261422028621') AND esd_state IS NULL;
UPDATE esfsg SET coverage_units = 'ft' WHERE coverage_units IS NULL;

INSERT INTO esfsg (seqnum, coverage, start_mark, end_mark, esfsg_state, esfsg_mlra, esfsg_site, esd_state, state_community, community_desc, esfsg_name, plotkey)
SELECT 1 AS seqnum, 'all' AS coverage, 0 AS start_mark, 0 AS end_mark,
       CASE WHEN a.eco_id IS NULL OR LOWER(eco_id) LIKE ('%new site%') OR a.eco_id = '' THEN NULL
            ELSE SUBSTRING(REPLACE(a.eco_id,' ',''),10,2) END AS esfsg_state,
       CASE WHEN a.eco_id IS NULL OR LOWER(a.eco_id) LIKE ('%new site%') OR a.eco_id = '' THEN NULL
            ELSE SUBSTRING(REPLACE(a.eco_id,' ',''),2,4) END AS esfsg_mlra,
       CASE WHEN a.eco_id IS NULL OR LOWER(a.eco_id) LIKE ('%new site%') OR a.eco_id = '' THEN NULL
            ELSE SUBSTRING(REPLACE(a.eco_id,' ',''),6,4) END AS esfsg_site,
       CASE WHEN a.eco_state = '' THEN NULL ELSE a.eco_state END AS esd_state, 
       CASE WHEN a.eco_phase = '' THEN NULL ELSE a.eco_phase END AS state_community, 
       CASE WHEN a.eco_notes = '' THEN NULL ELSE a.eco_notes END AS community_desc,
       CASE WHEN LOWER(a.eco_id) LIKE ('%new site%') THEN 'new site' ELSE eco_name END AS esfsg_name,
       plotkey
  FROM unr_plots AS a;
  
ALTER TABLE esfsg ADD eco_group NVARCHAR(20), eco_group_type NVARCHAR(50);

UPDATE a
   SET a.eco_group = b.drg_group,
       a.eco_group_type = 'disturbance response group'
  FROM esfsg AS a
 INNER JOIN unr_plots AS b ON a.plotkey = b.plotkey;
 
ALTER TABLE esfsg ADD survey_date DATETIME2(7); 

UPDATE a
   SET a.survey_date = b.establish_date
  FROM esfsg AS a
 INNER JOIN point AS b ON a.plotkey = b.plotkey; 

ALTER TABLE esfsg ADD survey_year INT NULL; 

UPDATE esfsg SET survey_year = YEAR(survey_date);

ALTER TABLE esfsg DROP COLUMN survey;

--pointcoordinates/gps/pointweight
ALTER TABLE pointcoordinates DROP COLUMN objectid;
ALTER TABLE pointcoordinates ADD latitude numeric(38,8) NULL;
ALTER TABLE pointcoordinates ADD longitude numeric(38,8) NULL;

UPDATE pointcoordinates SET longitude = field_longitude;
UPDATE pointcoordinates SET latitude = field_latitude;

CREATE TABLE sampledesign(
    survey int NULL,
    state int NULL,
    county int NULL,
    psu nvarchar(7) NULL,
    point int NULL,
    latitude numeric(38, 8) NULL,
    longitude numeric(38, 8) NULL,
    weight numeric(38, 8) NULL,
    plotkey nvarchar(255) NULL);
    
INSERT INTO sampledesign (survey, state, county, psu, point, latitude, longitude, weight, plotkey)
SELECT a.survey, a.state, a.county, a.psu, a.point, a.target_latitude AS latitude, a.target_longitude AS longitude,
       b.weight, a.plotkey
  FROM pointcoordinates AS a
 INNER JOIN pointweight AS b ON a.plotkey = b.plotkey AND a.psu = b.psu AND a.point = b.point AND a.survey = b.survey;
 
ALTER TABLE pointcoordinates DROP COLUMN target_latitude;
ALTER TABLE pointcoordinates DROP COLUMN target_longitude;
ALTER TABLE pointcoordinates DROP COLUMN field_latitude;
ALTER TABLE pointcoordinates DROP COLUMN field_longitude;
ALTER TABLE pointcoordinates ADD nogps nvarchar(12) NULL;
ALTER TABLE pointcoordinates ADD capdate datetime2(7) NULL;
ALTER TABLE pointcoordinates ADD elevation numeric(38, 8) NULL;

UPDATE a
   SET a.nogps = b.nogps,
       a.capdate = b.capdate,
       a.elevation = b.elevation
  FROM pointcoordinates AS a
 INNER JOIN gps AS b ON a.plotkey = b.plotkey AND a.psu = b.psu AND a.point = b.point AND a.survey = b.survey;

DROP TABLE gps;
DROP TABLE pointweight;

ALTER TABLE sampledesign ADD design_year INT NULL;
UPDATE sampledesign SET design_year = survey;
ALTER TABLE sampledesign DROP COLUMN survey;


--point
ALTER TABLE point DROP COLUMN objectid;
ALTER TABLE point ADD aspect INT NULL;
ALTER TABLE point ADD montype nvarchar(3) NULL;
ALTER TABLE point ADD sitekey nvarchar(20) NULL;
ALTER TABLE point ADD plotid nvarchar(30) NULL;
ALTER TABLE point ADD landform_major nvarchar(50) NULL;
ALTER TABLE point ADD landform_minor nvarchar(50) NULL;
ALTER TABLE point ADD latitude numeric(38, 8) NULL;
ALTER TABLE point ADD longitude numeric(38, 8) NULL;
ALTER TABLE point ADD elevation numeric(38, 8) NULL;
ALTER TABLE point ADD nogps nvarchar(12) NULL;
ALTER TABLE point ADD establish_date datetime2(7) NULL;

UPDATE point 
   SET aspect = CASE WHEN slope_aspect = 'N' THEN 0
                     WHEN slope_aspect = 'NE' THEN 45
                     WHEN slope_aspect = 'E' THEN 90
                     WHEN slope_aspect = 'SE' THEN 135
                     WHEN slope_aspect = 'S' THEN 180
                     WHEN slope_aspect = 'SW' THEN 225
                     WHEN slope_aspect = 'W' THEN 270
                     WHEN slope_aspect = 'NW' THEN 315
                     ELSE NULL END;
                     
ALTER TABLE point DROP COLUMN slope_aspect;

UPDATE a
   SET a.latitude = b.latitude,
       a.longitude = b.longitude,
       a.elevation = b.elevation,
       a.nogps = b.nogps,
       a.establish_date = b.capdate
  FROM point AS a 
 INNER JOIN pointcoordinates AS b ON a.plotkey = b.plotkey AND a.psu = b.psu AND a.point = b.point AND a.survey = b.survey;
 
INSERT INTO point (survey, sitekey, plotkey, plotid, montype, state, county, vertical_slope_shape, 
                   horizontal_slope_shape, slope_percent, musym, component_name, aspect, 
                   landform_major, landform_minor, latitude, longitude, elevation, nogps, establish_date)
SELECT YEAR(a.EstablishDate) AS survey, a.SiteKey, a.PlotKey, a.plotID, 'aim' AS montype, b.state, b.county,
       CASE WHEN SUBSTRING(a.ESD_SlopeShape,1,1) = 'C' THEN 'concave'
            WHEN SUBSTRING(a.ESD_SlopeShape,1,1) = 'V' THEN 'convex'
            WHEN SUBSTRING(a.ESD_SlopeShape,1,1) = 'L' THEN 'linear'
            ELSE NULL END AS vertical_slope_shape,
       CASE WHEN SUBSTRING(a.ESD_SlopeShape,2,1) = 'C' THEN 'concave'
            WHEN SUBSTRING(a.ESD_SlopeShape,2,1) = 'V' THEN 'convex'
            WHEN SUBSTRING(a.ESD_SlopeShape,2,1) = 'L' THEN 'linear'
            ELSE NULL END AS horizontal_slope_shape,
       CAST(a.Slope AS numeric(38,8)) AS slope_percent, 
       CASE WHEN Len(a.Soil) > 6 THEN NULL ELSE a.Soil END AS musym, 
       a.MapUnitComponent AS component_name,
       CASE WHEN a.Aspect IN ('','-1','None') OR a.Aspect IS NULL THEN NULL
            ELSE CAST(CAST(Aspect AS NUMERIC) AS INT) END AS aspect,
       a.LandscapeType AS landform_major, a.LandscapeTypeSecondary AS landform_minor,
       a.Latitude, a.Longitude, a.Elevation, 'Successful' AS nogps, a.EstablishDate as monitordate
  FROM tblPlots AS a
 INNER JOIN (
       SELECT x.state, x.stabbr, y.county, y.countynm
         FROM statenm AS x
         LEFT JOIN countynm AS y ON x.state = y.state
         GROUP BY x.state, x.stabbr, y.county, y.countynm
       ) AS b ON a.State = b.stabbr AND a.County = b.countynm;
       
UPDATE point SET montype = 'lmf' WHERE montype IS NULL;
UPDATE point SET sitekey = CAST(state AS NVARCHAR) + CAST(county AS NVARCHAR) + psu WHERE montype = 'lmf'
UPDATE point SET plotid = point WHERE montype = 'lmf';

DROP TABLE pointcoordinates;

ALTER TABLE point ADD geo_datum NVARCHAR(10), elev_units NVARCHAR(2);

INSERT INTO point (sitekey, plotkey, establish_date, plotid, landform_major, aspect, slope_percent, musym, component_name, nogps, latitude, longitude, elevation,
                   montype, mlra, survey, geo_datum, elev_units)
SELECT sitekey, plotkey, survey_date AS establish_date, plot_id AS plotid, landform AS landform_major, aspect, slope_pct * 100 AS slope_percent, soil_mapunit AS musym, 
       soil_series AS component_name, 'Successful' AS nogps, latitude_dd_calc AS latitude, longitude_dd_calc AS longitude, elevation_ft AS elevation, 
       'unr' AS montype, mlra, YEAR(survey_date) AS survey, 'NAD83' AS geo_datum, 'ft' AS elev_units
  FROM unr_plots;

UPDATE point SET geo_datum = 'NAD83' WHERE montype IN ('aim','lmf');
UPDATE point SET elev_units = 'ft' WHERE montype = 'lmf';
UPDATE point SET elev_units = 'm' WHERE montype = 'aim';  

--ptnote
ALTER TABLE ptnote DROP COLUMN objectid;

INSERT INTO ptnote (plotkey, ptnote, survey)
SELECT PlotKey, Note AS ptnote, YEAR(NoteDate) AS survey
  FROM tblPlotNotes;
  
INSERT INTO ptnote (plotkey, ptnote)
SELECT plotkey, plot_notes
  FROM unr_plots
 WHERE plot_notes IS NOT NULL;
  
--plantcensus
/*
SELECT RecKey, SpeciesList
  FROM tblSpecRichDetail
--export to csv
--python script to split
import csv
with open(r'U:\AIM\EcoMonitoring\out.csv', 'w', newline='') as outfile:
    fieldnames=['reckey','species']
    writer = csv.DictWriter(outfile, delimiter=',', quotechar = '"', quoting=csv.QUOTE_MINIMAL, fieldnames=fieldnames)
    writer.writeheader()
    with open(r'U:\AIM\EcoMonitoring\SpeciesList.csv', newline='') as csvfile:
        reader = csv.reader(csvfile, delimiter=',')
        for row in reader:
            reckey = row[0]
            speclist = row[1]
            species = speclist.split(';')
            for s in species:
                if not s == '':  
                    writer.writerow({'reckey':reckey, 'species':s})
*/
--import newly created csv into new table called specrich, to be dropped after processing
ALTER TABLE plantcensus DROP COLUMN objectid;
ALTER TABLE plantcensus ALTER COLUMN cplant NVARCHAR(9) NOT NULL;
ALTER TABLE plantcensus ADD reckey NVARCHAR(20) NULL;

UPDATE plantcensus SET reckey = plotkey + 'C1';

CREATE TABLE plantcensusmeta (plotkey NVARCHAR(20) NOT NULL, reckey NVARCHAR(20) NOT NULL, survey_date DATETIME2(7), 
                               survey_size NUMERIC(6,1), size_units NVARCHAR(4), time_search_min INT, notes NVARCHAR(MAX),
                               survey INT NULL);
                               
INSERT INTO plantcensusmeta (plotkey, reckey, survey_date, survey_size, time_search_min, size_units, survey)
SELECT a.plotkey, a.reckey, b.establish_date AS survey_date, 17671.5 AS survey_size, 
       15 AS time_search_min, 'sqft' AS size_units, a.survey
  FROM (
        SELECT plotkey, reckey, survey
          FROM plantcensus
         GROUP BY plotkey, reckey, survey
       ) AS a
 INNER JOIN point AS b ON a.plotkey = b.plotkey;

INSERT INTO plantcensus (reckey, cplant, seqnum)
SELECT reckey, species AS cplant, ROW_NUMBER() OVER(PARTITION BY reckey ORDER BY species ) AS seqnum
  FROM specrich;
  
DROP TABLE specrich;

INSERT INTO plantcensusmeta (plotkey, reckey, survey_date, size_units, survey_size, notes, time_search_min, survey)
SELECT y.plotkey, y.reckey, y.survey_date, y.size_units, CAST(y.survey_size AS NUMERIC(6,1)) AS survey_size, y.notes, 
       15 AS time_search_min, YEAR(y.survey_date) AS survey
  FROM (
        SELECT x.plotkey, x.reckey, x.survey_date, x.size_units, SUM(x.size) AS survey_size, MIN(x.notes) AS notes
          FROM (
                SELECT b.PlotKey AS plotkey, a.LineKey AS linekey, a.RecKey AS reckey, a.FormDate AS survey_date,
                       a.SpecRich1Area + a.SpecRich2Area + a.SpecRich3Area + a.SpecRich4Area + a.SpecRich5Area + a.SpecRich6Area AS size,
                       CASE WHEN a.SpecRichMeasure = 1 THEN 'sqm'
                            ELSE 'sqft' END AS size_units, a.Notes AS notes
                  FROM tblSpecRichHeader AS a
                 INNER JOIN tblLines AS b ON a.LineKey = b.LineKey
                 WHERE plotkey NOT IN ('888888888','999999999') AND plotkey IS NOT NULL
               ) AS x
         GROUP BY plotkey, reckey, survey_date, size_units
       ) AS y;

  
--soilhorizon/soilpit
ALTER TABLE soilhorizon DROP COLUMN objectid;

CREATE TABLE soilpit (plotkey NVARCHAR(20) NOT NULL, pitkey NVARCHAR(20) NOT NULL, survey INT NULL, 
             depth_units NVARCHAR(2) NULL, pitdesc NVARCHAR(30) NULL, notes NVARCHAR(255) NULL);

INSERT INTO soilpit (plotkey, pitkey, pitdesc, notes, survey)
SELECT plotkey, soilkey AS pitkey, pitdesc, notes, YEAR(DateRecorded) AS survey
  FROM tblSoilPits
 WHERE plotkey NOT IN ('888888888','999999999');
 
ALTER TABLE soilhorizon ADD pitkey NVARCHAR(20) NULL;
ALTER TABLE soilhorizon ADD horizon_note NVARCHAR(255) NULL;

UPDATE soilhorizon SET pitkey = plotkey + 'S1';
UPDATE soilhorizon SET horizon_note = unusual_features WHERE unusual_features != '';

ALTER TABLE soilhorizon DROP COLUMN unusual_features; 

INSERT INTO soilpit (plotkey, pitkey, survey)
SELECT plotkey, pitkey, survey
  FROM soilhorizon;

-- At this point needed to QAQC DepthMeasure in the raw AIM data due to some inconsistancies of unit choice in the pits or NULL unit data.
-- Pits should have only one unit type when they are being measured. Any inconsistancy is almost certainly an error.
UPDATE tblSoilPitHorizons SET DepthMeasure = 'cm' WHERE SoilKey = '1403020838066463';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'in' WHERE SoilKey = '1403110951169783';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'in' WHERE SoilKey = '1407240745394366';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'in' WHERE SoilKey = '14080210135847';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'cm' WHERE SoilKey = '1408251134392001';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'in' WHERE SoilKey = '140825161051779';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'cm' WHERE SoilKey = '1409171549083946';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'cm' WHERE SoilKey = '1707270938422956';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'cm' WHERE SoilKey = '1708171643408689';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'cm' WHERE SoilKey = '1708231703343906';
UPDATE tblSoilPitHorizons SET DepthMeasure = 'cm' WHERE SoilKey = '1708241027135061';
UPDATE x
   SET x.depth_units = y.depth_units
  FROM soilpit AS x
 INNER JOIN (
       SELECT b.plotkey, a.SoilKey AS pitkey, a.DepthMeasure as depth_units
         FROM tblSoilPitHorizons AS a
        INNER JOIN tblSoilPits AS b ON a.SoilKey = b.SoilKey
        GROUP BY b.plotkey, a.SoilKey, a.DepthMeasure
       ) AS y ON x.plotkey = y.plotkey AND x.pitkey = y.pitkey;
UPDATE soilpit SET depth_units = 'in' WHERE RIGHT(pitkey, 2) = 'S1' AND depth_units IS NULL;

ALTER TABLE soilhorizon ADD tempcol NUMERIC(5,2) NULL;

UPDATE soilhorizon SET tempcol = depth;

ALTER TABLE soilhorizon DROP COLUMN depth;
ALTER TABLE soilhorizon ADD depth NUMERIC(5,2) NULL;

UPDATE soilhorizon SET depth = tempcol;

ALTER TABLE soilhorizon DROP COLUMN tempcol;

-- at this point I need to create a conversion table for AIM texture modifiers since crews did a shite job of inputing them in a standard fashion.
CREATE TABLE modifier_conv (modifier NVARCHAR(50) NULL, code_NASIS NVARCHAR(4) NULL);
-- manual population using insert:
INSERT INTO modifier_conv (modifier)
SELECT lower(ESD_HorizonModifier) AS modifier
  FROM tblSoilPitHorizons 
 WHERE ESD_HorizonModifier IS NOT NULL 
 GROUP BY lower(ESD_HorizonModifier) 
 ORDER BY lower(ESD_HorizonModifier);
--now the code_NASIS field needs to be manually edited to produce correct codes from obvious entries, NULLs for non-matching.

/*--my values
INSERT modifier_conv (modifier, code_NASIS) VALUES 
('very cobbly', 'CBV'),
('xcb', 'CBX'),
('very fine sandy', NULL),
('gr/cb', 'CB'),
('k', NULL),
('fgr/cb', 'CB'),
('cbx', 'CBX'),
('cnv', 'CBV'),
('cgr/xcb', 'CBX'),
('2', NULL),
('vcb', 'CBV'),
('pgrv', 'PGRV'),
('gr vcb', 'CBV'),
('very gravelly', 'GRV'),
('t', NULL),
('gravvelly', 'GR'),
('gr/vcb', 'CBV'),
('vs', 'STV'),
('fine gravelly', 'GRF'),
('n/a', NULL),
('w', 'W'),
('cemented', 'CEM'),
('qkm', NULL),
('stv', 'STV'),
(' very cobbly', 'CBV'),
('mgr/vcb', 'CBV'),
('boulder', 'BY'),
('extreme gravel', 'GRX'),
('egr', 'GRX'),
('dense soil', NULL),
('pgrx', 'PGRX'),
('high clay', NULL),
('gr vcb st', 'CBV'),
('none', NULL),
('gravelly, clay', 'GR'),
('high sand %', NULL),
('hard', NULL),
('gr', 'GR'),
('cobbly', 'CB'),
('gravelly', 'GR'),
('xcb/gr', 'CBX'),
('cobble', 'CB'),
('grv', 'GRV'),
('fgr', 'GRF'),
('fine', NULL),
('cgr', 'GRC'),
('st', 'ST'),
('more clay', NULL),
('extremelygravel', 'GRX'),
('cbv', 'CBV'),
('silty', NULL),
('vgr', 'GRV'),
('10', NULL),
('grc', 'GRC'),
('vst', 'STV'),
('gravely', 'GR'),
('fine sand', NULL),
('pstv', 'PSTV'),
('cnx', 'CNX'),
('cb-grv', 'GRV'),
('vgr/cb', 'GRV'),
('cobbley', 'CB'),
('cb', 'CB'),
('xgr', 'GRX'),
('cobbles', 'CB'),
('gravel', 'GR'),
('2k', NULL),
('grx', 'GRX'),
('bedrock', 'BR'),
('indurated', 'CEM'),
('stones', 'ST'),
('stony', 'ST'),
('some fine sand', NULL),
('fract. bedrock', 'BR'),
('grabelly', 'GR'),
('xst', 'STX'),
('calichi', 'CEM'),
('1', NULL),
('caliche', 'CEM'),
('b', NULL),
('tk', NULL),
('likely 25 yr', NULL),
('high sand', NULL),
('v', NULL),
('xgr/cb', 'GRX'),
('blocky/cemented', 'CEM'),
('mgr', 'GRM'),
('stx', 'STX'),
('gr/by', 'BY');
*/

--add some fields to soilhorizon to capture color info etc.
ALTER TABLE soilhorizon ADD horizon_label NVARCHAR(10) NULL, rockfrag_pct INT, color_hue NVARCHAR(6) NULL, 
            color_value NUMERIC(3,1), color_chroma NUMERIC(3,1), color_measure_type NVARCHAR(5) NULL, 
            struct_type NVARCHAR(3) NULL, struct_size NVARCHAR(2) NULL, struct_grade INT NULL;
            
INSERT INTO soilhorizon (plotkey, pitkey, depth, horizon_texture, seqnum, texture_modifier, effervescence_class,
                         survey, rockfrag_pct, color_hue, color_value, color_chroma, color_measure_type, struct_type,
                         struct_size, struct_grade, horizon_note, horizon_label)
SELECT a.plotkey, b.SoilKey AS pitkey, 
       TRY_CAST(REPLACE(REPLACE(HorizonDepthLower,'+',''),' ','') AS NUMERIC(5,2)) AS depth, 
       LOWER(b.Texture) AS horizon_texture, 
       ROW_NUMBER() OVER(PARTITION BY b.SoilKey ORDER BY b.HorizonDepthUpperNum) AS seqnum,
       c.code_NASIS AS texture_modifier, b.Effer AS effervescence_class, YEAR(a.DateRecorded) AS survey,
       b.RockFragments AS rockfrag_pct, b.ESD_Hue AS color_hue, 
       TRY_CAST(b.ESD_Value AS NUMERIC(3,1)) AS color_value, 
       TRY_CAST(b.ESD_Chroma AS NUMERIC(3,1)) AS color_chroma, 
       LOWER(b.ESD_Color) AS color_measure_type,
       LOWER(b.ESD_Structure) AS struct_type,
       LOWER(b.ESD_Size) AS struct_size,
       TRY_CAST(b.ESD_Grade AS INT) AS struct_grade,
       b.ESD_Notes AS horizon_note,
       b.ESD_Horizon AS horizon_label
  FROM tblSoilPits AS a
 INNER JOIN tblSoilPitHorizons AS b ON a.SoilKey = b.SoilKey
 LEFT JOIN modifier_conv AS c ON b.ESD_HorizonModifier = c.modifier;
 
ALTER TABLE soilpit ADD survey_date DATETIME2(7);

UPDATE x
   SET x.survey_date = y.DateRecorded
  FROM soilpit AS x
 INNER JOIN tblSoilPits AS y ON x.pitkey = y.SoilKey;
 
UPDATE x
   SET x.survey_date = y.establish_date
  FROM soilpit AS x
 INNER JOIN point AS y ON x.plotkey = y.plotkey
 WHERE y.montype = 'lmf';
 
ALTER TABLE soilpit ADD field_taxon NVARCHAR(255);

INSERT INTO soilpit (plotkey, pitkey, field_taxon, notes, survey_date, survey)
SELECT plotkey, plotkey + '3' AS pitkey, soil_taxon AS field_taxon, soil_taxon_notes AS notes, survey_date, YEAR(survey_date) AS survey
  FROM unr_plots
 WHERE soil_taxon IS NOT NULL OR soil_taxon_notes IS NOT NULL;
 
--pastureheights/pinercept
ALTER TABLE pintercept DROP COLUMN objectid;
ALTER TABLE pastureheights DROP COLUMN objectid;
ALTER TABLE pastureheights ADD htemp NUMERIC(4,1), wtemp NUMERIC(4,1), hunits NVARCHAR(2), wunits NVARCHAR(2);

UPDATE pastureheights SET hplant = NULL WHERE hplant = '';
UPDATE pastureheights SET wplant = NULL WHERE wplant = '';

UPDATE pastureheights
   SET hunits = CASE WHEN hplant = '' OR hplant IS NULL THEN NULL
                     WHEN height = '0' THEN 'in'
                     WHEN height = '' THEN NULL
                     ELSE RIGHT(height,2) END;
                     
UPDATE pastureheights
   SET htemp  = CASE WHEN hplant = '' OR hplant IS NULL THEN NULL
                     WHEN height = '0' THEN 0
                     WHEN height = '' THEN NULL
                     ELSE TRY_CAST(SUBSTRING(height,1,CHARINDEX(' ', height)) AS NUMERIC(4,1)) END;

UPDATE pastureheights
   SET wunits = CASE WHEN wplant = '' OR wplant IS NULL THEN NULL
                     WHEN wheight = '0' THEN 'in'
                     WHEN wheight = '' THEN NULL
                     ELSE RIGHT(wheight,2) END;

UPDATE pastureheights
   SET wtemp  = CASE WHEN wplant = '' OR wplant IS NULL THEN NULL
                     WHEN wheight = '0' THEN 0
                     WHEN wheight = '' THEN NULL
                     ELSE TRY_CAST(SUBSTRING(wheight,1,CHARINDEX(' ', wheight)) AS NUMERIC(4,1)) END;
                     
ALTER TABLE pastureheights DROP COLUMN height, wheight;
ALTER TABLE pastureheights ADD hheight NUMERIC(4,1), wheight NUMERIC(4,1);    
               
UPDATE pastureheights SET hheight = htemp, wheight = wtemp;

ALTER TABLE pastureheights DROP COLUMN htemp, wtemp;
ALTER TABLE pastureheights ADD linekey NVARCHAR(20);

UPDATE pastureheights 
   SET linekey = plotkey + 
                 CASE WHEN transect = 'nwse' AND distance <= 75 THEN 'NW'
                      WHEN transect = 'nwse' AND distance > 75 THEN 'SE'
                      WHEN transect = 'nesw' AND distance <= 75 THEN 'NE'
                      WHEN transect = 'nesw' AND distance > 75 THEN 'SW'
                      ELSE NULL END;
                      
ALTER TABLE pastureheights ADD mark NUMERIC(4,1);

UPDATE pastureheights
   SET mark =  
       CASE WHEN transect = 'nwse' AND distance <= 75 THEN 75-distance
            WHEN transect = 'nwse' AND distance > 75 THEN distance-75
            WHEN transect = 'nesw' AND distance <= 75 THEN 75-distance
            WHEN transect = 'nesw' AND distance > 75 THEN distance-75
            ELSE NULL END;
            
ALTER TABLE pastureheights ADD reckey NVARCHAR(20) NULL;

UPDATE pastureheights SET reckey = linekey + '1';

ALTER TABLE pintercept ADD linekey NVARCHAR(20), reckey NVARCHAR(20);

UPDATE pintercept
   SET linekey = plotkey + 
                 CASE WHEN transect = 'nwse'THEN 'NWSE'
                      WHEN transect = 'nesw' THEN 'NESW'
                      ELSE NULL END;
                      
ALTER TABLE pintercept ADD distance INT NULL;
ALTER TABLE pintercept ADD distance INT NULL;

UPDATE pintercept SET distance = mark;

ALTER TABLE pintercept DROP COLUMN mark;
ALTER TABLE pintercept ADD mark NUMERIC(4,1);

UPDATE pintercept
   SET mark =  
       CASE WHEN transect = 'nwse' AND distance <= 75 THEN 75-distance
            WHEN transect = 'nwse' AND distance > 75 THEN distance-75
            WHEN transect = 'nesw' AND distance <= 75 THEN 75-distance
            WHEN transect = 'nesw' AND distance > 75 THEN distance-75
            ELSE NULL END;

ALTER TABLE pastureheights ALTER COLUMN hplant NVARCHAR(9) NULL;
ALTER TABLE pastureheights ALTER COLUMN wplant NVARCHAR(9) NULL;

INSERT INTO pastureheights (reckey, mark, wplant, hplant, wheight, hheight, wunits, hunits)
SELECT reckey, mark, wplant, hplant, wheight, hheight, 
       CASE WHEN wheight IS NULL THEN NULL
            ELSE 'cm' END AS wunits,
       CASE WHEN hheight IS NULL THEN NULL
            ELSE 'cm' END AS hunits
  FROM (
        SELECT RecKey AS reckey, PointLoc AS mark, 
               CASE WHEN SpeciesWoody = '' THEN NULL 
                    ELSE SpeciesWoody END AS wplant, 
               CASE WHEN SpeciesHerbaceous = '' THEN NULL
                    ELSE SpeciesHerbaceous END AS hplant,
               CASE WHEN HeightWoody = '' THEN NULL 
                    ELSE TRY_CAST(HeightWoody AS NUMERIC(4,1)) END AS wheight,
               CASE WHEN HeightHerbaceous = '' THEN NULL 
                    ELSE TRY_CAST(HeightHerbaceous AS NUMERIC(4,1)) END AS hheight
          FROM tblLPIDetail) AS a
WHERE wplant IS NOT NULL OR hplant IS NOT NULL OR wheight IS NOT NULL OR hheight IS NOT NULL;

ALTER TABLE pintercept ADD hit1_top NVARCHAR(9), hit2_l NVARCHAR(9), hit3_l NVARCHAR(9), hit4_l NVARCHAR(9), 
                           hit5_l NVARCHAR(9), hit6_l NVARCHAR(9), hit7_l NVARCHAR(9), hit8_l NVARCHAR(9), hit9_surf NVARCHAR(9);
UPDATE pintercept
   SET hit1_top  = CASE WHEN hit1 = '' THEN NULL ELSE hit1 END, 
       hit2_l    = CASE WHEN hit2 = '' THEN NULL ELSE hit2 END,
       hit3_l    = CASE WHEN hit3 = '' THEN NULL ELSE hit3 END,
       hit4_l    = CASE WHEN hit4 = '' THEN NULL ELSE hit4 END,
       hit5_l    = CASE WHEN hit5 = '' THEN NULL ELSE hit5 END,
       hit6_l    = CASE WHEN hit6 = '' THEN NULL ELSE hit6 END,
       hit7_l    = NULL,
       hit8_l    = NULL, 
       hit9_surf = CASE WHEN basal = 'None' AND nonsoil = '' THEN 'S'
                        WHEN basal = 'None' AND nonsoil != '' THEN nonsoil
                        ELSE basal END;
                        
ALTER TABLE pintercept ADD temp1 NVARCHAR(1) NULL;

UPDATE pintercept 
   SET temp1 = CASE WHEN sagebrush_shape = 0 THEN NULL
                    WHEN sagebrush_shape = 1 THEN 'C' 
                    WHEN sagebrush_shape = 2 THEN 'S'
                    WHEN sagebrush_shape = 3 THEN 'M'
                    ELSE NULL END;
                    
ALTER TABLE pintercept DROP COLUMN sagebrush_shape;
ALTER TABLE pintercept ADD sagebrush_shape NVARCHAR(1) NULL;

UPDATE pintercept SET sagebrush_shape = temp1;

ALTER TABLE pintercept DROP COLUMN temp1;

UPDATE pintercept SET sagebrush_spp = NULL WHERE sagebrush_spp = '';

ALTER TABLE pintercept ADD hit1_chk BIT NULL, hit2_chk BIT NULL, hit3_chk BIT NULL, hit4_chk BIT NULL, hit5_chk BIT NULL,  
                           hit6_chk BIT NULL, hit7_chk BIT NULL, hit8_chk BIT NULL, hit9_chk BIT NULL;
                           
INSERT INTO pintercept (reckey, mark, hit1_top, hit2_l, hit3_l, hit4_l, hit5_l, hit6_l, hit7_l, hit8_l, hit9_surf,
                        hit1_chk, hit2_chk, hit3_chk, hit4_chk, hit5_chk, hit6_chk, hit7_chk, hit8_chk, hit9_chk, 
                        sagebrush_shape)
SELECT RecKey AS reckey, PointLoc AS mark, 
       TopCanopy AS hit1_top, 
       CASE WHEN Lower1 = '' THEN NULL ELSE Lower1 END AS hit2_l, 
       CASE WHEN Lower2 = '' THEN NULL ELSE Lower2 END AS hit3_l, 
       CASE WHEN Lower3 = '' THEN NULL ELSE Lower3 END AS hit4_l, 
       CASE WHEN Lower4 = '' THEN NULL ELSE Lower4 END AS hit5_l, 
       CASE WHEN Lower5 = '' THEN NULL ELSE Lower5 END AS hit6_l, 
       CASE WHEN Lower6 = '' THEN NULL ELSE Lower6 END AS hit7_l, 
       CASE WHEN Lower7 = '' THEN NULL ELSE Lower7 END AS hit8_l, 
       CASE WHEN SoilSurface = '' THEN NULL ELSE SoilSurface END AS hit9_surf,
       CASE WHEN TopCanopy = 'None' THEN NULL ELSE ChkboxTop END AS hit1_chk, 
       CASE WHEN Lower1 IN ('','BR','BY','CB','CY','D','DS','EL','GR','L','NL','R','S','ST','WA','WL') THEN NULL ELSE ChkboxLower1 END AS hit2_chk, 
       CASE WHEN Lower2 IN ('','BR','BY','CB','CY','D','DS','EL','GR','L','NL','R','S','ST','WA','WL') THEN NULL ELSE ChkboxLower2 END AS hit3_chk, 
       CASE WHEN Lower3 IN ('','BR','BY','CB','CY','D','DS','EL','GR','L','NL','R','S','ST','WA','WL') THEN NULL ELSE ChkboxLower3 END AS hit4_chk, 
       CASE WHEN Lower4 IN ('','BR','BY','CB','CY','D','DS','EL','GR','L','NL','R','S','ST','WA','WL') THEN NULL ELSE ChkboxLower4 END AS hit5_chk,  
       CASE WHEN Lower5 IN ('','BR','BY','CB','CY','D','DS','EL','GR','L','NL','R','S','ST','WA','WL') THEN NULL ELSE ChkboxLower5 END AS hit6_chk, 
       CASE WHEN Lower6 IN ('','BR','BY','CB','CY','D','DS','EL','GR','L','NL','R','S','ST','WA','WL') THEN NULL ELSE ChkboxLower6 END AS hit7_chk, 
       CASE WHEN Lower7 IN ('','BR','BY','CB','CY','D','DS','EL','GR','L','NL','R','S','ST','WA','WL') THEN NULL ELSE ChkboxLower7 END AS hit8_chk, 
       CASE WHEN SoilSurface IN ('','BR','BY','CB','CY','D','DS','EL','GR','L','NL','R','S','ST','WA','WL') THEN NULL ELSE ChkboxSoil END AS hit9_chk,
       ShrubShape AS sagebrush_shape
  FROM tblLPIDetail;
  
UPDATE pintercept SET reckey = linekey + '1' WHERE reckey IS NULL;
ALTER TABLE pintercept DROP COLUMN hit1, hit2, hit3, hit4, hit5, hit6, basal, nonsoil;
              
--for tblLines
CREATE TABLE transect (plotkey NVARCHAR(20) NOT NULL, linekey NVARCHAR(20) NOT NULL, survey INT NULL, establish_date DATETIME2(7), 
                       lineid NVARCHAR(15), azimuth INT NULL, azimuth_type NVARCHAR(10) NULL, 
                       elevation_units NVARCHAR(2), transect_length INT NULL, transect_units NVARCHAR(2), latitude_start NUMERIC(38,8), longitude_start NUMERIC(38,8), 
                       elevation_start NUMERIC(38,8), latitude_end NUMERIC(38,8), longitude_end NUMERIC(38,8), 
                       elevation_end NUMERIC(38,8));

INSERT INTO transect (plotkey, linekey, survey, lineid, azimuth, azimuth_type, elevation_units, transect_units)
SELECT b.plotkey, b.plotkey + UPPER(b.transect_center) AS linekey, b.survey,
       CASE WHEN b.transect_center = 'ne' THEN '1a'
            WHEN b.transect_center = 'sw' THEN '1b'
            WHEN b.transect_center = 'nw' THEN '2a'
            WHEN b.transect_center = 'se' THEN '2b'
            ELSE NULL END AS lineid,
       CASE WHEN b.transect_center = 'ne' THEN 45
            WHEN b.transect_center = 'sw' THEN 225
            WHEN b.transect_center = 'nw' THEN 315
            WHEN b.transect_center = 'se' THEN 135
            ELSE NULL END AS azimuth,
       'magnetic' AS azimuthtype, 'ft' AS elevation_units, 'ft' AS transect_units
  FROM (
        SELECT a.plotkey, a.survey, a.transect, a.transect_center
          FROM (
                SELECT plotkey, survey, transect,
                       CASE WHEN transect = 'nwse' AND mark <= 75 THEN 'nw'
                            WHEN transect = 'nwse' AND mark > 75 THEN 'se'
                            WHEN transect = 'nesw' AND mark <= 75 THEN 'ne'
                            WHEN transect = 'nesw' AND mark > 75 THEN 'sw'
                            ELSE NULL END AS transect_center
                  FROM pintercept
                ) AS a
         GROUP BY a.plotkey, a.survey, a.transect, a.transect_center
       ) AS b
 WHERE plotkey IS NOT NULL;

UPDATE x
   SET x.establish_date = y.establish_date
  FROM transect AS x
 INNER JOIN point AS y ON x.plotkey = y.plotkey
 WHERE x.establish_date IS NULL;
 
INSERT INTO transect (plotkey, linekey, lineid, azimuth, azimuth_type, elevation_units, latitude_start, longitude_start, elevation_start,
                      latitude_end, longitude_end, elevation_end, transect_units, establish_date, survey)
SELECT a.PlotKey AS plotkey, a.LineKey AS linekey, a.LineID AS lineid, a.Azimuth AS azimuth,
       CASE WHEN a.NorthType = 1 THEN 'magnetic'
            WHEN a.NorthType = 2 THEN 'true'
            ELSE NULL END AS azimuth_type,
       CASE WHEN a.ElevationType = 1 THEN 'm'
            WHEN a.ElevationType = 2 THEN 'ft'
            ELSE NULL END AS elevation_units,
       a.LatitudeStart AS latitude_start, a.LongitudeStart AS longitude_start, a.ElevationStart AS elevation_start,
       a.LatitudeEnd AS latitude_end, a.LongitudeEnd AS longitude_end, a.ElevationEnd AS elevation_end,
       'm' AS transect_units, b.establish_date, b.survey
  FROM tblLines AS a
  LEFT JOIN (
       SELECT x.LineKey, x.establish_date, YEAR(x.establish_date) AS survey
         FROM (
               SELECT LineKey, Min(FormDate) AS establish_date
                 FROM tblLPIHeader
                GROUP BY LineKey
              ) AS x
       ) AS b ON a.LineKey = b.LineKey
 WHERE PlotKey NOT IN ('888888888','999999999');

UPDATE x
   SET x.transect_length = y.transect_length
  FROM transect AS x
 INNER JOIN (
        SELECT LineKey AS linekey, LineLengthAmount AS transect_length
            FROM tblLPIHeader
            GROUP BY LineKey, LineLengthAmount
       ) AS y ON x.linekey = y.linekey;

UPDATE transect SET transect_length = 75 WHERE transect_units = 'ft' AND transect_length IS NULL;

--pinterceptmeta
CREATE TABLE pinterceptmeta (linekey NVARCHAR(20) NOT NULL, reckey NVARCHAR(20) NOT NULL, survey INT NULL, 
                             survey_date DATETIME2(7), chklabel NVARCHAR(20), notes NVARCHAR(MAX));
                         
INSERT INTO pinterceptmeta (linekey, reckey, survey, survey_date)
SELECT a.linekey, a.reckey, YEAR(b.establish_date) AS survey, b.establish_date AS survey_date
  FROM (
        SELECT linekey, reckey
          FROM pintercept
         GROUP BY linekey, reckey
       ) AS a
 INNER JOIN transect AS b ON a.linekey = b.linekey;

INSERT INTO pinterceptmeta (linekey, reckey, survey, survey_date, chklabel, notes)
SELECT LineKey AS linekey, RecKey AS reckey, YEAR(FormDate) AS survey, FormDate AS survey_date, 
       CheckBoxLabel AS chklabel, Notes AS notes
FROM tblLPIHeader;

--production
CREATE TABLE productionmeta (plotkey NVARCHAR(20) NOT NULL, reckey NVARCHAR(20) NOT NULL, survey INT NOT NULL, survey_date DATETIME2(7) NOT NULL, 
                             subplot_no INT NULL, subplot_size NUMERIC(6,2) NULL, subplot_units NVARCHAR(4) NULL, subplot_locs NVARCHAR(MAX) NULL, prod_method NVARCHAR(20),
                             prod_total_min NUMERIC(7,2) NULL, prod_total_max NUMERIC(7,2) NULL, prod_units NVARCHAR(6) NULL, notes NVARCHAR(MAX) NULL);

INSERT INTO productionmeta (plotkey, reckey, survey, survey_date, subplot_no, subplot_size, subplot_units, subplot_locs, prod_method, prod_total_min, prod_total_max, prod_units)
SELECT a.PlotKey AS plotkey, a.RecKey AS reckey, YEAR(a.FormDate) AS survey, a.FormDate AS survey_date, a.numSubPlots AS subplot_no,
       b.subplot_size,
       CASE WHEN b.subplot_units = 'sq. ft' THEN 'sqft'
            WHEN b.subplot_units = 'sq. m' THEN 'sqm'
            ELSE NULL END AS subplot_units, 
       a.SubPlotLocs AS subplot_locs, 'double-sampling' AS prod_method, a.TotalProdHectare AS prod_total_min, a.TotalProdHectare AS prod_total_max, 'kg/ha' AS prod_units
  FROM tblPlantProdHeader AS a
 INNER JOIN (
       SELECT RecKey, MIN(SubPlotSize) AS subplot_size, Min(SubPlotUOM) AS subplot_units
         FROM tblPlantProdDetail
        GROUP BY RecKey
       ) AS b ON a.RecKey = b.RecKey;
       
INSERT INTO productionmeta (plotkey, reckey, survey, survey_date, subplot_no, subplot_size, subplot_units, subplot_locs, prod_method, prod_total_min, prod_total_max, prod_units, notes)
SELECT plotkey, plotkey + '1' AS reckey, YEAR(survey_date) AS survey, survey_date, NULL AS subplot_no, NULL AS subplot_size, NULL AS subplot_units,
       NULL AS subplot_locs, NULL AS prod_method, prod_total_lbsac_min AS prod_total_min, prod_total_lbsac_max AS prod_total_max, 'lbs/ac' AS prod_units, prod_notes AS notes
  FROM unr_plots;
  
CREATE TABLE production (reckey NVARCHAR(20) NOT NULL, plant_id NVARCHAR(20) NOT NULL, id_type NVARCHAR(20) NOT NULL, prod_min NUMERIC(7,2) NULL, prod_max NUMERIC(7,2) NULL);

INSERT INTO production (reckey, plant_id, id_type, prod_min, prod_max)
SELECT RecKey AS reckey, SpeciesCode AS plant_id, 'species code' AS id_type, TotalWtHectare AS prod_min, TotalWtHectare AS prod_max
  FROM tblPlantProdDetail;

INSERT INTO production (reckey, plant_id, id_type, prod_min, prod_max)
SELECT plotkey + '1' AS reckey, 'forb' AS plant_id, 'growth habit' AS id_type, prod_forbs_lbsac_min AS prod_min, prod_forbs_lbsac_max AS prod_max
  FROM unr_plots
 WHERE prod_forbs_lbsac_min IS NOT NULL AND prod_forbs_lbsac_max IS NOT NULL;
 
INSERT INTO production (reckey, plant_id, id_type, prod_min, prod_max)
SELECT plotkey + '1' AS reckey, 'graminoid' AS plant_id, 'growth habit' AS id_type, prod_grass_lbsac_min AS prod_min, prod_grass_lbsac_max AS prod_max
  FROM unr_plots
 WHERE prod_grass_lbsac_min IS NOT NULL AND prod_grass_lbsac_max IS NOT NULL;
 
INSERT INTO production (reckey, plant_id, id_type, prod_min, prod_max)
SELECT plotkey + '1' AS reckey, 'perennial graminoid' AS plant_id, 'growth habit' AS id_type, prod_pgrass_lbsac_min AS prod_min, prod_pgrass_lbsac_max AS prod_max
  FROM unr_plots
 WHERE prod_pgrass_lbsac_min IS NOT NULL AND prod_pgrass_lbsac_max IS NOT NULL;
 
INSERT INTO production (reckey, plant_id, id_type, prod_min, prod_max)
SELECT plotkey + '1' AS reckey, 'shrub' AS plant_id, 'growth habit' AS id_type, prod_shrub_lbsac_min AS prod_min, prod_shrub_lbsac_max AS prod_max
  FROM unr_plots
 WHERE prod_shrub_lbsac_min IS NOT NULL AND prod_shrub_lbsac_max IS NOT NULL;
 
INSERT INTO production (reckey, plant_id, id_type, prod_min, prod_max)
SELECT plotkey + '1' AS reckey, 'BRTE' AS plant_id, 'species code' AS id_type, prod_brte_lbsac_min AS prod_min, prod_brte_lbsac_max AS prod_max
  FROM unr_plots
 WHERE prod_brte_lbsac_min IS NOT NULL AND prod_brte_lbsac_max IS NOT NULL;

--UNR Species Richness
INSERT INTO plantcensusmeta (plotkey, reckey, survey_date, survey, notes)
SELECT plotkey, plotkey + '2' AS reckey, survey_date, YEAR(survey_date) AS survey, species_list AS notes
FROM unr_plots
WHERE species_list IS NOT NULL AND species_list != '';

ALTER TABLE plantcensus ADD notes NVARCHAR(50) NULL;

UPDATE plantcensus 
   SET notes = CASE WHEN density = 1 THEN '1-10 plants in plot.' 
                    WHEN density = 2 THEN '11-100 plants in plot.' 
                    WHEN density = 3 THEN '101-500 plants in plot.' 
                    WHEN density = 4 THEN '501-1000 plants in plot.' 
                    WHEN density = 5 THEN '>1000 plants in plot.'
                    ELSE NULL END;
                    
ALTER TABLE plantcensus DROP COLUMN density;

INSERT INTO plantcensus (reckey, seqnum, cplant, notes)
SELECT plotkey + '2' AS reckey, seqnum, species AS cplant, note AS notes
  FROM unr_specrich
 WHERE species IS NOT NULL AND species != '';


