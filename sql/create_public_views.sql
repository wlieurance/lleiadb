--
-- site
--
DROP MATERIALIZED VIEW IF EXISTS public.site CASCADE; 
CREATE MATERIALIZED VIEW public.site AS 
WITH lmf_rname AS (
SELECT b.dbkey, b.description source, lpad(CAST(a."STATE" AS VARCHAR), 2, '0') || lpad(CAST(a."COUNTY" AS VARCHAR), 2, '0') || a."PSU" AS sitekey,
       CASE WHEN a."OWN" = '9' THEN 'BLM' ELSE a."OWN" END AS own,
	   a."PSU" AS siteid
  FROM lmf."POINT" AS a
 LEFT JOIN lmf.db  AS b ON a.dbkey = b.dbkey

), lmf_rgroup AS (
SELECT sitekey, siteid, own, source
  FROM lmf_rname
 GROUP BY sitekey, siteid, own, source

), lmf_rgroup2 AS (
SELECT sitekey, siteid, source, string_agg(own, ', ') AS ownership
  FROM lmf_rgroup
 GROUP BY siteid, sitekey, source

), lmf_final AS (
SELECT sitekey, siteid, NULL AS site_name, ownership, source, 'lmf' AS source_type, NULL contact_name, NULL AS notes
  FROM lmf_rgroup2

), dima_first_dbkey AS (
SELECT min(dbkey) dbkey, "SiteKey"
  FROM dima.db_site
 GROUP BY "SiteKey"

), dima_rname AS (
SELECT b.dbkey, c.description source, a."SiteKey" sitekey, a."SiteID" siteid, a."SiteName" site_name, a."Notes" notes, a."Ownership" ownership, 
       a."ContactName" contact_name, 'dima' source_type
  FROM dima."tblSites" a
  LEFT JOIN dima_first_dbkey b ON a."SiteKey" = b."SiteKey"
  LEFT JOIN dima.db c ON b.dbkey = c.dbkey 

), dima_final AS (
SELECT sitekey, siteid, site_name, ownership, source, source_type, contact_name, notes
  FROM dima_rname

), eco_final AS (
SELECT sitekey, siteid, site_name, ownership, source, source_type, contact_name, notes
  FROM eco.site
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco_final;

--
-- point
--
DROP MATERIALIZED VIEW IF EXISTS public.point CASCADE;
CREATE MATERIALIZED VIEW public.point AS
WITH lmf_process0 AS (
SELECT concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") plotkey, 
       concat(lpad(CAST(a."STATE" AS VARCHAR), 2, '0'), lpad(CAST(a."COUNTY" AS VARCHAR), 2, '0'), a."PSU") sitekey, 
	   CAST(a."POINT" AS VARCHAR) plotid,
	   a."SURVEY" survey, 
	   c."CAPDATE" AT TIME ZONE 'UTC' establish_dt, 
	   lpad(CAST(a."STATE" AS VARCHAR), 2, '0') state, 
	   lpad(CAST(a."COUNTY" AS VARCHAR), 2, '0') county, 
	   a."MLRA" mlra, 
	   CASE WHEN a."OWN" = '9' THEN 'BLM' ELSE a."OWN" END own, 
	   NULL landform_major, 
	   NULL landform_minor, 
       a."VERTICAL_SLOPE_SHAPE" vertical_slope_shape, 
	   a."HORIZONTAL_SLOPE_SHAPE" horizontal_slope_shape, 
	   a."SLOPE_PERCENT" slope_percent, 
       a."SLOPE_LENGTH" slope_length, 
	   CASE WHEN a."SLOPE_ASPECT" = 'N' THEN 0
            WHEN a."SLOPE_ASPECT" = 'NE' THEN 45
            WHEN a."SLOPE_ASPECT" = 'E' THEN 90
            WHEN a."SLOPE_ASPECT" = 'SE' THEN 135
            WHEN a."SLOPE_ASPECT" = 'S' THEN 180
            WHEN a."SLOPE_ASPECT" = 'SW' THEN 225
            WHEN a."SLOPE_ASPECT" = 'W' THEN 270
            WHEN a."SLOPE_ASPECT" = 'NW' THEN 315
            ELSE NULL END aspect, 
       a."APPARENT_TREND" apparent_trend, 
	   a."GRAZING_USE" grazing_use, a."HAYED" = 'Y' hayed, 
	   CASE WHEN b."FIELD_LATITUDE" = 0 THEN NULL ELSE b."FIELD_LATITUDE" END latitude, 
	   CASE WHEN b."FIELD_LONGITUDE" = 0 THEN NULL 
	        WHEN b."FIELD_LONGITUDE" > 0 THEN b."FIELD_LONGITUDE" * -1 
			ELSE b."FIELD_LONGITUDE" END longitude, 
	   CASE WHEN c."ELEVATION" = 0 THEN NULL ELSE round(cast(c."ELEVATION"/3.28084 as numeric),1) END elevation_m, 
	   c."NOGPS" nogps
  FROM lmf."POINT" AS a
  LEFT JOIN lmf."POINTCOORDINATES" AS b ON a."SURVEY" = b."SURVEY" AND a."STATE" = b."STATE" AND a."COUNTY" = b."COUNTY" 
                           AND a."PSU" = b."PSU" AND a."POINT" = b."POINT"
  LEFT JOIN lmf."GPS" AS c ON a."SURVEY" = c."SURVEY" AND a."STATE" = c."STATE" AND a."COUNTY" = c."COUNTY" 
                           AND a."PSU" = c."PSU" AND a."POINT" = c."POINT"

), lmf_coords0 AS (
SELECT plotkey, establish_dt,
       ST_Transform(ST_SetSRID(ST_MakePoint(longitude, latitude, elevation_m), 4269), 4326) AS geom
  FROM lmf_process0

), lmf_coords1 AS (
SELECT a.plotkey, a.establish_dt, a.geom,  b.tzid tz
  FROM lmf_coords0 a
  LEFT JOIN timezone b ON ST_Intersects(a.geom, b.geom)

), lmf_coords_tz AS (
SELECT plotkey, date(establish_dt AT TIME ZONE tz) establish_date, tz, geom
  FROM lmf_coords1

), lmf_final AS (
SELECT a.plotkey, a.plotid, a.survey, 
       coalesce(b.establish_date, date(a.establish_dt)) establish_date, 
       a.state, a.county, a.mlra, a.own, a.landform_major, 
       a.landform_minor, a.vertical_slope_shape, a.horizontal_slope_shape, 
	   a.slope_percent, a.slope_length, a.aspect, a.apparent_trend, a.grazing_use, 
	   a.hayed, a.latitude, a.longitude, a.elevation_m, a.nogps, b.tz, b.geom,
       a.sitekey
  FROM lmf_process0 a
  LEFT JOIN lmf_coords_tz b ON a.plotkey = b.plotkey

), dima_process0 AS (
SELECT a."PlotKey" plotkey, a."SiteKey" sitekey, a."PlotID" plotid, 
       a."EstablishDate" establish_date, extract(year from a."EstablishDate") survey,
	   cast(substring(a."EcolSite", '[RrFf]*(\d{3})') as integer) || substring(a."EcolSite", '[RrFf]*\d{3}([^X]{0,1})') mlra,
       b.statefp state, c.countyfp county, 
	   regexp_replace(a."LandscapeType", '\*+', '') landform_major, 
	   CASE WHEN trim(a."LandscapeTypeSecondary") = '' THEN NULL
	        ELSE a."LandscapeTypeSecondary" END landform_minor, 
       CASE WHEN substring(a."ESD_SlopeShape" from 1 for 1) = 'C' THEN 'concave' 
            WHEN substring(a."ESD_SlopeShape" from 1 for 1) = 'V' THEN 'convex' 
            WHEN substring(a."ESD_SlopeShape" from 1 for 1) = 'L' THEN 'linear' 
            ELSE NULL END vertical_slope_shape, 
	   CASE WHEN substring(a."ESD_SlopeShape" from 2 for 1) = 'C' THEN 'concave' 
		    WHEN substring(a."ESD_SlopeShape" from 2 for 1) = 'V' THEN 'convex' 
		    WHEN substring(a."ESD_SlopeShape" from 2 for 1) = 'L' THEN 'linear' 
		    ELSE NULL END horizontal_slope_shape, 
       a."Slope" slope_percent, 
	   CASE WHEN a."Aspect" = '-1' THEN NULL
	        ELSE cast(round(cast((regexp_match(a."Aspect", '\d+\.*\d*'))[1] AS double precision)) AS integer) END aspect, 
	   a."Longitude" longitude, a."Latitude" latitude,
	   CASE WHEN "ElevationType" = 1 THEN "Elevation"
	        WHEN "ElevationType" = 2 THEN "Elevation" / 3.281
	        ELSE 0 END elevation_m
  FROM dima."tblPlots" a
  LEFT JOIN public.state b ON a."State" = b.stusps
  LEFT JOIN public.county c ON b.statefp = c.statefp AND a."County" = c.name
 WHERE a."PlotKey" NOT IN ('888888888', '999999999')

), dima_coords0 AS (
SELECT "PlotKey" plotkey, 
	   CASE WHEN ("Easting" IS NULL OR "Easting" = 0) AND 
	             ("Longitude" IS NOT NULL AND "Longitude" != 0) THEN "Longitude"
	        ELSE "Easting" END easting, 
	   CASE WHEN ("Northing" IS NULL OR "Northing" = 0) AND 
	             ("Latitude" IS NOT NULL AND "Latitude" != 0) THEN "Latitude" 
	        ELSE "Northing" END northing, 
	   "GPSCoordSys" gpscoordsys, "Datum" datum, "Zone" zone_str,
       substring("Zone", '\d{2}') zone_num, 
	   CASE WHEN "ElevationType" = 1 THEN "Elevation"
	        WHEN "ElevationType" = 2 THEN "Elevation" / 3.281
	        ELSE 0 END elevation_m
  FROM dima."tblPlots"

), dima_coords1 AS (
SELECT plotkey, easting, northing, gpscoordsys, datum, zone_num, round(cast(elevation_m as numeric), 1) elevation_m,
	   CASE WHEN easting BETWEEN -180 AND 180 AND northing BETWEEN -90 AND 90 THEN True
	        ELSE False END AS valid_latlong
  FROM dima_coords0
 WHERE easting IS NOT NULL AND easting != 0 AND northing IS NOT NULL AND northing != 0

), dima_coords2 AS (
SELECT plotkey, 
	   CASE WHEN easting > 0 AND valid_latlong = True THEN easting * -1
	        ELSE easting END easting, 
	   northing, elevation_m, gpscoordsys, datum, zone_num, valid_latlong,
       CASE WHEN datum ~ 'WGS\s*84' AND valid_latlong = True THEN 4326
	        WHEN datum ~ 'NAD\s*83' AND valid_latlong = True THEN 4269
            WHEN datum ~ 'WGS\s*84' AND valid_latlong = False THEN CAST('326' || lpad(zone_num, 2, '0') AS integer)
			WHEN datum ~ 'NAD\s*83' AND valid_latlong = False THEN CAST('269' || lpad(zone_num, 2, '0') AS integer)
			ELSE NULL END srid
  FROM dima_coords1

), dima_coords3 AS (
SELECT plotkey, elevation_m,
       ST_Transform(ST_SetSRID(ST_MakePoint(easting, northing, elevation_m), srid), 4326) AS geom
  FROM dima_coords2 
 WHERE srid IS NOT NULL

), dima_coords_tz AS (
SELECT a.plotkey, b.tzid tz, st_x(a.geom) longitude, st_y(a.geom) latitude, elevation_m, 'Successful' nogps, a.geom
  FROM dima_coords3 a
  LEFT JOIN public.timezone b ON ST_Intersects(a.geom, b.geom)

), dima_process1 AS (
SELECT a.plotkey, a.sitekey, a.plotid, a.mlra,
       a.establish_date, a.survey, b.tz, 
       a.state, a.county, a.landform_major, a.landform_minor, 
       a.vertical_slope_shape, a.horizontal_slope_shape, a.slope_percent, a.aspect, 
	   coalesce(b.nogps, 'No GPS') nogps,
	   coalesce(b.longitude, a.longitude) longitude, 
	   coalesce(b.latitude, a.latitude) latitude, 
	   coalesce(b.elevation_m, a.elevation_m) elevation_m,
	   b.geom
  FROM dima_process0 a
  LEFT JOIN dima_coords_tz b ON a.plotkey = b.plotkey
	
), dima_final AS (
SELECT plotkey, plotid, survey, establish_date, state, county, mlra, NULL own, landform_major, 
       landform_minor, vertical_slope_shape, horizontal_slope_shape, slope_percent, CAST(NULL as integer) slope_length, 
	   aspect, NULL apparent_trend, CAST(NULL as integer) grazing_use, CAST(NULL as boolean) hayed, latitude, longitude, elevation_m, nogps, tz, geom,
       sitekey
  FROM dima_process1
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.point;

--
-- disturbance
--
DROP MATERIALIZED VIEW IF EXISTS public.disturbance CASCADE;
CREATE MATERIALIZED VIEW public.disturbance AS
WITH lmf_final AS (
SELECT concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") plotkey, 
       b."CAPDATE" AT TIME ZONE 'UTC' survey_date, 
	   a."PASTPRES" pastpres, a."CULTIVATION" = 'Y' cultivation, 
       a."MOWING" = 'Y' mowing, a."HAY_REMOVAL" = 'Y' hay_removal, a."HEAVY_MACHINERY" = 'Y' heavy_machinery, 
	   a."SEEDBED_PREPARATION" = 'Y' seedbed_preparation, a."LIVESTOCK_TANKS" = 'Y' livestock_tanks, 
	   a."LIVESTOCK_HEAVY_USE" = 'Y' livestock_heavy_use, a."LIVESTOCK_GRAZING" = 'Y' livestock_grazing, 
	   a."INSECTS" = 'Y' insects, a."SMALL_RODENTS" = 'Y' small_rodents, a."NON_RODENT_ANIMALS" = 'Y' non_rodent_animals, 
	   a."WILDLIFE_GRAZING" = 'Y' wildlife_grazing, a."MINING_EQUIPMENT_OPERATIONS" = 'Y' mining_equipment_operations, 
	   a."RECREATION_FOOT_TRAFFIC" = 'Y' recreation_foot_traffic, a."RECREATION_VEHICLES_BIKES" = 'Y' recreation_vehicles_bikes, 
	   a."LIVESTOCK_WALKWAYS" = 'Y' livestock_walkways, a."ROADS_DIRT" = 'Y' roads_dirt, a."ROADS_GRAVEL" = 'Y' roads_gravel, 
	   a."ROADS_PAVED" = 'Y' roads_paved, a."DRAINAGE" = 'Y' drainage, a."UNDERGROUND_UTILITIES" = 'Y' underground_utilities, 
	   a."OVERHEAD_TRANSMISSION_LINES" = 'Y' overhead_transmission_lines, a."CONSTRUCTION" = 'Y' construction, 
	   a."WATER_PONDING" = 'Y' water_ponding, a."SOIL_DEPOSITION_WATER" = 'Y' soil_deposition_water, 
	   a."SOIL_DEPOSITION_WIND" = 'Y' soil_deposition_wind, a."WATER" = 'Y' water, a."WIND" = 'Y' wind, 
	   a."TRANSPORTED_FILL" = 'Y' transported_fill, a."WILDFIRE" = 'Y' wildfire, a."PRESCRIBED_FIRE" = 'Y' prescribed_fire, 
	   a."FIRE_FIGHTING_OPERATIONS" = 'Y' fire_fighting_operations, a."BRUSH_MANAGEMENT_CHEMICAL" = 'Y' brush_management_chemical, 
	   a."BRUSH_MANAGEMENT_MECHANICAL" = 'Y' brush_management_mechanical, 
	   a."BRUSH_MANAGEMENT_BIOLOGICAL" = 'Y' brush_management_biological, NULL::boolean other, NULL other_desc, NULL notes
  FROM lmf."DISTURBANCE" a
  LEFT JOIN lmf."GPS" AS b ON a."SURVEY" = b."SURVEY" AND a."STATE" = b."STATE" AND a."COUNTY" = b."COUNTY" 
                           AND a."PSU" = b."PSU" AND a."POINT" = b."POINT"

), dima_final AS (
SELECT a."PlotKey" plotkey, cast(a."EstablishDate" as timestamp) AT TIME ZONE b.tz survey_date, 
       NULL pastpres, NULL::boolean cultivation, NULL::boolean mowing, NULL::boolean hay_removal, 
	   NULL::boolean heavy_machinery, NULL::boolean seedbed_preparation, NULL::boolean livestock_tanks, 
       NULL::boolean livestock_heavy_use, NULL::boolean livestock_grazing, NULL::boolean insects, 
	   a."DisturbRodents" small_rodents, a."DisturbMammals" non_rodent_animals, 
	   NULL::boolean wildlife_grazing, NULL::boolean mining_equipment_operations, 
	   NULL::boolean recreation_foot_traffic, NULL::boolean recreation_vehicles_bikes, 
	   NULL::boolean livestock_walkways, NULL::boolean roads_dirt, NULL::boolean roads_gravel, 
	   NULL::boolean roads_paved, NULL::boolean drainage, 
	   a."DisturbUndgroundUtils" underground_utilities, 
	   a."DisturbOverhdTransLines" overhead_transmission_lines, 
	   NULL::boolean construction, NULL::boolean water_ponding, a."DisturbWaterSoilDep" soil_deposition_water, 
	   a."DisturbWindSoilDep" soil_deposition_wind, a."DisturbWater" water, a."DisturbWind" wind, 
	   NULL::boolean transported_fill, a."DisturbWildfire" wildfire, NULL::boolean prescribed_fire, 
	   NULL::boolean fire_fighting_operations, NULL::boolean brush_management_chemical, 
	   NULL::boolean brush_management_mechanical, NULL::boolean brush_management_biological, 
	   a."DisturbOther" other, a."DisturbOtherDesc" other_desc, NULL notes
  FROM dima."tblPlots" a
  LEFT JOIN public.point b ON a."PlotKey" = b.plotkey
 WHERE a."PlotKey" NOT IN ('888888888', '999999999')
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.disturbance;


--
-- transect
--
DROP MATERIALIZED VIEW IF EXISTS public.transect CASCADE;
CREATE MATERIALIZED VIEW public.transect AS
WITH lmf_keys AS (
SELECT "SURVEY" survey, "STATE" state, "COUNTY" county, "PSU" psu, "POINT" point, "TRANSECT" transect 
  FROM lmf."PINTERCEPT"
 UNION ALL
SELECT "SURVEY" survey, "STATE" state, "COUNTY" county, "PSU" psu, "POINT" point, "TRANSECT" transect
  FROM lmf."PASTUREHEIGHTS"

), lmf_group_keys AS (
SELECT survey, state, county, psu, point, transect
  FROM lmf_keys
 GROUP BY survey, state, county, psu, point, transect

), lmf_final AS (
SELECT concat(survey, state, county, psu, point, upper(right(transect, 2))) linekey,
	   b."CAPDATE" AT TIME ZONE 'UTC' establish_date,
	   transect lineid,
	   CASE WHEN transect = 'nwse' THEN 135
	        WHEN transect = 'nesw' THEN 225
			ELSE NULL END azimuth, 
	   'magnetic' azimuth_type, 150 transect_length, 'ft' transect_units,
	   NULL::double precision latitude_start, NULL::double precision longitude_start, 
	   NULL::double precision elevation_start, NULL::double precision latitude_end, 
	   NULL::double precision longitude_end, NULL::double precision elevation_end, 
	   NULL::geometry geom, concat(survey, state, county, psu, point) plotkey
  FROM lmf_group_keys a
  LEFT JOIN lmf."GPS" AS b ON a.survey = b."SURVEY" AND a.state = b."STATE" AND a.county = b."COUNTY" 
                           AND a.psu = b."PSU" AND a.point = b."POINT"

), dima_lpi_line AS (
SELECT "LineKey" linekey, min("FormDate") formdate, min("Measure") measure, min("LineLengthAmount") linelengthamount
  FROM dima."tblLPIHeader"
  WHERE "LineKey" IS NOT NULL
 GROUP BY "LineKey"

), dima_process0 AS (
SELECT a."PlotKey" plotkey, a."LineKey" linekey, a."LineID" lineid, a."Azimuth" azimuth, 
	   CASE WHEN a."NorthType" = 1 THEN 'magnetic'
	        WHEN a."NorthType" = 2 THEN 'true'
			ELSE NULL END azimuth_type, 
	   a."NorthingStart" northing_start,
	   a."EastingStart" easting_start,  
	   CASE WHEN a."ElevationType" = 1 THEN round(a."ElevationStart"::numeric, 1)
	        WHEN a."ElevationType" = 2 THEN round(a."ElevationStart"::numeric / 3.281, 1)
			ELSE NULL END elevation_start_m, 
	   a."NorthingEnd" northing_end,
	   a."EastingEnd" easting_end,  
	   CASE WHEN a."ElevationType" = 1 THEN round(a."ElevationEnd"::numeric, 1)
	        WHEN a."ElevationType" = 2 THEN round(a."ElevationEnd"::numeric / 3.281, 1)
			ELSE NULL END elevation_end_m,
	   CASE WHEN b.formdate IS NOT NULL THEN b.formdate::timestamp AT TIME ZONE c.tz
	        ELSE c.establish_date::timestamp AT TIME ZONE c.tz END establish_date, 
	   CASE WHEN b.measure = 1 THEN 'm' 
	        WHEN b.measure = 2 THEN 'ft'
			ELSE NULL END transect_units, b.linelengthamount transect_length
  FROM dima."tblLines" a
  LEFT JOIN dima_lpi_line b ON a."LineKey" = b.linekey
  LEFT JOIN public.point c ON a."PlotKey" = c.plotkey

), dima_coords0 AS (
SELECT a.linekey, a.easting_start, a.northing_start, a.elevation_start_m,
	   a.easting_end, a.northing_end, a.elevation_end_m,
	   b."GPSCoordSys" gpscoordsys, b."Datum" datum, b."Zone" zone_str,
       (regexp_match(b."Zone", '\d{2}'))[1] zone_num 
  FROM dima_process0 a
  LEFT JOIN dima."tblPlots" b ON a.plotkey = b."PlotKey"

), dima_coords1 AS (
SELECT *,
	   CASE WHEN easting_start BETWEEN -180 AND 180 AND northing_start BETWEEN -90 AND 90 THEN True
	        ELSE False END AS valid_latlong_start,
	   CASE WHEN easting_end BETWEEN -180 AND 180 AND northing_end BETWEEN -90 AND 90 THEN True
	        ELSE False END AS valid_latlong_end
  FROM dima_coords0
 WHERE (easting_start IS NOT NULL AND easting_start != 0 AND northing_start IS NOT NULL AND northing_start != 0) OR
	   (easting_end IS NOT NULL AND easting_end != 0 AND northing_end IS NOT NULL AND northing_end != 0)

), dima_coords2 AS (
SELECT linekey, 
	   CASE WHEN easting_start > 0 AND valid_latlong_start = True THEN easting_start * -1
	        ELSE easting_start END easting_start,
	  northing_start, elevation_start_m, valid_latlong_start,
	   CASE WHEN easting_end > 0 AND valid_latlong_end = True THEN easting_end * -1
	        ELSE easting_end END easting_end, 
	   northing_end, elevation_end_m, valid_latlong_end, gpscoordsys, datum, zone_num,
       CASE WHEN datum ~ 'WGS\s*84' AND valid_latlong_start = True THEN 4326
	        WHEN datum ~ 'NAD\s*83' AND valid_latlong_start = True THEN 4269
            WHEN datum ~ 'WGS\s*84' AND valid_latlong_start = False THEN CAST('326' || lpad(zone_num, 2, '0') AS integer)
			WHEN datum ~ 'NAD\s*83' AND valid_latlong_start = False THEN CAST('269' || lpad(zone_num, 2, '0') AS integer)
			ELSE NULL END srid_start,
	   CASE WHEN datum ~ 'WGS\s*84' AND valid_latlong_end = True THEN 4326
	        WHEN datum ~ 'NAD\s*83' AND valid_latlong_end = True THEN 4269
            WHEN datum ~ 'WGS\s*84' AND valid_latlong_end = False THEN CAST('326' || lpad(zone_num, 2, '0') AS integer)
			WHEN datum ~ 'NAD\s*83' AND valid_latlong_end = False THEN CAST('269' || lpad(zone_num, 2, '0') AS integer)
			ELSE NULL END srid_end
  FROM dima_coords1

), dima_coords3 AS (
SELECT linekey, elevation_start_m, elevation_end_m,
       ST_Transform(ST_SetSRID(ST_MakePoint(easting_start, northing_start, elevation_start_m), srid_start), 4326) AS geom_start,
	   ST_Transform(ST_SetSRID(ST_MakePoint(easting_end, northing_end, elevation_end_m), srid_end), 4326) AS geom_end
  FROM dima_coords2 
 WHERE srid_start IS NOT NULL and srid_end IS NOT NULL

), dima_coords_line AS (
SELECT linekey, 
	   st_x(geom_start) longitude_start, st_y(geom_start) latitude_start, elevation_start_m,
	   st_x(geom_end) longitude_end, st_y(geom_end) latitude_end, elevation_end_m,
	   st_makeline(geom_start, geom_end) geom
  FROM dima_coords3
  
), dima_final AS (
SELECT a.linekey, a.establish_date, a.lineid, a.azimuth, a.azimuth_type, a.transect_length, a.transect_units, 
       b.latitude_start, b.longitude_start, b.elevation_start_m, b.latitude_end, b.longitude_end, b.elevation_end_m, b.geom,
       a.plotkey
  FROM dima_process0 a
  LEFT JOIN dima_coords_line b ON a.linekey = b.linekey
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.transect;

--
-- pintercept_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.pintercept_meta CASCADE;
CREATE MATERIALIZED VIEW public.pintercept_meta AS
WITH lmf_keys AS (
SELECT "SURVEY" survey, "STATE" state, "COUNTY" county, "PSU" psu, "POINT" point, "TRANSECT" transect 
  FROM lmf."PINTERCEPT"
 UNION ALL
SELECT "SURVEY" survey, "STATE" state, "COUNTY" county, "PSU" psu, "POINT" point, "TRANSECT" transect
  FROM lmf."PASTUREHEIGHTS"

), lmf_group_keys AS (
SELECT survey, state, county, psu, point, transect
  FROM lmf_keys
 GROUP BY survey, state, county, psu, point, transect

), lmf_final AS (
SELECT concat(a.survey, a.state, a.county, a.psu, a.point, upper(right(a.transect, 2)), '1') reckey,
	   b."CAPDATE" AT TIME ZONE 'UTC' survey_date, 
       NULL observer, NULL checkboxlabel, NULL notes,
       concat(a.survey, a.state, a.county, a.psu, a.point, upper(right(a.transect, 2))) linekey
  FROM lmf_group_keys a
  LEFT JOIN lmf."GPS" AS b ON a.survey = b."SURVEY" AND a.state = b."STATE" AND a.county = b."COUNTY" 
                           AND a.psu = b."PSU" AND a.point = b."POINT"

), dima_final AS (
SELECT a."RecKey" reckey, a."FormDate"::timestamp AT TIME ZONE c.tz survey_date, 
       a."Observer" observer, a."CheckboxLabel" checkboxlabel, a."Notes" notes, a."LineKey" linekey
  FROM dima."tblLPIHeader" a
  LEFT JOIN dima."tblLines" b ON a."LineKey" = b."LineKey"
  LEFT JOIN public.point c ON b."PlotKey" = c.plotkey
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.pintercept_meta;

--
-- pintercept
--
DROP MATERIALIZED VIEW IF EXISTS public.pintercept CASCADE;
CREATE MATERIALIZED VIEW public.pintercept AS
WITH lmf_pi_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2)), '1') reckey,
	   "MARK" mark, "HIT1" hit1, "HIT2" hit2, "HIT3" hit3, "HIT4" hit4, "HIT5" hit5, "HIT6" hit6,
	   CASE WHEN "BASAL" = 'None' AND "NONSOIL" = '' THEN 'S' 
	        WHEN "BASAL" = 'None' AND "NONSOIL" != '' THEN "NONSOIL"
			WHEN "NONSOIL" = 'W' THEN "NONSOIL" 
			WHEN trim("BASAL") = '' THEN NULL 
			ELSE "BASAL" END hit7
  FROM lmf."PINTERCEPT"

), lmf_pi_process1 AS (
SELECT reckey, mark,
       unnest(array[1, 2, 3, 4, 5, 6, 9]) AS hit_order,
	   -- l = layer
	   unnest(array['l', 'l', 'l', 'l', 'l', 'l', 'l']) hit_type,
	   -- t = top, l = lower, s = surface
       unnest(array['t', 'l', 'l', 'l', 'l', 'l', 's']) hit_sub,
       unnest(array[hit1, hit2, hit3, hit4, hit5, hit6, hit7]) AS hit
  FROM lmf_pi_process0
	
), lmf_pi_final AS (
SELECT reckey, mark, hit_order, hit_type, hit_sub, trim(hit) hit, NULL::numeric height_cm, NULL::boolean dead
  FROM lmf_pi_process1
 WHERE hit IS NOT NULL AND trim(hit) != ''

), lmf_ph_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2)), '1') reckey,
       "DISTANCE" mark, 
	   nullif(trim("HPLANT"), '') hplant, 
	   cast(substring("HEIGHT", '\d+\.{0,1}\d*') AS numeric(6, 1)) hheight,
	   substring("HEIGHT", '[^\d\s\.]+') hunits,
	   nullif(trim("WPLANT"), '') wplant, 
	   cast(substring("WHEIGHT", '\d+\.{0,1}\d*') AS numeric(6, 1)) wheight,
	   substring("WHEIGHT", '[^\d\s\.]+') wunits
  FROM lmf."PASTUREHEIGHTS"

), lmf_ph_process1 AS (
SELECT reckey, mark,
	   -- g = growth habit
       unnest(ARRAY['g', 'g']) AS hit_type,
	   -- w = woody, h = 'herbaceous'
       unnest(ARRAY['w', 'h']) AS hit_sub,
	   unnest(ARRAY[1, 2]) AS hit_order,
       unnest(ARRAY[wplant, hplant]) AS hit,
       unnest(ARRAY[wheight, hheight]) AS height,
	   unnest(ARRAY[wunits, hunits]) AS units
  FROM lmf_ph_process0

), lmf_ph_final AS (
SELECT reckey, mark, hit_order, hit_type, hit_sub, hit, 
       CASE WHEN units = 'in' THEN round(height * 2.54, 1)
	        WHEN units = 'ft' THEN round(height * 30.48, 1)
			ELSE height END height_cm,
	   NULL::boolean chk
  FROM lmf_ph_process1

), dima_nonchk (code) AS (
VALUES 
(''), ('BR'), ('BY'), ('CB'), ('CY'), ('D'), ('DS'), ('EL'), ('GR'), ('L'), 
('NL'), ('R'), ('S'), ('ST'), ('WA'), ('WL'), ('None')
	
), dima_process0 AS (
SELECT "RecKey" reckey, "PointLoc" mark,
        unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3]) hit_order,
		-- l = canopy layer, g = growth habit
		unnest(array['l', 'l', 'l', 'l', 'l', 'l', 'l', 'l', 
					 'l', 'g', 'g', 'g']) hit_type,
		-- t = top, l = lower, s = surface, w = woody, h = herbaceous
		unnest(array['t', 'l', 'l', 'l', 'l', 'l', 'l', 'l', 
					 's', 'w', 'h', 'h']) hit_sub,
        unnest(array["TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", 
					 "Lower5", "Lower6", "Lower7", "SoilSurface", "SpeciesWoody", 
					 "SpeciesHerbaceous", "SpeciesLowerHerb"]) hit,
		unnest(array["ChkboxTop", "ChkboxLower1", "ChkboxLower2", "ChkboxLower3", "ChkboxLower4",
					 "ChkboxLower5", "ChkboxLower6", "ChkboxLower7", "ChkboxSoil", "ChkboxWoody", 
					 "ChkboxHerbaceous", "ChkboxLowerHerb"]) chk,
		unnest(array["HeightTop", "HeightLower1", "HeightLower2", "HeightLower3", "HeightLower4", 
					 "HeightLower5", "HeightLower6", "HeightLower7", "HeightSurface", "HeightWoody", 
					 "HeightHerbaceous", "HeightLowerHerb"]) height
	FROM dima."tblLPIDetail"

), dima_process1 AS (
SELECT reckey, mark, hit_order, hit_type, hit_sub, 
       nullif(trim(hit), '') hit, 
	   CASE WHEN hit IN (SELECT code FROM dima_nonchk) THEN NULL ELSE chk END chk, 
	   cast(substring(height, '\d+\.{0,1}\d*') as numeric(6, 1)) height_num
  FROM dima_process0
	
), dima_final AS (
SELECT a.reckey, a.mark, a.hit_order, a.hit_type, a.hit_sub, a.hit,
	   CASE WHEN b."HeightUOM" = 'in' THEN round(height_num * 2.54, 1)
	        ELSE round(height_num, 1) END height_cm,
	   -- takes headers with checkbox labels of dead or residual and converts the remaining to NULL
	   -- also assumes that NULL or blank values mean 'dead' as is the most common usage. 
	   CASE WHEN trim(b."CheckboxLabel") = '' OR 
	             b."CheckboxLabel" IS NULL OR 
	             substring(lower(b."CheckboxLabel"), 'dead|residual') IS NOT NULL THEN a.chk
	        ELSE NULL END dead
  FROM dima_process1 a
  LEFT JOIN dima."tblLPIHeader" b ON a.reckey = b."RecKey"
 WHERE a.hit IS NOT NULL OR a.height_num IS NOT NULL
)
SELECT * FROM lmf_pi_final
 UNION ALL
SELECT * FROM lmf_ph_final
 UNION ALL 
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.pintercept;

--
-- shrubshape
--
DROP MATERIALIZED VIEW IF EXISTS public.shrubshape CASCADE;
CREATE MATERIALIZED VIEW public.shrubshape AS
WITH lmf_final AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2)), '1') reckey,
       "MARK" mark,
	   nullif(trim("SAGEBRUSH_SPP"), '') species_code,
	   CASE WHEN "SAGEBRUSH_SHAPE" = 0 THEN NULL
	        WHEN "SAGEBRUSH_SHAPE" = 1 THEN 'C' 
			WHEN "SAGEBRUSH_SHAPE" = 2 THEN 'S' 
			WHEN "SAGEBRUSH_SHAPE" = 3 THEN 'M' 
			ELSE NULL END shape
  FROM lmf."PINTERCEPT"
  WHERE "SAGEBRUSH_SHAPE" != 0 AND "SAGEBRUSH_SHAPE" IS NOT NULL

), dima_final AS (
SELECT "RecKey" reckey, "PointLoc" mark,
       NULL species_code, 
	   nullif(trim("ShrubShape"), '') shape
  FROM dima."tblLPIDetail" 
 WHERE nullif(trim("ShrubShape"), '') IS NOT NULL
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.shrubshape;

--
-- esfsg_line
--
DROP MATERIALIZED VIEW IF EXISTS public.esfsg_line CASCADE;
CREATE MATERIALIZED VIEW public.esfsg_line AS
WITH lmf_process0 AS (
SELECT "SURVEY" survey, "STATE" state, "COUNTY" county, "PSU" psu, "POINT" point, "SEQNUM" seq_no, 'nesw' coverage, 
       CASE WHEN "START_MARK" IS NULL THEN 0 ELSE "START_MARK" END AS start_mark, 
	   CASE WHEN "END_MARK" IS NULL OR "END_MARK" = 0 THEN 150 ELSE "END_MARK" END AS end_mark, 
	   "ESFSG_STATE" esfsg_state, "ESFSG_MLRA" esfsg_mlra, "ESFSG_SITE" esfsg_site, 
	   "ESFSG_NAME" esfsg_name
	FROM lmf."ESFSG" WHERE "COVERAGE" = 'all'
 UNION ALL
SELECT "SURVEY" survey, "STATE" state, "COUNTY" county, "PSU" psu, "POINT" point, "SEQNUM" seq_no, 'nwse' coverage, 
       CASE WHEN "START_MARK" IS NULL THEN 0 ELSE "START_MARK" END AS start_mark, 
	   CASE WHEN "END_MARK" IS NULL OR "END_MARK" = 0 THEN 150 ELSE "END_MARK" END AS end_mark, 
	   "ESFSG_STATE" esfsg_state, "ESFSG_MLRA" esfsg_mlra, "ESFSG_SITE" esfsg_site, 
	   "ESFSG_NAME" esfsg_name
	FROM lmf."ESFSG" WHERE "COVERAGE" = 'all'
 UNION ALL
SELECT "SURVEY" survey, "STATE" state, "COUNTY" county, "PSU" psu, "POINT" point, "SEQNUM" seq_no, "COVERAGE" coverage, 
       "START_MARK" start_mark, "END_MARK" end_mark, 
	   "ESFSG_STATE" esfsg_state, "ESFSG_MLRA" esfsg_mlra, "ESFSG_SITE" esfsg_site, 
	   "ESFSG_NAME" esfsg_name
	FROM lmf."ESFSG" WHERE "COVERAGE" != 'all'

), lmf_final AS (
SELECT concat(a.survey, a.state, a.county, a.psu, a.point, upper(right(a.coverage, 2)), '9') reckey,
	   b."CAPDATE" AT TIME ZONE 'UTC' survey_date,
	   a.seq_no,
	   concat(a.esfsg_mlra, a.esfsg_site, a.esfsg_state) ecoid_std,
	   a.start_mark, a.end_mark,
       concat(a.survey, a.state, a.county, a.psu, a.point, upper(right(a.coverage, 2))) linekey
  FROM lmf_process0 a
  LEFT JOIN lmf."GPS" AS b ON a.survey = b."SURVEY" AND a.state = b."STATE" AND a.county = b."COUNTY" 
                           AND a.psu = b."PSU" AND a.point = b."POINT"
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM eco.esfsg_line;


--
-- esfsg_plot
--
DROP MATERIALIZED VIEW IF EXISTS public.esfsg_plot CASCADE;
CREATE MATERIALIZED VIEW public.esfsg_plot AS
WITH lmf_plot_eco AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey,
	   concat("ESFSG_MLRA", "ESFSG_SITE", "ESFSG_STATE") ecoid_std, 
	   CASE WHEN trim("ESFSG_NAME") = '' THEN NULL ELSE trim("ESFSG_NAME") END econame,
	   1 area_pct, 1 ecorank
  FROM lmf."ESFSG"
  WHERE "COVERAGE" = 'all'

), lmf_line_eco AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey, "COVERAGE" transect,
	   concat("ESFSG_MLRA", "ESFSG_SITE", "ESFSG_STATE") ecoid_std,
	   "END_MARK" - "START_MARK" eco_length,
	   CASE WHEN trim("ESFSG_NAME") = '' THEN NULL ELSE trim("ESFSG_NAME") END econame
  FROM lmf."ESFSG"
  WHERE "COVERAGE" != 'all'

), lmf_line_eco_sum AS (
SELECT plotkey, ecoid_std, sum(eco_length) eco_length, min(econame) econame
  FROM lmf_line_eco
 GROUP BY plotkey, ecoid_std

), lmf_plot_eco_sum AS (
SELECT plotkey, sum(eco_length) transect_sum, min(econame) econame
  FROM lmf_line_eco_sum
 GROUP BY plotkey

), lmf_plot_eco_ranked AS (
SELECT a.plotkey, a.ecoid_std, a.econame, 
	   round(a.eco_length::numeric/b.transect_sum, 3) area_pct,
       row_number() over(partition by a.plotkey order by a.eco_length desc) AS ecorank
  FROM lmf_line_eco_sum a
  INNER JOIN lmf_plot_eco_sum b ON a.plotkey = b.plotkey

), lmf_all_eco AS (
SELECT * FROM lmf_plot_eco
 UNION ALL
SELECT * FROM lmf_plot_eco_ranked

), lmf_final AS (
SELECT concat(a.plotkey, '9') reckey,
	   c."CAPDATE" AT TIME ZONE 'UTC' survey_date,
	   a.ecoid_std,
	   substring(b.ecoid, '^(?:F|R|G?)') ecotype,
	   a.econame,
	   a.area_pct, a.ecorank,
	   NULL esd_state, NULL state_community, NULL communitydesc,
       a.plotkey
  FROM lmf_all_eco a
  LEFT JOIN public.ecosite b ON a.ecoid_std = b.ecoid_std
  LEFT JOIN lmf."GPS" c ON a.plotkey = concat(c."SURVEY", c."STATE", c."COUNTY", c."PSU", c."POINT")

), dima_state AS (
SELECT a."PlotKey" plotkey, a."RecKey" reckey,
	   a."DateRecorded" date_recorded,
       a."ESD_StateWithinEcologicalSite" esd_state, 
       a."ESD_CommunityWithinState" state_community, 
	   regexp_replace(a."ESD_CommunityDescription", '\s+', ' ') communitydesc
  FROM dima."tblPlotHistory" a
 WHERE "RecType" = 'E' 
   AND coalesce("ESD_StateWithinEcologicalSite", "ESD_CommunityWithinState", 
				"ESD_CommunityDescription") IS NOT NULL 

), dima_final AS (
SELECT coalesce(b.reckey, substring(a."PlotKey" from 1 for 12) || '9999') reckey, 
       coalesce(b.date_recorded, a."EstablishDate")::timestamp AT TIME ZONE c.tz survey_date, 
	   coalesce(substring(trim(a."EcolSite"), '^(?:F|R|G?)(\d{3}[A-z]?[A-z]?\d{3}[A-z]{2})(_*\d*)$'),
			   trim(a."EcolSite")) ecoid_std,
	   substring(trim(a."EcolSite"), '^(?:F|R|G?)') ecotype,
	   NULL econame, 
	   1 area_pct, 1 ecorank,
	   b.esd_state, b.state_community, b.communitydesc,
       a."PlotKey" plotkey
  FROM dima."tblPlots" a
  LEFT JOIN dima_state b ON a."PlotKey" = b.plotkey
  LEFT JOIN public.point c ON a."PlotKey" = c.plotkey
  WHERE a."PlotKey" NOT IN ('888888888', '999999999')
)

SELECT * FROM lmf_final
  UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.esfsg_plot;

--
-- plantcensus_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.plantcensus_meta CASCADE;
CREATE MATERIALIZED VIEW public.plantcensus_meta AS
WITH lmf_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", '2') reckey,
	   concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey
  FROM lmf."PLANTCENSUS"
 GROUP BY "SURVEY", "STATE", "COUNTY", "PSU", "POINT"

), lmf_final AS  (
SELECT reckey, b."CAPDATE" AT TIME ZONE 'UTC' survey_date, NULL observer,
	   1641.6 survey_size_m2, NULL notes, plotkey       
  FROM lmf_process0 a
  LEFT JOIN lmf."GPS" b ON a.plotkey = concat(b."SURVEY", b."STATE", b."COUNTY", b."PSU", b."POINT")  

), dima_final AS (
SELECT a."RecKey" reckey,  
       "FormDate"::timestamp AT TIME ZONE d.tz survey_date, a."Observer" observer,
       CAST(a."SpecRich1Container"::integer * a."SpecRich1Area" +  
	   a."SpecRich2Container"::integer * a."SpecRich2Area" + 
	   a."SpecRich3Container"::integer * a."SpecRich3Area" + 
	   a."SpecRich4Container"::integer * a."SpecRich4Area" + 
	   a."SpecRich5Container"::integer * a."SpecRich5Area" + 
	   a."SpecRich6Container"::integer * a."SpecRich6Area" AS numeric(6,1)) survey_size_m2, 
	   a."Notes" notes, c."PlotKey" plotkey
  FROM dima."tblSpecRichHeader" a
  LEFT JOIN dima."tblLines" b ON a."LineKey" = b."LineKey"
  LEFT JOIN dima."tblPlots" c ON b."PlotKey" = c."PlotKey"
  LEFT JOIN public.point d ON c."PlotKey" = d.plotkey
 WHERE c."PlotKey" IS NOT NULL
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.plantcensus_meta;

--
-- plantcensus
--
DROP MATERIALIZED VIEW IF EXISTS public.plantcensus CASCADE;
CREATE MATERIALIZED VIEW public.plantcensus AS
WITH lmf_final AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", '2') reckey, 
       "SEQNUM" seq_no, "CPLANT" species_code, NULL notes
	FROM lmf."PLANTCENSUS"

), dima_process0 AS (
SELECT "RecKey" reckey, "subPlotID" subplotid,  
          unnest(regexp_split_to_array(
	      CASE WHEN trim("SpeciesList") = '' THEN NULL ELSE trim("SpeciesList") END,
	      ';')) species_code
  FROM dima."tblSpecRichDetail" 

), dima_process1 AS (
SELECT reckey, species_code
  FROM dima_process0
 WHERE species_code IS NOT NULL and trim(species_code) != ''
 GROUP BY reckey, species_code

), dima_final AS (
SELECT reckey, row_number() over(partition by reckey order by species_code) seq_no,
       species_code, NULL notes
  FROM dima_process1
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.plantcensus;


--
-- production_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.production_meta CASCADE;
CREATE MATERIALIZED VIEW public.production_meta AS
WITH dima_final AS (
SELECT a."RecKey" AS reckey, a."FormDate"::timestamp AT TIME ZONE b.tz AS survey_date,
       a."Observer" observer, a."numSubPlots" AS subplot_no, a."Notes" AS notes,
       a."PlotKey" AS plotkey
  FROM dima."tblPlantProdHeader" a
  LEFT JOIN point b ON a."PlotKey" = b.plotkey
)

SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.production_meta;


--
-- production_subplot
--
DROP MATERIALIZED VIEW IF EXISTS public.production_subplot CASCADE;
CREATE MATERIALIZED VIEW public.production_subplot AS 
WITH dima_subplot0 AS (
SELECT "RecKey" reckey, "numSubPlots" subplot_no, 
       unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 , 16, 17, 18, 19, 20]) subplot,
       unnest(regexp_split_to_array(rtrim("SubPlotLocs", ';'), ';')) subloc, 
	   unnest(array["SubPlot1Exp", "SubPlot2Exp", "SubPlot3Exp", "SubPlot4Exp", "SubPlot5Exp", 
					"SubPlot6Exp", "SubPlot7Exp", "SubPlot8Exp", "SubPlot9Exp", "SubPlot10Exp", 
					"SubPlot11Exp", "SubPlot12Exp", "SubPlot13Exp", "SubPlot14Exp", "SubPlot15Exp", 
					"SubPlot16Exp", "SubPlot17Exp", "SubPlot18Exp", "SubPlot19Exp", "SubPlot20Exp"]) expanded, 
	   unnest(array["SubPlot1NotSamp", "SubPlot2NotSamp", "SubPlot3NotSamp", "SubPlot4NotSamp", "SubPlot5NotSamp", 
					"SubPlot6NotSamp", "SubPlot7NotSamp", "SubPlot8NotSamp", "SubPlot9NotSamp", "SubPlot10NotSamp", 
					"SubPlot11NotSamp", "SubPlot12NotSamp", "SubPlot13NotSamp", "SubPlot14NotSamp", "SubPlot15NotSamp", 
					"SubPlot16NotSamp", "SubPlot17NotSamp", "SubPlot18NotSamp", "SubPlot19NotSamp", "SubPlot20NotSamp"]) not_sampled
  FROM dima."tblPlantProdHeader"

-- gets rid of subplots that don't actually exists 
), dima_final AS (
SELECT reckey, subplot, subloc, expanded, not_sampled
  FROM dima_subplot0
 WHERE subplot <= subplot_no
)

SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.production_subplot;


--
-- production_species
--
DROP MATERIALIZED VIEW IF EXISTS public.production_species CASCADE;
CREATE MATERIALIZED VIEW public.production_species AS 
WITH dima_species0 AS (
SELECT "RecKey" reckey, "SpeciesCode" species_code,
        CASE WHEN "SubPlotUOM" = 'sq. ft' THEN round("SubPlotSize"::numeric /10.764, 4) 
		     WHEN "SubPlotUOM" = 'ac' THEN round("SubPlotSize"::numeric * 4046.86, 4)
			 ELSE "SubPlotSize" END subsize_m2, 
	    CASE WHEN "WtUnitWt" = 0 THEN 1 ELSE "WtUnitWt" END unit_wgt,
	    "WtMeas" wgt_measure,
	    "ADWAdj"::double precision adjust_airdrywgt, "UtilAdj"::double precision adjust_utilization, 
	    "GwthAdj"::double precision adjust_growth, "WthrAdj"::double precision adjust_climate
  FROM dima."tblPlantProdDetail"

), dima_species1 AS (
SELECT reckey, species_code, subsize_m2, 
	   CASE WHEN wgt_measure = 'lb' THEN round(unit_wgt::numeric * 453.592, 1)
		    ELSE unit_wgt END unit_wgt_g,   
       adjust_airdrywgt, adjust_utilization, adjust_growth, adjust_climate
  FROM dima_species0

), dima_final AS (
-- this final query needed for dima inconsistancies where the same species code occupies
-- the same record (reckey), which should only need to happen if the same species
-- was measured at two different subplot sizes (which isn't supported by the method docs).
-- the detailkey exists seemingly for the purpose of differentiating two identitical species
-- codes with perhaps different subplot sizes or uit weights, but this needlessly overcomplicates
-- the data management issue.  In these rare instances, mean values are used.
SELECT reckey, species_code, avg(subsize_m2) subsize_m2, avg(unit_wgt_g) unit_wgt_g, 
       avg(adjust_airdrywgt) adjust_airdrywgt, avg(adjust_utilization) adjust_utilization, 
	   avg(adjust_growth) adjust_growth, avg(adjust_climate) adjust_climate
  FROM dima_species1
 GROUP BY reckey, species_code
) 

SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.production_species;

---
--- production
---
DROP MATERIALIZED VIEW IF EXISTS public.production CASCADE;
CREATE MATERIALIZED VIEW public.production AS
WITH dima_detail0 AS (
SELECT "RecKey" reckey, "SpeciesCode" species_code, "WtMeas" measure_units,
       unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 , 16, 17, 18, 19, 20]) subplot,
       unnest(array["Sub1Wt", "Sub2Wt", "Sub3Wt", "Sub4Wt", "Sub5Wt", 
					"Sub6Wt", "Sub7Wt", "Sub8Wt", "Sub9Wt", "Sub10Wt", 
					"Sub11Wt", "Sub12Wt", "Sub13Wt", "Sub14Wt", "Sub15Wt", 
					"Sub16Wt", "Sub17Wt", "Sub18Wt", "Sub19Wt", "Sub20Wt"]) units, 
	   unnest(array["Sub1Clip", "Sub2Clip", "Sub3Clip", "Sub4Clip", "Sub5Clip", 
					"Sub6Clip", "Sub7Clip", "Sub8Clip", "Sub9Clip", "Sub10Clip", 
					"Sub11Clip", "Sub12Clip", "Sub13Clip", "Sub14Clip", "Sub15Clip", 
					"Sub16Clip", "Sub17Clip", "Sub18Clip", "Sub19Clip", "Sub20Clip"]) clipped, 
	   unnest(array["ClipWt1", "ClipWt2", "ClipWt3", "ClipWt4", "ClipWt5", 
					"ClipWt6", "ClipWt7", "ClipWt8", "ClipWt9", "ClipWt10", 
					"ClipWt11", "ClipWt12", "ClipWt13", "ClipWt14", "ClipWt15", 
					"ClipWt16", "ClipWt17", "ClipWt18", "ClipWt19", "ClipWt20"]) clipped_wgt
	FROM dima."tblPlantProdDetail"

), dima_detail1 AS (
SELECT a.reckey, a.species_code, a.subplot, a.units,
	   cast((regexp_match(a.units, '[\d\.]+'))[1] AS double precision) units_dbl,
	   (regexp_match(a.units, 'T'))[1] trace,
	   CASE WHEN a.clipped_wgt = 0 THEN NULL
            WHEN measure_units = 'lb' THEN round(a.clipped_wgt::numeric * 453.592, 2) 
			ELSE a.clipped_wgt END clipped_wgt_g
  FROM dima_detail0 a
  LEFT JOIN dima."tblPlantProdHeader" b ON a.reckey = b."RecKey"
 WHERE a.subplot <= b."numSubPlots" AND a.units IS NOT NULL

), dima_final AS (
SELECT reckey, species_code, subplot, coalesce(units_dbl, 0) units, 
	   coalesce(trace = 'T', False) trace, clipped_wgt_g
  FROM dima_detail1
)

SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.production;

--
-- plantdensity_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.plantdensity_meta CASCADE;
CREATE MATERIALIZED VIEW public.plantdensity_meta AS
WITH lmf_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey,
	   concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", 'SE7') reckey,
	   concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", 'SE') linekey
  FROM lmf."PLANTCENSUS"
 GROUP BY "SURVEY", "STATE", "COUNTY", "PSU", "POINT"

), lmf_final AS (
SELECT a.reckey, b."CAPDATE" AT TIME ZONE 'UTC' survey_date, NULL observer,
       2 subplot_no, NULL notes, NULL species_searched, a.linekey
  FROM lmf_process0 a
  LEFT JOIN lmf."GPS" b ON a.plotkey = concat(b."SURVEY", b."STATE", b."COUNTY", b."PSU", b."POINT")

), dima_final AS (
SELECT a."RecKey" reckey, a."FormDate"::timestamp AT TIME ZONE c.tz survey_date, 
       a."Observer" obsrever, a."numQuadrats" subplot_no, 
       a."Notes" notes, a."SpeciesSearchedFor" species_searched, a."LineKey" linekey
  FROM dima."tblPlantDenHeader" a
  LEFT JOIN dima."tblLines" b ON a."LineKey" = b."LineKey"
  LEFT JOIN public.point c ON b."PlotKey" = c.plotkey
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.plantdensity_meta;

--
-- plantdensity_class
--
DROP MATERIALIZED VIEW IF EXISTS public.plantdensity_class CASCADE;
CREATE MATERIALIZED VIEW public.plantdensity_class AS
WITH lmf_final AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", 'SE7') reckey, 1 class_no, 'all' class_lbl
  FROM lmf."PLANTCENSUS"
  GROUP BY "SURVEY", "STATE", "COUNTY", "PSU", "POINT"

), dima_base AS ( 
SELECT "PlotKey" plotkey, 
       unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9]) class_no,
       unnest(array["PlantDenClass1", "PlantDenClass2", "PlantDenClass3", "PlantDenClass4", 
					"PlantDenClass5", "PlantDenClass6", "PlantDenClass7", "PlantDenClass8", 
					"PlantDenClass9"]) class_lbl
  FROM dima."tblPlotFormDefaults"

), dima_final AS (
SELECT c."RecKey" reckey, a.class_no, a.class_lbl
  FROM dima_base a
 INNER JOIN dima."tblLines" b ON a.plotkey = b."PlotKey"
 INNER JOIN dima."tblPlantDenHeader" c ON b."LineKey" = c."LineKey"
 GROUP BY a.plotkey, c."RecKey", a.class_no, a.class_lbl
 HAVING a.class_lbl NOT IN ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.plantdensity_class;

--
-- plantdensity_species
--
DROP MATERIALIZED VIEW IF EXISTS public.plantdensity_species CASCADE;
CREATE MATERIALIZED VIEW public.plantdensity_species AS
WITH lmf_final AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", 'SE7') reckey, "CPLANT" species_code, 1641.6 subsize_m2
  FROM lmf."PLANTCENSUS"
  GROUP BY "SURVEY", "STATE", "COUNTY", "PSU", "POINT", "CPLANT", subsize_m2

), dima_process0 AS (
SELECT "RecKey" reckey, "SpeciesCode" species_code, "Quadrat" subplot,
	   "AreaInHectares" * 10000 subsize_m2
  FROM dima."tblPlantDenDetail"

), dima_process1 AS (
SELECT reckey, species_code, subsize_m2, count(subplot) n
  FROM dima_process0
GROUP BY reckey, species_code, subsize_m2

), dima_process2 AS (
SELECT reckey, species_code, subsize_m2, n,
       row_number() over(partition by reckey, species_code order by subsize_m2 desc) rn
  FROM dima_process1

), dima_final AS (
SELECT reckey, species_code, subsize_m2 
  FROM dima_process2 
 WHERE rn = 1
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.plantdensity_species;


--
-- plantdensity
--
DROP MATERIALIZED VIEW IF EXISTS public.plantdensity CASCADE;
CREATE MATERIALIZED VIEW public.plantdensity AS
WITH lmf_density (code, cnt_lower, cnt_upper) AS (
VALUES
-- 2000 chosen for upper range of density for calc purposes
(1, 1, 10),
(2, 11, 100), 
(3, 101, 500),
(4, 501, 1000), 
(5, 1001, 2000) 

), lmf_lower AS (
-- here we parse out the density estimation range into two separate subplots
SELECT concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT", 'SE7') reckey, 
       1 subplot,
       a."CPLANT" species_code, 1 class_no, b.cnt_lower total
  FROM lmf."PLANTCENSUS" a
  LEFT JOIN lmf_density b ON a."DENSITY" = b.code

), lmf_upper AS (
SELECT concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT", 'SE7') reckey, 
       2 subplot,
       a."CPLANT" species_code, 1 class_no, b.cnt_upper total
  FROM lmf."PLANTCENSUS" a
  LEFT JOIN lmf_density b ON a."DENSITY" = b.code

), lmf_final AS (
SELECT * FROM lmf_lower
 UNION ALL
SELECT * FROM lmf_upper

), dima_final AS (
SELECT "RecKey" reckey, "Quadrat" subplot, "SpeciesCode" species_code, 
       unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9]) class_no,
       unnest(array["Class1total", "Class2total", "Class3total", "Class4total", "Class5total", "Class6total", "Class7total", "Class8total", "Class9total"]) total, 
	   unnest(array["Class1density", "Class2density", "Class3density", "Class4density", "Class5density", "Class6density", "Class7density", "Class8density", "Class9density"]) density
  FROM dima."tblPlantDenDetail"
)

SELECT reckey, subplot, species_code, class_no, total
  FROM lmf_final
UNION ALL
SELECT reckey, subplot, species_code, class_no, total 
 FROM dima_final
 UNION ALL
SELECT * FROM eco.plantdensity;

--
-- gap_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.gap_meta CASCADE;
CREATE MATERIALIZED VIEW public.gap_meta AS
WITH lmf_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey,
	   concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2))) linekey,
       concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2)), '3') reckey,
	   "GAP_TYPE" stops_gap
  FROM lmf."GINTERCEPT"
 GROUP BY plotkey, linekey, reckey, stops_gap

), lmf_final AS (
SELECT a.reckey,	   
       b."CAPDATE" AT TIME ZONE 'UTC' survey_date, NULL observer,
	   30::double precision gapmin_cm, 'canopy' gap_types,
	   CASE WHEN stops_gap = 'canopy' THEN 'perennial;annual' 
	        WHEN stops_gap = 'peren' THEN 'perennial' 
			ELSE NULL END canopy_stops_gap,
	   False canopy_no_gap, NULL basal_stops_gap, NULL::boolean basal_no_gap, NULL notes,
       a.linekey
  FROM lmf_process0 a
  LEFT JOIN lmf."GPS" b ON a.plotkey = concat(b."SURVEY", b."STATE", b."COUNTY", b."PSU", b."POINT")

), dima_process0 AS (
SELECT a."LineKey" linekey, a."RecKey" reckey, 
       a."FormDate"::timestamp AT TIME ZONE c.tz survey_date, 
       a."Observer" observer,
	   CASE WHEN "Measure" = 2 THEN round("GapMin"::numeric * 30.48)
	        ELSE "GapMin" END gapmin_cm, 
	   CASE WHEN "GapData" = '1' THEN 'canopy;basal'
	        WHEN "GapData" = '2' THEN 'canopy'
			WHEN "GapData" = '3' THEN 'basal'
			ELSE NULL END gap_types, 
	   array_to_string(array[CASE WHEN "PerennialsCanopy" = True THEN 'perennial' ELSE NULL END, 
			 CASE WHEN "AnnualGrassesCanopy" = True AND "AnnualForbsCanopy" = True THEN 'annual' ELSE NULL END,
			 CASE WHEN "AnnualGrassesCanopy" = True AND "AnnualForbsCanopy" = False THEN 'annual grass' ELSE NULL END,   
			 CASE WHEN "AnnualGrassesCanopy" = False AND "AnnualForbsCanopy" = True THEN 'annual forb' ELSE NULL END,
			 CASE WHEN "OtherCanopy" = True THEN 'other' ELSE NULL END], ';') canopy_stops_gap, 
	   "NoCanopyGaps" canopy_no_gap, 
	   array_to_string(array[CASE WHEN "PerennialsBasal" = True THEN 'perennial' ELSE NULL END, 
			 CASE WHEN "AnnualGrassesBasal" = True AND "AnnualForbsBasal" = True THEN 'annual' ELSE NULL END,
			 CASE WHEN "AnnualGrassesBasal" = True AND "AnnualForbsBasal" = False THEN 'annual grass' ELSE NULL END,   
			 CASE WHEN "AnnualGrassesBasal" = False AND "AnnualForbsBasal" = True THEN 'annual forb' ELSE NULL END,
			 CASE WHEN "OtherBasal" = True THEN 'other' ELSE NULL END], ';') basal_stops_gap,
			 "NoBasalGaps" basal_no_gap, "Notes" notes
  FROM dima."tblGapHeader" a
  LEFT JOIN dima."tblLines" b ON a."LineKey" = b."LineKey"
  LEFT JOIN public.point c ON b."PlotKey" = c.plotkey

), dima_final AS (
SELECT reckey, survey_date, observer, gapmin_cm, gap_types, 
	   CASE WHEN canopy_stops_gap = '' THEN NULL ELSE canopy_stops_gap END canopy_stops_gap,
	   canopy_no_gap, 
	   CASE WHEN basal_stops_gap = '' THEN NULL ELSE basal_stops_gap END basal_stops_gap,
	   basal_no_gap, notes, linekey
  FROM dima_process0
)


SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.gap_meta;

--
-- gap
--
DROP MATERIALIZED VIEW IF EXISTS public.gap CASCADE;
CREATE MATERIALIZED VIEW public.gap AS
WITH lmf_final AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2)), '3') reckey, 
       'C' rectype, "SEQNUM" seqno, 
	   round("START_GAP"::numeric * 30.48, 0) gap_start_cm,
	   round("END_GAP"::numeric * 30.48, 0) gap_end_cm
  FROM lmf."GINTERCEPT"

), dima_final AS (
SELECT "RecKey" reckey, "RecType" rectype, "SeqNo" seqno, 
	    ((regexp_match("GapStart", '[\d\.]+'))[1])::double precision gap_start_cm, 
	    ((regexp_match("GapEnd", '[\d\.]+'))[1])::double precision gap_end_cm
  FROM dima."tblGapDetail"
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.gap;


--
-- note
--
DROP MATERIALIZED VIEW IF EXISTS public.note CASCADE;
CREATE MATERIALIZED VIEW public.note AS
WITH lmf_final AS (
SELECT substring(md5(concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT", a."PTSECTION", a."PTNOTE")) for 16) reckey, 
       b."CAPDATE" AT TIME ZONE 'UTC' note_date, NULL recorder,
       a."PTSECTION" note_type, a."PTNOTE" note,
	   concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") plotkey
  FROM lmf."PTNOTE" a
  LEFT JOIN lmf."GPS" b ON concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") = 
                           concat(b."SURVEY", b."STATE", b."COUNTY", b."PSU", b."POINT")
	
), dima_final AS (
SELECT a."CommentID" reckey, a."NoteDate"::timestamp AT TIME ZONE b.tz note_date, 
       a."Recorder" recorder, NULL note_type, a."Note" note, a."PlotKey" plotkey
  FROM dima."tblPlotNotes" a 
  LEFT JOIN public.point b ON a."PlotKey" = b.plotkey
 WHERE a."Note" IS NOT NULL AND trim(a."Note") != ''
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.note;

--
-- rangehealth_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.rangehealth_meta CASCADE;
CREATE MATERIALIZED VIEW public.rangehealth_meta AS
WITH lmf_eco_center0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey, "SEQNUM", "COVERAGE", "START_MARK", "END_MARK", 
       concat("ESFSG_MLRA", "ESFSG_SITE", "ESFSG_STATE") ecoid_std
  FROM lmf."ESFSG"
 WHERE "COVERAGE" = 'all' OR 75 BETWEEN "START_MARK" AND "END_MARK"

), lmf_eco_center1 AS (
SELECT plotkey, ecoid_std, count(ecoid_std) n
  FROM lmf_eco_center0
 GROUP BY plotkey, ecoid_std

), lmf_eco_center2 AS (
SELECT plotkey, ecoid_std, n,
       row_number() over(partition by plotkey order by n DESC, ecoid_std) rn
  FROM lmf_eco_center1

), lmf_eco_center_final AS (
SELECT plotkey, ecoid_std 
  FROM lmf_eco_center2
 WHERE rn = 1

), lmf_final AS (
SELECT concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT", '4') reckey,
       b."CAPDATE" AT TIME ZONE 'UTC' survey_date, NULL observer,
	   c.ecoid_std, NULL refsheet_src, NULL::date refsheet_datepub, NULL refsheet_author,
	   NULL::date refsheet_dategot, 'preponderance of evidence' indicator_wgt_method,
	   NULL weight_src, NULL rep_criteria, NULL composition_base,
       concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") plotkey
  FROM lmf."RANGEHEALTH" a
  LEFT JOIN lmf."GPS" b ON concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") = 
                           concat(b."SURVEY", b."STATE", b."COUNTY", b."PSU", b."POINT")
  LEFT JOIN lmf_eco_center_final c ON concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") = c.plotkey

), dima_final AS (
SELECT a."RecKey" reckey, 
       a."FormDate"::timestamp AT TIME ZONE b.tz survey_date, a."Observer" observer,
	   coalesce(substring(trim(a."EcolSite"), '^(?:F|R|G?)(\d{3}[A-z]?[A-z]?\d{3}[A-z]{2})(_*\d*)$'), 
				trim(a."EcolSite")) ecoid_std, 
	   CASE WHEN a."RefSheetType" = 1 THEN 'new'
	        WHEN a."RefSheetType" = 2 THEN 'nrcs'
			WHEN a."RefSheetType" = 3 THEN 'other'
			ELSE NULL END refsheet_src, 
			a."RefSheetDate" refsheet_datepub, 
			a."RefSheetAuthor" refsheet_author, 
			a."DateDownloaded" refsheet_dategot, 
       CASE WHEN a."AttrEvalMethod" = 0 THEN 'preponderance of evidence'
	        WHEN a."AttrEvalMethod" = 1 THEN 'equal'
	        WHEN a."AttrEvalMethod" = 2 THEN 'downloaded'
	        WHEN a."AttrEvalMethod" = 3 THEN 'user defined'
	        ELSE NULL END indicator_wgt_method, 
	   a."WeightsSource" weight_src, 
	   a."RepCriteria" rep_criteria, 
       CASE WHEN a."CompositionBase" = 1 THEN 'production'
	        WHEN a."CompositionBase" = 2 THEN 'cover'
			WHEN a."CompositionBase" = 3 THEN 'biomass'
			ELSE NULL END composition_base,
	   a."PlotKey" plotkey
  FROM dima."tblQualHeader" a
  LEFT JOIN public.point b ON a."PlotKey" = b.plotkey
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.rangehealth_meta;

--
-- rangehealth
--
DROP MATERIALIZED VIEW IF EXISTS public.rangehealth CASCADE;
CREATE MATERIALIZED VIEW public.rangehealth AS 
WITH lmf_ratings (rating_str, rating) AS (
VALUES 
('NS', 1), 
('SM', 2),
('MO', 3), 
('ME', 4), 
('EX', 5)

), indicators (seq_no, rate_abbr) AS (
VALUES 
(1,'R'), 
(2,'WFP'), 
(3,'PT'), 
(4,'BG'), 
(5,'G'), 
(6,'WSBDA'), 
(7,'LM'), 
(8,'SSRE'), 
(9,'SSLD'), 
(10,'PCCDRIR'), 
(11,'CL'), 
(12,'FSG'), 
(13,'PMD'), 
(14,'LA'), 
(15,'AP'), 
(16,'IP'), 
(17,'RCPP'),
(100, 'SSS'),
(200, 'HF'),
(300, 'BI')

), lmf_process0 AS ( 
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", '4') reckey,
       unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 100, 300, 200]) seq_no,
	   unnest(array['i', 'i', 'i', 'i', 'i', 'i', 'i', 'i', 'i', 'i', 
					'i', 'i', 'i', 'i', 'i', 'i', 'i', 'a', 'a', 'a']) rate_type,
       unnest(array["RILLS", "WATER_FLOW_PATTERNS", "PEDESTALS_TERRACETTES", "BARE_GROUND", "GULLIES", 
	   "WIND_SCOURED_AREAS", "LITTER_MOVEMENT", "SOIL_SURF_RESIS_EROSION", "SOIL_SURFACE_LOSS_DEG", 
	   "INFILTRATION_RUNOFF", "COMPACTION_LAYER", "FUNC_STRUCT_GROUPS", "PLANT_MORTALITY_DEC", 
	   "LITTER_AMOUNT", "ANNUAL_PRODUCTION", "INVASIVE_PLANTS", "REPROD_CAPABILITY_PEREN", 
	   "SOILSITE_STABILITY", "BIOTIC_INTEGRITY", "HYDROLOGIC_FUNCTION"]) rating_str
  FROM lmf."RANGEHEALTH"

), lmf_final AS (
SELECT a.reckey, a.seq_no, c.rate_abbr, a.rate_type, b.rating, NULL note
  FROM lmf_process0 a
  LEFT JOIN lmf_ratings b ON a.rating_str = b.rating_str
  LEFT JOIN indicators c ON a.seq_no = c.seq_no

), dima_process0 AS (
SELECT a."RecKey" reckey, a."Seq" seq_no, b.rate_abbr, 'i' rate_type, a."Rating" rating, a."Comment" note
  FROM dima."tblQualDetail" a
  LEFT JOIN indicators AS b on a."Seq" = b.seq_no

), dima_process1 AS (
SELECT "RecKey" reckey, 
       unnest(array[100, 200, 300]) seq_no,
	   unnest(array['a', 'a', 'a']) rate_type,
	   unnest(array["SSSVxWRatingFinal", "HFVxWRatingFinal", "BIVxWRatingFinal"]) rating_str,
	   unnest(array["CommentSSS", "CommentHF", "CommentBI"]) note
  FROM dima."tblQualHeader"

), dima_process2 AS (
SELECT a.reckey, a.seq_no, c.rate_abbr, a.rate_type, b."Rating" rating, a.note
  FROM dima_process1 a
  LEFT JOIN dima."tblMaintQualRatings" b ON a.rating_str = b."Code"
  LEFT JOIN indicators c ON a.seq_no = c.seq_no
  WHERE a.rating_str IS NOT NULL

), dima_final AS ( 
SELECT * FROM dima_process0
 UNION ALL
SELECT * FROM dima_process2
)

SELECT * FROM lmf_final 
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.rangehealth;

--
-- soilstability_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.soilstability_meta CASCADE;
CREATE MATERIALIZED VIEW public.soilstability_meta AS
WITH lmf_final AS (
SELECT concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT", '5') reckey, 
       b."CAPDATE" AT TIME ZONE 'UTC' survey_date, NULL observer,
	   'surface' rectype, NULL notes,
       concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") plotkey 
  FROM lmf."SOILDISAG" a
  LEFT JOIN lmf."GPS" b ON concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT") = 
                           concat(b."SURVEY", b."STATE", b."COUNTY", b."PSU", b."POINT")
		
), dima_final AS (
SELECT a."RecKey" reckey, a."FormDate"::timestamp AT TIME ZONE b.tz survey_date, 
       a."Observer" observer,
       CASE WHEN a."SoilStabSubSurface" = 2 THEN 'surface/subsurface'
	        ELSE 'surface' END rectype, a."Notes" notes, a."PlotKey" plotkey
  FROM dima."tblSoilStabHeader" a
  LEFT JOIN public.point b ON a."PlotKey" = b.plotkey
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.soilstability_meta;

--
-- soilstability
--
DROP MATERIALIZED VIEW IF EXISTS public.soilstability CASCADE;
CREATE MATERIALIZED VIEW public.soilstability AS
WITH lmf_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey, 1 box_no,
       unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18]) cell,
	   unnest(array['nesw', 'nesw', 'nesw', 'nesw', 'nesw', 'nesw', 'nesw', 'nesw', 'nesw', 
					'nwse', 'nwse', 'nwse', 'nwse', 'nwse', 'nwse', 'nwse', 'nwse', 'nwse']) lineid,
	   unnest(array[15, 30, 45, 60, 75, 90, 105, 120, 135, 15, 30, 45, 60, 75, 90, 105, 120, 135]) pos,
       unnest(array["VEG1", "VEG2", "VEG3", "VEG4", "VEG5", "VEG6", "VEG7", "VEG8", "VEG9", 
					"VEG10", "VEG11", "VEG12", "VEG13", "VEG14", "VEG15", "VEG16", "VEG17", "VEG18"]) veg, 
	   unnest(array["STABILITY1", "STABILITY2", "STABILITY3", "STABILITY4", "STABILITY5", "STABILITY6", "STABILITY7", "STABILITY8", "STABILITY9", 
					"STABILITY10", "STABILITY11", "STABILITY12", "STABILITY13", "STABILITY14", "STABILITY15", "STABILITY16", "STABILITY17", "STABILITY18"]) rating,
	   NULL::boolean hydrophobic
  FROM lmf."SOILDISAG"

), lmf_final AS (
SELECT concat(plotkey, '5') reckey,
       box_no, cell, lineid, pos, veg, rating, hydrophobic
  FROM lmf_process0
 WHERE trim(veg) != '' AND veg IS NOT NULL AND rating != 0

), dima_process0 AS (
SELECT "RecKey" reckey, "BoxNum" box_no,
	   unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18]) cell,
       unnest(array["Line1", "Line1", "Line1", "Line2", "Line2", "Line2", "Line3", "Line3", "Line3", 
					"Line4", "Line4", "Line4", "Line5", "Line5", "Line5", "Line6", "Line6", "Line6"]) lineid, 
	   unnest(array["Pos1", "Pos2", "Pos3", "Pos4", "Pos5", "Pos6", "Pos7", "Pos8", "Pos9", 
					"Pos10", "Pos11", "Pos12", "Pos13", "Pos14", "Pos15", "Pos16", "Pos17", "Pos18"]) pos, 
	   unnest(array["Veg1", "Veg2", "Veg3", "Veg4", "Veg5", "Veg6", "Veg7", "Veg8", "Veg9", 
					"Veg10", "Veg11", "Veg12", "Veg13", "Veg14", "Veg15", "Veg16", "Veg17", "Veg18"]) veg, 
	   unnest(array["Rating1", "Rating2", "Rating3", "Rating4", "Rating5", "Rating6", "Rating7", "Rating8", "Rating9", 
					"Rating10", "Rating11", "Rating12", "Rating13", "Rating14", "Rating15", "Rating16", "Rating17", "Rating18"]) rating, 
	   unnest(array["Hydro1", "Hydro2", "Hydro3", "Hydro4", "Hydro5", "Hydro6", "Hydro7", "Hydro8", "Hydro9", 
					"Hydro10", "Hydro11", "Hydro12", "Hydro13", "Hydro14", "Hydro15", "Hydro16", "Hydro17", "Hydro18"]) hydrophobic, 
	   unnest(array["In1", "In2", "In3", "In4", "In5", "In6", "In7", "In8", "In9", 
					"In10", "In11", "In12", "In13", "In14", "In15", "In16", "In17", "In18"]) time_wet, 
	   unnest(array["Dip1", "Dip2", "Dip3", "Dip4", "Dip5", "Dip6", "Dip7", "Dip8", "Dip9", 
					"Dip10", "Dip11", "Dip12", "Dip13", "Dip14", "Dip15", "Dip16", "Dip17", "Dip18"]) time_dip
  FROM dima."tblSoilStabDetail"

), dima_process1 AS (
SELECT reckey, box_no, cell,
       CASE WHEN trim(lineid) = '' THEN NULL ELSE trim(lineid) END lineid,
	   CASE WHEN trim(pos) = '' THEN NULL ELSE cast(substring(pos, '[0-9]+\.{0,1}[0-9]*') as double precision) END pos,
	   CASE WHEN trim(veg) = '' THEN NULL ELSE trim(veg) END veg,
	   CASE WHEN trim(rating) = '' THEN NULL ELSE cast(substring(rating, '\d+') as integer) END rating,
	   hydrophobic
  FROM dima_process0

), dima_final AS (
SELECT reckey, box_no, cell, lineid, pos, veg, rating, hydrophobic
  FROM dima_process1
 WHERE coalesce(pos::varchar, veg, rating::varchar) IS NOT NULL
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.soilstability;

--
-- soil_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.soil_meta CASCADE;
CREATE MATERIALIZED VIEW public.soil_meta AS
WITH lmf_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", '6') reckey,
       concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey
  FROM lmf."SOILHORIZON"
 GROUP BY "SURVEY", "STATE", "COUNTY", "PSU", "POINT"
						   
), lmf_final AS (
SELECT a.reckey,
       c."CAPDATE" AT TIME ZONE 'UTC' survey_date, 
	   NULL observer, NULL pit_desc, 
	   nullif(trim(b."SSAID"), '') area_symbol, 
	   nullif(trim(b."MUSYM"), '') mapunit_symbol, 
	   NULL taxon_name, NULL series_name,
	   nullif(trim(b."COMPONENT_NAME"), '') component_name, 
	   nullif(trim(b."COMPONENT_ID"), '') component_key, 
       b."SOIL_CONFIDENCE_RATING" soil_confidence_rating,
	   NULL notes,
	   NULL::double precision longitude, NULL::double precision latitude,
	   NULL::double precision elevation_m, NULL::geometry geom,
	   a.plotkey
  FROM lmf_process0 a
  LEFT JOIN lmf."POINT" b ON a.plotkey = concat(b."SURVEY", b."STATE", b."COUNTY", b."PSU", b."POINT")
  LEFT JOIN lmf."GPS" c ON a.plotkey = concat(c."SURVEY", c."STATE", c."COUNTY", c."PSU", c."POINT")

), dima_coords0 AS (
SELECT a."PlotKey" plotkey, 
	   CASE WHEN (a."Easting" IS NULL OR a."Easting" = 0) AND 
	             (a."Longitude" IS NOT NULL AND a."Longitude" != 0) THEN a."Longitude"
	        ELSE a."Easting" END easting, 
	   CASE WHEN (a."Northing" IS NULL OR a."Northing" = 0) AND 
	             (a."Latitude" IS NOT NULL AND a."Latitude" != 0) THEN a."Latitude" 
	        ELSE a."Northing" END northing, 
	   b."GPSCoordSys" gpscoordsys, b."Datum" datum, b."Zone" zone_str,
	   substring(b."Zone", '\d{2}') zone_num, 
	   CASE WHEN a."ElevationType" = 1 THEN a."Elevation"
	        WHEN a."ElevationType" = 2 THEN a."Elevation" / 3.281
	        ELSE 0 END elevation_m
  FROM dima."tblSoilPits" a
  LEFT JOIN dima."tblPlots" b ON a."PlotKey" = b."PlotKey"

), dima_coords1 AS (
SELECT plotkey, easting, northing, gpscoordsys, datum, zone_num, round(cast(elevation_m as numeric), 1) elevation_m,
	   CASE WHEN easting BETWEEN -180 AND 180 AND northing BETWEEN -90 AND 90 THEN True
	        ELSE False END AS valid_latlong
  FROM dima_coords0
 WHERE easting IS NOT NULL AND easting != 0 AND northing IS NOT NULL AND northing != 0

), dima_coords2 AS (
SELECT plotkey, 
	   CASE WHEN easting > 0 AND valid_latlong = True THEN easting * -1
	        ELSE easting END easting, 
	   northing, elevation_m, gpscoordsys, datum, zone_num, valid_latlong,
       CASE WHEN datum ~ 'WGS\s*84' AND valid_latlong = True THEN 4326
	        WHEN datum ~ 'NAD\s*83' AND valid_latlong = True THEN 4269
            WHEN datum ~ 'WGS\s*84' AND valid_latlong = False THEN CAST('326' || lpad(zone_num, 2, '0') AS integer)
			WHEN datum ~ 'NAD\s*83' AND valid_latlong = False THEN CAST('269' || lpad(zone_num, 2, '0') AS integer)
			ELSE NULL END srid
  FROM dima_coords1

), dima_coords3 AS (
SELECT plotkey, elevation_m,
       ST_Transform(ST_SetSRID(ST_MakePoint(easting, northing, elevation_m), srid), 4326) AS geom
  FROM dima_coords2 
 WHERE srid IS NOT NULL

), dima_coords4 AS (
SELECT a.plotkey, st_x(a.geom) longitude, st_y(a.geom) latitude, elevation_m, a.geom
  FROM dima_coords3 a

), dima_process0 AS (
SELECT a."SoilKey" reckey, a."DateRecorded"::timestamp AT TIME ZONE c.tz survey_date, 
	   a."Observer" observer, a."PitDesc" pit_desc, NULL area_symbol, b."Soil" mapunit_symbol,
	   array_to_string(array[
	     array_to_string(array[
		   nullif(trim(b."ESD_ParticleSizeClass"), ''), 
		   nullif(trim(b."ESD_Mineralogy"), ''), 
		   nullif(trim(b."ESD_CationExchangeActivityClass"), ''), 
		   nullif(trim(b."ESD_Reaction"), ''), 
		   nullif(trim(b."ESD_SoilTempRegime"), ''), 
		   nullif(trim(b."ESD_DepthClass"), '')], ', '),
	     nullif(trim(b."ESD_Subgroup"), ''), 
	     nullif(trim(b."ESD_SoilMoistureRegime"), ''), 
	     nullif(trim(b."ESD_Greatgroup"), '')], ' ') taxon_name,
	   b."ESD_Series" series_name, b."MapUnitComponent" component_name,
	   NULL component_key, NULL::integer soil_confidence_rating, a."Notes" notes,
	   CASE WHEN a."ElevationType" = 2 THEN a."Elevation"/3.281
	        ELSE a."Elevation" END elevation_m, 
       a."Latitude" latitude, a."Longitude" longitude,
	   a."PlotKey" plotkey
  FROM dima."tblSoilPits" a
  LEFT JOIN dima."tblPlots" b on a."PlotKey" = b."PlotKey"
  LEFT JOIN public.point c ON a."PlotKey" = c.plotkey
	
), dima_final AS (
SELECT a.reckey, a.survey_date, a.observer, a.pit_desc, a.area_symbol,
	   nullif(trim(a.mapunit_symbol), '') mapunit_symbol, 
	   nullif(trim(a.taxon_name),'') taxon_name, 
	   nullif(trim(a.series_name), '') series_name, 
	   nullif(trim(a.component_name), '') component_name, 
	   a.component_key, a.soil_confidence_rating, a.notes,
       coalesce(b.longitude, a.longitude) longitude, 
	   coalesce(b.latitude, a.latitude) latitude, 
	   coalesce(b.elevation_m, a.elevation_m) elevation_m,
	   b.geom, a.plotkey
  FROM dima_process0 a
  LEFT JOIN dima_coords4 b ON a.plotkey = b.plotkey
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.soil_meta;

--
-- soil
--
DROP MATERIALIZED VIEW IF EXISTS public.soil CASCADE;
CREATE MATERIALIZED VIEW public.soil AS 
WITH dima_process0 AS (
-- pages references from NRCS Field Book for Describing and Sampling Soils v3
SELECT "SoilKey" reckey,
       CASE WHEN "DepthMeasure" = 'cm' THEN cast(substring("HorizonDepthUpper", '\d+\.{0,1}\d*') AS double precision)
	        ELSE round(cast(substring("HorizonDepthUpper", '\d+\.{0,1}\d*') AS numeric) * 2.54, 1) END depth_upper_cm, 
       CASE WHEN "DepthMeasure" = 'cm' THEN cast(substring("HorizonDepthLower", '\d+\.{0,1}\d*') AS double precision)
	        ELSE round(cast(substring("HorizonDepthLower", '\d+\.{0,1}\d*') AS numeric) * 2.54, 1) END depth_lower_cm, 
	   -- Horizon and Layer Designations, 2-2
	   "ESD_Horizon" horizon_lbl,
	   -- Soil Color, 2-8
	   "ESD_Hue" color_hue, "ESD_Value"::double precision color_value, "ESD_Chroma" color_chroma, lower("ESD_Color") color_measure_type,
	   -- Concentrations, 2-20
	   "ESD_GravelCarbonateCoatPct"::double precision/100 conc_gr_co3_pct,
	   -- Pedogenic Carbonate Stages (Discussion), 2-28
	   "ESD_CarbonateStage" conc_carbonate_stage,
	   -- Ped and Void Surface Features, 2-32
	   "ESD_ClayFilm" film_clay,
	   -- Soil Texture, 2-36
       "Texture" texture, "ESD_HorizonModifier" texture_modifier, "ESD_Gypsic" texture_gypsic,
	   -- Rock and Other Fragments, 2-46
	   "RockFragments"::double precision/100 frag_total_pct, "ESD_PetrocalcicRubble" frag_petrocalcic,
	   -- (Soil) Structure, 2-52
	   "ESD_Structure" struct_type1, "ESD_Grade"::smallint struct_grade1, "ESD_Size" struct_size1, "ESD_StructQual" struct_verb,
	   "ESD_Structure2" struct_type2, "ESD_Grade2"::smallint struct_grade2, "ESD_Size2" struct_size2,
	   -- Consistence - Rupture Resistance, 2-62
	   "ESD_RuptureResistance" rupture_resist, 
	   -- Roots, 2-70
	   "ESD_RootSize" root_size, "ESD_RootQty" root_qty, "ESD_PoresSize" pore_size, "ESD_PoresQty" pore_qty,
	   -- Chemical Response, 2-85
	   "ESD_pH" chem_ph, 
	   -- Chemical Response - Effervescence, 2-87
	   "Effer" chem_effer_class, "ESD_CaCO3EquivPct"::double precision/100 chem_caco3_equiv_pct, 
       "ESD_GypsumPct"::double precision/100 chem_gypsum_pct, 
	   -- Chemical Response - Salinity Class, 2-89
	   "ESD_EC" chem_ec, "ESD_NAabsorptionRatio" chem_sar,
	   -- notes
	   "ESD_Notes" notes
  FROM dima."tblSoilPitHorizons"

), dima_final AS (
SELECT reckey, row_number() over(partition by reckey order by depth_upper_cm) seq_no,
	   depth_upper_cm, depth_lower_cm, horizon_lbl, color_hue, color_value, color_chroma, color_measure_type, 
	   conc_gr_co3_pct, conc_carbonate_stage, film_clay, texture, texture_modifier, texture_gypsic, frag_total_pct, 
	   frag_petrocalcic, struct_type1, struct_grade1, struct_size1, struct_verb, struct_type2, struct_grade2, struct_size2, 
	   rupture_resist, root_size, root_qty, pore_size, pore_qty, chem_ph, chem_effer_class, chem_caco3_equiv_pct, 
	   chem_gypsum_pct, chem_ec, chem_sar, notes
  FROM dima_process0

), lmf_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", '6') reckey,
	   "SEQNUM" seq_no,
	   lag("DEPTH", 1) over(partition by "SURVEY", "STATE", "COUNTY", "PSU", "POINT" order by "SEQNUM") depth_upper_in,
	   "DEPTH" depth_lower_in, 
	   CASE WHEN "HORIZON_TEXTURE" = '' THEN NULL
	        WHEN "HORIZON_TEXTURE" = 'bed' THEN 'BR'
	        ELSE upper("HORIZON_TEXTURE") END texture, 
	   CASE WHEN "TEXTURE_MODIFIER" IN ('', 'none') THEN NULL
	        ELSE upper("TEXTURE_MODIFIER") END texture_modifier, 
	   CASE WHEN "EFFERVESCENCE_CLASS" = '' THEN NULL 
	        ELSE "EFFERVESCENCE_CLASS" END chem_effer_class, 
	   CASE WHEN trim("UNUSUAL_FEATURES") = '' THEN NULL 
	        ELSE trim("UNUSUAL_FEATURES") END notes
  FROM lmf."SOILHORIZON"

	
), lmf_final AS (
SELECT reckey, seq_no,
	   coalesce(round(depth_upper_in::numeric * 2.54, 1), 0) depth_upper_cm, 
	   round(depth_lower_in::numeric * 2.54, 1) depth_lower_cm,
	   NULL horizon_lbl, NULL color_hue, NULL::double precision color_value, NULL::double precision color_chroma, NULL color_measure_type, 
	   NULL::double precision conc_gr_co3_pct, NULL::smallint conc_carbonate_stage, NULL::boolean film_clay, 
	   texture, texture_modifier, 
	   NULL::boolean texture_gypsic, 
	   NULL::double precision frag_total_pct, NULL::boolean frag_petrocalcic, 
	   NULL struct_type1, NULL::smallint struct_grade1, NULL struct_size1, NULL struct_verb, 
	   NULL struct_type2, NULL::smallint struct_grade2, NULL struct_size2, 
	   NULL rupture_resist, NULL root_size, NULL root_qty, NULL pore_size, NULL pore_qty, NULL::double precision chem_ph, 
	   chem_effer_class, 
	   NULL::double precision chem_caco3_equiv_pct, NULL::double precision chem_gypsum_pct, 
	   NULL::double precision chem_ec, NULL::double precision chem_sar, 
	   notes
  FROM lmf_process0
)

SELECT * FROM lmf_final 
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.soil;

--
-- soil_component
--
DROP MATERIALIZED VIEW IF EXISTS public.soil_component CASCADE;
CREATE MATERIALIZED VIEW public.soil_component AS
WITH code_lbls (lbl, code, seq_no) AS (
VALUES
('Gravel', 'GR', 4),
('Cobble', 'CB', 5),
('Stone', 'ST', 6),
('Carbonate nodule', 'CAN', 7),
('Durinode', 'DNN', 8)

), dima_soiltexture0 AS (
SELECT "HorizonKey" detailkey,
	   unnest(array[1, 2, 3, 1, 2, 3]) seq_no, 
	   unnest(array['field', 'field', 'field', 'lab', 'lab', 'lab']) analysis,
	   unnest(array['sand', 'silt', 'clay', 'sand', 'silt', 'clay']) component,
       unnest(array["ESD_PctSand"/100, (100-("ESD_PctSand" + "ESD_PctClay")/100), "ESD_PctClay"/100, 
					"ESD_PSAPctSand"/100, "ESD_PSAPctSilt"/100, "ESD_PSAPctClay"/100]) vol_pct,
	   unnest(array[NULL, NULL, NULL, "ESD_SandFractPctVeryFine"/100, NULL, NULL]) frac_vfine,
	   unnest(array[NULL, NULL, NULL, "ESD_SandFractPctFine"/100, NULL, NULL]) frac_fine,
	   unnest(array[NULL, NULL, NULL, "ESD_SandFractPctMed"/100, NULL, NULL]) frac_med,
	   unnest(array[NULL, NULL, NULL, "ESD_SandFractPctCoarse"/100, NULL, NULL]) frac_coarse,
	   unnest(array[NULL, NULL, NULL, "ESD_SandFractPctVeryCoarse"/100, NULL, NULL]) frac_vcoarse
  FROM dima."tblSoilPitHorizons"

), dima_soiltexture1 AS (
SELECT detailkey, seq_no, analysis, component, vol_pct,
	   NULL frag_roundness, frac_vfine, frac_fine, frac_med, frac_coarse, frac_vcoarse
  FROM dima_soiltexture0
 WHERE coalesce(vol_pct, frac_vfine, frac_fine, frac_med, frac_coarse, frac_vcoarse) IS NOT NULL
	

), dima_fragtexture0 AS (
SELECT "HorizonKey" detailkey,
       unnest(array["ESD_FragmentType", "ESD_FragmentType2", "ESD_FragmentType3"]) texture_id, 
	   unnest(array["ESD_FragVolPct", "ESD_FragVolPct2", "ESD_FragVolPct3"]) vol_pct
  FROM dima."tblSoilPitHorizons"

), dima_fragtexture1 AS (
SELECT a.detailkey, c.seq_no, 'field' analysis, lower(b."Abbrev") component,
       cast(substring(a.vol_pct, '\d+\.{0,1}\d*') as double precision)/100 vol_pct
  FROM dima_fragtexture0 a
  LEFT JOIN dima."tblMaintESDFragmentTypes" b ON a.texture_id = b."Id"::varchar
  LEFT JOIN code_lbls c ON b."Abbrev" = c.lbl
  WHERE a.vol_pct  IS NOT NULL AND a.texture_id IS NOT NULL AND b."Abbrev" != 'None'

), dima_gravel_field AS (
SELECT "HorizonKey" detailkey, 'field' analysis, 'gravel' component, "ESD_FragmentRoundness" frag_roundness,
	   NULL::double precision frac_vfine, "ESD_GravelClassPctFine"/100 frac_fine, "ESD_GravelClassPctMed"/100 frac_med, 
	   "ESD_GravelClassPctCoarse"/100 frac_coarse, NULL::double precision frac_vcoarse
  FROM dima."tblSoilPitHorizons"
 WHERE coalesce("ESD_GravelClassPctFine", "ESD_GravelClassPctMed", "ESD_GravelClassPctCoarse") IS NOT NULL OR
       "ESD_FragmentRoundness" IS NOT NULL

), dima_fragtexture2 AS ( 
SELECT a.detailkey, a.seq_no, a.analysis, a.component, a.vol_pct,
       b.frag_roundness, b.frac_vfine, b.frac_fine, b.frac_med, b.frac_coarse, b.frac_vcoarse
  FROM dima_fragtexture1 a
  LEFT JOIN dima_gravel_field b ON a.detailkey = b.detailkey AND a.analysis = b.analysis AND a.component = b.component

), dima_labgravel AS (
SELECT "HorizonKey" detailkey, 4 seq_no, 'lab' analysis, 'gravel' component,
	   NULL::double precision vol_pct, NULL frag_roundness,
       NULL::double precision frac_vfine, "ESD_LabGravelPctFine"/100 frac_fine, 
	   "ESD_LabGravelPctMed"/100 frac_med, "ESD_LabGravelPctCoarse"/100 frac_coarse,
	   NULL::double precision frac_vcoarse
  FROM dima."tblSoilPitHorizons"
 WHERE coalesce("ESD_LabGravelPctFine", "ESD_LabGravelPctMed", "ESD_LabGravelPctCoarse") IS NOT NULL

), dima_final AS (
SELECT * FROM dima_soiltexture1
 UNION ALL 
SELECT * FROM dima_fragtexture2
 UNION ALL
SELECT * FROM dima_labgravel
)

SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.soil_component;

------------------------------
------------------------------
--- NON MATERIALIZED VIEWS ---
------------------------------
------------------------------

DROP VIEW IF EXISTS public.plant_mod CASCADE;
CREATE VIEW public.plant_mod AS 
SELECT accepted_symbol, code_type, scientific_name, common_name, family, 
duration, split_part(duration, ', ', 1) AS duration_first, 
growth_habit, split_part(growth_habit, ', ', 1) AS growth_habit_first, 
native_status, hybrid_genus_indicator, genus, hybrid_species_indicator, species, subspecies_prefix, 
hybrid_subspecies_indicator, subspecies, variety_prefix, hybrid_variety_indicator, variety, forma_prefix, 
forma, genera_binomial_author, trinomial_author, quadranomial_author, parents, state_and_province 
  FROM public.plant;
COMMENT ON VIEW public.plant_mod IS 'creates a version of the ''plant'' table with first growth habit and duration extracted for later use';

--
--
DROP VIEW IF EXISTS public.shrubshape_plot CASCADE;
CREATE VIEW public.shrubshape_plot AS
WITH ct AS (
SELECT * 
FROM crosstab(
$$
	WITH shape AS (
	SELECT a.plotkey, (date_part('year', c.survey_date))::integer AS survey,
		   b.linekey, d.reckey, d.mark, d.species_code,
		   CASE WHEN d.shape = 'C' THEN 'columnar'
				WHEN d.shape = 'M' THEN 'mixed'
				WHEN d.shape = 'S' THEN 'spreading'
				ELSE NULL
		   END AS shape
	  FROM public.point AS a
	  LEFT JOIN public.transect AS b ON a.plotkey = b.plotkey
	  LEFT JOIN public.pintercept_meta AS c ON b.linekey = c.linekey
	  LEFT JOIN public.shrubshape AS d ON c.reckey = d.reckey
	 WHERE shape IS NOT NULL

	), shape_counts AS (
	SELECT plotkey, survey, shape, count(shape) AS n
	  FROM shape
	 GROUP BY plotkey, survey, shape

	), shape_totals AS (
	SELECT plotkey, survey, count(shape) AS n
	  FROM shape
	 GROUP BY plotkey, survey

	), shape_pct AS (
	SELECT a.plotkey, a.survey, a.shape, a.n, b.n AS total_n,
		   ((a.n)::double precision / (b.n)::double precision) AS shape_pct
	  FROM shape_counts AS a
	 INNER JOIN shape_totals AS b ON a.plotkey = b.plotkey AND a.survey = b.survey
	)

	SELECT array[plotkey, survey::text] key_year,
		   shape, shape_pct::numeric(4,3) 
	  FROM shape_pct 
	 ORDER BY 1,2
$$
) AS t (key_year text[], columnar numeric, mixed numeric, spreading numeric)
)

SELECT key_year[1] plotkey, key_year[2]::integer survey,
       coalesce(columnar, 0) columnar_pct, 
	   coalesce(mixed, 0) mixed_pct, 
	   coalesce(spreading,0) spreading_pct
  FROM ct;
COMMENT ON VIEW public.shrubshape_plot  IS 'summarizes shrub shape data by plot';

--
--
DROP VIEW IF EXISTS public.plant_discrepencies CASCADE;
CREATE VIEW public.plant_discrepencies AS
WITH gh_compare AS (
SELECT a.accepted_symbol, a.scientific_name, a.family, trim(split_part(a.growth_habit, ',', 1)) AS growth_habit_first, 
       CASE WHEN c."GrowthHabitSub" = 'Sub-Shrub' THEN 'Subshrub'
            WHEN c."GrowthHabitSub" = 'Sedge' THEN 'Graminoid' 
            ELSE c."GrowthHabitSub" END AS growth_habit_dima,
       a.growth_habit, trim(split_part(a.duration, ',', 1)) AS duration_first, a.duration, b."Duration" AS duration_dima
  FROM public.plant AS a
 INNER JOIN dima."tblSpecies" AS b ON a.accepted_symbol = b."SpeciesCode"
 INNER JOIN dima."tblSpeciesGrowthHabit" AS c ON b."GrowthHabitCode" = c."Code"),

code_count AS (
SELECT a.hit, b.common_name, b.scientific_name, b.family, count(a.hit) AS n
  FROM public.pintercept AS a
 INNER JOIN public.plant AS b ON a.hit = b.accepted_symbol
 GROUP BY a.hit, b.common_name, b.scientific_name, b.family)

SELECT a.*, b.n 
  FROM gh_compare AS a
 INNER JOIN code_count AS b ON a.accepted_symbol = b.hit
 WHERE a.growth_habit_first != a.growth_habit_dima AND
       a.growth_habit_dima != 'Succulent'
 ORDER BY b.n DESC;
COMMENT ON VIEW public.plant_discrepencies IS 'compares growth habit and duration differences between dima."tblSpecies" and public.plant';


-- production_calc
DROP VIEW IF EXISTS public.production_calc CASCADE;
CREATE VIEW public.production_calc AS
WITH base AS (
SELECT a.reckey, a.species_code, a.subplot, a.units, a.trace, a.clipped_wgt_g, 
	   b.subsize_m2, b.unit_wgt_g,
       CASE WHEN a.units = 0 AND a.clipped_wgt_g > 0 THEN a.clipped_wgt_g 
	        ELSE a.units * b.unit_wgt_g END est_wgt_g
  FROM public.production a
  LEFT JOIN public.production_species b  ON a.reckey = b.reckey AND a.species_code = b.species_code

), cf0 AS (
SELECT reckey, species_code, sum(est_wgt_g) est_wgt_g, sum(clipped_wgt_g) clipped_wgt_g 
  FROM base 
 WHERE clipped_wgt_g IS NOT NULL
 GROUP BY reckey, species_code

), cf1 AS (
SELECT reckey, species_code, est_wgt_g, clipped_wgt_g, clipped_wgt_g/est_wgt_g adjust_clip
  FROM cf0
WHERE est_wgt_g > 0

), species0 AS (
SELECT a.reckey, a.species_code, a.subsize_m2, a.unit_wgt_g, a.adjust_airdrywgt, 
       a.adjust_utilization, a.adjust_growth, a.adjust_climate, coalesce(b.adjust_clip, 1) adjust_clip
  FROM public.production_species a
  LEFT JOIN cf1 b ON a.reckey = b.reckey AND a.species_code = b.species_code

), base_sum AS (
SELECT reckey, species_code, sum(est_wgt_g) est_wgt_g, sum(subsize_m2) samplesize_m2 
  FROM base 
 GROUP BY reckey, species_code

), pre_calc AS (
SELECT a.reckey, a.species_code, a.est_wgt_g, a.samplesize_m2,
       b.adjust_airdrywgt, b.adjust_utilization, b.adjust_growth, b.adjust_climate, 
	   coalesce(b.adjust_clip, 1) adjust_clip
  FROM base_sum a
  LEFT JOIN species0 b ON a.reckey = b.reckey AND a.species_code = b.species_code

), calc AS ( 
-- 1g/m2 = 10 kg/ha
SELECT reckey, species_code, 
       (est_wgt_g * adjust_airdrywgt * adjust_clip * 10)/
	   (samplesize_m2 * adjust_utilization * adjust_growth * adjust_climate) prod_kgha
  FROM pre_calc
)
SELECT * FROM calc;