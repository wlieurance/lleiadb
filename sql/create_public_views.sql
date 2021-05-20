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
-- plantdensity_subplot
--
DROP MATERIALIZED VIEW IF EXISTS public.plantdensity_subplot CASCADE;
CREATE MATERIALIZED VIEW public.plantdensity_subplot AS
WITH lmf_lower AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", 'SE7') reckey, 'plot_low' subid, 1 subplot, 
       1641.6::numeric(7,2) subsize_m2
  FROM lmf."PLANTCENSUS"
  GROUP BY reckey, subid, subplot, subsize_m2
    
), lmf_upper AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", 'SE7') reckey, 'plot_high' subid, 2 subplot, 
       1641.6::numeric(7,2) subsize_m2
  FROM lmf."PLANTCENSUS"
  GROUP BY reckey, subid, subplot, subsize_m2

), lmf_final AS (
SELECT * FROM lmf_lower
 UNION ALL
SELECT * FROM lmf_upper

), dima_process0 AS (
SELECT "RecKey" reckey,
       "Quadrat" || '_' || CAST("SubQuadSize" AS NUMERIC(7,2)) || '_' || CASE WHEN "SubQuadSizeUOM" = '2' THEN 'ft' ELSE 'm' END subid, 
       "Quadrat" subplot, "SpeciesCode" species_code,
       CAST(CASE WHEN "SubQuadSizeUOM" = '2' THEN "SubQuadSize" / 10.764 ELSE "SubQuadSize" END AS NUMERIC(7,2)) subsize_m2,  
       "Class1total" + "Class2total" + "Class3total" + "Class4total" + "Class5total" + 
       "Class6total" + "Class7total" + "Class8total" + "Class9total" class_total
  FROM dima."tblPlantDenDetail"
  WHERE "SubQuadSize" > 0   

), dima_final AS (
SELECT reckey, subid, subplot, subsize_m2
  FROM dima_process0
 GROUP BY reckey, subid, subplot, subsize_m2
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.plantdensity_subplot;



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
       'plot_low' subid,
       a."CPLANT" species_code, 1 class_no, b.cnt_lower total
  FROM lmf."PLANTCENSUS" a
  LEFT JOIN lmf_density b ON a."DENSITY" = b.code

), lmf_upper AS (
SELECT concat(a."SURVEY", a."STATE", a."COUNTY", a."PSU", a."POINT", 'SE7') reckey, 
       'plot_high' subid,
       a."CPLANT" species_code, 1 class_no, b.cnt_upper total
  FROM lmf."PLANTCENSUS" a
  LEFT JOIN lmf_density b ON a."DENSITY" = b.code

), lmf_final AS (
SELECT * FROM lmf_lower
 UNION ALL
SELECT * FROM lmf_upper

), dima_process0 AS (
SELECT "RecKey" reckey, 
       "Quadrat" || '_' || CAST("SubQuadSize" AS NUMERIC(7,2)) || '_' || CASE WHEN "SubQuadSizeUOM" = '2' THEN 'ft' ELSE 'm' END subid, 
       "SpeciesCode" species_code, 
       unnest(array[1, 2, 3, 4, 5, 6, 7, 8, 9]) class_no,
       unnest(array["Class1total", "Class2total", "Class3total", "Class4total", "Class5total", "Class6total", "Class7total", "Class8total", "Class9total"]) total, 
	   unnest(array["Class1density", "Class2density", "Class3density", "Class4density", "Class5density", "Class6density", "Class7density", "Class8density", "Class9density"]) density
  FROM dima."tblPlantDenDetail"

), dima_final AS (
SELECT reckey, subid, species_code, class_no, total 
 FROM dima_process0
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final
 UNION ALL
SELECT * FROM eco.plantdensity;

--
-- gap_meta
--
DROP MATERIALIZED VIEW IF EXISTS public.gap_meta CASCADE;
CREATE MATERIALIZED VIEW public.gap_meta AS
WITH lmf_gap_info AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", 'SW') linekey, 
       NULLIF("BASAL_GAPS_NESW", '') basal_gaps, NULLIF("CANOPY_GAPS_NESW", '') canopy_gaps,  
       NULLIF("PERENNIAL_CANOPY_GAPS_NESW", '') perennial_canopy_gaps, NULLIF("GAPS_DIFFERENT_NESW", '') gaps_different 
  FROM lmf."POINT"
 UNION ALL
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", 'SE') linekey,   
       NULLIF("BASAL_GAPS_NWSE", '') basal_gaps, NULLIF("CANOPY_GAPS_NWSE", '') canopy_gaps, 
       NULLIF("PERENNIAL_CANOPY_GAPS_NWSE", '') perennial_canopy_gaps, NULLIF("GAPS_DIFFERENT_NWSE", '') gaps_different
  FROM lmf."POINT"

), lmf_gap_info_final AS (
SELECT linekey, basal_gaps = 'Y' basal_gaps, canopy_gaps = 'Y' canopy_gaps,
       perennial_canopy_gaps = 'Y' perennial_canopy_gaps, gaps_different = 'Y' gaps_different
  FROM lmf_gap_info

), lmf_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT") plotkey,
	   concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2))) linekey,
       concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2)), '3') reckey,
	   "GAP_TYPE" gap_type
  FROM lmf."GINTERCEPT"
 GROUP BY plotkey, linekey, reckey, gap_type

), lmf_process1 AS (
SELECT plotkey, linekey, reckey, string_agg(gap_type, ';' ORDER BY gap_type) gap_types
  FROM lmf_process0
 GROUP BY plotkey, linekey, reckey
   
), lmf_final AS (
SELECT a.reckey,	   
       b."CAPDATE" AT TIME ZONE 'UTC' survey_date, NULL observer,
	   30::double precision gapmin_cm, a.gap_types,
	   CASE WHEN substring(gap_types FROM 'canopy') IS NOT NULL THEN 'perennial;annual'  
			ELSE 'perennial' END canopy_stops_gap,
       CASE WHEN substring(gap_types FROM 'canopy') IS NULL THEN NULL
            ELSE NOT c.canopy_gaps END canopy_no_gap, 
       CASE WHEN substring(gap_types FROM 'basal') IS NOT NULL THEN 'perennial'
            ELSE NULL END basal_stops_gap, 
       CASE WHEN substring(gap_types FROM 'basal') IS NULL THEN NULL
            ELSE NOT c.basal_gaps END basal_no_gap, 
       NULL notes, a.linekey
  FROM lmf_process1 a
  LEFT JOIN lmf."GPS" b ON a.plotkey = concat(b."SURVEY", b."STATE", b."COUNTY", b."PSU", b."POINT")
  LEFT JOIN lmf_gap_info_final c ON a.linekey = c.linekey

), dima_process0 AS (
SELECT a."LineKey" linekey, a."RecKey" reckey, 
       a."FormDate"::timestamp AT TIME ZONE c.tz survey_date, 
       a."Observer" observer,
	   CASE WHEN "Measure" = 2 THEN round("GapMin"::numeric * 30.48)
	        ELSE "GapMin" END gapmin_cm, 
	   CASE WHEN "GapData" = '1' THEN 'basal;canopy'
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
       CASE WHEN "GAP_TYPE" = 'canopy' THEN 'C' 
            WHEN "GAP_TYPE" = 'peren' THEN 'P'
            WHEN "GAP_TYPE" = 'basal' THEN 'B'
            ELSE NULL END AS rectype, 
       "SEQNUM" seqno, 
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
WITH code_lbls (lbl, code) AS (
VALUES
('Gravel', 'GR'),
('Cobble', 'CB'),
('Stone', 'ST'),
('Carbonate nodule', 'CAN'),
('Durinode', 'DNN')

), dima_convert AS (
SELECT "SoilKey" reckey,
       CASE WHEN "DepthMeasure" = 'cm' THEN cast(substring("HorizonDepthUpper", '\d+\.{0,1}\d*') AS double precision)
	        ELSE round(cast(substring("HorizonDepthUpper", '\d+\.{0,1}\d*') AS numeric) * 2.54, 1) END depth_upper_cm,
       "ESD_PctSand"/100 sand_pct, (100-("ESD_PctSand" + "ESD_PctClay")/100) silt_pct, "ESD_PctClay"/100 clay_pct,
       "ESD_PSAPctSand"/100 psa_sand_pct, "ESD_PSAPctSilt"/100 psa_silt_pct, "ESD_PSAPctClay"/100 psa_clay_pct,
       "ESD_SandFractPctVeryFine"/100 sand_pct_vf, "ESD_SandFractPctFine"/100 sand_pct_f, "ESD_SandFractPctMed"/100 sand_pct_m, 
       "ESD_SandFractPctCoarse"/100 sand_pct_c, "ESD_SandFractPctVeryCoarse"/100 sand_pct_vc,
       "ESD_FragmentType" fragtype1, "ESD_FragmentType2" fragtype2, "ESD_FragmentType3" fragtype3,
       cast(substring("ESD_FragVolPct", '\d+\.{0,1}\d*') as double precision)/100 fragvolpct1, 
       cast(substring("ESD_FragVolPct2", '\d+\.{0,1}\d*') as double precision)/100 fragvolpct2, 
       cast(substring("ESD_FragVolPct3", '\d+\.{0,1}\d*') as double precision)/100 fragvolpct3,
       "ESD_FragmentRoundness" frag_roundness, "ESD_GravelClassPctFine"/100 gr_pct_f, "ESD_GravelClassPctMed"/100 gr_pct_m, 
	   "ESD_GravelClassPctCoarse"/100 gr_pct_c, "ESD_LabGravelPctFine"/100 lab_gr_pct_f, "ESD_LabGravelPctMed"/100 lab_gr_pct_m, 
       "ESD_LabGravelPctCoarse"/100 lab_gr_pct_c
  FROM dima."tblSoilPitHorizons"

), dima_rn AS (
SELECT *, row_number() over(partition by reckey order by depth_upper_cm) seq_no
  FROM dima_convert

), dima_texture_unnest AS (
SELECT reckey, seq_no,
	   unnest(array['field', 'field', 'field', 'lab', 'lab', 'lab']) analysis,
	   unnest(array['S', 'SI', 'C', 'S', 'SI', 'C']) component,
       unnest(array[sand_pct, silt_pct, clay_pct, 
					psa_sand_pct, psa_silt_pct, psa_clay_pct]) vol_pct,
	   unnest(array[NULL, NULL, NULL, sand_pct_vf, NULL, NULL]) frac_vfine,
	   unnest(array[NULL, NULL, NULL, sand_pct_f, NULL, NULL]) frac_fine,
	   unnest(array[NULL, NULL, NULL, sand_pct_m, NULL, NULL]) frac_med,
	   unnest(array[NULL, NULL, NULL, sand_pct_c, NULL, NULL]) frac_coarse,
	   unnest(array[NULL, NULL, NULL, sand_pct_vc, NULL, NULL]) frac_vcoarse
  FROM dima_rn

), dima_texture_filter AS (
SELECT reckey, seq_no, analysis, component, vol_pct,
	   NULL frag_roundness, frac_vfine, frac_fine, frac_med, frac_coarse, frac_vcoarse
  FROM dima_texture_unnest
 WHERE coalesce(vol_pct, frac_vfine, frac_fine, frac_med, frac_coarse, frac_vcoarse) IS NOT NULL
	

), dima_frag_unnest AS (
SELECT reckey, seq_no,
       unnest(array[fragtype1, fragtype2, fragtype3]) texture_id, 
	   unnest(array[fragvolpct1, fragvolpct2, fragvolpct3]) vol_pct
  FROM dima_rn

), dima_frag_join AS (
SELECT a.reckey, a.seq_no, 'field' analysis, c.code component, a.vol_pct
  FROM dima_frag_unnest a
  LEFT JOIN dima."tblMaintESDFragmentTypes" b ON a.texture_id = b."Id"::varchar
  LEFT JOIN code_lbls c ON b."Abbrev" = c.lbl
  WHERE a.vol_pct IS NOT NULL AND a.texture_id IS NOT NULL AND b."Abbrev" != 'None'

), dima_frag_sum AS (
SELECT reckey, seq_no, analysis, component, sum(vol_pct) vol_pct
  FROM dima_frag_join
 GROUP BY reckey, seq_no, analysis, component
 
), dima_gravel_field AS (
SELECT reckey, seq_no, 'field' analysis, 'GR' component, frag_roundness,
	   NULL::double precision frac_vfine, gr_pct_f frac_fine, gr_pct_m frac_med, 
	   gr_pct_c frac_coarse, NULL::double precision frac_vcoarse
  FROM dima_rn
 WHERE coalesce(gr_pct_f, gr_pct_m, gr_pct_c) IS NOT NULL OR
       frag_roundness IS NOT NULL

), dima_frag_final AS ( 
SELECT a.reckey, a.seq_no, a.analysis, a.component, a.vol_pct,
       b.frag_roundness, b.frac_vfine, b.frac_fine, b.frac_med, b.frac_coarse, b.frac_vcoarse
  FROM dima_frag_sum a
  LEFT JOIN dima_gravel_field b ON a.reckey = b.reckey AND a.seq_no = b.seq_no 
        AND a.analysis = b.analysis AND a.component = b.component

), dima_labgravel AS (
SELECT reckey, seq_no, 'lab' analysis, 'GR' component,
	   NULL::double precision vol_pct, NULL frag_roundness,
       NULL::double precision frac_vfine, lab_gr_pct_f frac_fine, 
	   lab_gr_pct_m frac_med, lab_gr_pct_c frac_coarse,
	   NULL::double precision frac_vcoarse
  FROM dima_rn
 WHERE coalesce(lab_gr_pct_f, lab_gr_pct_m, lab_gr_pct_c) IS NOT NULL

), dima_final AS (
SELECT * FROM dima_texture_filter
 UNION ALL 
SELECT * FROM dima_frag_final
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

--
-- plant_mod
--
DROP VIEW IF EXISTS public.plant_mod CASCADE;
CREATE VIEW public.plant_mod AS 
WITH family_conv AS ( 
SELECT substring(upper(family), 1, 6) six_symbol, 'family' code_type, family
  FROM public.plant
 WHERE family IS NOT NULL

), family_grp AS (
SELECT six_symbol, code_type, family
  FROM family_conv
 GROUP BY six_symbol, code_type, family

), family_rn AS (
SELECT six_symbol, code_type, family, 
       row_number() over(partition by six_symbol order by family) AS rn
  FROM family_grp

), family_codes AS (
SELECT six_symbol,
       CASE WHEN rn = 1 THEN six_symbol ELSE six_symbol || rn END AS accepted_symbol,
       code_type, family
  FROM family_rn

), plant_fam AS (  
SELECT accepted_symbol, code_type, scientific_name, common_name, family, 
       duration, split_part(duration, ', ', 1) AS duration_first, 
       growth_habit, split_part(growth_habit, ', ', 1) AS growth_habit_first,
       native_status, hybrid_genus_indicator, genus, hybrid_species_indicator, species, 
       subspecies_prefix, hybrid_subspecies_indicator, subspecies, variety_prefix, hybrid_variety_indicator, 
       variety, forma_prefix, forma, genera_binomial_author, trinomial_author, quadranomial_author, parents, 
       state_and_province
  FROM public.plant
 UNION
SELECT accepted_symbol, code_type, NULL scientific_name, NULL common_name, family, NULL duration, NULL duration_first, 
       NULL growth_habit, NULL growth_habit_first, NULL native_status, NULL hybrid_genus_indicator, NULL genus, 
       NULL hybrid_species_indicator, NULL species, NULL subspecies_prefix, NULL hybrid_subspecies_indicator, 
       NULL subspecies, NULL variety_prefix, NULL hybrid_variety_indicator, NULL variety, NULL forma_prefix, 
       NULL forma, NULL genera_binomial_author, NULL trinomial_author, NULL quadranomial_author, NULL parents, 
       NULL state_and_province
  FROM family_codes
)
SELECT * FROM plant_fam;
COMMENT ON VIEW public.plant_mod IS 'creates a version of the ''plant'' table with first growth habit and duration extracted and family codes added';

--
-- plant_discrepencies
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


--
-- method_species_regex
--
DROP VIEW IF EXISTS public.method_species_regex CASCADE;
CREATE VIEW public.method_species_regex AS
WITH family_genus_unk AS (
SELECT *
  FROM plant_mod
 WHERE code_type IN ('genus', 'family', 'unknown')

), method_codes AS (
SELECT species_code FROM public.plantcensus GROUP BY species_code
 UNION
SELECT hit species_code FROM public.pintercept GROUP BY hit
 UNION
SELECT species_code FROM public.plantdensity GROUP BY species_code
 UNION
SELECT species_code FROM public.production_species GROUP BY species_code

), unk_code_list As (
SELECT a.species_code
  FROM method_codes a
  LEFT JOIN public.plant_mod b ON a.species_code = b.accepted_symbol
 WHERE b.accepted_symbol IS NULL

), code_reg AS (
SELECT species_code, 
       regexp_match(species_code, '^([a-zA-Z][a-zA-Z\d]{2,})\s*(?<!AA|PP)(AF|PF|AG|PG|SH|TR|SU)$') unk_genus,
       regexp_match(species_code, '^(AA|PP)(SU|SH|TR|GG|FF)$') unk_generic,
       regexp_match(species_code, '^(AF|PF|AG|PG|SH|TR|SU)\s*(\d+|XX)$') unk_aim,
       regexp_match(species_code, '^(2[a-zA-Z]+)(\d+)$') unk_usda
  FROM unk_code_list

), code_reg2 AS (
SELECT species_code, unk_genus, unk_generic, unk_aim, unk_usda, 
       coalesce(unk_genus[1], unk_usda[1]) code_base,
       coalesce(unk_aim[2]::integer, unk_usda[2]::integer) unk_no,
       coalesce(unk_genus[2], unk_aim[1]) gh_code
  FROM code_reg

), code_reg3 AS (
SELECT a.species_code, a.code_base, a.unk_no,
       CASE WHEN a.gh_code IN ('AF', 'AG') OR a.unk_generic[1] = 'AA' THEN 'Annual'
            WHEN a.gh_code IN ('PF', 'PG', 'SH', 'TR') OR a.unk_generic[1] = 'PP' THEN 'Perennial'
            ELSE NULL END duration,
       CASE WHEN a.gh_code IN ('AF', 'PF') THEN 'Forb/herb'
            WHEN a.gh_code IN ('AG', 'PG') THEN 'Graminoid'
            WHEN a.gh_code = 'SH' OR a.unk_generic[2] = 'SH' THEN 'Shrub'
            WHEN a.gh_code = 'TR' OR a.unk_generic[2] = 'TR' THEN 'Tree'
            ELSE NULL END growth_habit
  FROM code_reg2 a

), code_reg4 AS (
SELECT a.species_code, a.code_base, a.unk_no, 
       coalesce(a.duration, b.duration) duration_code, 
       coalesce(a.growth_habit, b.growth_habit) growth_habit_code,
       b.*
  FROM code_reg3 a
  LEFT JOIN family_genus_unk b ON a.code_base = b.accepted_symbol
)
SELECT * FROM code_reg4 WHERE coalesce(accepted_symbol, unk_no::varchar, duration, growth_habit) IS NOT NULL;
COMMENT ON VIEW public.method_species_regex IS 'parses method species codes that are unmatched in the plant table given known patterns.';


--
-- plant_regex
--
DROP VIEW IF EXISTS public.plant_regex CASCADE;
CREATE VIEW public.plant_regex AS
SELECT accepted_symbol, code_type, scientific_name, common_name, family, duration, duration_first, growth_habit, growth_habit_first, 
       native_status, hybrid_genus_indicator, genus, hybrid_species_indicator, species, subspecies_prefix, hybrid_subspecies_indicator, 
       subspecies, variety_prefix, hybrid_variety_indicator, variety, forma_prefix, forma, genera_binomial_author, trinomial_author, 
       quadranomial_author, parents, state_and_province
  FROM public.plant_mod
 UNION ALL
SELECT species_code accepted_symbol, 'parsed' code_type, scientific_name, common_name, family, 
       duration_code duration, split_part(duration_code, ', ', 1) AS duration_first, 
       growth_habit_code growth_habit, split_part(growth_habit_code, ', ', 1) growth_habit_first, 
       native_status, hybrid_genus_indicator, genus, hybrid_species_indicator, species, subspecies_prefix, hybrid_subspecies_indicator, 
       subspecies, variety_prefix, hybrid_variety_indicator, variety, forma_prefix, forma, genera_binomial_author, trinomial_author, 
       quadranomial_author, parents, state_and_province 
  FROM method_species_regex;
COMMENT ON VIEW public.plant_regex IS 'unions plant_mod and method_species_regex to use as a modified plant table';


--
-- pintercept
--
DROP VIEW IF EXISTS public.pintercept_plot CASCADE;
CREATE VIEW public.pintercept_plot AS
WITH plot_indicator AS (
-- creates a unique list if indicators by plot, year, and hit type
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, c.hit, c.hit_type
  FROM public.transect AS a
 INNER JOIN public.pintercept_meta AS b ON a.linekey = b.linekey
 INNER JOIN public.pintercept AS c ON b.reckey = c.reckey
 WHERE c.hit NOT IN ('None', 'N')
 GROUP BY a.plotkey, survey_year, c.hit, c.hit_type
    
), year_recs AS (
-- creates a unique list of rec keys for each plot/year
SELECT b.plotkey, a.reckey, date_part('year', a.survey_date) survey_year
  FROM public.pintercept_meta a
 INNER JOIN public.transect b ON a.linekey = b.linekey

), rec_indicators AS (
-- creates a list of indicators that is the same for every record entered in the same survey year 
SELECT a.*, b.reckey
  FROM plot_indicator a
 INNER JOIN year_recs b ON a.plotkey = b.plotkey AND a.survey_year = b.survey_year
  
-- calculates indicators at the mark level
), mark_grp AS (
SELECT reckey, mark, hit, hit_type, avg(height_cm::float) height_cm, bool_and(dead) dead
  FROM public.pintercept
 WHERE hit NOT IN ('None', 'N')
 GROUP BY reckey, mark, hit, hit_type

), rec_grp AS (
-- calculates indicators at the method record level
SELECT reckey, hit, hit_type, count(mark) mark_n, avg(height_cm) height_cm, sum(cast(dead AS integer)) dead_n 
  FROM mark_grp
 GROUP BY reckey, hit, hit_type

), good_marks AS (
-- creates a list of marks for method record that are valid (no NULLS in top canopy or soil surface)
SELECT reckey, mark, count(hit) n
  FROM public.pintercept 
 WHERE hit_type = 'l' AND hit_sub IN ('t', 's') AND hit IS NOT NULL
 GROUP BY reckey, mark
HAVING count(hit) = 2

), total_marks AS (
-- counts the total number of valid marks for each method record for use in calculating hit percentages 
SELECT reckey, count(mark) total_mark, 'l' hit_type
  FROM good_marks
 GROUP BY reckey
ORDER BY total_mark

), rec_joined AS (
-- fills in the indicator list with counts and fills mark_n NULLS with zeros where appropriate
SELECT a.*, b.total_mark, 
       case when c.mark_n IS NULL AND a.hit_type = 'l' then 0 else c.mark_n end mark_n, 
       c.height_cm, c.dead_n
  FROM rec_indicators a
  LEFT JOIN total_marks b ON a.reckey = b.reckey AND a.hit_type = b.hit_type
  LEFT JOIN rec_grp c ON a.reckey = c.reckey AND a.hit = c.hit AND a.hit_type = c.hit_type

), rec_calc AS (
-- calculates indicators at the method record level
SELECT plotkey, survey_year, hit, hit_type, reckey, total_mark, mark_n, height_cm, dead_n,
       mark_n::float/total_mark hit_pct, dead_n::float/mark_n dead_pct
  FROM rec_joined

), plt_grp AS (
-- averages/counts indicators at the plot/year level 
SELECT plotkey, survey_year, hit, hit_type, 
       count(height_cm) height_n, avg(height_cm) height_cm, stddev(height_cm) height_sd,
       count(hit_pct) hit_n, avg(hit_pct) hit_pct, stddev(hit_pct) hit_pct_sd,
       count(dead_pct) dead_n, avg(dead_pct) dead_pct, stddev(dead_pct) dead_pct_sd
  FROM rec_calc
 GROUP BY plotkey, survey_year, hit, hit_type

), pi_final AS (
-- joins calculations to the plant table for additional indicator data
SELECT a.plotkey, a.survey_year, a.hit, 
       b.scientific_name, b.common_name, b.duration_first duration, b.growth_habit_first growth_habit,
       case when a.hit_type = 'g' then 'growth habit'
            when a.hit_type = 'l' then 'canopy'
            else NULL end hit_type, 
       a.hit_n, a.hit_pct, a.hit_pct_sd,
       a.dead_n, a.dead_pct, a.dead_pct_sd,
       a.height_n, a.height_cm, a.height_sd
  FROM plt_grp a
  LEFT JOIN public.plant_regex b ON a.hit = b.accepted_symbol
)

SELECT * FROM pi_final;
COMMENT ON VIEW public.pintercept_plot  IS 'summarizes basic LPI heights and cover by plot/species.';

--
-- gap
--
DROP VIEW IF EXISTS public.gap_plot CASCADE;
CREATE VIEW public.gap_plot AS
WITH cat_cross AS (
--creates a crosstab query for formating long to wide data
SELECT * FROM crosstab(
$$
WITH breaks (gap_min, gap_max, cat_order, cat_lbl, cat_lbl2) AS (VALUES
--creates a table of break sizes for later joining
(0, 20, 1, 'pct000_020cm', '0-20'),
(20, 50, 2, 'pct021_050cm', '21-50'),
(50, 100, 3, 'pct051_100cm', '51-100'),
(100, 200, 4, 'pct101_200cm', '101-200'),
(200, 100000, 5, 'pct200cm_plus', '>200')

), gap_calc AS (
-- calculates gap sizes
SELECT reckey, rectype, seqno, gap_start_cm, gap_end_cm, abs(gap_end_cm - gap_start_cm) gap_cm
    FROM public.gap

), gap_cat AS (
-- puts gaps into category classes
SELECT a.reckey, a.rectype, a.gap_cm, b.cat_lbl, b.cat_order
  FROM gap_calc a
  LEFT JOIN breaks b ON a.gap_cm > b.gap_min AND a.gap_cm <= b.gap_max
  WHERE a.gap_cm > 0 AND a.gap_cm IS NOT NULL

), gap_cat_sum AS (
-- sums gap by category class
SELECT reckey, rectype, cat_lbl, cat_order,
       sum(gap_cm) gap_cm_sum
  FROM gap_cat
 GROUP BY reckey, rectype, cat_lbl, cat_order

), gap_sum AS (
-- sums gap without category class
SELECT reckey, rectype,
       sum(gap_cm) gap_cm_total
  FROM gap_cat
 GROUP BY reckey, rectype

), gap_plot_header AS (
-- creates a header that has all rectypes for every plot/year
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, c.rectype
  FROM transect a
  INNER JOIN public.gap_meta b ON a.linekey = b.linekey
  INNER JOIN public.gap c ON b.reckey = c.reckey
 GROUP BY a.plotkey, survey_year, c.rectype

), gap_rec_header AS (
--creates a line/rec header for joining gap data to plot header
SELECT a.plotkey, a.linekey, b.reckey, date_part('year', b.survey_date) survey_year
  FROM transect a
  INNER JOIN public.gap_meta b ON a.linekey = b.linekey

), joined_header AS (
-- creates header that is a full list of all plots, years, rectypes, and gap classes
-- for joining to gap data.  This is necessary to fill in zeros for lines that have no
-- gaps for specific lines/classes
SELECT a.*, b.linekey, b.reckey, c.cat_lbl, c.cat_order
  FROM gap_plot_header a
 INNER JOIN gap_rec_header b ON a.plotkey = b.plotkey AND a.survey_year = b.survey_year
 CROSS JOIN breaks c

), gap_cat_joined AS (
-- joins header and fills in nulls with zeros
SELECT a.*, coalesce(b.gap_cm_sum, 0) gap_cm_sum
  FROM joined_header a
  LEFT JOIN gap_cat_sum b ON a.reckey = b.reckey AND a.rectype = b.rectype AND a.cat_lbl = b.cat_lbl
    
), gap_cat_pct AS (
-- calculates gap percent for each gap class
SELECT a.*, b.gap_cm_total, a.gap_cm_sum/b.gap_cm_total gap_pct
  FROM gap_cat_joined a
  INNER JOIN gap_sum b ON a.reckey = b.reckey AND a.rectype = b.rectype

), plot_avg AS (
-- summarizes gap percent by plot/year
SELECT plotkey, survey_year, rectype, cat_lbl, cat_order, 
       count(reckey) rec_n, avg(gap_pct) gap_pct_mean, stddev(gap_pct) gap_pct_sd 
  FROM gap_cat_pct
 GROUP BY plotkey, survey_year, rectype, cat_lbl, cat_order
)

SELECT array[plotkey, survey_year::text, rectype] id, cat_lbl, 
       cast(gap_pct_mean AS NUMERIC(5,4)) gap_pct_mean
--final select query in crosstab format
  FROM plot_avg
 ORDER BY id, cat_order
$$
) AS final_result(id TEXT[], pct000_020cm numeric, pct021_050cm numeric, 
                  pct051_100cm numeric, pct101_200cm numeric, 
                  pct200cm_plus numeric)

), cat_class_final AS (
SELECT id[1] plotkey, id[2]::integer survey_year, id[3] rectype, 
       pct000_020cm, pct021_050cm, pct051_100cm, pct101_200cm, pct200cm_plus
  FROM cat_cross

), gap_calc2 AS (
-- calculates gap sizes (again, outside of the crosstab string)
SELECT reckey, rectype, seqno, gap_start_cm, gap_end_cm, abs(gap_end_cm - gap_start_cm) gap_cm
    FROM public.gap

), gap_avg AS (
-- sums/avgs gap for each method record
SELECT reckey, rectype, count(gap_cm) gap_n,
       sum(gap_cm) gap_cm_total, avg(gap_cm) gap_cm_avg, stddev(gap_cm) gap_cm_sd
  FROM gap_calc2
 GROUP BY reckey, rectype

), transect_join AS (
--joins up gap calulations to relevant transect info in prep for summarization
SELECT a.plotkey, a.linekey, date_part('year', b.survey_date) survey_year,
       CASE WHEN a.transect_units = 'ft' THEN round(a.transect_length::numeric * 30.48, 0)
            ELSE round(a.transect_length::numeric * 100, 0) END transect_cm,
       c.reckey, c.rectype, c.gap_n, c.gap_cm_total, c.gap_cm_avg, c.gap_cm_sd
  FROM transect a
 INNER JOIN public.gap_meta b ON a.linekey = b.linekey
 INNER JOIN gap_avg c ON b.reckey = c.reckey

), plot_avg2 AS (
-- calculates plot means/sds for number of gaps (gap_n), gap size (gap_cm), and 
-- percent of line that is gap (gap_pct)
SELECT plotkey, survey_year, rectype, count(reckey) rec_n, avg(gap_n) gap_n_mean,
       stddev(gap_n) gap_n_sd, avg(gap_cm_avg) gap_cm_mean, stddev(gap_cm_avg) gap_cm_sd,
       avg(gap_cm_total/transect_cm) gap_pct_mean, stddev(gap_cm_total/transect_cm) gap_pct_sd
  FROM transect_join
 GROUP BY plotkey, survey_year, rectype

), final_calcs AS (
SELECT a.plotkey, a.survey_year, a.rectype,
       CASE WHEN a.rectype = 'C' THEN 'canopy'
            WHEN a.rectype = 'B' THEN 'basal'
            WHEN a.rectype = 'P' THEN 'perennial canopy'
            ELSE NULL END reclbl,
       b.rec_n, b.gap_n_mean::numeric(4,1), b.gap_n_sd::numeric(4,1), b.gap_cm_mean::numeric(5,1), b.gap_cm_sd::numeric(5,1), 
       b.gap_pct_mean::numeric(5,4), b.gap_pct_sd::numeric(5,4),
       a.pct000_020cm, a.pct021_050cm, a.pct051_100cm, a.pct101_200cm, a.pct200cm_plus
  FROM cat_class_final a
  LEFT JOIN plot_avg2 b ON a.plotkey = b.plotkey AND a.survey_year = b.survey_year AND a.rectype = b.rectype
)

SELECT * FROM final_calcs;
COMMENT ON VIEW public.gap_plot  IS 'summarizes basic gap intecept data by plot, measure type and gap class.';

--
-- plantcensus
--
DROP VIEW IF EXISTS public.plantcensus_plot CASCADE;
CREATE VIEW public.plantcensus_plot AS
WITH sr_plot AS (
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, 
       c.species_code, count(b.reckey) rec_n, string_agg(c.notes, '; ') notes
  FROM public.point a
  INNER JOIN public.plantcensus_meta b ON a.plotkey = b.plotkey
  INNER JOIN public.plantcensus c ON b.reckey = c.reckey
  GROUP BY a.plotkey, survey_year, c.species_code

), joined AS (
SELECT a.plotkey, a.survey_year, a.species_code, a.rec_n, a.notes,
       b.duration_first duration, b.growth_habit_first growth_habit,
       b.code_type, b.scientific_name, b.common_name, b.family
  FROM sr_plot a
  LEFT JOIN public.plant_regex b ON a.species_code = b.accepted_symbol
)

SELECT * FROM joined;
COMMENT ON VIEW public.plantcensus_plot  IS 'summarizes basic plant census data by plot and joins plant info.';

--
-- plantdensity
--
DROP VIEW IF EXISTS public.plantdensity_plot CASCADE;
CREATE VIEW public.plantdensity_plot AS
WITH plot_species AS (
-- creates a unique list of all species found in a plot for later joining
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, c.species_code
  FROM transect a
 INNER JOIN plantdensity_meta b ON a.linekey = b.linekey
 INNER JOIN plantdensity c ON b.reckey = c.reckey
 GROUP BY a.plotkey, survey_year, c.species_code

), plot_classes AS (
-- creates a unique list of all classes found in a plot for later joining
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, c.class_no, c.class_lbl
  FROM transect a
 INNER JOIN plantdensity_meta b ON a.linekey = b.linekey
 INNER JOIN plantdensity_class c ON b.reckey = c.reckey
 GROUP BY a.plotkey, survey_year, c.class_no, c.class_lbl

), plot_recs AS (
-- creates a unique list of all reckeys found in a plot for later joining
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, b.reckey
  FROM transect a
 INNER JOIN plantdensity_meta b ON a.linekey = b.linekey
  
), plot_header AS (
-- joins species, classes, and reckeys to form a header with which to join summarized data
-- this allows for the filling in of nulls as zeros before summing by plot in order
-- in order to have accurate averages.
SELECT a.plotkey, a.survey_year, c.reckey, b.class_no, b.class_lbl, a.species_code
  FROM plot_species a
  INNER JOIN plot_classes b ON a.plotkey = b.plotkey AND a.survey_year = b.survey_year
  INNER JOIN plot_recs c ON a.plotkey = c.plotkey AND a.survey_year = c.survey_year

), subplot_density AS (
-- calculates density in plants/heactares at the method record/species/class scale.
SELECT a.reckey, a.subid, b.subplot, a.species_code, a.class_no, a.total, b.subsize_m2, 
       cast(coalesce(a.total, 0)/b.subsize_m2*10000 AS double precision) density_ha
  FROM public.plantdensity a
 INNER JOIN public.plantdensity_subplot b ON  a.reckey = b.reckey AND a.subid = b.subid
 WHERE b.subsize_m2 > 0

), species_searched AS (
-- creates a unique list of species for each suplot size, in order to fill in nulls
-- for missing species (there shouldn't be any but just in case).
SELECT reckey, species_code, subsize_m2 
  FROM subplot_density
 GROUP BY reckey, species_code, subsize_m2
 
), rec_header_subs AS ( 
-- creates a complete list of reckeys and species for joining completion
SELECT a.reckey, a.species_code, a.subsize_m2, b.subid, b.subplot
  FROM species_searched a
 INNER JOIN public.plantdensity_subplot b ON a.reckey = b.reckey AND a.subsize_m2 = b.subsize_m2

), rec_header AS (
-- creates a complete list of reckeys, species, and subplot ids for joining completion
SELECT a.reckey, a.species_code, a.subsize_m2, a.subid, a.subplot,
       b.class_no, b.class_lbl
  FROM rec_header_subs a
 INNER JOIN public.plantdensity_class b ON a.reckey = b.reckey

), rec_filled AS (
-- creates a complete method record (reckey) header for joining completion
-- filling in the header and joining like this shouldn't be necessary, but for safety we do it anyway
SELECT a.*, coalesce(b.density_ha, 0) density_ha 
  FROM rec_header a
  LEFT JOIN subplot_density b ON a.reckey = b.reckey 
        AND a.subid = b.subid AND a.species_code = b.species_code 
        AND a.class_no = b.class_no
 
), rec_grouped AS (
-- calculates counts, means, and std. devs for reckeys, classes by summarizing over subplots
SELECT reckey, species_code, class_no, class_lbl, count(subid) sub_n,
       avg(density_ha) density_ha_mean, stddev(density_ha) density_ha_sd
  FROM rec_filled
 GROUP BY reckey, species_code, class_no, class_lbl

), plot_header_filled AS (
-- joins the plot header to the summarized over subplot data and fills in nulls
-- with zeros for count and mean
SELECT a.*, coalesce(b.sub_n, 0) sub_n, coalesce(density_ha_mean, 0) density_ha_mean,
       density_ha_sd
  FROM plot_header a
  LEFT JOIN rec_grouped b ON a.reckey = b.reckey
        AND a.class_no = b.class_no AND a.species_code = b.species_code

), rec_plot_grouped AS (
-- summarizes density at the plot/year/class/species level
SELECT plotkey, survey_year, class_no, class_lbl, species_code,
       count(reckey) rec_n, avg(density_ha_mean) density_ha_mean, 
       stddev(density_ha_mean) density_ha_sd
  FROM plot_header_filled
 GROUP BY plotkey, survey_year, class_no, class_lbl, species_code

), plot_joined AS (
-- joins plot summary with plant level data
SELECT a.plotkey, a.survey_year, a.class_no, a.species_code,
       b.scientific_name, b.common_name, b.family, a.rec_n, 
       a.density_ha_mean, a.density_ha_sd
  FROM rec_plot_grouped a
  LEFT JOIN public.plant_regex b ON a.species_code = b.accepted_symbol
)

SELECT * FROM plot_joined;
COMMENT ON VIEW public.plantdensity_plot  IS 'summarizes plant density in plants/hectare at the plot/class/species level.';

--
-- production
--
DROP VIEW IF EXISTS public.production_plot CASCADE;
CREATE VIEW public.production_plot AS
WITH clip_sum AS (
-- sum up estimated weight and clipped weight for those records that have it to use
-- in calculating clip correction factor
SELECT b.plotkey, date_part('year', survey_date) survey_year, a.species_code, 
       sum(a.units * c.unit_wgt_g) estimated_g, sum(a.clipped_wgt_g) clipped_wgt_g
  FROM public.production a
 INNER JOIN public.production_meta b ON a.reckey = b.reckey
 INNER JOIN public.production_species c ON a.reckey = c.reckey AND a.species_code = c.species_code
 WHERE clipped_wgt_g IS NOT NULL
 GROUP BY b.plotkey, survey_year, a.species_code

), adjust_clip AS (
-- calculate clipped weight correction factor by reckey/species
SELECT a.plotkey, a.survey_year, b.reckey, a.species_code, a.estimated_g, a.clipped_wgt_g,
       a.clipped_wgt_g/(CASE WHEN a.estimated_g = 0 THEN a.clipped_wgt_g ELSE a.estimated_g END) adjust_clip
  FROM clip_sum a
 INNER JOIN public.production_meta b ON a.plotkey = b.plotkey

), production_species_clip AS (
-- join clipped correction factor back up to other correction factors to await processing
SELECT a.reckey, a.species_code, a.subsize_m2, a.unit_wgt_g, a.adjust_airdrywgt, a.adjust_utilization, 
       a.adjust_growth, a.adjust_climate, coalesce(b.adjust_clip, 1) adjust_clip
  FROM public.production_species a
  LEFT JOIN adjust_clip b ON a.reckey = b.reckey AND a.species_code = b.species_code

), rec_header AS (
-- create a full list of every species for every reckey/subplot just in case
SELECT a.reckey, b.subplot, a.species_code
  FROM  production_species_clip a
 INNER JOIN public.production_subplot b ON a.reckey = b.reckey
 WHERE b.not_sampled IS FALSE

), rec_header_filled AS (
-- join the rec header to data and fill in nulls as 0s
SELECT a.*, 
       coalesce(CASE WHEN b.units = 0 AND b.clipped_wgt_g IS NOT NULL THEN b.clipped_wgt_g 
            ELSE b.units END, 0) units
  FROM rec_header a
  LEFT JOIN public.production b ON a.reckey = b.reckey AND a.subplot = b.subplot AND a.species_code = b.species_code

), rec_species_mean AS (
-- summarize over method records (reckey) and species
SELECT reckey, species_code, count(subplot) sub_n, avg(units) unit_mean, stddev(units) unit_sd 
  FROM rec_header_filled
 GROUP BY reckey, species_code

), rec_species_calc AS (
-- calculate production in grams/square meter using the adjustment factors, unit weights, and subplot size
SELECT a.*, b.subsize_m2, b.unit_wgt_g, b.adjust_airdrywgt, b.adjust_utilization,
       b.adjust_growth, b.adjust_climate, b.adjust_clip,
       unit_mean * unit_wgt_g * (1/subsize_m2) * b.adjust_airdrywgt * b.adjust_clip * 
           (1/b.adjust_utilization) * (1/b.adjust_growth) * (1/b.adjust_climate) prod_g_m2,
       unit_sd * unit_wgt_g * (1/subsize_m2) * b.adjust_airdrywgt * b.adjust_clip * 
           (1/b.adjust_utilization) * (1/b.adjust_growth) * (1/b.adjust_climate) prod_sd
  FROM rec_species_mean a
 INNER JOIN production_species_clip b ON a.reckey = b.reckey AND a.species_code = b.species_code

), plot_species AS (
-- create a unique list of species for each plot/year
SELECT a.plotkey, date_part('year', a.survey_date) survey_year, b.species_code
  FROM public.production_meta a
 INNER JOIN public.production_species b ON a.reckey = b.reckey
 GROUP BY a.plotkey, survey_year, b.species_code

), plot_recs AS (
-- create a unique list of rec keys for each plot/year
SELECT a.plotkey, date_part('year', a.survey_date) survey_year, reckey
  FROM public.production_meta a

), plot_header AS (
-- create a header which has all species for all reckeys for each plot/year
SELECT a.plotkey, a.survey_year, a.reckey, b.species_code
  FROM plot_recs a
 INNER JOIN plot_species b ON a.plotkey = b.plotkey AND a.survey_year = b.survey_year

), plot_header_filled AS (
-- join the header to the actual calculations and fill in nulls as 0s
SELECT a.*, coalesce(b.sub_n, 0) sub_n, coalesce(b.prod_g_m2, 0) prod_g_m2, b.prod_sd
  FROM plot_header a
 LEFT JOIN rec_species_calc b ON a.reckey = b.reckey AND a.species_code = b.species_code

), plot_grouped AS (
-- summarize by plot/year/species
SELECT plotkey, survey_year, species_code, count(reckey) rec_n, avg(prod_g_m2) prod_g_m2,
       stddev(prod_g_m2) prod_sd
  FROM plot_header_filled
 GROUP BY plotkey, survey_year, species_code
    
), plot_joined AS (
-- join plot level summary to plant table for more info
SELECT a.plotkey, a.survey_year, a.species_code, b.scientific_name, b.common_name, b.family, 
       b.duration_first duration, b.growth_habit_first growth_habit,
       a.rec_n, a.prod_g_m2::numeric(6,2), a.prod_sd::numeric(6,2)
  FROM plot_grouped a
  LEFT JOIN public.plant_regex b ON a.species_code = b.accepted_symbol
  WHERE a.prod_g_m2 > 0
)
    
SELECT * FROM plot_joined;
COMMENT ON VIEW public.production_plot  IS 'Summarizes production in grams/square meter at the plot/species level.
 Calculations assume that the same types of veg. were recorded for each method record';

--
-- rangehealth
--
DROP VIEW IF EXISTS public.rangehealth_plot CASCADE;
CREATE VIEW public.rangehealth_plot AS
WITH ind_cross AS (
SELECT * FROM crosstab (
$$
SELECT a.reckey, a.rate_type || substring(a.seq_no::text from 1 for 1) || '_' || lower(a.rate_abbr), b."Code"::varchar(2)
  FROM public.rangehealth a
 INNER JOIN dima."tblMaintQualRatings" b ON a.rating = b."Rating" 
 ORDER BY a.reckey, a.seq_no
$$
) AS final_result(reckey text, i1_r varchar(2), i2_wfp varchar(2), i3_pt varchar(2), i4_bg varchar(2), i5_g varchar(2), 
                  i6_wsbda varchar(2), i7_lm varchar(2), i8_ssre varchar(2), i9_ssld varchar(2), i10_pccdrir varchar(2), 
                  i11_cl varchar(2), i12_fsg varchar(2), i13_pmd varchar(2), i14_la varchar(2), i15_ap varchar(2), i16_ip varchar(2), 
                  i17_rcpp varchar(2), a1_sss varchar(2), a2_hf varchar(2), a3_bi varchar(2))

), rh_final AS (
SELECT a.plotkey, a.survey_date, a.ecoid_std, b.* 
  FROM public.rangehealth_meta a
 INNER JOIN ind_cross b ON a.reckey = b.reckey
)

SELECT * FROM rh_final;
COMMENT ON VIEW public.rangehealth_plot  IS 'Summarizes rangeland health data by plot in crosstab format.';

--
-- shrubshape
--
DROP VIEW IF EXISTS public.shrubshape_plot CASCADE;
CREATE VIEW public.shrubshape_plot AS
WITH shape_codes (code, label) AS 
(VALUES
('C', 'columnar'),
('M', 'mixed'),
('S', 'spreading')

), shape_count AS (
SELECT reckey, coalesce(species_code, 'SHRUB') species_code, shape, count(mark) shape_n
  FROM public.shrubshape a
 GROUP BY reckey, species_code, shape

), species_count AS (
SELECT reckey, coalesce(species_code, 'SHRUB') species_code, count(mark) species_n
  FROM public.shrubshape a
 GROUP BY reckey, species_code

), shape_calc AS (
SELECT a.reckey, a.species_code, c.label shape, 
       a.shape_n::double precision/b.species_n::double precision shape_pct
  FROM shape_count a
 INNER JOIN species_count b ON a.reckey = b.reckey AND a.species_code = b.species_code
 LEFT JOIN shape_codes c ON a.shape = c.code

), rec_header AS (
SELECT a.*, b.label shape
  FROM species_count a
  CROSS JOIN shape_codes b
    
), rec_header_filled AS (
SELECT a.*, coalesce(b.shape_pct, 0) shape_pct
  FROM rec_header a
  LEFT JOIN shape_calc b ON a.reckey = b.reckey AND a.species_code = b.species_code
       AND a.shape = b.shape

), plot_group AS (
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, c.species_code, c.shape,
       count(c.reckey) rec_n, avg(shape_pct) shape_pct_mean, stddev(shape_pct) shape_pct_sd
  FROM transect a
 INNER JOIN pintercept_meta b ON a.linekey = b.linekey
 INNER JOIN rec_header_filled c ON b.reckey = c.reckey
 GROUP BY a.plotkey, survey_year, c.species_code, c.shape

), plot_species AS (
SELECT plotkey, survey_year, species_code
  FROM plot_group
 GROUP BY plotkey, survey_year, species_code

), plot_header AS (
SELECT a.*, b.label shape
  FROM plot_species a
 CROSS JOIN shape_codes b

), plot_header_filled AS (
SELECT a.*, coalesce(b.rec_n, 0) rec_n, coalesce(b.shape_pct_mean, 0)::numeric(4,3) shape_pct_mean, 
       b.shape_pct_sd::numeric(4,3)
  FROM plot_header a
  LEFT JOIN plot_group b ON a.plotkey = b.plotkey AND a.survey_year = b.survey_year 
       AND a.species_code = b.species_code AND a.shape = b.shape

), plot_joined AS (
SELECT a.plotkey, a.survey_year, a.shape, a.species_code, b.scientific_name, b.common_name, b.family, 
       b.duration_first duration, b.growth_habit_first growth_habit, a.rec_n, a.shape_pct_mean,
       a.shape_pct_sd
  FROM plot_header_filled a
  LEFT JOIN public.plant_regex b ON a.species_code = b.accepted_symbol
)

SELECT * FROM plot_joined;
COMMENT ON VIEW public.shrubshape_plot  IS 'Summarizes shrub shape data by plot.';

--
-- soilstability
--
DROP VIEW IF EXISTS public.soilstability_plot CASCADE;
CREATE VIEW public.soilstability_plot AS
WITH cell_sum AS (
SELECT b.plotkey, date_part('year', b.survey_date) survey_year, a.reckey, b.rectype, 
       coalesce(a.box_no, 1) box_no, a.veg, count(a.cell) cell_n, 
       avg(a.rating) rating_mean, stddev(a.rating) rating_sd
  FROM public.soilstability a
 INNER JOIN public.soilstability_meta b ON a.reckey = b.reckey
 WHERE a.rating IS NOT NULL
 GROUP BY b.plotkey, survey_year, a.reckey, b.rectype, coalesce(a.box_no, 1), a.veg

), box_sum AS (
SELECT plotkey, survey_year, reckey, rectype, veg, count(box_no) box_n, 
       sum(cell_n) cell_n_sum, avg(rating_mean) rating_mean, stddev(rating_mean) rating_sd
  FROM cell_sum
 GROUP BY plotkey, survey_year, reckey, rectype, veg

), plot_sum AS (
SELECT plotkey, survey_year, rectype, veg, count(reckey) rec_n,
       avg(rating_mean)::numeric(3, 2) rating_mean, stddev(rating_mean)::numeric(3, 2) rating_sd
  FROM box_sum
 GROUP BY plotkey, survey_year, rectype, veg
)

SELECT * FROM plot_sum;
COMMENT ON VIEW public.soilstability_plot  IS 'Summarizes soils stability data by plot, year, and veg.';


