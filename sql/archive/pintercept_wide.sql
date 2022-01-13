--
-- pintercept
--
DROP MATERIALIZED VIEW IF EXISTS public.pintercept CASCADE;
CREATE MATERIALIZED VIEW public.pintercept AS 
WITH lmf_final AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2)), '1') reckey,
       "MARK" mark,
       CASE WHEN trim("HIT1") = '' THEN NULL ELSE trim("HIT1") END hit1_top, 
	   CASE WHEN trim("HIT2") = '' THEN NULL ELSE trim("HIT2") END hit2_l, 
	   CASE WHEN trim("HIT3") = '' THEN NULL ELSE trim("HIT3") END hit3_l, 
	   CASE WHEN trim("HIT4") = '' THEN NULL ELSE trim("HIT4") END hit4_l, 
	   CASE WHEN trim("HIT5") = '' THEN NULL ELSE trim("HIT5") END hit5_l, 
	   CASE WHEN trim("HIT6") = '' THEN NULL ELSE trim("HIT6") END hit6_l,
	   NULL hit7_l, NULL hit8_l,  
	   CASE WHEN "BASAL" = 'None' AND "NONSOIL" = '' THEN 'S' 
	        WHEN "BASAL" = 'None' AND "NONSOIL" != '' THEN "NONSOIL"
			WHEN "NONSOIL" = 'W' THEN "NONSOIL" 
			WHEN trim("BASAL") = '' THEN NULL 
			ELSE "BASAL" END hit9_surf,
	   NULL::boolean hit1_chk, NULL::boolean hit2_chk, NULL::boolean hit3_chk, 
	   NULL::boolean hit4_chk, NULL::boolean hit5_chk, NULL::boolean hit6_chk, 
	   NULL::boolean hit7_chk, NULL::boolean hit8_chk, NULL::boolean hit9_chk,
	   CASE WHEN trim("SAGEBRUSH_SPP") = '' THEN NULL ELSE trim("SAGEBRUSH_SPP") END sagebrush_spp,
	   CASE WHEN "SAGEBRUSH_SHAPE" = 0 THEN NULL
	        WHEN "SAGEBRUSH_SHAPE" = 1 THEN 'C' 
			WHEN "SAGEBRUSH_SHAPE" = 2 THEN 'S' 
			WHEN "SAGEBRUSH_SHAPE" = 3 THEN 'M' 
			ELSE NULL END sagebrush_shape
  FROM lmf."PINTERCEPT"

), dima_nonchk (code) AS (
VALUES 
(''), ('BR'), ('BY'), ('CB'), ('CY'), ('D'), ('DS'), ('EL'), ('GR'), ('L'), 
('NL'), ('R'), ('S'), ('ST'), ('WA'), ('WL')

), pl AS ( 
SELECT accepted_symbol, code_type, scientific_name, common_name, family, 
       duration, split_part(duration, ', ', 1) AS duration_first, 
       growth_habit, split_part(growth_habit, ', ', 1) AS growth_habit_first, 
       native_status, hybrid_genus_indicator, genus, hybrid_species_indicator, species, subspecies_prefix, 
       hybrid_subspecies_indicator, subspecies, variety_prefix, hybrid_variety_indicator, variety, forma_prefix, 
       forma, genera_binomial_author, trinomial_author, quadranomial_author, parents, state_and_province 
  FROM public.plant

), sagebrush_codes AS (
SELECT accepted_symbol code
  FROM pl
  WHERE genus = 'Artemisia' AND growth_habit_first IN ('Tree', 'Shrub', 'SubShrub')

), dima_final AS (
SELECT "RecKey" reckey, "PointLoc" mark,
        CASE WHEN trim("TopCanopy") = '' THEN NULL ELSE trim("TopCanopy") END hit1_top,
		CASE WHEN trim("Lower1") = '' THEN NULL ELSE trim("Lower1") END hit2_l, 
		CASE WHEN trim("Lower2") = '' THEN NULL ELSE trim("Lower2") END hit3_l, 
		CASE WHEN trim("Lower3") = '' THEN NULL ELSE trim("Lower3") END hit4_l, 
		CASE WHEN trim("Lower4") = '' THEN NULL ELSE trim("Lower4") END hit5_l, 
		CASE WHEN trim("Lower5") = '' THEN NULL ELSE trim("Lower5") END hit6_l, 
		CASE WHEN trim("Lower6") = '' THEN NULL ELSE trim("Lower6") END hit7_l, 
		CASE WHEN trim("Lower7") = '' THEN NULL ELSE trim("Lower7") END hit8_l, 
		CASE WHEN trim("SoilSurface") = '' THEN NULL ELSE trim("SoilSurface") END hit9_surf,
		CASE WHEN "TopCanopy" = 'None' OR "TopCanopy" IS NULL THEN NULL ELSE "ChkboxTop" END hit1_chk,
		CASE WHEN "Lower1" IN (SELECT code FROM dima_nonchk) THEN NULL ELSE "ChkboxLower1" END hit2_chk,
		CASE WHEN "Lower2" IN (SELECT code FROM dima_nonchk) THEN NULL ELSE "ChkboxLower2" END hit3_chk,
		CASE WHEN "Lower3" IN (SELECT code FROM dima_nonchk) THEN NULL ELSE "ChkboxLower3" END hit4_chk,
		CASE WHEN "Lower4" IN (SELECT code FROM dima_nonchk) THEN NULL ELSE "ChkboxLower4" END hit5_chk,
		CASE WHEN "Lower5" IN (SELECT code FROM dima_nonchk) THEN NULL ELSE "ChkboxLower5" END hit6_chk,
		CASE WHEN "Lower6" IN (SELECT code FROM dima_nonchk) THEN NULL ELSE "ChkboxLower6" END hit7_chk,
		CASE WHEN "Lower7" IN (SELECT code FROM dima_nonchk) THEN NULL ELSE "ChkboxLower7" END hit8_chk,
		CASE WHEN "SoilSurface" IN (SELECT code FROM dima_nonchk) THEN NULL ELSE "ChkboxSoil" END hit8_chk,
		CASE WHEN "ShrubShape" = '' OR "ShrubShape" IS NULL THEN NULL 
		     WHEN "SpeciesWoody" IN (SELECT code FROM sagebrush_codes) THEN "SpeciesWoody"
		     WHEN "TopCanopy" IN (SELECT code FROM sagebrush_codes) THEN "TopCanopy"
			 WHEN "Lower1" IN (SELECT code FROM sagebrush_codes) THEN "Lower1"
			 WHEN "Lower2" IN (SELECT code FROM sagebrush_codes) THEN "Lower2"
			 WHEN "Lower3" IN (SELECT code FROM sagebrush_codes) THEN "Lower3"
			 WHEN "Lower4" IN (SELECT code FROM sagebrush_codes) THEN "Lower4"
			 WHEN "Lower5" IN (SELECT code FROM sagebrush_codes) THEN "Lower5"
			 WHEN "Lower6" IN (SELECT code FROM sagebrush_codes) THEN "Lower6"
			 WHEN "Lower7" IN (SELECT code FROM sagebrush_codes) THEN "Lower7"
			 WHEN "SoilSurface" IN (SELECT code FROM sagebrush_codes) THEN "SoilSurface"
			 ELSE NULL END sagebrush_spp,
		CASE WHEN "ShrubShape" = '' OR "ShrubShape" IS NULL THEN NULL
		     ELSE "ShrubShape" END sagebrush_shape
  FROM dima."tblLPIDetail"
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final;
			

--
-- pheight
--
DROP MATERIALIZED VIEW IF EXISTS public.pheight CASCADE;
CREATE MATERIALIZED VIEW public.pheight AS 
WITH lmf_process0 AS (
SELECT concat("SURVEY", "STATE", "COUNTY", "PSU", "POINT", upper(right("TRANSECT", 2)), '1') reckey,
       "DISTANCE" mark, 
	   CASE WHEN trim("HPLANT") = '' OR "HPLANT" IS NULL THEN NULL ELSE trim("HPLANT") END hplant, 
	   cast(substring("HEIGHT", '[\d\.]+') AS numeric(6, 1)) hheight,
	   substring("HEIGHT", '[^\d\s\.]+') hunits,
	   CASE WHEN trim("WPLANT") = '' OR "WPLANT" IS NULL THEN NULL ELSE trim("WPLANT") END wplant, 
	   cast(substring("WHEIGHT", '[\d\.]+') AS numeric(6, 1)) wheight,
	   substring("WHEIGHT", '[^\d\s\.]+') wunits
  FROM lmf."PASTUREHEIGHTS"

), lmf_process1 AS (
SELECT reckey, mark,
       unnest(ARRAY['growth habit', 'growth habit']) AS hit_type,
       unnest(ARRAY['herbaceous', 'woody']) AS hit_sub,
	   unnest(ARRAY[2, 1]) AS hit_order,
       unnest(ARRAY[hplant, wplant]) AS pcode,
       unnest(ARRAY[hheight, wheight]) AS height,
	   unnest(ARRAY[hunits, wunits]) AS units
  FROM lmf_process0

), lmf_final AS (
SELECT reckey, mark, hit_type, hit_sub, hit_order, pcode, 
       CASE WHEN units = 'in' THEN round(height * 2.54, 1)
	        WHEN units = 'ft' THEN round(height * 30.48, 1)
			ELSE height END height_cm,
	   NULL::boolean chk
  FROM lmf_process1

), dima_process0 AS (
SELECT "RecKey" reckey, "PointLoc" mark,
       unnest(ARRAY[1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2]) AS hit_order,
	   unnest(ARRAY['layer', 'layer', 'layer', 'layer', 'layer', 'layer', 'layer', 'layer', 'layer', 
					'growth habit', 'growth habit']) AS hit_type,
       unnest(ARRAY['top', 'lower', 'lower', 'lower', 'lower', 'lower', 'lower', 'lower', 'surface', 
					'woody', 'herbaceous']) AS hit_sub,
       unnest(ARRAY["TopCanopy", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "Lower6", "Lower7", 
					"SoilSurface", "SpeciesWoody", "SpeciesHerbaceous"]) AS pcode,
       unnest(ARRAY["ChkboxTop", "ChkboxLower1", "ChkboxLower2", "ChkboxLower3", "ChkboxLower4", 
					"ChkboxLower5", "ChkboxLower6", "ChkboxLower6", "ChkboxSoil", "ChkboxWoody", "ChkboxHerbaceous"]) AS chk,
	   unnest(ARRAY["HeightTop", "HeightLower1", "HeightLower2", "HeightLower3", "HeightLower4", "HeightLower5", 
					"HeightLower6", "HeightLower7", "HeightSurface", "HeightWoody", "HeightHerbaceous"]) AS height
  FROM dima."tblLPIDetail"

), dima_process1 AS (
SELECT a.reckey, a.mark, a.hit_type, a.hit_sub, a.hit_order, a.pcode, 
       cast(substring(a.height, '[\d\.]+') as numeric(6, 1)) height, 
	   a.chk, b."HeightUOM" units 
  FROM dima_process0 a
  LEFT JOIN dima."tblLPIHeader" b ON a.reckey = b."RecKey"
  WHERE trim(height) != '' AND height IS NOT NULL

), dima_final AS (
SELECT reckey, mark, hit_type, hit_sub, hit_order, pcode,
       CASE WHEN units = 'in' THEN round(height * 2.54, 1)
	        ELSE round(height, 1) END height_cm,
	   chk
  FROM dima_process1
)

SELECT * FROM lmf_final
 UNION ALL
SELECT * FROM dima_final;