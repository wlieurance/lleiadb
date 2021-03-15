--
-- Using query to populate tblSpecies FROM public.plant table
--
ALTER TABLE dima."tblSpecies" DISABLE TRIGGER species_insert;
WITH gh_first AS (
SELECT *, split_part(duration, ', ', 1) AS duration_first, split_part(growth_habit, ', ', 1) AS growth_habit_first
  FROM public.plant   

), raw AS (
SELECT 'base' AS dbkey, accepted_symbol AS "SpeciesCode", scientific_name AS "ScientificName", common_name AS "CommonName", family AS "Family",
       99 AS "SortSeq", NULL AS "synonymOf", 
       CASE WHEN family = 'Cyperaceae' THEN 'Sedge'
            WHEN family = 'Cactaceae' THEN 'Succulent'
            WHEN growth_habit_first = 'Subshrub' THEN 'Sub-Shrub' 
            WHEN growth_habit_first = 'Vine' THEN 'Forb/herb' 
            WHEN growth_habit_first = 'Nonvascular' THEN 'Forb/herb' 
            WHEN growth_habit_first = 'Lichenous' THEN 'Forb/herb'
            ELSE growth_habit_first END AS "GrowthHabitSub", 
       duration_first AS "Duration", False AS "Stabilizing", False AS "Invasive", NULL AS "Group"
  FROM gh_first

), converted AS (
SELECT a.dbkey, a."SpeciesCode", a."ScientificName", a."CommonName", a."Family", a."SortSeq", a."synonymOf", 
       a."GrowthHabitSub", b."Code" AS "GrowthHabitCode", a."Duration", a."Stabilizing", a."Invasive", a."Group"
  FROM raw AS a
  LEFT JOIN dima."tblSpeciesGrowthHabit" AS b ON a."GrowthHabitSub" = b."GrowthHabitSub"

)
INSERT INTO dima."tblSpecies" (
            dbkey, "SpeciesCode", "ScientificName", "CommonName", "Family", "SortSeq", 
            "synonymOf", "GrowthHabitCode", "Duration", "Stabilizing", "Invasive", "Group")
SELECT dbkey, "SpeciesCode", "ScientificName", "CommonName", "Family", "SortSeq", 
       "synonymOf", "GrowthHabitCode", "Duration", "Stabilizing", "Invasive", "Group"
  FROM converted;
ALTER TABLE dima."tblSpecies" ENABLE TRIGGER species_insert;

--
-- Using query to populate tblEcolSIte FROM public.ecosite table
--
ALTER TABLE dima."tblEcolSites" DISABLE TRIGGER eco_insert;
INSERT INTO dima."tblEcolSites" (dbkey, "EcolSite", "SiteName", "DateModified")
SELECT 'base' AS dbkey, ecoid AS "EcolSite", econame AS "SiteName", now() AS "DateModified"
  FROM public.ecosite;
INSERT INTO dima."tblEcolSites" (dbkey, "EcolSite", "SiteName", "DateModified") VALUES
('base', ' UNKNOWN', 'Unknown Site', now());
ALTER TABLE dima."tblEcolSites" ENABLE TRIGGER eco_insert;
