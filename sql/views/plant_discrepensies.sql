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