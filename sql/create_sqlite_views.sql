--
-- plant_mod
--
DROP VIEW IF EXISTS plant_mod;
CREATE VIEW plant_mod AS
WITH family_conv AS (
SELECT substr(upper(family), 1, 6) six_symbol, 'family' code_type, family
  FROM plant
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
       duration,
	   coalesce(iif(substr(duration, 1, instr(duration, ',')-1) = '', NULL, substr(duration, 1, instr(duration, ',')-1)), duration) AS duration_first,
       growth_habit,
	   coalesce(iif(substr(growth_habit, 1, instr(growth_habit, ',')-1) = '', NULL, substr(growth_habit, 1, instr(growth_habit, ',')-1)), growth_habit) AS growth_habit_first,
       native_status, hybrid_genus_indicator, genus, hybrid_species_indicator, species,
       subspecies_prefix, hybrid_subspecies_indicator, subspecies, variety_prefix, hybrid_variety_indicator,
       variety, forma_prefix, forma, genera_binomial_author, trinomial_author, quadranomial_author, parents,
       state_and_province
  FROM plant
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

--
-- production_plot
--
DROP VIEW IF EXISTS production_plot;
CREATE VIEW production_plot AS
WITH clip_sum AS (
-- sum up estimated weight and clipped weight for those records that have it to use
-- in calculating clip correction factor
SELECT b.plotkey, cast(strftime('%Y', survey_date) AS INTEGER) survey_year, a.species_code,
       sum(a.units * c.unit_wgt_g) estimated_g, sum(a.clipped_wgt_g) clipped_wgt_g
  FROM production a
 INNER JOIN production_meta b ON a.reckey = b.reckey
 INNER JOIN production_species c ON a.reckey = c.reckey AND a.species_code = c.species_code
 WHERE clipped_wgt_g IS NOT NULL
 GROUP BY b.plotkey, survey_year, a.species_code

), adjust_clip AS (
-- calculate clipped weight correction factor by reckey/species
SELECT a.plotkey, a.survey_year, b.reckey, a.species_code, a.estimated_g, a.clipped_wgt_g,
       a.clipped_wgt_g/(CASE WHEN a.estimated_g = 0 THEN a.clipped_wgt_g ELSE a.estimated_g END) adjust_clip
  FROM clip_sum a
 INNER JOIN production_meta b ON a.plotkey = b.plotkey

), production_species_clip AS (
-- join clipped correction factor back up to other correction factors to await processing
SELECT a.reckey, a.species_code, a.subsize_m2, a.unit_wgt_g, a.adjust_airdrywgt, a.adjust_utilization,
       a.adjust_growth, a.adjust_climate, coalesce(b.adjust_clip, 1) adjust_clip
  FROM production_species a
  LEFT JOIN adjust_clip b ON a.reckey = b.reckey AND a.species_code = b.species_code

), rec_header AS (
-- create a full list of every species for every reckey/subplot just in case
SELECT a.reckey, b.subplot, a.species_code
  FROM  production_species_clip a
 INNER JOIN production_subplot b ON a.reckey = b.reckey
 WHERE b.not_sampled IS FALSE

), rec_header_filled AS (
-- join the rec header to data and fill in nulls as 0s
SELECT a.*,
       coalesce(CASE WHEN b.units = 0 AND b.clipped_wgt_g IS NOT NULL THEN b.clipped_wgt_g
            ELSE b.units END, 0) units
  FROM rec_header a
  LEFT JOIN production b ON a.reckey = b.reckey AND a.subplot = b.subplot AND a.species_code = b.species_code

), rec_species_mean AS (
-- summarize over method records (reckey) and species
SELECT reckey, species_code, count(subplot) sub_n, avg(units) unit_mean
  FROM rec_header_filled
 GROUP BY reckey, species_code

), rec_species_calc AS (
-- calculate production in grams/square meter using the adjustment factors, unit weights, and subplot size
SELECT a.*, b.subsize_m2, b.unit_wgt_g, b.adjust_airdrywgt, b.adjust_utilization,
       b.adjust_growth, b.adjust_climate, b.adjust_clip,
       unit_mean * unit_wgt_g * (1/subsize_m2) * b.adjust_airdrywgt * b.adjust_clip *
           (1/b.adjust_utilization) * (1/b.adjust_growth) * (1/b.adjust_climate) prod_g_m2
  FROM rec_species_mean a
 INNER JOIN production_species_clip b ON a.reckey = b.reckey AND a.species_code = b.species_code

), plot_species AS (
-- create a unique list of species for each plot/year
SELECT a.plotkey, cast(strftime('%Y', a.survey_date) AS INTEGER) survey_year, b.species_code
  FROM production_meta a
 INNER JOIN production_species b ON a.reckey = b.reckey
 GROUP BY a.plotkey, survey_year, b.species_code

), plot_recs AS (
-- create a unique list of rec keys for each plot/year
SELECT a.plotkey, cast(strftime('%Y', a.survey_date) AS INTEGER) survey_year, reckey
  FROM production_meta a

), plot_header AS (
-- create a header which has all species for all reckeys for each plot/year
SELECT a.plotkey, a.survey_year, a.reckey, b.species_code
  FROM plot_recs a
 INNER JOIN plot_species b ON a.plotkey = b.plotkey AND a.survey_year = b.survey_year

), plot_header_filled AS (
-- join the header to the actual calculations and fill in nulls as 0s
SELECT a.*, coalesce(b.sub_n, 0) sub_n, coalesce(b.prod_g_m2, 0) prod_g_m2
  FROM plot_header a
 LEFT JOIN rec_species_calc b ON a.reckey = b.reckey AND a.species_code = b.species_code

), plot_grouped AS (
-- summarize by plot/year/species
SELECT plotkey, survey_year, species_code, count(reckey) rec_n, avg(prod_g_m2) prod_g_m2
  FROM plot_header_filled
 GROUP BY plotkey, survey_year, species_code

), plot_joined AS (
-- join plot level summary to plant_mod table for more info
SELECT a.plotkey, a.survey_year, a.species_code, b.scientific_name, b.common_name, b.family,
       b.duration_first duration, b.growth_habit_first growth_habit,
       a.rec_n, a.prod_g_m2
  FROM plot_grouped a
  LEFT JOIN plant_mod b ON a.species_code = b.accepted_symbol
  WHERE a.prod_g_m2 > 0
)

SELECT * FROM plot_joined;

--
-- plantdensity
--
DROP VIEW IF EXISTS plantdensity_plot;
CREATE VIEW plantdensity_plot AS
WITH plot_species AS (
-- creates a unique list of all species found in a plot for later joining
SELECT a.plotkey, CAST(strftime('%Y', b.survey_date) AS INTEGER) survey_year, c.species_code
  FROM transect a
 INNER JOIN plantdensity_meta b ON a.linekey = b.linekey
 INNER JOIN plantdensity c ON b.reckey = c.reckey
 GROUP BY a.plotkey, survey_year, c.species_code

), plot_classes AS (
-- creates a unique list of all classes found in a plot for later joining
SELECT a.plotkey, CAST(strftime('%Y', b.survey_date) AS INTEGER) survey_year, c.class_no, c.class_lbl
  FROM transect a
 INNER JOIN plantdensity_meta b ON a.linekey = b.linekey
 INNER JOIN plantdensity_class c ON b.reckey = c.reckey
 GROUP BY a.plotkey, survey_year, c.class_no, c.class_lbl

), plot_recs AS (
-- creates a unique list of all reckeys found in a plot for later joining
SELECT a.plotkey, CAST(strftime('%Y', b.survey_date) AS INTEGER) survey_year, b.reckey
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
  FROM plantdensity a
 INNER JOIN plantdensity_subplot b ON  a.reckey = b.reckey AND a.subid = b.subid
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
 INNER JOIN plantdensity_subplot b ON a.reckey = b.reckey AND a.subsize_m2 = b.subsize_m2

), rec_header AS (
-- creates a complete list of reckeys, species, and subplot ids for joining completion
SELECT a.reckey, a.species_code, a.subsize_m2, a.subid, a.subplot,
       b.class_no, b.class_lbl
  FROM rec_header_subs a
 INNER JOIN plantdensity_class b ON a.reckey = b.reckey

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
       avg(density_ha) density_ha_mean
  FROM rec_filled
 GROUP BY reckey, species_code, class_no, class_lbl

), plot_header_filled AS (
-- joins the plot header to the summarized over subplot data and fills in nulls
-- with zeros for count and mean
SELECT a.*, coalesce(b.sub_n, 0) sub_n, coalesce(density_ha_mean, 0) density_ha_mean
  FROM plot_header a
  LEFT JOIN rec_grouped b ON a.reckey = b.reckey
        AND a.class_no = b.class_no AND a.species_code = b.species_code

), rec_plot_grouped AS (
-- summarizes density at the plot/year/class/species level
SELECT plotkey, survey_year, class_no, class_lbl, species_code,
       count(reckey) rec_n, avg(density_ha_mean) density_ha_mean
  FROM plot_header_filled
 GROUP BY plotkey, survey_year, class_no, class_lbl, species_code

), plot_joined AS (
-- joins plot summary with plant level data
SELECT a.plotkey, a.survey_year, a.class_no, a.class_lbl, a.species_code,
       b.scientific_name, b.common_name, b.family, b.duration_first duration,
       b.growth_habit_first growth_habit,
       a.rec_n, a.density_ha_mean
  FROM rec_plot_grouped a
  LEFT JOIN plant_mod b ON a.species_code = b.accepted_symbol
)

SELECT * FROM plot_joined;
