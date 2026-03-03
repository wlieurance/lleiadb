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
-- pintercept_plot
--
DROP VIEW IF EXISTS pintercept_plot;
-- summarizes basic LPI heights and cover by plot/species.
CREATE VIEW pintercept_plot AS
WITH plot_indicator AS (
-- creates a unique list if indicators by plot, year, and hit type
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, c.hit, c.hit_type
  FROM transect AS a
 INNER JOIN pintercept_meta AS b ON a.linekey = b.linekey
 INNER JOIN pintercept AS c ON b.reckey = c.reckey
 WHERE c.hit NOT IN ('None', 'N')
 GROUP BY a.plotkey, survey_year, c.hit, c.hit_type
    
), year_recs AS (
-- creates a unique list of rec keys for each plot/year
SELECT b.plotkey, a.reckey, date_part('year', a.survey_date) survey_year
  FROM pintercept_meta a
 INNER JOIN transect b ON a.linekey = b.linekey

), rec_indicators AS (
-- creates a list of indicators that is the same for every record entered in the same survey year 
SELECT a.*, b.reckey
  FROM plot_indicator a
 INNER JOIN year_recs b ON a.plotkey = b.plotkey AND a.survey_year = b.survey_year
  
-- calculates indicators at the mark level
), mark_grp AS (
SELECT reckey, mark, hit, hit_type, avg(height_cm::float) height_cm, bool_and(dead) dead
  FROM pintercept
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
  FROM pintercept 
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
       case when a.hit_type = 'g' then 'growth habit'
            when a.hit_type = 'l' then 'canopy'
            else NULL end hit_type, 
       b.scientific_name, b.common_name, b.family, b.duration_first duration, 
       b.growth_habit_first growth_habit,
       a.hit_n, a.hit_pct, a.hit_pct_sd,
       a.dead_n, a.dead_pct, a.dead_pct_sd,
       a.height_n, a.height_cm, a.height_sd
  FROM plt_grp a
  LEFT JOIN plant_regex b ON a.hit = b.accepted_symbol
)

SELECT * FROM pi_final;

--
-- gap
--
DROP VIEW IF EXISTS gap_plot;
--summarizes basic gap intecept data by plot, measure type and gap class.
CREATE VIEW gap_plot AS
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
    FROM gap

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
  INNER JOIN gap_meta b ON a.linekey = b.linekey
  INNER JOIN gap c ON b.reckey = c.reckey
 GROUP BY a.plotkey, survey_year, c.rectype

), gap_rec_header AS (
--creates a line/rec header for joining gap data to plot header
SELECT a.plotkey, a.linekey, b.reckey, date_part('year', b.survey_date) survey_year
  FROM transect a
  INNER JOIN gap_meta b ON a.linekey = b.linekey

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
    FROM gap

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
 INNER JOIN gap_meta b ON a.linekey = b.linekey
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

--
-- plantcensus
--
DROP VIEW IF EXISTS plantcensus_plot;
-- summarizes basic plant census data by plot and joins plant info.
CREATE VIEW plantcensus_plot AS
WITH sr_plot AS (
SELECT a.plotkey, date_part('year', b.survey_date) survey_year, 
       c.species_code, count(b.reckey) rec_n, string_agg(c.notes, '; ') notes
  FROM point a
  INNER JOIN plantcensus_meta b ON a.plotkey = b.plotkey
  INNER JOIN plantcensus c ON b.reckey = c.reckey
  GROUP BY a.plotkey, survey_year, c.species_code

), joined AS (
SELECT a.plotkey, a.survey_year, a.species_code,
       b.code_type, b.scientific_name, b.common_name, b.family,
       b.duration_first duration, b.growth_habit_first growth_habit,
       a.rec_n, a.notes
  FROM sr_plot a
  LEFT JOIN plant_regex b ON a.species_code = b.accepted_symbol
)

SELECT * FROM joined;

--
-- plantdensity
--
DROP VIEW IF EXISTS plantdensity_plot;
-- summarizes plant density in plants/hectare at the plot/class/species level.
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


--
-- production_plot
--
DROP VIEW IF EXISTS production_plot;

-- Summarizes production in grams/square meter at the plot/species level.
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
-- rangehealth
--
DROP VIEW IF EXISTS rangehealth_plot;

-- Summarizes rangeland health data by plot in crosstab format.
CREATE VIEW rangehealth_plot AS
WITH ind_cross AS (
SELECT * FROM crosstab (
$$
SELECT a.reckey, a.rate_type || substring(a.seq_no::text from 1 for 1) || '_' || lower(a.rate_abbr), b."Code"::varchar(2)
  FROM rangehealth a
 INNER JOIN dima."tblMaintQualRatings" b ON a.rating = b."Rating" 
 ORDER BY a.reckey, a.seq_no
$$
) AS final_result(reckey text, i1_r varchar(2), i2_wfp varchar(2), i3_pt varchar(2), i4_bg varchar(2), i5_g varchar(2), 
                  i6_wsbda varchar(2), i7_lm varchar(2), i8_ssre varchar(2), i9_ssld varchar(2), i10_pccdrir varchar(2), 
                  i11_cl varchar(2), i12_fsg varchar(2), i13_pmd varchar(2), i14_la varchar(2), i15_ap varchar(2), i16_ip varchar(2), 
                  i17_rcpp varchar(2), a1_sss varchar(2), a2_hf varchar(2), a3_bi varchar(2))

), rh_final AS (
SELECT a.plotkey, a.survey_date, a.ecoid_std, b.* 
  FROM rangehealth_meta a
 INNER JOIN ind_cross b ON a.reckey = b.reckey
)

SELECT * FROM rh_final;

--
-- shrubshape
--
DROP VIEW IF EXISTS shrubshape_plot;
-- Summarizes shrub shape data by plot.
CREATE VIEW shrubshape_plot AS
WITH shape_codes (code, label) AS 
(VALUES
('C', 'columnar'),
('M', 'mixed'),
('S', 'spreading')

), shape_count AS (
SELECT reckey, coalesce(species_code, 'SHRUB') species_code, shape, count(mark) shape_n
  FROM shrubshape a
 GROUP BY reckey, species_code, shape

), species_count AS (
SELECT reckey, coalesce(species_code, 'SHRUB') species_code, count(mark) species_n
  FROM shrubshape a
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
  LEFT JOIN plant_regex b ON a.species_code = b.accepted_symbol
)

SELECT * FROM plot_joined;

--
-- soilstability
--
DROP VIEW IF EXISTS soilstability_plot;
-- Summarizes soils stability data by plot, year, and veg.
CREATE VIEW soilstability_plot AS
WITH cell_sum AS (
SELECT b.plotkey, date_part('year', b.survey_date) survey_year, a.reckey, b.rectype, 
       coalesce(a.box_no, 1) box_no, a.veg, count(a.cell) cell_n, 
       avg(a.rating) rating_mean, stddev(a.rating) rating_sd
  FROM soilstability a
 INNER JOIN soilstability_meta b ON a.reckey = b.reckey
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
