--QAQC_Header_Line_IncorrectField	
SELECT a."RecKey" error_key,
       d."SiteID", c."PlotID", b."LineID", a."RecKey", a."FormDate",
       a."{field_name}"
  FROM dima."{header_tbl}" AS a
 INNER JOIN dima."tblLines" AS b ON a."LineKey" = b."LineKey"
 INNER JOIN dima."tblPlots" AS c ON b."PlotKey" = c."PlotKey"
 INNER JOIN dima."tblSites" AS d ON c."SiteKey" = d."SiteKey"
 WHERE a."{field_name}" NOT IN ({field_list})
 ORDER BY d."SiteID", c."PlotID", b."LineID", a."FormDate";
 
--QAQC_Header_Line_MissingField	
SELECT a."RecKey" error_key,
       d."SiteID", c."PlotID", b."LineID", a."RecKey", a."FormDate",
       a."{field_name}"
  FROM dima."{header_tbl}" AS a
 INNER JOIN dima."tblLines" AS b ON a."LineKey" = b."LineKey"
 INNER JOIN dima."tblPlots" AS c ON b."PlotKey" = c."PlotKey"
 INNER JOIN dima."tblSites" AS d ON c."SiteKey" = d."SiteKey"
 WHERE a."{field_name}" IS NULL
 ORDER BY d."SiteID", c."PlotID", b."LineID", a."FormDate";
 
-- QAQC_LI_LengthLessMin
SELECT a."RecKey" || ';' || a."Species" || ';' || a."{start}" error_key,
       e."SiteID", d."PlotID", c."LineID", a."RecKey", a."{species}",
       a."{startpos}", a."{endpos}",
       abs(a."{endpos}" - a."{startpos}") AS seg_length
  FROM dima."tblLICDetail" AS a
 INNER JOIN dima."{tbl}" AS b ON a."RecKey" = b."RecKey"
 INNER JOIN dima."tblLines" AS c ON b."LineKey" = c."LineKey"
 INNER JOIN dima."tblPlots" AS d ON c."PlotKey" = d."PlotKey"
 INNER JOIN dima."tblSites" AS e ON d."SiteKey" = e."SiteKey"
 WHERE seg_length < {min_length}
 ORDER BY e."SiteID", d."PlotID", c."LineID", a."{startpos}";
 
-- QAQC_MissingDetailForm
SELECT d."SiteID", c."PlotID", b."LineID", a."RecKey", a."Notes",
       e."RecKey"
  FROM dima."{tbl_header}" a 
 INNER JOIN dima."tblLines" AS b ON a."LineKey" = b."LineKey"
 INNER JOIN dima."tblPlots" AS c ON b."PlotKey" = c."PlotKey"
 INNER JOIN dima."tblSites" AS d ON c."SiteKey" = d."SiteKey"
  LEFT JOIN dima."{tbl_detail}" AS e ON a."RecKey" = e."RecKey"
 WHERE e."RecKey" IS NULL
 ORDER BY d."SiteID", c."PlotID", b."LineID";

--QAQC_Plot_Methods_FormNumberCheck
WITH methods (tbl, method_name, abbr, form_req) AS (VALUES                        
('tblBSNE_Stack', 'Aeolian Sediment Collection', 'bsne', {bsne}),
('tblCanopyGapHeader', 'Canopy Gap with Species', 'cgi', {cgi}),
('tblCompactHeader', 'Soil Compaction', 'sc' , {sc}),
('tblDKHeader', 'Plant Composition (Domin-Krajina)', 'dk' , {dk}),
('tblDryWtHeader', 'Dry Weight Rank', 'dwr', {dwr}),
('tblGapHeader', 'Gap Intercept', 'gi', {gi}),
('tblInfiltrationHeader', 'Infiltration', 'inf', {inf}),
('tblLICHeader', 'Continuous Line Intercept', 'cli', {cli}),
('tblLPIHeader', 'Line-Point Intercept', 'lpi', {lpi}),
('tblNestedFreqHeader', 'Nested Frequency', 'nf', {nf}),
('tblOcularCovHeader', 'Ocular Estimates', 'oe' , {oe}),
('tblPastCondHeader', 'Pasture Condition', 'pc' , {pc}),
('tblPlantDenHeader', 'Plant Density', 'pd', {pd}),
('tblPlantLenHeader', 'Plant Length', 'pl', {pl}),
('tblPlantProdHeader', 'Plant Production', 'pp', {pp}),
('tblPlotMgtHeader', 'Management Record', 'mr', {mr}),
('tblPTFrameHeader', 'Point Frame', 'pf', {pf}),
('tblQualHeader', 'Rangeland Health', 'rh', {rh}),
('tblRiparProHeader', 'Riparian Channel/Gully Profile', 'rp', {rp}),
('tblRiparSurvHeader', 'Riparian Channel Veg. Survey', 'rv', {rv}),
('tblSageRange', 'Sage-grouse Habitat', 'sg', {sg}),
('tblSoilPits', 'Soil Pit', 'sp', {sp}),
('tblSoilStabHeader', 'Soil Stability', 'ss', {ss}),
('tblSpecRichHeader', 'Species Richness', 'sr', {sr}),
('tblTreeDenHeader', 'Tree Inventory', 'ti', {ti}),
('tblUtilHeader', 'Utilization', 'uti', {uti}),
('tblVegStructHeader', 'Vegetation Structure', 'vs', {vs})

), headers AS (
SELECT "StackID" "RecKey", "DateEstablished" "FormDate", "PlotKey", 'bsne' abbr 
  FROM dima."tblBSNE_Stack" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'cgi' abbr 
  FROM dima."tblCanopyGapHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'sc' abbr 
  FROM dima."tblCompactHeader" UNION
  
SELECT "RecKey", "FormDate", "PlotKey", 'dk' abbr 
  FROM dima."tblDKHeader" UNION
  
SELECT a."RecKey", a."FormDate", b."PlotKey", 'dwr' abbr 
  FROM dima."tblDryWtHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'gi' abbr 
  FROM dima."tblGapHeader" a
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'inf' abbr 
  FROM dima."tblInfiltrationHeader" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'cli' abbr 
  FROM dima."tblLICHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT a."RecKey", a."FormDate", b."PlotKey", 'lpi' abbr 
  FROM dima."tblLPIHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT a."RecKey", a."FormDate", b."PlotKey", 'nf' abbr 
  FROM dima."tblNestedFreqHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT a."RecKey", a."FormDate", b."PlotKey", 'oe' abbr 
  FROM dima."tblOcularCovHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT "RecKey", "FormDate", "PlotKey", 'pc' abbr 
  FROM dima."tblPastCondHeader" UNION
 
SELECT a."RecKey", a."FormDate", b."PlotKey", 'pd' abbr 
  FROM dima."tblPlantDenHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT "RecKey", "FormDate", "PlotKey", 'pl' abbr 
  FROM dima."tblPlantLenHeader" UNION
  
SELECT a."RecKey", a."FormDate", b."PlotKey", 'pp' abbr 
  FROM dima."tblPlantProdHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'mr' abbr 
  FROM dima."tblPlotMgtHeader" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'pf' abbr 
  FROM dima."tblPTFrameHeader" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'rh' abbr 
  FROM dima."tblQualHeader" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'rp' abbr 
  FROM dima."tblRiparProHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'rv' abbr 
  FROM dima."tblRiparSurvHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'sg' abbr 
  FROM dima."tblSageRange" UNION

SELECT "SoilKey" "RecKey", "DateRecorded" "FormDate", "PlotKey", 'sp' abbr 
  FROM dima."tblSoilPits" UNION
  
SELECT a."RecKey", a."FormDate", b."PlotKey", 'ss' abbr 
  FROM dima."tblSoilStabHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'sr' abbr 
  FROM dima."tblSpecRichHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'ti' abbr 
  FROM dima."tblTreeDenHeader" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'uti' abbr 
  FROM dima."tblUtilHeader" UNION
  
SELECT a."RecKey", a."FormDate", b."PlotKey", 'vs' abbr 
  FROM dima."tblVegStructHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey"

), form_cnt AS (
SELECT "PlotKey", date_part('year', "FormDate") survey_year, abbr,
       count("RecKey") form_n
  FROM headers
 GROUP BY "PlotKey", survey_year, abbr

), survey_years AS (
SELECT "PlotKey", survey_year
  FROM form_cnt
 WHERE survey_year IS NOT NULL
 GROUP BY "PlotKey", survey_year

), header_joined AS (
SELECT a."SiteID", b."PlotID", b."PlotKey", c.survey_year, d.*
  FROM dima."tblSites" a
 INNER JOIN dima."tblPlots" b ON a."SiteKey" = b."SiteKey"
 INNER JOIN survey_years c ON b."PlotKey" = c."PlotKey"
 CROSS JOIN methods d
 WHERE b."PlotKey" NOT IN ('888888888', '999999999')
)

SELECT a.*, coalesce(b.form_n, 0) form_n
  FROM header_joined a
  LEFT JOIN form_cnt b ON a."PlotKey" = b."PlotKey" 
                       AND a.survey_year = b.survey_year
                       AND a.abbr = b.abbr
 WHERE a.form_req != coalesce(b.form_n, 0)
 ORDER BY "SiteID", "PlotID", survey_year, method_name;


--QAQC_Plot_Methods_FormDateCheck
WITH methods (tbl, method_name, abbr) AS (VALUES                        
('tblBSNE_Stack', 'Aeolian Sediment Collection', 'bsne'),
('tblCanopyGapHeader', 'Canopy Gap with Species', 'cgi'),
('tblCompactHeader', 'Soil Compaction', 'sc'),
('tblDKHeader', 'Plant Composition (Domin-Krajina)', 'dk'),
('tblDryWtHeader', 'Dry Weight Rank', 'dwr'),
('tblGapHeader', 'Gap Intercept', 'gi'),
('tblInfiltrationHeader', 'Infiltration', 'inf'),
('tblLICHeader', 'Continuous Line Intercept', 'cli'),
('tblLPIHeader', 'Line-Point Intercept', 'lpi'),
('tblNestedFreqHeader', 'Nested Frequency', 'nf'),
('tblOcularCovHeader', 'Ocular Estimates', 'oe'),
('tblPastCondHeader', 'Pasture Condition', 'pc'),
('tblPlantDenHeader', 'Plant Density', 'pd'),
('tblPlantLenHeader', 'Plant Length', 'pl'),
('tblPlantProdHeader', 'Plant Production', 'pp'),
('tblPlotMgtHeader', 'Management Record', 'mr'),
('tblPTFrameHeader', 'Point Frame', 'pf'),
('tblQualHeader', 'Rangeland Health', 'rh'),
('tblRiparProHeader', 'Riparian Channel/Gully Profile', 'rp'),
('tblRiparSurvHeader', 'Riparian Channel Veg. Survey', 'rv'),
('tblSageRange', 'Sage-grouse Habitat', 'sg'),
('tblSoilPits', 'Soil Pit', 'sp'),
('tblSoilStabHeader', 'Soil Stability', 'ss'),
('tblSpecRichHeader', 'Species Richness', 'sr'),
('tblTreeDenHeader', 'Tree Inventory', 'ti'),
('tblUtilHeader', 'Utilization', 'uti'),
('tblVegStructHeader', 'Vegetation Structure', 'vs')

), headers AS (
SELECT "StackID" "RecKey", "DateEstablished" "FormDate", "PlotKey", 'bsne' abbr 
  FROM dima."tblBSNE_Stack" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'cgi' abbr 
  FROM dima."tblCanopyGapHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'sc' abbr 
  FROM dima."tblCompactHeader" UNION
  
SELECT "RecKey", "FormDate", "PlotKey", 'dk' abbr 
  FROM dima."tblDKHeader" UNION
  
SELECT a."RecKey", a."FormDate", b."PlotKey", 'dwr' abbr 
  FROM dima."tblDryWtHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'gi' abbr 
  FROM dima."tblGapHeader" a
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'inf' abbr 
  FROM dima."tblInfiltrationHeader" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'cli' abbr 
  FROM dima."tblLICHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT a."RecKey", a."FormDate", b."PlotKey", 'lpi' abbr 
  FROM dima."tblLPIHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT a."RecKey", a."FormDate", b."PlotKey", 'nf' abbr 
  FROM dima."tblNestedFreqHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT a."RecKey", a."FormDate", b."PlotKey", 'oe' abbr 
  FROM dima."tblOcularCovHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT "RecKey", "FormDate", "PlotKey", 'pc' abbr 
  FROM dima."tblPastCondHeader" UNION
 
SELECT a."RecKey", a."FormDate", b."PlotKey", 'pd' abbr 
  FROM dima."tblPlantDenHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION
 
SELECT "RecKey", "FormDate", "PlotKey", 'pl' abbr 
  FROM dima."tblPlantLenHeader" UNION
  
SELECT a."RecKey", a."FormDate", b."PlotKey", 'pp' abbr 
  FROM dima."tblPlantProdHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'mr' abbr 
  FROM dima."tblPlotMgtHeader" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'pf' abbr 
  FROM dima."tblPTFrameHeader" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'rh' abbr 
  FROM dima."tblQualHeader" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'rp' abbr 
  FROM dima."tblRiparProHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'rv' abbr 
  FROM dima."tblRiparSurvHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'sg' abbr 
  FROM dima."tblSageRange" UNION

SELECT "SoilKey" "RecKey", "DateRecorded" "FormDate", "PlotKey", 'sp' abbr 
  FROM dima."tblSoilPits" UNION
  
SELECT a."RecKey", a."FormDate", b."PlotKey", 'ss' abbr 
  FROM dima."tblSoilStabHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT a."RecKey", a."FormDate", b."PlotKey", 'sr' abbr 
  FROM dima."tblSpecRichHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'ti' abbr 
  FROM dima."tblTreeDenHeader" UNION

SELECT "RecKey", "FormDate", "PlotKey", 'uti' abbr 
  FROM dima."tblUtilHeader" UNION
  
SELECT a."RecKey", a."FormDate", b."PlotKey", 'vs' abbr 
  FROM dima."tblVegStructHeader" a 
 INNER JOIN dima."tblLines" b ON a."LineKey" = b."LineKey"

), date_grp AS (
SELECT "PlotKey", date_part('year', "FormDate") survey_year, abbr,
       count("RecKey") form_n, min("FormDate") date_min,  max("FormDate") date_max
  FROM headers
 GROUP BY "PlotKey", survey_year, abbr

), dif AS (
SELECT c."SiteID", b."PlotID", a.*, d.method_name, d.tbl, 
       a.date_max - a.date_min days_dif
  FROM date_grp a
 INNER JOIN dima."tblPlots" b ON a."PlotKey" = b."PlotKey"
 INNER JOIN dima."tblSites" c ON b."SiteKey" = c."SiteKey"
 INNER JOIN methods d ON a.abbr = d.abbr
)

SELECT "SiteID", "PlotID", "PlotKey", survey_year, method_name, tbl,
       form_n, date_min, date_max, days_dif
  FROM dif 
 WHERE days_dif > {days_dif} AND days_dif IS NOT NULL
 ORDER BY "SiteID", "PlotID", survey_year, method_name;
 
 
--QAQC_Species_DurationIncorrect
SELECT a."RecKey" || ';' || a."Species" || ';' || a."StartPos" error_key,
       e."SiteID", d."PlotID", c."LineID", a."RecKey", a."Species",
       a."StartPos", a."EndPos", f."ScientificName", f."CommonName", 
       f."Duration", g."GrowthHabitSub"
  FROM dima."tblLICDetail" AS a
 INNER JOIN dima."tblLICHeader" AS b ON a."RecKey" = b."RecKey"
 INNER JOIN dima."tblLines" AS c ON b."LineKey" = c."LineKey"
 INNER JOIN dima."tblPlots" AS d ON c."PlotKey" = d."PlotKey"
 INNER JOIN dima."tblSites" AS e ON d."SiteKey" = e."SiteKey"
 LEFT JOIN dima."tblSpecies" AS f ON a."Species" = f."SpeciesCode"
 LEFT JOIN dima."tblSpeciesGrowthHabit" g ON f."GrowthHabitCode" = g."Code"
 WHERE f."SpeciesCode" IS NOT NULL AND f."Duration" NOT IN ({duration_list}})
 ORDER BY e."SiteID", d."PlotID", c."LineID", a."StartPos";
 
