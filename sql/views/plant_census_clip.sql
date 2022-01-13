WITH point_clip AS(
SELECT a.* 
  FROM public.point AS a
  INNER JOIN my.table AS b ON ST_Intersects(a.geom, b.geom)

), plant_clip AS (
SELECT a.*
  FROM plantcensus AS a
 INNER JOIN plantcensusmeta AS b ON a.reckey = b.reckey
 INNER JOIN point_clip AS c ON b.plotkey = c.plotkey

), plant_group AS (
SELECT cplant, count(reckey) As n
  FROM plant_clip AS a
  GROUP BY cplant

)
SELECT a.*, b.scientific_name, b.common_name, b.growth_habit, b.duration
  FROM plant_group AS a
  LEFT JOIN plant AS b ON a.cplant = b.accepted_symbol
  ORDER BY n DESC