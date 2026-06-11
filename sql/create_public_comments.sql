/*-----------
PUBLIC: Tables
------------*/
-- public.county
COMMENT ON TABLE public."county" IS 'County data from the U.S. Census Bureau''s Current County and Equivalent National Dataset.';
COMMENT ON COLUMN public."county"."geoid" IS 'County identifier; a concatenation of Current state FIPS code and county FIPS code.';
COMMENT ON COLUMN public."county"."statefp" IS 'Current state Federal Information Processing Series (FIPS) code.';
COMMENT ON COLUMN public."county"."countyfp" IS 'Current county Federal Information Processing Series (FIPS) code.';
COMMENT ON COLUMN public."county"."countyns" IS 'Current county GNIS code.';
COMMENT ON COLUMN public."county"."name" IS 'Current county name.';
COMMENT ON COLUMN public."county"."namelsad" IS 'Current name and the translated legal/statistical area description for county.';
COMMENT ON COLUMN public."county"."lsad" IS 'Current legal/statistical area description code for county.';
COMMENT ON COLUMN public."county"."geom" IS 'The geometry for the county (not loaded by default).';

-- public.ecosite
COMMENT ON TABLE public."ecosite" IS 'NRCS ecological sites which may be associated with a plot.';
COMMENT ON COLUMN public."ecosite"."ecoid_std" IS 'The identifier of a particular ecological community. For NRCS ecological sites, ecological site MLRA, ecological site LRU, ecological site number and ecological site state FIPS alpha code. Primary key for this table, it differs from the ecoid field in that ecological site type (range, forest, etc.) has been stripped. Any underscores and trailing numerical identifiers, if present, are also stripped.';
COMMENT ON COLUMN public."ecosite"."ecoid" IS 'The identifier of a particular ecological community. For NRCS ecological sites, it is the concatenated form of ecological site type, ecological site MLRA, ecological site LRU, ecological site number and ecological site state FIPS alpha code.';
COMMENT ON COLUMN public."ecosite"."econame" IS 'The descriptive name of a particular ecological community. For NRCS ecological sites, it is the concatenated form of three or six other fields. The actual fields that are concatenated together to form this name differ between range and forest ecological sites.';
COMMENT ON COLUMN public."ecosite"."type" IS 'The ecological site type.';
COMMENT ON COLUMN public."ecosite"."mlra" IS ' NRCS Major Land Resource Area ID associated with the ecological site.';
COMMENT ON COLUMN public."ecosite"."mlra_sub" IS ' The subdivision code for a Major Land Resource Area (in the case that it is subdivided) associated with the ecological site.';
COMMENT ON COLUMN public."ecosite"."lru" IS ' The NRCS Land Resource Unit code associated with an ecological site.';
COMMENT ON COLUMN public."ecosite"."site_no" IS 'The numeric identifier for an ecological site, within its LRU/MLRA.';
COMMENT ON COLUMN public."ecosite"."state" IS 'Current United States Postal Service state abbreviation.';
COMMENT ON COLUMN public."ecosite"."ecogroup" IS 'The name/label/id of a user custom category or grouping of ecological sites above the ecological site scale but below the MLRA/LRU scale.';
COMMENT ON COLUMN public."ecosite"."ecogroup" IS 'The type/category/name of the user custom ecological grouping.';

-- public.plant
COMMENT ON TABLE public."plant" IS 'USDA PLANTS database plant list.';
COMMENT ON COLUMN public."plant"."accepted_symbol" IS 'A 3 to 7 digit alphanumeric code which uniquely identifies a genus, species, subspecies, etc. within the PLANTS database.';
COMMENT ON COLUMN public."plant"."code_type" IS 'Identifies the taxonomic category in which the plant code falls (Genus, Species, Subspecies, Variety, Forma).';
COMMENT ON COLUMN public."plant"."scientific_name" IS 'The scientific name associated with the plant in binomial nomenclature (without author). Scientific Names also sometimes will have trinomial or quadrinomial names associated with botanical nomenclature.';
COMMENT ON COLUMN public."plant"."common_name" IS 'A common name for the plant (non-scientific).';
COMMENT ON COLUMN public."plant"."family" IS 'The taxonomic Family to which the plant belongs. A sub-taxon of Order.';
COMMENT ON COLUMN public."plant"."duration" IS 'The plant life cycle length. Some plants have different durations depending on environment or location, so a plant can have more than one value. Multiple values are reported in order of increasing longevity in nature.';
COMMENT ON COLUMN public."plant"."growth_habit" IS 'Refers to the shape, height, appearance, and form of growth of a plant species.';
COMMENT ON COLUMN public."plant"."native_status" IS 'The status of a plants distribution in an area and its relationship to the historic populations within that area.';
COMMENT ON COLUMN public."plant"."hybrid_genus_indicator" IS 'The indicator used before the genus name in the "scientific_name" field (×) which indicates that this plant is a hybrid of two genera.';
COMMENT ON COLUMN public."plant"."genus" IS 'The genus name of the plant, the first half of the scientific name in binomial nomenclature. A taxonomic category that ranks below Family.';
COMMENT ON COLUMN public."plant"."hybrid_species_indicator" IS 'The indicator used before the species name in the "scientific_name" field (×) which indicates that this plant is a hybrid of two species.';
COMMENT ON COLUMN public."plant"."species" IS 'The species name of the plant, the second half of the scientific name in binomial nomenclature. A taxonomic category that ranks below Genus.';
COMMENT ON COLUMN public."plant"."subspecies_prefix" IS 'The indicator used before the subspecies name in the "scientific_name" field (ssp.) which indicates that this plant is a subspecies of another species.';
COMMENT ON COLUMN public."plant"."hybrid_subspecies_indicator" IS 'The indicator used before the subspecies name (but after the subspecies prefix) in the "scientific_name" field (×) which indicates that this plant is a hybrid of two subspecies.';
COMMENT ON COLUMN public."plant"."subspecies" IS 'The subspecies name of the plant, the third portion of the scientific name in trinomial nomenclature. A taxonomic category that ranks below Species.';
COMMENT ON COLUMN public."plant"."variety_prefix" IS 'The indicator used before the subspecies name in the "scientific_name" field (var.) which indicates that this plant is a variety (varietas) of another species.';
COMMENT ON COLUMN public."plant"."hybrid_variety_indicator" IS 'The indicator used before the variety name (but after the variety prefix) in the "scientific_name" field (×) which indicates that this plant is a hybrid of two varieties.';
COMMENT ON COLUMN public."plant"."variety" IS 'The variety name of the plant, the third portion of the scientific name in trinomial nomenclature. A taxonomic category that ranks below Subspecies. It is sometimes recommended that the subspecies rank should be used to recognize geographic distinctiveness, whereas the variety rank is appropriate if the taxon is seen throughout the geographic range of the species.';
COMMENT ON COLUMN public."plant"."forma_prefix" IS 'The indicator used before the form name in the "scientific_name" field (f.) which indicates that this plant is a form (forma) of another species/subspecies/variety.';
COMMENT ON COLUMN public."plant"."forma" IS 'The form name of the plant, the third or fourth portion of the scientific name in the botanical ternary naming system. A taxonomic category that ranks below Variety.';
COMMENT ON COLUMN public."plant"."genera_binomial_author" IS 'The author citation of the species. In botanical nomenclature, author citation refers to citing the person or group of people who validly published a botanical name, i.e. who first published the name while fulfilling the formal requirements as specified by the International Code of Nomenclature for algae, fungi, and plants (ICN). In cases where a species is no longer in its original generic placement (i.e. a new combination of genus and specific epithet), both the author(s) of the original genus placement and those of the new combination are given (the former in parentheses). In botany, it is customary (though not obligatory) to abbreviate author names according to a recognized list of standard abbreviations.';
COMMENT ON COLUMN public."plant"."trinomial_author" IS 'The author citation of the trinomial taxon (subspecies, variety, etc.) in the ternary name of botanical nomenclature. In botanical nomenclature, author citation refers to citing the person or group of people who validly published a botanical name, i.e. who first published the name while fulfilling the formal requirements as specified by the International Code of Nomenclature for algae, fungi, and plants (ICN). In cases where a species is no longer in its original generic placement (i.e. a new combination of genus and specific epithet), both the author(s) of the original genus placement and those of the new combination are given (the former in parentheses). In botany, it is customary (though not obligatory) to abbreviate author names according to a recognized list of standard abbreviations.';
COMMENT ON COLUMN public."plant"."quadranomial_author" IS 'The author citation of the quadrinomial taxon (e.g. form) in the ternary name of botanical nomenclature. In botanical nomenclature, author citation refers to citing the person or group of people who validly published a botanical name, i.e. who first published the name while fulfilling the formal requirements as specified by the International Code of Nomenclature for algae, fungi, and plants (ICN). In cases where a species is no longer in its original generic placement (i.e. a new combination of genus and specific epithet), both the author(s) of the original genus placement and those of the new combination are given (the former in parentheses). In botany, it is customary (though not obligatory) to abbreviate author names according to a recognized list of standard abbreviations.';
COMMENT ON COLUMN public."plant"."parents" IS 'In the case that a plant is a hybrid, gives the hybrid parents.';
COMMENT ON COLUMN public."plant"."state_and_province" IS 'The distribution of the plant within U.S. States, Territories, and Protectorates; Canada Provinces and Territories; Greenland (Denmark), and St. Pierre and Miquelon (France).';

-- public.state
COMMENT ON TABLE public."state" IS 'State data from the U.S. Census Bureau''s Current State and Equivalent National Dataset.';
COMMENT ON COLUMN public."state"."statefp" IS 'Current state Federal Information Processing Series (FIPS) code.';
COMMENT ON COLUMN public."state"."region" IS 'Current Census region code.';
COMMENT ON COLUMN public."state"."division" IS 'Current Census division code.';
COMMENT ON COLUMN public."state"."statens" IS 'Current state GNIS code.';
COMMENT ON COLUMN public."state"."stusps" IS 'Current United States Postal Service state abbreviation.';
COMMENT ON COLUMN public."state"."name" IS 'Current state name.';
COMMENT ON COLUMN public."state"."geom" IS 'The geometry for the state (not loaded by default).';

/*-----------
PUBLIC: Views
------------*/

-- public.gap_plot
COMMENT ON VIEW public."gap_plot" IS 'A view which calculates gap indicators at the plot/year level. Join to the public.point (plotkey) table and then to the public.site (sitekey) table for plot and site level metadata respectively.';
COMMENT ON COLUMN public."gap_plot"."plotkey" IS 'A unique alphanumeric string identifier for the plot.';
COMMENT ON COLUMN public."gap_plot"."survey_year" IS 'The year in which the survey was completed.';
COMMENT ON COLUMN public."gap_plot"."rectype" IS 'An identifier of the types of vegetation that could stop a gap for the collection instance. An instance with multiple rectypes will have multiple record sets per plot key and survey_year, one for each rectype.';
COMMENT ON COLUMN public."gap_plot"."reclbl" IS 'An identifier of the types of vegetation that could stop a gap for the collection instance. An instance with multiple rectypes will have multiple record sets per plot key and survey_year, one for each rectype.';
COMMENT ON COLUMN public."gap_plot"."rec_n" IS 'The number of line method collection instances summarized.';
COMMENT ON COLUMN public."gap_plot"."gap_n_mean" IS 'The mean number of gaps detected of all line method instances.';
COMMENT ON COLUMN public."gap_plot"."gap_n_sd" IS 'The sample standard deviation of the number of gaps detected of all line method instances.';
COMMENT ON COLUMN public."gap_plot"."gap_cm_mean" IS 'The mean gap size in centimeters of all line method instances.';
COMMENT ON COLUMN public."gap_plot"."gap_cm_sd" IS 'The sample standard deviation gap size in centimeters of all line method instances.';
COMMENT ON COLUMN public."gap_plot"."gap_pct_mean" IS 'The mean fractional percentage of the line that is made up of gaps across all line method instances.';
COMMENT ON COLUMN public."gap_plot"."gap_pct_sd" IS 'The sample standard deviation of the fractional percent of the line that is made up of gaps across all line method instances.';
COMMENT ON COLUMN public."gap_plot"."pct000_020cm" IS 'The fractional percent of total gaps that are made up of gaps > 0 cm and <= 20 cm.';
COMMENT ON COLUMN public."gap_plot"."pct021_050cm" IS 'The fractional percent of total gaps that are made up of gaps > 20 cm and <= 50 cm.';
COMMENT ON COLUMN public."gap_plot"."pct051_100cm" IS 'The fractional percent of total gaps that are made up of gaps > 50 cm and <= 100 cm.';
COMMENT ON COLUMN public."gap_plot"."pct101_200cm" IS 'The fractional percent of total gaps that are made up of gaps > 100 cm and <= 200 cm.';
COMMENT ON COLUMN public."gap_plot"."pct200cm_plus" IS 'The fractional percent of total gaps that are made up of gaps > 200 cm.';

-- public.method_species_regex
COMMENT ON VIEW public."method_species_regex" IS 'The is a view which produces fields similar to the public.plant_mod view, but sources its species codes from the data of method data that use species codes. Regex is used on these codes to separate the "unknown" portion of the plant codes from the base codes and attach growth habits ad durations based on those modified parts (e.g. AF for annual forb or PG for perennial grass). For undocumented field definitions, please see public.plant documentation.';
COMMENT ON COLUMN public."method_species_regex"."species_code" IS 'The USDA plant code sourced from either public.plant or from the method data.';
COMMENT ON COLUMN public."method_species_regex"."code_base" IS 'The portion of the species_code that matches an accepted_symbol in public.plants.';
COMMENT ON COLUMN public."method_species_regex"."unk_no" IS 'The numerical portion of the species_code that is generally appended to an accepted_symbol in method data which denotes that it is an unidentified subcategory of that code.';
COMMENT ON COLUMN public."method_species_regex"."duration_code" IS 'The duration matching the text portion of the species_code that regex matching has identified as either a prepended or appended duration code.';
COMMENT ON COLUMN public."method_species_regex"."growth_habit_code" IS 'The growth habit matching the text portion of the species_code that regex matching has identified as either a prepended or appended growth habit code.';

-- public.pintercept_plot
COMMENT ON VIEW public."pintercept_plot" IS 'A view which calculates line-point intercept indicators at the plot/year level. Join to the public.point (plotkey) table and then to the public.site (sitekey) table for plot and site level metadata respectively.';
COMMENT ON COLUMN public."pintercept_plot"."plotkey" IS 'A unique alphanumeric string identifier for the plot.';
COMMENT ON COLUMN public."pintercept_plot"."survey_year" IS 'The year in which the survey was completed.';
COMMENT ON COLUMN public."pintercept_plot"."hit_type" IS 'Text designating if the hit was a layer hit or nearby vegetation selected by growth habit.';
COMMENT ON COLUMN public."pintercept_plot"."hit_n" IS 'The number of line method collection instances of a particular hit/hit_type. This should be the same number of method instances for a plot/year as zero has been imputed for method instances within a plot/year where that hit/hit_type was missing before summarizing.';
COMMENT ON COLUMN public."pintercept_plot"."hit_pct" IS 'The mean fractional percentage of a particular hit/hit_type across line method collection instances (out of the total hits for an instance). Zero has been imputed for missing instance values before summarizing.';
COMMENT ON COLUMN public."pintercept_plot"."hit_pct_sd" IS 'The sample standard deviation of the fractional percentage of a particular hit/hit_type across line method collection instances (out of the total hits for an instance). Zero has been imputed for missing instance values before summarizing.';
COMMENT ON COLUMN public."pintercept_plot"."dead_n" IS 'The number of line method collection instances of a particular hit/hit_type which had dead data. May be less than total instances.';
COMMENT ON COLUMN public."pintercept_plot"."dead_pct" IS 'The mean fractional percentage of dead cover for a particular hit/hit_type across line method collection instances.';
COMMENT ON COLUMN public."pintercept_plot"."dead_pct_sd" IS 'The sample standard deviation of the fractional percentage of dead cover for a particular hit/hit_type across line method collection instances.';
COMMENT ON COLUMN public."pintercept_plot"."height_n" IS 'The number of line method collection instances of a particular hit/hit_type which had height data. May be less than total instances.';
COMMENT ON COLUMN public."pintercept_plot"."height_cm" IS 'The mean height in centimeters for a particular hit/hit_type across line method collection instances.';
COMMENT ON COLUMN public."pintercept_plot"."height_sd" IS 'The sample standard deviation of the height in centimeters for a particular hit/hit_type across line method collection instances.';

-- public.plant_mod
COMMENT ON VIEW public."plant_mod" IS 'This is a view which produces fields similar to the public.plant table, but adds a couple of calculated fields for convenience. For undocumented field definitions, please see public.plant documentation.';
COMMENT ON COLUMN public."plant_mod"."duration_first" IS 'The first duration in the comma/space delimited list given in a code''s duration field, which according to NRCS is supposed to be the most common duration found in floras referenced for the USDA PLANTS database.';
COMMENT ON COLUMN public."plant_mod"."growth_habit_first" IS 'The first growth habit in the comma/space delimited list given in a code''s growth_habit field, which according to NRCS is supposed to be the most common growth habit found in floras referenced for the USDA PLANTS database.';

-- public.plant_regex
COMMENT ON VIEW public."plant_regex" IS 'This is a view which produces identical fields to the public.plant_mod view, but adds a number of records. Essentially this is union of the public.plant table, the public.method_species_regex view, and a compilation of six digit plant family codes created from the public.plant table. Generally, this view will be the one used to join method calculations for species codes to their attributes instead of the public.plant table due to its inclusion of family codes and unknown plant codes oftentimes found in method data. Please see public.plant_mod documentation for field definitions.';


-- public.plantcensus_plot
COMMENT ON VIEW public."plantcensus_plot" IS 'A view which summarizes plant census species at the plot/year level. Join to the public.point (plotkey) table and then to the public.site (sitekey) table for plot and site level metadata respectively.';
COMMENT ON COLUMN public."plantcensus_plot"."plotkey" IS 'A unique alphanumeric string identifier for the plot.';
COMMENT ON COLUMN public."plantcensus_plot"."survey_year" IS 'The year in which the survey was completed.';
COMMENT ON COLUMN public."plantcensus_plot"."species_code" IS 'The NRCS plant code of a plant species.';
COMMENT ON COLUMN public."plantcensus_plot"."rec_n" IS 'The number of line method collection instances summarized.';
COMMENT ON COLUMN public."plantcensus_plot"."notes" IS 'Individual species codes notes, aggregated for each code and delimited by a semicolon then a space.';

-- public.plantdensity_plot
COMMENT ON VIEW public."plantdensity_plot" IS 'A view which summarizes plant density at the plot/year level. Join to the public.point (plotkey) table and then to the public.site (sitekey) table for plot and site level metadata respectively.';
COMMENT ON COLUMN public."plantdensity_plot"."plotkey" IS 'A unique alphanumeric string identifier for the plot.';
COMMENT ON COLUMN public."plantdensity_plot"."survey_year" IS 'The year in which the survey was completed.';
COMMENT ON COLUMN public."plantdensity_plot"."class_no" IS 'A number identifying a specific plant class definition.';
COMMENT ON COLUMN public."plantdensity_plot"."class_lbl" IS 'A label describing the class (e.g. seedlings, forbs, dead, etc.).';
COMMENT ON COLUMN public."plantdensity_plot"."species_code" IS 'The NRCS plant code of a plant species.';
COMMENT ON COLUMN public."plantdensity_plot"."rec_n" IS 'The number of line method collection instances summarized.';
COMMENT ON COLUMN public."plantdensity_plot"."density_ha_mean" IS 'The mean density in units of plants/hectare of a particular species code across line method collection instances. Zero has been imputed for missing instance values before summarizing.';
COMMENT ON COLUMN public."plantdensity_plot"."density_ha_sd" IS 'The sample standard deviation of the density in units of plants/hectare of a particular species code across line method collection instances. Zero has been imputed for missing instance values before summarizing.';

-- public.production_plot
COMMENT ON VIEW public."production_plot" IS 'A view which summarizes annual production at the plot/year level. Join to the public.point (plotkey) table and then to the public.site (sitekey) table for plot and site level metadata respectively.';
COMMENT ON COLUMN public."production_plot"."plotkey" IS 'A unique alphanumeric string identifier for the plot.';
COMMENT ON COLUMN public."production_plot"."survey_year" IS 'The year in which the survey was completed.';
COMMENT ON COLUMN public."production_plot"."species_code" IS 'The NRCS plant code of a plant species.';
COMMENT ON COLUMN public."production_plot"."rec_n" IS 'The number of line method collection instances summarized.';
COMMENT ON COLUMN public."production_plot"."prod_g_m2" IS 'The annual production in units of grams per square meter of a particular species code across line method collection instances. Zero has been imputed for missing instance values before summarizing.';
COMMENT ON COLUMN public."production_plot"."prod_sd" IS 'The sample standard deviation of the annual production in units of grams per square meter of a particular species code across line method collection instances. Zero has been imputed for missing instance values before summarizing.';

-- public.rangehealth_plot
COMMENT ON VIEW public."rangehealth_plot" IS 'Data from a Interpreting/Describing Indicators of Rangeland Health method collection instance, transformed to wide format, with plot and date metadata added.';
COMMENT ON COLUMN public."rangehealth_plot"."plotkey" IS 'A unique alphanumeric string identifier for the plot.';
COMMENT ON COLUMN public."rangehealth_plot"."survey_date" IS 'The date and time of the method collection instance.';
COMMENT ON COLUMN public."rangehealth_plot"."ecoid_std" IS 'The identifier of a particular ecological community. For NRCS ecological sites, ecological site MLRA, ecological site LRU, ecological site number and ecological site state FIPS alpha code. Primary key for this table, it differs from the ecoid field in that ecological site type (range, forest, etc.) has been stripped. Any underscores and trailing numerical identifiers, if present, are also stripped.';
COMMENT ON COLUMN public."rangehealth_plot"."reckey" IS 'A unique alphanumeric string identifier for the method collection instance.';
COMMENT ON COLUMN public."rangehealth_plot"."i1_r" IS 'The rating for indicator 1: Rills.';
COMMENT ON COLUMN public."rangehealth_plot"."i1_r" IS 'The rating for indicator 1: Rills.';
COMMENT ON COLUMN public."rangehealth_plot"."i2_wfp" IS 'The rating for indicator 2: Water Flow Patterns.';
COMMENT ON COLUMN public."rangehealth_plot"."i3_pt" IS 'The rating for indicator 3: Pedestals and/or Terracettes.';
COMMENT ON COLUMN public."rangehealth_plot"."i4_bg" IS 'The rating for indicator 4: Bare Ground.';
COMMENT ON COLUMN public."rangehealth_plot"."i5_g" IS 'The rating for indicator 5: Gullies.';
COMMENT ON COLUMN public."rangehealth_plot"."i6_wsbda" IS 'The rating for indicator 6: Wind-Scoured and/or Depositional Areas.';
COMMENT ON COLUMN public."rangehealth_plot"."i7_lm" IS 'The rating for indicator 7: Litter Movement (Wind or Water).';
COMMENT ON COLUMN public."rangehealth_plot"."i8_ssre" IS 'The rating for indicator 8: Soil Surface Resistance to Erosion.';
COMMENT ON COLUMN public."rangehealth_plot"."i9_ssld" IS 'The rating for indicator 9: Soil Surface Loss and Degradation.';
COMMENT ON COLUMN public."rangehealth_plot"."i10_pccdrir" IS 'The rating for indicator 10: Effects of Plant Community Composition and Distribution on Infiltration.';
COMMENT ON COLUMN public."rangehealth_plot"."i11_cl" IS 'The rating for indicator 11: Compaction Layer.';
COMMENT ON COLUMN public."rangehealth_plot"."i12_fsg" IS 'The rating for indicator 12: Functional/Structural (F/S) Groups.';
COMMENT ON COLUMN public."rangehealth_plot"."i13_pmd" IS 'The rating for indicator 13: Plant Mortality and Decadence (Dead or Dying Plants or Plant Parts).';
COMMENT ON COLUMN public."rangehealth_plot"."i14_la" IS 'The rating for indicator 14: Litter Amount (Litter Cover and Depth).';
COMMENT ON COLUMN public."rangehealth_plot"."i15_ap" IS 'The rating for indicator 15: Annual Production.';
COMMENT ON COLUMN public."rangehealth_plot"."i16_ip" IS 'The rating for indicator 16: Invasive Plants.';
COMMENT ON COLUMN public."rangehealth_plot"."i17_rcpp" IS 'The rating for indicator 17: Reproductive Capability of Perennial Plants (Vigor with an Emphasis on).';
COMMENT ON COLUMN public."rangehealth_plot"."a1_sss" IS 'The rating for attribute 1: Soil/Site Stability.';
COMMENT ON COLUMN public."rangehealth_plot"."a2_hf" IS 'The rating for attribute 2: Hydrologic Function.';
COMMENT ON COLUMN public."rangehealth_plot"."a3_bi" IS 'The rating for attribute 3: Biotic Integrity.';

-- public.shrubshape_plot
COMMENT ON VIEW public."shrubshape_plot" IS 'A view which summarizes shrub shape data (from the line-point intercept method) at the plot/year level. Join to the public.point (plotkey) table and then to the public.site (sitekey) table for plot and site level metadata respectively.';
COMMENT ON COLUMN public."shrubshape_plot"."plotkey" IS 'A unique alphanumeric string identifier for the plot.';
COMMENT ON COLUMN public."shrubshape_plot"."survey_year" IS 'The year in which the survey was completed.';
COMMENT ON COLUMN public."shrubshape_plot"."shape" IS 'The categorical shape of the shrub being measured.';
COMMENT ON COLUMN public."shrubshape_plot"."species_code" IS 'The NRCS plant code of a plant species.';
COMMENT ON COLUMN public."shrubshape_plot"."rec_n" IS 'The number of line method collection instances summarized. Can be fewer than number of total instances.';
COMMENT ON COLUMN public."shrubshape_plot"."shape_pct_mean" IS 'The fractional mean of a particular species code/shape across line method collection instances.';
COMMENT ON COLUMN public."shrubshape_plot"."shape_pct_sd" IS 'The sample standard deviation of the fractional mean of a particular species code/shape across line method collection instances.';

-- public.soilstability_plot
COMMENT ON VIEW public."soilstability_plot" IS 'A view which summarizes soil stability data at the plot/year level. Join to the public.point (plotkey) table and then to the public.site (sitekey) table for plot and site level metadata respectively.';
COMMENT ON COLUMN public."soilstability_plot"."plotkey" IS 'A unique alphanumeric string identifier for the plot.';
COMMENT ON COLUMN public."soilstability_plot"."survey_year" IS 'The year in which the survey was completed.';
COMMENT ON COLUMN public."soilstability_plot"."rectype" IS 'What types of peds were sampled.';
COMMENT ON COLUMN public."soilstability_plot"."rec_n" IS 'The number of plot method collection instances summarized.';
COMMENT ON COLUMN public."soilstability_plot"."rating_mean" IS 'The mean rating of a particular veg code across plot method collection instances.';
COMMENT ON COLUMN public."soilstability_plot"."rating_sd" IS 'The sample standard deviation of the rating of a particular veg code across plot method collection instances.';
