-- Level 0, no foreign key
DROP TABLE IF EXISTS public.plant CASCADE; 
CREATE TABLE public.plant (
    accepted_symbol character varying(7) PRIMARY KEY,
    code_type character varying(20),
    scientific_name character varying(100),
    common_name character varying(100),
    family character varying(50),
    duration character varying(50),
    growth_habit character varying(50),
    native_status character varying(100),
    hybrid_genus_indicator character varying(1),
    genus character varying(30),
    hybrid_species_indicator character varying(1),
    species character varying(50),
    subspecies_prefix character varying(4),
    hybrid_subspecies_indicator character varying(1),
    subspecies character varying(30),
    variety_prefix character varying(4),
    hybrid_variety_indicator character varying(1),
    variety character varying(30),
    forma_prefix character varying(2),
    forma character varying(30),
    genera_binomial_author character varying(100),
    trinomial_author character varying(100),
    quadranomial_author character varying(100),
    parents character varying(100),
    state_and_province character varying(500),
    subvariety_prefix character varying(7),
    subvariety character varying(30),
    questionable_taxon_indicator character varying(1)
);

DROP TABLE IF EXISTS public.ecosite CASCADE; 
CREATE TABLE public.ecosite (
    ecoid_std character varying(10) PRIMARY KEY,
    ecoid character varying(11) NOT NULL,
    econame character varying(200),
    type character varying(1),
    mlra integer,
    mlra_sub character varying(1),
    lru character varying(1),
    site_no integer,
    state character varying(2),
    ecogroup character varying(20),
    ecogroup_type character varying(50)
);

DROP TABLE IF EXISTS public.plant_nativestatus CASCADE; 
CREATE TABLE public.plant_nativestatus (
    code character varying(3) PRIMARY KEY,
    code_type character varying(15),
    code_desc character varying(100)
);

DROP TABLE IF EXISTS public.state CASCADE;
CREATE TABLE public.state (
    statefp character varying(2) PRIMARY KEY,
    region character varying(2) NOT NULL,
    division character varying(2) NOT NULL,
    statens character varying(8) NOT NULL,
    stusps character varying(2) NOT NULL,
    name character varying(100) NOT NULL,
    geom geometry(MULTIPOLYGON, 4269)
);
CREATE INDEX IF NOT EXISTS state_geom_gix ON public.state USING gist (geom);

-- Level 1, references lvl 0
DROP TABLE IF EXISTS public.county CASCADE;
CREATE TABLE public.county (
    geoid character varying(5) PRIMARY KEY,
    statefp character varying(2) NOT NULL,
    countyfp character varying(3) NOT NULL,
    countyns character varying(8) NOT NULL,
    name character varying(100) NOT NULL,
    namelsad character varying(100) NOT NULL,
    lsad character varying(2) NOT NULL,
    geom geometry(MULTIPOLYGON, 4269),  --can fill geometery later if need be
    FOREIGN KEY (statefp) REFERENCES public.state(statefp) ON UPDATE CASCADE ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS county_geom_gix ON public.county USING gist (geom);