CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA public;
COMMENT ON EXTENSION postgis IS 'PostGIS geometry, geography, and raster spatial types and functions';
CREATE EXTENSION IF NOT EXISTS tablefunc WITH SCHEMA public;
COMMENT ON EXTENSION tablefunc IS 'functions that manipulate whole tables, including crosstab';