/* Schema created from DIMA 5.5 template. */

-- Check for deleted linekey elsewhere in db_line and if it not longer exists in the table, 
-- delete that linekey from tblLines. Provides a way to remove specific databases after loading. 
DROP TRIGGER IF EXISTS line_delete ON dima.db_line;
DROP FUNCTION IF EXISTS dima.check_line();
CREATE OR REPLACE FUNCTION dima.check_line()
  RETURNS TRIGGER 
  LANGUAGE PLPGSQL
  AS
$BODY$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM dima.db_line WHERE "LineKey" = OLD."LineKey") THEN
         DELETE FROM dima."tblLines" WHERE "LineKey" = OLD."LineKey";
    END IF;
    RETURN NULL;
END;
$BODY$;

CREATE TRIGGER line_delete
  AFTER DELETE
  ON dima.db_line
  FOR EACH ROW
  EXECUTE PROCEDURE dima.check_line();
  
-- Check for deleted plotkey elsewhere in db_plot and if it not longer exists in the table, 
-- delete that plotkey from tblPlots. Provides a way to remove specific databases after loading. 
DROP TRIGGER IF EXISTS plot_delete ON dima.db_plot;
DROP FUNCTION IF EXISTS dima.check_plot();
CREATE OR REPLACE FUNCTION dima.check_plot()
  RETURNS TRIGGER 
  LANGUAGE PLPGSQL
  AS
$BODY$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM dima.db_plot WHERE "PlotKey" = OLD."PlotKey") THEN
         DELETE FROM dima."tblPlots" WHERE "PlotKey" = OLD."PlotKey";
    END IF;
    RETURN NULL;
END;
$BODY$;

CREATE TRIGGER plot_delete
  AFTER DELETE
  ON dima.db_plot
  FOR EACH ROW
  EXECUTE PROCEDURE dima.check_plot();
  
-- Check for deleted sitekey elsewhere in db_site and if it not longer exists in the table, 
-- delete that sitekey from tblSites. Provides a way to remove specific databases after loading.
DROP TRIGGER IF EXISTS site_delete ON dima.db_site;
DROP FUNCTION IF EXISTS dima.check_site();
CREATE OR REPLACE FUNCTION dima.check_site()
  RETURNS TRIGGER 
  LANGUAGE PLPGSQL
  AS
$BODY$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM dima.db_site WHERE "SiteKey" = OLD."SiteKey") THEN
         DELETE FROM dima."tblSites" WHERE "SiteKey" = OLD."SiteKey";
    END IF;
    RETURN NULL;
END;
$BODY$;

CREATE TRIGGER site_delete
  AFTER DELETE
  ON dima.db_site
  FOR EACH ROW
  EXECUTE PROCEDURE dima.check_site();
  
-- tblSpecies trigger
-- Keeps track of changes to the base tblSpecies by individual DIMAs without creating millions of duplicate records
-- using a delta table strategy. Requires that tblSpecies be pre-populated before first data load, otherwise first data load will
-- be used to populate base table (must implemented in code base). Delta table is only inserted on if the new values are not null and
-- different, or if any of the new values are not null when they are null in the base table.

DROP TABLE IF EXISTS dima."tblSpecies_delta" CASCADE;

CREATE TABLE dima."tblSpecies_delta" (
  dbkey VARCHAR(255) NOT NULL, 
  "SpeciesCode" VARCHAR(255) NOT NULL, 
  "ScientificName" VARCHAR(255), 
  "CommonName" VARCHAR(255), 
  "Family" VARCHAR(255), 
  "SortSeq" INTEGER DEFAULT 99, 
  "synonymOf" VARCHAR(255), 
  "GrowthHabitCode" VARCHAR(20), 
  "Duration" VARCHAR(20), 
  "Stabilizing" BOOLEAN, 
  "Invasive" BOOLEAN, 
  "Group" VARCHAR(30), 
  PRIMARY KEY (dbkey, "SpeciesCode"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);
  
DROP TRIGGER IF EXISTS species_insert ON dima."tblSpecies";
DROP FUNCTION IF EXISTS dima.check_species();
CREATE OR REPLACE FUNCTION dima.check_species()
  RETURNS TRIGGER 
  LANGUAGE PLPGSQL
  AS
$BODY$
DECLARE 
  existing record;
  inserting record;
  do_insert BOOLEAN := False;
  icount integer;
BEGIN
    IF EXISTS (SELECT 1 FROM dima."tblSpecies" WHERE "SpeciesCode" = NEW."SpeciesCode") THEN
        SELECT "SpeciesCode", "ScientificName", "CommonName", "Family", "synonymOf", "GrowthHabitCode", 
               "Duration", "Stabilizing", "Invasive", "Group" INTO existing FROM dima."tblSpecies" 
         WHERE "SpeciesCode" = NEW."SpeciesCode";
        SELECT NEW."SpeciesCode", NEW."ScientificName", NEW."CommonName", NEW."Family", NEW."synonymOf", NEW."GrowthHabitCode", 
               NEW."Duration", NEW."Stabilizing", NEW."Invasive", NEW."Group" INTO inserting;
        IF existing IS DISTINCT FROM inserting THEN
            IF existing."ScientificName" != inserting."ScientificName" OR (existing."ScientificName" IS NULL AND inserting."ScientificName" IS NOT NULL) OR
               existing."CommonName" != inserting."CommonName" OR (existing."CommonName" IS NULL AND inserting."CommonName" IS NOT NULL) OR
               existing."Family" != inserting."Family" OR (existing."Family" IS NULL AND inserting."Family" IS NOT NULL) OR
               existing."synonymOf" != inserting."synonymOf" OR (existing."synonymOf" IS NULL AND inserting."synonymOf" IS NOT NULL) OR
               existing."GrowthHabitCode" != inserting."GrowthHabitCode" OR (existing."GrowthHabitCode" IS NULL AND inserting."GrowthHabitCode" IS NOT NULL) OR
               existing."Duration" != inserting."Duration" OR (existing."Duration" IS NULL AND inserting."Duration" IS NOT NULL) OR
               existing."Stabilizing" != inserting."Stabilizing" OR (existing."Stabilizing" IS NULL AND inserting."Stabilizing" IS NOT NULL) OR
               existing."Invasive" != inserting."Invasive" OR (existing."Invasive" IS NULL AND inserting."Invasive" IS NOT NULL) OR
               existing."Group" != inserting."Group" OR (existing."Group" IS NULL AND inserting."Group" IS NOT NULL) THEN
                do_insert := True;
            END IF;
        END IF;
    ELSE
        do_insert := True;
    END IF;
    IF do_insert = True THEN
        INSERT INTO dima."tblSpecies_delta" (dbkey, "SpeciesCode", "ScientificName", "CommonName", "Family", "synonymOf", 
                                                 "GrowthHabitCode", "Duration", "Stabilizing", "Invasive", "Group") 
             VALUES (NEW.dbkey, NEW."SpeciesCode", NEW."ScientificName", NEW."CommonName", NEW."Family", NEW."synonymOf", 
                     NEW."GrowthHabitCode", NEW."Duration", NEW."Stabilizing", NEW."Invasive", NEW."Group")
         ON CONFLICT DO NOTHING;
        GET DIAGNOSTICS icount = ROW_COUNT;
    ELSE
       icount = 0;
       --RAISE NOTICE 'Inserted % records into tblSpecies_delta', icount;
    END IF;
    RETURN NULL;
END;
$BODY$;

CREATE TRIGGER species_insert
  BEFORE INSERT
  ON dima."tblSpecies"
  FOR EACH ROW
  EXECUTE PROCEDURE dima.check_species();
  
-- tblSpeciesGeneric trigger
-- Similar to the tblSpecies trigger, but simpler, only checking if all values that are not dbkey, SortSeq, and DateModified
-- are DISTINCT from existing values in the base table.  Any differences in these fields results in an insert into the delta table.
--
DROP TABLE IF EXISTS dima."tblSpeciesGeneric_delta" CASCADE;

CREATE TABLE dima."tblSpeciesGeneric_delta" (
  dbkey VARCHAR(255),
  "SpeciesCode" VARCHAR(255) NOT NULL, 
  "ScientificName" VARCHAR(255), 
  "CommonName" VARCHAR(255), 
  "Family" VARCHAR(255), 
  "SortSeq" INTEGER, 
  "synonymOf" VARCHAR(255), 
  "GrowthHabitCode" VARCHAR(255), 
  "Duration" VARCHAR(255), 
  "Stabilizing" BOOLEAN, 
  "Group" VARCHAR(255), 
  "DateModified" TIMESTAMP, 
  "DateFound" TIMESTAMP, 
  "PlotFirstFound" VARCHAR(255), 
  "PotentialGenus" VARCHAR(255), 
  "PotenialSpecies" VARCHAR(255), 
  "PotentialSubspecies" VARCHAR(255), 
  "Comments" VARCHAR(255), 
  "PhotosTaken" BOOLEAN, 
  "PhotoNumbers" VARCHAR(255), 
  "SpecimenCollected" BOOLEAN, 
  "IdentifiedTo" VARCHAR(255), 
  "FinalCode" VARCHAR(255), 
  "ChangedInDIMA" BOOLEAN, 
  "petalsNumber" INTEGER, 
  "petalsColor" VARCHAR(255), 
  "petalsSize" VARCHAR(255), 
  "sepalsNumber" INTEGER, 
  "sepalsColor" VARCHAR(255), 
  "sepalsSize" VARCHAR(255), 
  "stamensNumber" INTEGER, 
  "stamensColor" VARCHAR(255), 
  "stamensSize" VARCHAR(255), 
  "glumesPresent" BOOLEAN, 
  "ligulesPresent" BOOLEAN, 
  PRIMARY KEY (dbkey, "SpeciesCode"),
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

DROP TRIGGER IF EXISTS generic_insert ON dima."tblSpeciesGeneric";
DROP FUNCTION IF EXISTS dima.check_generic();
CREATE OR REPLACE FUNCTION dima.check_generic()
  RETURNS TRIGGER 
  LANGUAGE PLPGSQL
  AS
$BODY$
DECLARE 
  existing record;
  inserting record;
  do_insert BOOLEAN := False;
  icount integer;
BEGIN
    IF EXISTS (SELECT 1 FROM dima."tblSpeciesGeneric" WHERE "SpeciesCode" = NEW."SpeciesCode") THEN
        SELECT "SpeciesCode", "ScientificName", "CommonName", "Family", "synonymOf", "GrowthHabitCode", "Duration", 
               "Stabilizing", "Group", "DateFound", "PlotFirstFound", "PotentialGenus", "PotenialSpecies", "PotentialSubspecies", 
               "Comments", "PhotosTaken", "PhotoNumbers", "SpecimenCollected", "IdentifiedTo", "FinalCode", "ChangedInDIMA", 
               "petalsNumber", "petalsColor", "petalsSize", "sepalsNumber", "sepalsColor", "sepalsSize", "stamensNumber", 
               "stamensColor", "stamensSize", "glumesPresent", "ligulesPresent"
          INTO existing FROM dima."tblSpeciesGeneric" 
         WHERE "SpeciesCode" = NEW."SpeciesCode";
        SELECT NEW."SpeciesCode", NEW."ScientificName", NEW."CommonName", NEW."Family", NEW."synonymOf", NEW."GrowthHabitCode", NEW."Duration", 
               NEW."Stabilizing", NEW."Group", NEW."DateFound", NEW."PlotFirstFound", NEW."PotentialGenus", NEW."PotenialSpecies", NEW."PotentialSubspecies", 
               NEW."Comments", NEW."PhotosTaken", NEW."PhotoNumbers", NEW."SpecimenCollected", NEW."IdentifiedTo", NEW."FinalCode", NEW."ChangedInDIMA", 
               NEW."petalsNumber", NEW."petalsColor", NEW."petalsSize", NEW."sepalsNumber", NEW."sepalsColor", NEW."sepalsSize", NEW."stamensNumber", 
               NEW."stamensColor", NEW."stamensSize", NEW."glumesPresent", NEW."ligulesPresent" INTO inserting;
        IF existing IS DISTINCT FROM inserting THEN
            do_insert := True;
        END IF;
    ELSE
        do_insert := True;
    END IF;
    IF do_insert = True THEN
        INSERT INTO dima."tblSpeciesGeneric_delta" 
        (dbkey, "SpeciesCode", "ScientificName", "CommonName", "Family", "SortSeq", "synonymOf", "GrowthHabitCode", "Duration", "Stabilizing", 
        "Group", "DateModified", "DateFound", "PlotFirstFound", "PotentialGenus", "PotenialSpecies", "PotentialSubspecies", "Comments", 
        "PhotosTaken", "PhotoNumbers", "SpecimenCollected", "IdentifiedTo", "FinalCode", "ChangedInDIMA", "petalsNumber", "petalsColor", 
        "petalsSize", "sepalsNumber", "sepalsColor", "sepalsSize", "stamensNumber", "stamensColor", "stamensSize", "glumesPresent", "ligulesPresent")        
         VALUES 
         (NEW.dbkey, NEW."SpeciesCode", NEW."ScientificName", NEW."CommonName", NEW."Family", NEW."SortSeq", NEW."synonymOf", NEW."GrowthHabitCode", 
         NEW."Duration", NEW."Stabilizing", NEW."Group", NEW."DateModified", NEW."DateFound", NEW."PlotFirstFound", NEW."PotentialGenus", 
         NEW."PotenialSpecies", NEW."PotentialSubspecies", NEW."Comments", NEW."PhotosTaken", NEW."PhotoNumbers", NEW."SpecimenCollected", 
         NEW."IdentifiedTo", NEW."FinalCode", NEW."ChangedInDIMA", NEW."petalsNumber", NEW."petalsColor", NEW."petalsSize", NEW."sepalsNumber", 
         NEW."sepalsColor", NEW."sepalsSize", NEW."stamensNumber", NEW."stamensColor", NEW."stamensSize", NEW."glumesPresent", NEW."ligulesPresent") 
         ON CONFLICT DO NOTHING;
         GET DIAGNOSTICS icount = ROW_COUNT;
    ELSE
       icount = 0;
       --RAISE NOTICE 'Inserted % records into tblSpeciesGeneric_delta', icount;
    END IF;
    RETURN NULL;
END;
$BODY$;

CREATE TRIGGER generic_insert
  BEFORE INSERT
  ON dima."tblSpeciesGeneric"
  FOR EACH ROW
  EXECUTE PROCEDURE dima.check_generic();
  

DROP TABLE IF EXISTS dima."tblEcolSites_delta" CASCADE;

CREATE TABLE dima."tblEcolSites_delta" (
  dbkey VARCHAR(255) NOT NULL,
  "EcolSite" VARCHAR(50), 
  "SiteName" VARCHAR(255), 
  "DateModified" TIMESTAMP, 
  "DateComplete" TIMESTAMP, 
  "SSS1" FLOAT NULL DEFAULT 1, 
  "HF1" FLOAT NULL DEFAULT 1, 
  "BI1" FLOAT NULL DEFAULT 0, 
  "SSS2" FLOAT NULL DEFAULT 1, 
  "HF2" FLOAT NULL DEFAULT 1, 
  "BI2" FLOAT NULL DEFAULT 0, 
  "SSS3" FLOAT NULL DEFAULT 1, 
  "HF3" FLOAT NULL DEFAULT 1, 
  "BI3" FLOAT NULL DEFAULT 0, 
  "SSS4" FLOAT NULL DEFAULT 1, 
  "HF4" FLOAT NULL DEFAULT 1, 
  "BI4" FLOAT NULL DEFAULT 0, 
  "SSS5" FLOAT NULL DEFAULT 1, 
  "HF5" FLOAT NULL DEFAULT 1, 
  "BI5" FLOAT NULL DEFAULT 0, 
  "SSS6" FLOAT NULL DEFAULT 1, 
  "HF6" FLOAT NULL DEFAULT 0, 
  "BI6" FLOAT NULL DEFAULT 0, 
  "SSS7" FLOAT NULL DEFAULT 1, 
  "HF7" FLOAT NULL DEFAULT 0, 
  "BI7" FLOAT NULL DEFAULT 0, 
  "SSS8" FLOAT NULL DEFAULT 1, 
  "HF8" FLOAT NULL DEFAULT 1, 
  "BI8" FLOAT NULL DEFAULT 1, 
  "SSS9" FLOAT NULL DEFAULT 1, 
  "HF9" FLOAT NULL DEFAULT 1, 
  "BI9" FLOAT NULL DEFAULT 1, 
  "SSS10" FLOAT NULL DEFAULT 0, 
  "HF10" FLOAT NULL DEFAULT 1, 
  "BI10" FLOAT NULL DEFAULT 0, 
  "SSS11" FLOAT NULL DEFAULT 1, 
  "HF11" FLOAT NULL DEFAULT 1, 
  "BI11" FLOAT NULL DEFAULT 1, 
  "SSS12" FLOAT NULL DEFAULT 0, 
  "HF12" FLOAT NULL DEFAULT 0, 
  "BI12" FLOAT NULL DEFAULT 1, 
  "SSS13" FLOAT NULL DEFAULT 0, 
  "HF13" FLOAT NULL DEFAULT 0, 
  "BI13" FLOAT NULL DEFAULT 1, 
  "SSS14" FLOAT NULL DEFAULT 0, 
  "HF14" FLOAT NULL DEFAULT 1, 
  "BI14" FLOAT NULL DEFAULT 1, 
  "SSS15" FLOAT NULL DEFAULT 0, 
  "HF15" FLOAT NULL DEFAULT 0, 
  "BI15" FLOAT NULL DEFAULT 1, 
  "SSS16" FLOAT NULL DEFAULT 0, 
  "HF16" FLOAT NULL DEFAULT 0, 
  "BI16" FLOAT NULL DEFAULT 1, 
  "SSS17" FLOAT NULL DEFAULT 0, 
  "HF17" FLOAT NULL DEFAULT 0, 
  "BI17" FLOAT NULL DEFAULT 1, 
  "SSS18" FLOAT NULL DEFAULT 0, 
  "HF18" FLOAT NULL DEFAULT 0, 
  "BI18" FLOAT NULL DEFAULT 0, 
  "SSS19" FLOAT NULL DEFAULT 0, 
  "HF19" FLOAT NULL DEFAULT 0, 
  "BI19" FLOAT NULL DEFAULT 0,
  PRIMARY KEY (dbkey, "EcolSite"),  -- added
  FOREIGN KEY (dbkey) REFERENCES dima.db(dbkey) ON UPDATE CASCADE ON DELETE CASCADE
);

 
DROP TRIGGER IF EXISTS eco_insert ON dima."tblEcolSites";
DROP FUNCTION IF EXISTS dima.check_eco();
CREATE OR REPLACE FUNCTION dima.check_eco()
  RETURNS TRIGGER 
  LANGUAGE PLPGSQL
  AS
$BODY$
DECLARE 
  existing record;
  inserting record;
  do_insert BOOLEAN := False;
BEGIN
    IF EXISTS (SELECT 1 FROM dima."tblEcolSites" WHERE "EcolSite" = NEW."EcolSite") THEN
        SELECT "EcolSite", "SiteName", "SSS1", "HF1", "BI1", "SSS2", "HF2", "BI2", "SSS3", "HF3", "BI3", "SSS4", 
               "HF4", "BI4", "SSS5", "HF5", "BI5", "SSS6", "HF6", "BI6", "SSS7", "HF7", "BI7", "SSS8", "HF8", "BI8", "SSS9", "HF9", "BI9", "SSS10", "HF10", 
               "BI10", "SSS11", "HF11", "BI11", "SSS12", "HF12", "BI12", "SSS13", "HF13", "BI13", "SSS14", "HF14", "BI14", "SSS15", "HF15", "BI15", "SSS16", 
               "HF16", "BI16", "SSS17", "HF17", "BI17", "SSS18", "HF18", "BI18", "SSS19", "HF19", "BI19"
          INTO existing FROM dima."tblEcolSites"
         WHERE "EcolSite" = NEW."EcolSite";
        SELECT NEW."EcolSite", NEW."SiteName", NEW."SSS1", NEW."HF1", NEW."BI1", NEW."SSS2", NEW."HF2", NEW."BI2", NEW."SSS3", NEW."HF3", NEW."BI3", NEW."SSS4", 
               NEW."HF4", NEW."BI4", NEW."SSS5", NEW."HF5", NEW."BI5", NEW."SSS6", NEW."HF6", NEW."BI6", NEW."SSS7", NEW."HF7", NEW."BI7", NEW."SSS8", NEW."HF8", 
               NEW."BI8", NEW."SSS9", NEW."HF9", NEW."BI9", NEW."SSS10", NEW."HF10", NEW."BI10", NEW."SSS11", NEW."HF11", NEW."BI11", NEW."SSS12", NEW."HF12", 
               NEW."BI12", NEW."SSS13", NEW."HF13", NEW."BI13", NEW."SSS14", NEW."HF14", NEW."BI14", NEW."SSS15", NEW."HF15", NEW."BI15", NEW."SSS16", NEW."HF16", 
               NEW."BI16", NEW."SSS17", NEW."HF17", NEW."BI17", NEW."SSS18", NEW."HF18", NEW."BI18", NEW."SSS19", NEW."HF19", NEW."BI19" 
               INTO inserting;
        IF existing IS DISTINCT FROM inserting THEN
            do_insert := True;
        END IF;
    ELSE
        do_insert := True;
    END IF;
    IF do_insert = True THEN
        INSERT INTO dima."tblEcolSites_delta" 
        (dbkey, "EcolSite", "SiteName", "DateModified", "DateComplete", "SSS1", "HF1", "BI1", "SSS2", "HF2", "BI2", "SSS3", "HF3", "BI3", "SSS4", 
         "HF4", "BI4", "SSS5", "HF5", "BI5", "SSS6", "HF6", "BI6", "SSS7", "HF7", "BI7", "SSS8", "HF8", "BI8", "SSS9", "HF9", "BI9", "SSS10", "HF10", 
         "BI10", "SSS11", "HF11", "BI11", "SSS12", "HF12", "BI12", "SSS13", "HF13", "BI13", "SSS14", "HF14", "BI14", "SSS15", "HF15", "BI15", "SSS16", 
         "HF16", "BI16", "SSS17", "HF17", "BI17", "SSS18", "HF18", "BI18", "SSS19", "HF19", "BI19")        
         VALUES 
         (NEW.dbkey, NEW."EcolSite", NEW."SiteName", NEW."DateModified", NEW."DateComplete", NEW."SSS1", NEW."HF1", NEW."BI1", NEW."SSS2", NEW."HF2", NEW."BI2", 
         NEW."SSS3", NEW."HF3", NEW."BI3", NEW."SSS4", NEW."HF4", NEW."BI4", NEW."SSS5", NEW."HF5", NEW."BI5", NEW."SSS6", NEW."HF6", NEW."BI6", NEW."SSS7", NEW."HF7", 
         NEW."BI7", NEW."SSS8", NEW."HF8", NEW."BI8", NEW."SSS9", NEW."HF9", NEW."BI9", NEW."SSS10", NEW."HF10", NEW."BI10", NEW."SSS11", NEW."HF11", NEW."BI11", 
         NEW."SSS12", NEW."HF12", NEW."BI12", NEW."SSS13", NEW."HF13", NEW."BI13", NEW."SSS14", NEW."HF14", NEW."BI14", NEW."SSS15", NEW."HF15", NEW."BI15", NEW."SSS16", 
         NEW."HF16", NEW."BI16", NEW."SSS17", NEW."HF17", NEW."BI17", NEW."SSS18", NEW."HF18", NEW."BI18", NEW."SSS19", NEW."HF19", NEW."BI19")
         ON CONFLICT DO NOTHING;
    END IF;
    RETURN NULL;
END;
$BODY$;

CREATE TRIGGER eco_insert
  BEFORE INSERT
  ON dima."tblEcolSites"
  FOR EACH ROW
  EXECUTE PROCEDURE dima.check_eco();
  