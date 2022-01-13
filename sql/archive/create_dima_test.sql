/* Schema created from DIMA 5.5 template. */

INSERT INTO db (dbkey, md5hash) VALUES ('a', 'aaaaaa'), ('b', 'bbbbbb') ON CONFLICT DO NOTHING;

INSERT INTO db_site (dbkey, "SiteKey") VALUES 
('a', '1'),
('b', '1'),
('b', '2') 
ON CONFLICT DO NOTHING;

INSERT INTO "tblSites" ("SiteKey", "SiteName") VALUES 
('1', 'Site 1'),
('2', 'Site 2'),
('1', 'Site 2') 
ON CONFLICT DO NOTHING;

INSERT INTO db_plot (dbkey, "PlotKey") VALUES 
('a', '11'),
('b', '11'),
('b', '22') 
ON CONFLICT DO NOTHING;

INSERT INTO "tblPlots" ("SiteKey", "PlotKey", "PlotID") VALUES 
('1', '11', 'Plot 1'),
('1', '11', 'Plot 1'),
('2', '22', 'Plot 2'),
('2', '33', 'Plot 3') 
ON CONFLICT DO NOTHING;

INSERT INTO db_line (dbkey, "LineKey") VALUES 
('a', '111'),
('b', '111'),
('b', '222'),
('b', '333') 
ON CONFLICT DO NOTHING;

INSERT INTO "tblLines" ("PlotKey", "LineKey", "LineID") VALUES 
('11', '111', 'Line 1'),
('11', '111', 'Line 1'),
('22', '222', 'Line 2'),
('33', '333', 'Line 3') 
ON CONFLICT DO NOTHING;

INSERT INTO "tblLPIHeader" (dbkey, "LineKey", "RecKey") VALUES
('a', '111', '1111'),
('b', '222', '2222'),
('b', '333', '3333'),
('b', '111', '1111') 
ON CONFLICT DO NOTHING;

INSERT INTO "tblLPIDetail" ("RecKey", "PointLoc", "PointNbr", "TopCanopy") VALUES
('1111', 1, 1, "Top"),
('2222', 1, 1, "Top"),
('3333', 1, 1, "Top"),
('1111', 2, 2, "Top2") 
ON CONFLICT DO NOTHING;

/*
ALTER TABLE "tblSpecies" DISABLE TRIGGER species_insert;
INSERT INTO "tblSpecies" (dbkey, "SpeciesCode", "ScientificName", "Duration", "Invasive") VALUES 
('base', 'AAAA', 'Species A', 'Annual', False),
('base', 'BBBB', 'Species B', 'Perennial', False),
('base', 'CCCC', 'Species C', 'Perennial', True)
ON CONFLICT DO NOTHING;
ALTER TABLE "tblSpecies" ENABLE TRIGGER species_insert;
*/

INSERT INTO "tblSpecies" (dbkey, "SpeciesCode", "ScientificName", "Duration", "Invasive") VALUES 
('a', 'AAAA', NULL, 'Annual', False),
('a', 'BBBB', 'Species B', 'Perennial', False),
('a', 'CCCC', 'Species C', 'Perennial', False),
('b', 'AAAA', 'Species A', 'Annual', False),
('b', 'BBBB', 'Species B', 'Annual', False),
('b', 'CCCC', 'Species C', NULL, False),
('b', 'DDDD', 'Species D', 'Biennial', False)
ON CONFLICT DO NOTHING;

INSERT INTO "tblSpeciesGeneric" (dbkey, "SpeciesCode", "ScientificName", "CommonName", "Family", "SortSeq", "synonymOf", "GrowthHabitCode", "Duration", "Stabilizing", "Group", "DateModified", "DateFound", "PlotFirstFound", "PotentialGenus", 
								 "PotenialSpecies", "PotentialSubspecies", "Comments", "PhotosTaken", "PhotoNumbers", "SpecimenCollected", "IdentifiedTo", "FinalCode", "ChangedInDIMA", "petalsNumber", "petalsColor", "petalsSize", "sepalsNumber", 
								 "sepalsColor", "sepalsSize", "stamensNumber", "stamensColor", "stamensSize", "glumesPresent", "ligulesPresent") VALUES 
('a', E'AF01', E'Annual Forb', E'Annual Forb', E'', 99, E'', E'5', E'Annual', E'0', E'', '2000-01-01 00:01:00', NULL, E'', E'', E'', E'', E'', E'0', E'', E'0', E'', E'', E'0', NULL, E'', E'', NULL, E'', E'', NULL, E'', E'', E'0', E'0'),
('b', E'AF02', E'Annual Forb', E'Annual Forb', E'', 99, E'', E'5', E'Annual', E'0', E'', '2000-01-01 00:01:00', NULL, E'', E'', E'', E'', E'', E'0', E'', E'0', E'', E'', E'0', NULL, E'', E'', NULL, E'', E'', NULL, E'', E'', E'0', E'0'),
('a', E'AF03', E'Some Annual Forb', E'Annual Forb', E'', 99, E'', E'5', E'Annual', E'0', E'', '2000-01-01 00:01:00', NULL, E'', E'', E'', E'', E'', E'0', E'', E'0', E'', E'', E'0', NULL, E'', E'', NULL, E'', E'', NULL, E'', E'', E'0', E'0'),
('b', E'AF04', E'Annual Forb', E'Annual Forb', E'', 99, E'', E'5', E'Annual', E'0', E'', '2000-01-01 00:01:00', NULL, E'aaaa', E'', E'', E'', E'', E'0', E'', E'0', E'', E'', E'0', NULL, E'', E'', NULL, E'', E'', NULL, E'', E'', E'0', E'0') 
ON CONFLICT DO NOTHING;