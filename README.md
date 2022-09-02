# Landscape-Level Ecological Inventory and Assessment (LLEIA) Database

## Purpose
This project was initiated in order to facilitate the storage and manipulation of ecological monitoring data collected in shrublands and grasslands by various federal, state, and private entities in the United States.

More specifically, it is an effort to commonly house the Bureau of Land Management's (BLM) Assessment, Inventory and Monitoring (AIM) data, as well as U.S Dept. of Agriculture (USDA) Natural Resources Conservation Service's (NRCS) National Resources Inventory (NRI) Grazing Land data, both of which are used to collect ecological related data in non-forested areas across the US.

## Features
This project allows users to create a spatially enabled enterprise database (Postgres/PostGIS) with schemas for both USDA Agricultural Resource Service (ARS) Jornada's Database for Inventory Monitoring and Assessment (DIMA) and NRI Grazing Land databases. It then converts and combines data from both of these schemas into a third schema which naively houses both data types for those methodologies that overlap and are similar enough to warrant a shared schema.

### Database Functionality
1. Spatially enabled PostGIS database.
2. Robust design with appropriate primary/foreign keys as a check against orphaned records and other quality control issues.
3. Imported data are tied to specific database keys, which can be deleted from the database to cause the deletion of associated import records, giving the user direct control of database content.
4. Custom views to quickly give access to common data needs.

### Additional Project Features
This project also provides a number of data manipulation scripts to summarize raw monitoring data in various ways, by species, and user defined indicators. The following calculations are currently available via **lpi_calc.R**:
1. Line-point intercept (cover)
2. Percent dead (cover)
3. Vegetation height

## Prerequisites
The following software will be needed to use this project.

1. [PostgreSQL](https://www.postgresql.org/) installed (locally or remotely) with superuser access to the database.
2. [PostGIS](https://postgis.net/) installed along with the PostgreSQL installation.
3. [R >= 4.2](https://www.r-project.org/) installed. Additionally the path to your bin folder within your R installation directory will need to be in your system's PATH in order to run Rscript globally from the command line. Depending on your installation, this may have been done by default.
4. Optional. For importing MS Access databases into PostgreSQL. Either: 
	* [Microsoft Access Database Engine](https://www.microsoft.com/en-us/download/details.aspx?id=54920). **Please note that your R version (32 vs. 64 bit) must match your Access Database Engine version.**
	* [mdbtools](https://github.com/mdbtools/mdbtools). The Access DB engine will be used if it is found however, for systems where the engine cannot be installed, use mdbtools instead. Check your \*nix distribution for available packages of mdbtools or optionally compile it yourself. **Note: mdbtools must be executable and in your PATH**
5. [SpatiaLite](http://www.gaia-gis.it/gaia-sins/) libraries ([libspatialite](https://www.gaia-gis.it/fossil/libspatialite/index)). Optional. Specifically needed is the *mod_spatialite.dll* or *mod_spatialite.so* library compiled with its location in your system's PATH. This is only necessary when importing or exporting to and from SpatiaLite databases.

Various R libraries are also needed, which are listed at the beginning of each script and will be installed automatically if the project is installed as an R package.
## Installation

You may either download the code base manually or using [Git](https://git-scm.com/)
```
git clone https://github.com/wlieurance/lleiadb.git
```
Within the *R* environment, the project may be installed as a package with the *devtools* library.
```
devtools::install_github("wlieurance/lleiadb")
```

## Usage
Functionality is provided via command line usage of Rscript, located in the *bin* sub-folder of your R installation. In Linux this is typically your shell (**Bash**, **Zsh**, etc.), for Mac this is likely **Zsh** (Applications -> Utilities -> Terminal), and for MS Windows this is typically **PowerShell.exe** or **CMD.exe** (Search -> PowerShell). Alternatively, users can call functions directly from the R command line if the the code was installed as an R package.

### Database Creation
Database creation is accomplished through the *create_db.R* script or directly in R via the *create.lleiadb* function. The command line arguments and options which this script can accept can be seen by running:
```
Rscript create_db.R -h
```
Example usage:
```
Rscript create_db.R database_name database_user
```

See `help(create.lleiadb)` for R command line help.

Database user (database\_user) must already be present in the postgres server, but if database\_name is not found, the script attempts to create it by connecting to the user's default database with the password provided. This can be accomplished with a new install of postgres by setting database\_user=postgres. If no password is provided, the user will be asked for it interactively.

### Loading Data
Database loading is accomplished through the *import.R* script or directly in R via the *import.to.post* function. The command line arguments and options which this script can accept can be seen by running:
```
Rscript import.R -h
```
Example usage:
```
Rscript import.R --key some_unique_code --desc "My Project YYYY-YYYY" database_name database_user /path/to/source_database
```

See `help(import.to.post)` for R command line help.

Currently, a variety of formats are allowed for **source_database**, including MS Access (.mdb, .accdb), ESRI File Geodatabase (.gdb), or SQLite/SpatialLite (.sqlite, .db) databases.
Tables that are present in all three schema (dima, lmf, eco) will be scanned for in the **source_database** and data in matching fields will be imported to the relevant table in LLEIA.

### Exporting Data
Schema exporting to an SQLite/SpatiaLite database is accomplished through the *export.R* script or directly in R via the *post.to.sqlite* function. The command line arguments and options which this script can accept can be seen by running:
```
Rscript export.R -h
```
Example usage:
```
Rscript export.R --schema eco database_name database_user /path/to/export_database.sqlite
```

See `help(post.to.sqlite)` for R command line help.

If the schema contains spatial data (currently only the *eco* schema), a SpatiaLite database will be created, otherwise a standard SQLite database will be created. The will export an entire schema.

### Accessing Data
After data have been imported, they can be viewed within their individual schema in LLEIA. For instance, if a DIMA database was imported, its data will be located in the *dima* schema, within the same table name as the source. Data can be referenced back to their database of origin using the *db* table available in each schema. Removing or updating a record from a *db* table will cascade that update/delete throughout the rest of the database (mostly). This provides a convenient way to remove data contained in one source database if a user needs to.

#### dima 
The dima schema operates differently (experimentally) in which site, line and plot keys from loaded data are kept track of in special tables called db\_site, db\_plot, and db\_line (shim tables). These shim tables may contain duplicate site/plot/line keys tied to specific database, and utilize triggers to control the deletion of the main records in tblSites, tblPlots, and tblLines (collective tables) when a specific key is no longer found in a table shim table. Moreover, individual header tables (e.g. tblLPIHeader, tblGapHeader, etc.) are directly tied to the shim tables, allowing users to delete or update data from specific database sources without deleting or altering the data in the collective tables, which may be shared across multiple DIMA imports.

This design decision was made due to the nature of DIMA as a primary collection database, where multiple individual DIMAs may share the collective table keys. Users may want to delete or update date from a specific database that shares the same collective keys as other DIMA databases. The shim tables allow this functionality.

Additionally, the dima schema has special tables (tblSpecies, tblSpeciesGeneric, tblEcolSites), which may have similar data across multiple DIMAs. These tables are initially populated at the time of LLEIA creation, and all subsequent data (within specific criteria) are then stored in tablename\_delta versions of these tables. This setup allows for a specific version of these tables to be reconstructed for each source database, while limiting duplicate records to the extent possible. This may be helpful, for instance, if two DIMAs have different names and/or growth habits for species, different names for ecological site codes, or different identifying data stored for generic species codes.

#### public 
For viewing all data from all schemas simultaneously, materialized views have been created in the public schema which transform data from the dima and lmf schemas into the eco schema format and stores them along with eco schema data into a single table for viewing. This allows users to process any data from any data source with the same algorithm, regardless of its schema source. This is the primary way data are intended to be accessed in LLEIA, though there are many individual tables in the dima schema which have no counterpart in either the lmf or eco schemas, for which users will have to access the dima schema directly. Geometry and timezone data from the dima and lmf schemas are also extracted for these materialized views, allowing the geometry of certain tables to be plotted with software relevant to PostGIS geometry (e.g. GIS software, R plotting, etc.)

### Calculating Indicators
Indicator calculations at the plot/year level are available via native Postgres views and via Rscript. Summary at the plot and year level allows for multiple method instances to be calculated separately in the case of plot revisits, and it also allows transects that may have been collected at different days of the year to be aggregated into the same temporal group. The downside to this sort of aggregation is the loss of precise visit dates in the output data. For more date specific information, please consult the *public.method_meta* materialized views which contain the **survey_date** data.

#### Postgres Views
The following methodologies are currently calculated as PostgreSQL views.

| Method Name                                   | View Name                 | Summary Level         |
| :-------------------------------------------- | :------------------------ | :-------------------- |
| Gap intercept                                 | public.gap\_plot           | plot, year            |
| Line-point intercept                          | public.pintercept\_plot    | plot, year, species   |
| Species richness/plant census                 | public.plantcensus\_plot   | plot, year, species   |
| Plant density                                 | public.plantdensity\_plot  | plot, year, species   |
| Annual production                             | public.production\_plot    | plot, year, species   |
| Shrub shape                                   | public.shrubshape\_plot    | plot, year, species   |
| Soil aggregate stability                      | public.soilstability\_plot | plot, year, veg. type |
| Interpreting Indicators of Rangeland Health   | public.rangehealth\_plot   | record (long to wide) |


#### R Scripts
While SQL based views can provide quick and convenient access to species level data, a more customizable approach is achieved through R scripting. Currently, only the line-point intercept methodology has a script based calculator. This script calculator allows the user to specify custom indicators for output and gives them for multiple "hit types" (i.e. top, basal, any, etc.)

Users can export line-point intercept data by running the *lpi\_calc.R* script or by utilizing the *calc.lpi* function in R directly:

```
Rscript lpi_calc.R -h
```
Example usage:
```
Rscript lpi_calc.R --outfile "path/to/outfile.csv" --indicators "path/to/indicator/definitions.txt" database_name database_user
```

See `help(calc.lpi)` for R command line help.

This script calculates indicator-level data at the unique plot and survey year level for each indicator in the indicators definitions file to a delimited or RDS file. A call to this script without the indicator definitions file will result in species level data being exported, where each species code is its own indicator. Users can construct their own indicators by using [dplyr filter](https://dplyr.tidyverse.org/reference/filter.html) strings. Example indicator files can be found in the **indicators** sub-folder and examples of the **hits** tables that can be used are in the **example** sub-folder. Further information about what types of data each specific field name may contain can be found in the *pintercept* and *plant* subsections of the database metadata file.

Further simplification and widening of the data produced via the lpi\_calc module can be produced through the *lpi\_convert.R* script or by utilizing the *lpi.to.gis* function in R directly:  

```
Rscript lpi_convert.R -h
```
Example usage:
```
Rscript lpi_convert.R  --cover "indicator_name1, any; indicator_name2, top;" database_name database_user "/path/to/lpi_calc_output.csv" "/path/to/new_output.shp"
```

See `help(lpi.to.gis)` for R command line help.

Running this script will produce a spatial feature with one line per plot/survey\_year easily viewed by applications which prefer wide format data (e.g. GIS applications). Each indicator mean and standard deviation are given their own field (wide format). Indicator string examples can be found in the **examples** sub-folder. The format of the output file will depend on the extension chosen and is guessed by *st_write()* in R's *sf* library and will depend on driver availability on a user's individual system. Common choices would be ESRI shapefile (.shp), OGC geopackage (.gpkg), or SpatiaLite (.sqlite). Indicator strings are constructed in the format of *indicator_name1, hit_type1; indicator_name2, hit_type2*. This string is parsed to select the appropriate fields for pivoting the data wider.

## Database Structure and Documentation
Metadata documenting the database is available as an [xml file](/metadata/lleiadb_metadata_iso19110-2016.xml) in [ISO 19110:2016](https://www.iso.org/standard/57303.html) format. The metadata documents the *eco* and *public* schemas primarily, as neither the *dima* nor the *lmf* schema are maintained by the author.

Of the four schema in the database, each houses a different category of data.

### dima
Houses data natively found in Jornada's Database for Inventory, Monitoring & Assessment (currently version 5.5). This is distributed by the Jornada as an Microsoft Access database and can be downloaded [here](https://jornada.nmsu.edu/monit-assess/dima/download).

### lmf
Houses data collected via the NRI Grazing Land schema, collected via the NRCS's CASI Windows Mobile application. NRI Grazing Land data that are collected on BLM land are jointly managed by the BLM and NRCS and are referred to as Landscape Monitoring Framework data.

### eco
The custom LLEIA schema. This schema was developed in order to store data not otherwise already found in a schema, and to handle data conversion from both LMF and DIMA databases. 

### public
This schema houses tables from other data sources, such as state and county data or NRCS PLANTS data. It also houses a set of materialized views which mirror the naming of tables in the *eco* schema. These materialized views follow the design of the *eco* schema, and contain data from all the other three schemas, converting as necessary for the *dima* and *lmf* schemas. These materialized views are designed to be an easy way to view data from all three data sources at once, and users can reference the *eco* schema metadata for a reference to what fields and data these materialized views contain.

This schema also contains a number of other views which source their data from the public schema, including a number of view which calculate method indicators at the plot/year/species level (for those methods that have plant species).

### Entity Relationship Diagram

For a complete reference on what table and field relationships exist in the database, the user is encouraged to reference the SQL directly in  [sql](/sql) folder. A somewhat simplified ERD is also made available in vector form [here](/metadata/lleia_erd_simple.svg).


## Meta

Wade Lieurance – wlieurance@gmail.com

Distributed under the GNU General Public License v3. See ``LICENSE`` for more information.

[https://github.com/wlieurance/lleiadb](https://github.com/wlieurance/)

## Contributing

1. Fork it (<https://github.com/wlieurance/lleiadb/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Commit your changes (`git commit -am 'Add some fooBar'`)
4. Push to the branch (`git push origin feature/fooBar`)
5. Create a new Pull Request
