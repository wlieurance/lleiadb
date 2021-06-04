# Landscape-Level Ecological Inventory and Assessment (LLEIA) Database
## Purpose
This project was initiated in order to facilitate the storage and manipulation of ecological monitoring data collected in shrublands and grasslands by various federal, state, and private entities in the United States.

More specifically, it is an effort to commonly house the Bureau of Land Management's (BLM) Assessment, Inventory and Monitoring (AIM) data, as well as U.S Dept. of Agriculture (USDA) Natural Resources Conservation Service's (NRCS) National Resources Inventory (NRI) Grazing Land data, both of which are used to collect ecological related data in non-forested areas across the US.

## Features
This project allows users to create an spatially enabled enterprise database (Postgres/PostGIS) with schemas for both USDA Agricultural Resource Service (ARS) Jornada's Database for Inventory Monitoring and Assessment (DIMA) and NRI Grazing Land databases. It then converts and combines data from both of these schemas into a third schema which naively houses both data types for those methodologies that overlap and are similar enough to warrant a shared schema.

### Database Functionality
1. Spatially enabled PostGIS database.
2. Robust design with appropriate primary/foreign keys as a check against orphaned records and other quality control issues.
3. Imported data is tied to specific database keys, which can be deleted from the database to cause the deletion of associated import records, giving the user direct control of database content.
4. Custom views to quickly give access to common data needs.

### Additional Project Features
This project also provides a number of data manipulation scripts to summarize raw monitoring data in various ways, by species, and user defined indicators. The following calculations are currently available via **lpi_calc.R**:
1. Line-point intercept (cover)
2. Percent dead (cover)
3. Vegetation height

## Prerequisites
The following software will be needed to use this project.  Note that versions used during development are listed, and while prior versions may work, they have not been tested.

1. A running [Postgres](https://www.postgresql.org/) installation >= 13 either locally or remotely, with
2. [PostGIS](https://postgis.net/) >= 2.5 installed.
3. [R](https://www.r-project.org/) >= 4.0 installed

Additionally the path to your bin folder within your R installation directory will need to be in your system's PATH in order to run Rscript globally from the command line. Depending on your installation, this may have been done by default.

Various R libraries are also needed, which are listed at the beginning of each script. Users can run
```
Rscript install_libs.R
```
to install them all from the command line. In the case if a library loading error, manual intervention may be necessary with **install.packages("package")** or **install_github("user/package")** via the R command line (note **install_github** requires the *devtools* library). Some R library dependencies may require downloading if building R libraries from source. Pay attention to the build error messages to determine which dependencies need to be installed on your system.

## Installation

You may either download the code base manually or using [Git](https://git-scm.com/)
```
git clone https://github.com/wlieurance/lleiadb.git
```

## Usage
Functionality is provided via command line usage of Rscript, located in the *bin* sub-folder of your R installation.  In Linux this is typically your shell (**Bash**, **Zsh**, etc.), for Mac this is likely **Zsh** (Applications -> Utilities -> Terminal), and for MS Windows this is typically **PowerShell.exe** or **CMD.exe** (Search -> PowerShell).

### Database Creation
Database creation is accomplished through the *create_db.R* script. The command line arguments and options which this script can accept can be seen by running:
```
Rscript create_db.R -h
```
Example usage:
```
Rscript create_db.R database_name database_user
```

Database user (database_user) must already be present in the postgres server, but if database_name is not found, the script attempts to create it by connecting to the user's default database with the password provided. This can be accomplished with a new install of postgres by setting database_user=postgres. If no password is provided, the user will be asked for it interactively.

### Loading Data
Database loading is accomplished through the *import.R* script. The command line arguments and options which this script can accept can be seen by running:
```
Rscript import.R -h
```
Example usage:
```
Rscript import.R --key some_unique_code --desc "My Project YYYY-YYYY" database_name database_user /path/to/source_database
```

Currently, a variety of formats are allowed for **source_database**, including MS Access (.mdb, .accdb), ESRI File Geodatabase (.gdb), or SQLite/SpatialLite (.sqlite, .db) databases.
Tables that are present in all three schema (dima, lmf, eco) will be scanned for in the **source_database** and data in matching fields will be imported to the relevant table in LLEIA.

### Exporting Data
Schema exporting to an SQLite/SpatiaLite database is accomplished through the *export.R* script. The command line arguments and options which this script can accept can be seen by running:
```
Rscript export.R -h
```
Example usage:
```
Rscript export.R --schema eco database_name database_user /path/to/export_database.sqlite
```

If the schema contains spatial data (currently only the *eco* schema) a SpatiaLite database will be created, otherwise a standard SQLite database will be created.  The will export an entire schema.

### Accessing Data
After data have been imported, the can be viewed according to their individual schema in the Postgres instance.  For instance, if a DIMA database was imported, its data will be located in the **dima**
schema.  Data can be tied back to its database of import using the *db* table available in each schema.  In both the lmf and eco schemas, the db table is tied to specific sites in the site-level table, that is organization is db -> site_table (eco) -> plot_table -> line_table (eco)-> my_table and deleting a db record will cascade to other records in this way.

The dima schema operates differently (experimentally) in which site, line and plot keys from loaded data are kept track of in special tables called db_site, db_plot, and db_line (shim tables). These shim tables may contain duplicate site/plot/line keys tied to specific database, and utilize triggers to control the deletion of the main records in tblSites, tblPlots, and tblLines (collective tables) when a specific key is no longer found in a table shim table.  Moreover, individual header tables (e.g. tblLPIHeader, tblGapHeader, etc.) are directly tied to the shim tables, allowing users to delete or update data from specific database sources without deleting or altering the data in the collective tables, which may be shared across multiple DIMA databases.

This design decision was made due to the nature of DIMA as a primary collection database, where multiple individual DIMAs may share the collective table keys.  Users may want to delete or update date from a specific database that shares the same collective keys as other DIMA databases. The shim tables allow this to happen.

Additionally, the dima schema has special tables (tblSpecies, tblSpeciesGeneric, tblEcolSites), which may have similar data across multiple DIMAs. These tables are initially populated at the time of The Postgres DB creation and all subsequent data is then stored in tablename_delta versions of these tables, where the specific database is for the data is tracked in the table, and any day that differs significantly from the base table is inserted here.  This, in theory allows a specific version of these tables to be queried based on the source database.  This may be helpful, for instance, if two DIMAs have different names and/or growth habits for species, different names for ecological site codes, or different identifying data stored for generic species codes.

For viewing all data from all schemas simultaneously, materialized views have been created in the public schema which transform data from the dima and lmf schemas into the eco schema format and stores them along with eco schema data into a single table for viewing.  This allows users to process any data from any data source with the same algorithm, regardless of its schema source.  This is the primary way data are intended to be accessed in LLEIA, though there are many individual tables in the dima schema which have no counterpart in either the lmf or eco schemas, for which users will have to access the dima schema directly. Geometry and timezone data from the dima and lmf schemas are also extracted for these materialized views, allowing the geometry of certain tables to be plotted with software relevant to PostGIS geometry (e.g. GIS software, R plotting, etc.)

### Calculating Indicators
Plot level indicators are in active development, though some plot level methods are available as views in the public schema. All methods in the public schema are slated to eventually have plot level tables created for easy use drag-and-drop use in GIS applications. Currently one method, point intercept, is processed exclusively via R script (as opposed to natively with SQL) due to its complicated, multilayered nature. Users can export point intercept data by running:

```
Rscript lpi_calc.R -h
```
Example usage:
```
Rscript lpi_calc.R --outfile "path/to/outfile.csv" --indicators "path/to/indicator/definitions.txt" database_name database_user
```

This script calculates indicator-level data at the unique plot and survey year level for each indicator in the indicators definitions file to a delimited or RDS file. A call to this script without the indicator definitions file will result in species level data being exported, where each species code is its own indicator. Users can construct their own indicators by using [dplyr filter](https://dplyr.tidyverse.org/reference/filter.html) strings. Example indicator files can be found in the **indicators** sub-folder and examples of the **hits** tables that can be used are in the **example** sub-folder. Further information about what type of data each specific field name may contain can be found in the *pintercept* and *plant* subsections of the database metadata file.

Further simplification and widening of the data produced via the lpi_calc module can be produced by running:  

```
Rscript lpi_convert.R -h
```
Example usage:
```
Rscript lpi_convert.R  --cover "indicator_name1, any; indicator_name2, top;" database_name database_user "/path/to/lpi_calc_output.csv" "/path/to/new_output.shp"
```

Running this script will produce a spatial feature with one line per plot/survey_year easily viewed by applications which prefer wide format data (e.g. GIS applications). Each indicator mean and standard deviation are given their own field (wide format). Indicator string examples can be found in the **examples** sub-folder. The format of the output file will depend on the extension chosen and is guessed by *st_write()* in R's *sf* library and will depend on driver availability on a user's individual system. Common choices would be ESRI shapefile (.shp), OGC geopackage (.gpkg), or SpatiaLite (.sqlite). Indicator strings are constructed in the format of *indicator_name1, hit_type1; indicator_name2, hit_type2*. This string is parsed to select the appropriate fields for pivoting the data wider.


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
