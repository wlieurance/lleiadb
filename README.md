# Rangeland Monitoring Database
## Purpose
This project was initiated in order to facilitate the storage and manipulation of ecological monitoring data collected in shrublands and grasslands by various federal, state, and private entities in the United States.

More specifically, it is an effort to commonly house the Bureau of Land Management's (BLM) Assessment, Inventory and Monitoring (AIM) data, as well as U.S Dept. of Agriculture (USDA) Natural Resources Conservation Service's (NRCS) National Resources Inventory (NRI) Grazing Land data, both of which are used to collect ecological related data in non-forested areas across the US.

## Features
This project allows users to create an spatially enabled enterprise database (Postgres/PostGIS) with schemas for both USDA Agricultural Resource Service (ARS) Jornada's Database for Inventory Monitoring and Assessment (DIMA) and NRI Grazing Land databases. It then converts and combines data from both of these schemas into a third schema which naively houses both data types for those methodologies that overlap and are similar enough to warrant a shared schema.

### Database Functionality
1. Spatially enabled for use in GIS applications, R and python.
2. Robust design with appropriate primary/foreign keys as a check against orphaned records and other quality control issues.
3. Imported data is tied to specific database keys, which can be deleted from the database to cause the deletion of associated import records, giving the user more direct control of database content.
4. Custom views to quickly give access to common data needs.

### Additional Project Features
This project also provides a number of data manipulation scripts to summarize raw monitoring data in various ways, by species, and user defined indicators. The following calculations are currently available:
1. Line-point intercept (cover)
2. Vegetation height

## Prerequisites
The following software will be needed to use this project.  Note that versions used during development are listed, and while prior versions may work, they have not been tested.
 
1. A running [Postgres](https://www.postgresql.org/) installation >= 13 either locally or remotely, with
2. [PostGIS](https://postgis.net/) >= 2.5 installed.
3. [R](https://www.r-project.org/) >= 4.0 installed

Additionally the path to your bin folder within your R installtion directory will need to be in your system's PATH in order to run Rscript golabally from the command line. Depending on your installation, this may have been done by default.

Various R libraries are also needed, which are listed at the beginning of each script. Each script will attempt to automatically install missing dependencies when it is run, though in the case if a library loading error, manual intervention may be necessary with **install.packages("package")** or **install_github("user/package")** via the R command line (note **install_github** requires the *devtools* library). 

## Installation

You may either download the code base manually or using [Git](https://git-scm.com/)
```
git clone https://github.com/wlieurance/ecomonitoring.git
```

## Usage
Functionality is provided via command line usage of Rscript, located in the *bin* subfolder of your R installation.  In linux this is typically your shell (**Bash**, **Zsh**, etc.), for Mac this is likely **Zsh** (Applications -> Utilities -> Terminal), and for MS Windows this is typically **PowerShell.exe** or **CMD.exe** (Search -> PowerShell).

### Database Creation
Database creation is accomplished through the *create_db.R* script. The command line arguments and options which this script can accept can be seen by running: 
```
Rscript create_db.R -h 
```
Example usage:
```
Rscript create_db.R ---port 5432 --host localhost --password "my password" database_name database_user 
```
Database user (database_user) must already be present in the postgres server, but if database_name is not found, the script attempts to create it by connecting to the user's default database with the password provided. This can be accomplished with a new install of postgres by setting database_user=postgres. If no password is provided, the user will be asked for it interactively.

### Loading Data



## Meta

Your Name – [@YourTwitter](https://twitter.com/dbader_org) – YourEmail@example.com

Distributed under the XYZ license. See ``LICENSE`` for more information.

[https://github.com/yourname/github-link](https://github.com/dbader/)

## Contributing

1. Fork it (<https://github.com/yourname/yourproject/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Commit your changes (`git commit -am 'Add some fooBar'`)
4. Push to the branch (`git push origin feature/fooBar`)
5. Create a new Pull Request

