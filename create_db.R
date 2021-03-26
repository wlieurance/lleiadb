#!/usr/bin/env Rscript

libraries = c("DBI", "pool", "RPostgres", "getPass", "optparse", "readr", 
              "stringr", "sf", "dplyr")

for (lib in libraries){
  if(lib %in% rownames(installed.packages()) == FALSE) {
    install.packages(lib)
  }
  suppressMessages(library(lib, character.only = TRUE))
}

# custom sources
suppressMessages(source("parse_sql.R"))


#' Creates a pooled database object that other functions use to interact with
#' the database.
#'
#' @param dbname A string. The database name to connect to in the postgres 
#'   instance. 
#' @param host A string. The IP address or DNS name which hosts the database.
#' @param port An integer. The port which the postgres service monitors for 
#'   connections.
#' @param user A string. The database user used to connect to the database.
#' @param password A string. The password used to connect to the database.
#'
#' @return
#' @export
create.pool <- function(dbname, host, port, user, password){
  tryCatch(
    expr = {
      dbPool(RPostgres::Postgres(), dbname = dbname, host = host, 
                       port = port, user = user, password = password)
    },
    error = function(e){
      exists <- paste0('database "', dbname, '" does not exist')
      # print(e)
      if(grepl(exists, e, fixed = TRUE)){
        #print("caught it")
        if (interactive()){
          x <- tolower(substr(readline(
            prompt=paste(exists, "create it? (y/n): ")), 1, 1))
        } else {
          cat(exists, "create it? (y/n): ")
          resp <- readLines("stdin", n=1)
          x <- tolower(substr(resp, 1, 1))
          }
        if (x == "y"){
          print(paste0("Attemping temp connection to db '", user, 
                       "' for db creation..."))
          con <- dbConnect(RPostgres::Postgres(), dbname = user, host = host, 
                    port = port, user = user, password = password)
          stmt <- paste0("CREATE DATABASE ", dbname, ";")
          dbExecute(con, stmt)
          dbDisconnect(con)
          print(paste0("Connecting to ", dbname, "..."))
          dbPool(RPostgres::Postgres(), dbname = dbname, host = host, 
                    port = port, user = user, password = password)
        }
      }
    } #,
    # warning = function(w){},
    # finally = {}
  )
}


#' Loads SQL statements from \code{path}, separates them, and executes them in 
#' order. 
#'
#' @param path A string directory path which points to the location 
#'   where SQL statements to be executed on the PostGIS instance are stored.
#' @param params A named list of variables, which can be used with glue 
#'   syntax to replace parameters in \code{path}. 
#'
#' @return Nothing.
#' @export
execute.sql <- function(path, params = list()){
  sql <- sep.sql.stmts(sql.path = path, params = params)
  for (stmt in sql){
    # cat(stmt)
    res <- tryCatch(
      expr = {
        dbExecute(pool, stmt)
      },
      error = function(e){
        cat(stmt)
        print(e)
        stop(e)
      })
    # print(res)
  }
}


#' Creates extensions on the public schema of the Postgres instance.
#'
#' @param sql.path A string directory path which points to the location 
#'   where SQL statements to be executed on the PostGIS instance are stored.
#'
#' @return Nothing.
#' @export
create.exts <-  function(sql.path){
  print("PUBLIC: creating extensions...")
  execute.sql(path = file.path(sql.path, "execute_init.sql"))
}


#' Creates, loads data into, and executes other statements within dima schema 
#' of the Postgres instance.
#'
#' @param sql.path A string directory path which points to the location 
#'   where SQL statements to be executed on the PostGIS instance are stored.
#'
#' @return Nothing.
#' @export
create.dima <- function(sql.path){
  print("DIMA: creating tables...")
  execute.sql(path = file.path(sql.path, "create_dima_tables.sql"))
  print("DIMA: inserting base data...")
  execute.sql(path = file.path(sql.path, "create_dima_data.sql"))
  print("DIMA: creating triggers...")
  execute.sql(path = file.path(sql.path, "create_dima_triggers.sql"))
}


#' Creates, loads data into, and executes other statements within lmf schema 
#' of the Postgres instance.
#'
#' @param sql.path A string directory path which points to the location 
#'   where SQL statements to be executed on the PostGIS instance are stored.
#'
#' @return Nothing.
#' @export
create.lmf <- function(sql.path){
  print("LMF: creating tables...")
  execute.sql(path = file.path(sql.path, "create_lmf_tables.sql"))
}


#' Creates, loads data into, and executes other statements within eco schema 
#' of the Postgres instance.
#'
#' @param sql.path A string directory path which points to the location 
#'   where SQL statements to be executed on the PostGIS instance are stored.
#'
#' @return Nothing.
#' @export
create.eco <- function(sql.path){
  print("ECO: creating tables...")
  execute.sql(path = file.path(sql.path, "create_eco_tables.sql"))
}


#' Creates, loads data into, and executes other statements within public schema 
#' of the Postgres instance.
#'
#' @param sql.path A string directory path which points to the location 
#'   where SQL statements to be executed on the PostGIS instance are stored.
#'
#' @return Nothing.
#' @export
create.public <- function(sql.path){
  print("PUBLIC: creating tables...")
  execute.sql(path = file.path(sql.path, "create_public_tables.sql"))
  print("PUBLIC: inserting base data...")
  execute.sql(path = file.path(sql.path, "create_public_data.sql"))
  print("PUBLIC: creating views...")
  execute.sql(path = file.path(sql.path, "create_public_views.sql"))
  print("PUBLIC: executing statements...")
  execute.sql(path = file.path(sql.path, "execute_public_statements.sql"))
}


#' Imports spatial features into the PostGIS database.
#'
#' @param spatial.path A string directory path which points to the location 
#'   where spatial data to be imported into the PostGIS instance is stored.
#'
#' @return Nothing.
#' @export
create.spatial <- function(spatial.path){
  print("PUBLIC: importing spatial features...")
  stmt.d = "DROP TABLE IF EXISTS public.timezone CASCADE;"
  stmt.c = paste0("CREATE TABLE public.timezone (tzid VARCHAR(30) PRIMARY KEY, ", 
                "geom geometry(MULTIPOLYGON, 4326));")
  dbExecute(pool, stmt.d)
  dbExecute(pool, stmt.c)
  timezones <- read_sf(file.path(spatial.path, "tz_world_mp.shp")) %>% 
    rename_all(tolower) %>% rename(geom = geometry) %>% st_set_crs(4326)
  suppressMessages(st_write(obj = timezones, dsn = pool, layer = "timezone", 
                           quiet = TRUE, append = TRUE))
}


#' The main processing function.
#'
#' @param dbname A string. The database name to connect to in the postgres 
#'   instance.
#' @param user A string. The database user used to connect to the database.
#' @param password A string. The password used to connect to the database.
#' @param sql.path A string directory path which points to the location where
#'   the SQL files for DB creation are stored.
#' @param spatial.path A string directory path which points to the location 
#'   where spatial data to be imported into the PostGIS instance is stored.
#' @param host A string. The IP address or DNS name which hosts the database.
#' @param port An integer. The port which the postgres service monitors for 
#'   connections.
#'
#' @return Nothing.
#' @export
main <-  function(dbname, user, password, sql.path, spatial.path, 
                  host = "localhost", port = 5432){
  # <<- creates global pool (parent scope of main())
  pool <<- create.pool(dbname = opt$args[1], host = opt$options$host, 
                      port = opt$options$port, user = opt$args[2], 
                      password = opt$options$password)
  # assign("pool", pool, envir = .GlobalEnv)
  res <- dbExecute(pool, "SET client_min_messages TO WARNING;")
  create.exts(sql.path)
  create.dima(sql.path)
  create.lmf(sql.path)
  create.eco(sql.path)
  create.spatial(spatial.path)
  create.public(sql.path)

  poolClose(pool)
  rm(pool, envir = .GlobalEnv)
  cat("\nScript finished.\n")
}

# run only if called from a script.
if (sys.nframe() == 0) {
  option_list = list (
    make_option(opt_str = c("-p", "--port"), default = 5432, type = "integer",
                help = paste0("The Postgres connection port")),
    make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    make_option(opt_str = c("-w", "--password"),
                help = paste0("The password for the user provided.")),
    make_option(opt_str = c("-s", "--sql_dir"),
                help = paste0("The directory where the database creation SQL ", 
                              "files are stored.")),
    make_option(opt_str = c("-S", "--spatial_dir"),
                help = paste0("The directory where the spatial data to be 
                              imported into the PostGIS instance is stored."))
  )
  
  opt_parser = OptionParser(
    usage = paste0("usage: %prog [options] dbname user"), 
    option_list=option_list, 
    prog = NULL, 
    description = paste(sep = ' ',
                      "Will create an empty PostgreSQL database to store data",
                      "from the following data sources:\n",
                      "1) USDA-ARS Jornada's Database for Inventory Monitoring",
                      "and Assessment (DIMA),\n",
                      "2) U.S. DOI Bureau of Land Management's Landscape",
                      "Monitoring Framework (LMF) database (should also be",
                      "compatible with USDA-NRCS National Resources Inventory",
                      "(NRI) data).\n",
                      "3) Native format." 
    )
  )
  args = commandArgs(trailingOnly = TRUE)  
  opt = parse_args(opt_parser, positional_arguments = 2, args = args)
  if (is.null(opt$options$password)){
    opt$options$password = getPass()
  }
  if (is.null(opt$options$sql_dir)){
    opt$options$sql_dir = "sql"
  }
  if (is.null(opt$options$spatial_dir)){
    opt$options$spatial_dir = "spatial"
  }
  
  main(dbname = opt$args[1], user = opt$args[2], 
       password = opt$options$password, sql.path = opt$options$sql_dir, 
       spatial.path = opt$options$spatial_dir, host = opt$options$host, 
       port = opt$options$port)
}
