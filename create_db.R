#!/usr/bin/env Rscript

suppressMessages(library(DBI))
suppressMessages(library(pool))
suppressMessages(library(RPostgres))
suppressMessages(library(getPass))
suppressMessages(library(optparse))
suppressMessages(library(readr))
suppressMessages(library(stringr))
suppressMessages(library(sf))
suppressMessages(library(dplyr))

# custom sources
source("parse_sql.R")


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

create.exts <-  function(){
  print("PUBLIC: creating extensions...")
  execute.sql(path = "sql/execute_init.sql")
}
create.dima <- function(){
  print("DIMA: creating tables...")
  execute.sql(path = "sql/create_dima_tables.sql")
  print("DIMA: inserting base data...")
  execute.sql(path = "sql/create_dima_data.sql")
  print("DIMA: creating triggers...")
  execute.sql(path = "sql/create_dima_triggers.sql")
}

create.lmf <- function(){
  print("LMF: creating tables...")
  execute.sql(path = "sql/create_lmf_tables.sql")
}

create.eco <- function(){
  print("ECO: creating tables...")
  execute.sql(path = "sql/create_eco_tables.sql")
}

create.public <- function(){
  print("PUBLIC: creating tables...")
  execute.sql(path = "sql/create_public_tables.sql")
  print("PUBLIC: inserting base data...")
  execute.sql(path = "sql/create_public_data.sql")
  print("PUBLIC: creating views...")
  execute.sql(path = "sql/create_public_views.sql")
  print("PUBLIC: executing statements...")
  execute.sql(path = "sql/execute_public_statements.sql")
}

create.spatial <- function(){
  print("PUBLIC: importing spatial features...")
  stmt.d = "DROP TABLE IF EXISTS public.timezone CASCADE;"
  stmt.c = paste0("CREATE TABLE public.timezone (tzid VARCHAR(30) PRIMARY KEY, ", 
                "geom geometry(MULTIPOLYGON, 4326));")
  dbExecute(pool, stmt.d)
  dbExecute(pool, stmt.c)
  timezones <- read_sf("spatial/tz_world_mp.shp") %>% 
    rename_all(tolower) %>% rename(geom = geometry) %>% st_set_crs(4326)
  suppressMessages(st_write(obj = timezones, dsn = pool, layer = "timezone", 
                           quiet = TRUE, append = TRUE))
}



main <-  function(args){
  option_list = list (
    make_option(opt_str = c("-p", "--port"), default = 5432, type = "integer",
                help = paste0("the Postgres connection port")),
    make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("the host name or ip address of the connection")),
    make_option(opt_str = c("-w", "--password"),
                help = paste0("the password for the user provided."))
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
  
  opt = parse_args(opt_parser, positional_arguments = 2, args = args)
  if (is.null(opt$options$password)){
    opt$options$password = getPass()
  }
  
  #<<- creates global pool (parent scope of main())
  pool <<- create.pool(dbname = opt$args[1], host = opt$options$host, 
                      port = opt$options$port, user = opt$args[2], 
                      password = opt$options$password)
  # assign("pool", pool, envir = .GlobalEnv)
  res <- dbExecute(pool, "SET client_min_messages TO WARNING;")
  create.exts()
  create.dima()
  create.lmf()
  create.eco()
  create.spatial()
  create.public()


  
  poolClose(pool)
  rm(pool, envir = .GlobalEnv)
  cat("\nScript finished.\n")
}

# run only if called from a script.
if (sys.nframe() == 0) {
  args = commandArgs(trailingOnly = TRUE)
  # args = c("test", "postgres")
  main(args)
}
