#!/usr/bin/env Rscript

libraries = c("optparse", "dplyr", "dbplyr", "DBI", "RPostgres", "RSQLite", 
              "stringr")

for (lib in libraries){
  if(lib %in% rownames(installed.packages()) == FALSE) {
    install.packages(lib)
  }
  suppressMessages(library(lib, character.only = TRUE))
}

# custom sources
suppressMessages(source("parse_sql.R"))
suppressMessages(source("import.R"))

try.execute.sql <- function(dbcon, sql){
  for (stmt in sql){
    # cat(stmt)
    res <- tryCatch(
      expr = {
        dbExecute(dbcon, stmt)
      },
      error = function(e){
        cat(stmt)
        print(e)
        stop(e)
      })
    # print(res)
  }
}


#' Takes a CREATE TABLE SQL statement for a Postgres instance and converts it
#' to A CREATE TABLE statement that will work for SQLite.
#'
#' @param sql A string CREATE TABLE statement for a Postgres instance. 
#'
#' @return A list containing a string CREATE TABLE statement for an SQLite 
#'   database, the name of the table, and geometry type and srid variables if
#'   present.
#' @export
convert.sqlite <- function(sql){
  table.match <- sql %>% str_match(
    regex(paste0("(?:CREATE|DROP) TABLE (?:IF (?:NOT ){0,1}EXISTS ){0,1}",
                 "([^\\.]+)\\.([^\\s]+)"), 
          ignore_case = TRUE))
  if (is.na(table.match[1])){
    table <-  NULL
  } else {
    table <- table.match[3]
  }
  geom.match <- sql %>% str_match(
    regex("geom geometry\\s*\\(([^\\s,]+)[,\\s]+(\\d+)\\s*\\)[,\\s]*", 
          ignore_case = TRUE))
  if (is.na(geom.match[1])){
    geom <- NULL
  } else {
    geom <-  c(geom.match[2],as.integer(geom.match[3]))
  }
  
  sql.new <- sql %>% 
    str_replace_all(regex("eco\\.", ignore_case = TRUE), "") %>%
    str_replace_all(regex("CREATE SCHEMA IF NOT EXISTS eco;", 
                          ignore_case = TRUE), "") %>%
    str_replace_all(regex("(DROP TABLE IF EXISTS [a-z_A-Z\\.]+)\\s+CASCADE;", 
                      ignore_case = TRUE), 
                "\\1;") %>%
    str_replace_all(regex("geom geometry\\(.+", ignore_case = TRUE), "") %>%
    str_replace_all(regex("CREATE INDEX .*? USING gist .*?;", 
                          ignore_case = TRUE), 
                "") %>%
    str_replace_all(regex("\\s+USING btree", ignore_case = TRUE), "")
  return(list(sql = sql.new, table = table, geom = geom))
}


#' Loads CREATE TABLE statements from a file, separates them, converts them to
#' SQlite statements, executes thema nd adds spatial metadata and columns if
#' necessary
#' 
#' @param con An RSQLite database connection.
#' @param spatial logical. A flag telling the function to initialize SpatiaLite
#'   metadata, converting the database from SQLite to SpatialLite.
#' @param sql.path A string file path to a file containing Postgres CREATE TABLE
#'   statements.
#'
#' @return Nothing.
#' @export
create.sqlite <- function(con, spatial, sql.path){
  cat("Creating sqlite database...\n")
  if (spatial == TRUE){
    cat("Intializing spatial metadata...\n")
    res <- dbExecute(con, "SELECT InitSpatialMetadata(1);")
  }
  sql <- sep.sql.stmts(sql.path = sql.path)
  new.sql <- lapply(sql, FUN=convert.sqlite)
  for (s in new.sql){
    stmt <-  s$sql
    geom <- s$geom
    table <- s$table
    if (!trimws(stmt) %in% c("", ";")){
      try.execute.sql(dbcon = con, sql = stmt)
      if (!is.null(geom) & spatial == TRUE){
        geom.stmt <- paste0("SELECT AddGeometryColumn('", table, 
                            "', 'geom', ", geom[2], ", '", geom[1], "');")
        dbExecute(con, geom.stmt)
      }
    }
  }
}


#' Creates an SQLite SELECT statement based on the table definition present
#' in the Postgres instance.
#'
#' @param schema A string. The name of the schema in the Postgres instance.
#' @param table A string. The name of the table in the Postgres instance.
#' @param spatial logical. A flag tell the function that \code{table} contains
#'   PostGIS geometry columns. 
#'
#' @return A list containing the SELECT SQL statement and a vector of the 
#'   column names used to construct it.
#' @export
create.post.select <- function(schema, table, spatial){
  tbl.info <- get.dest.info(schema = schema, table = table)
  cols <- tbl.info$col.info$column_name
  if (spatial == TRUE){
    cols.new <- cols %>% str_replace("^geom$", "st_astext(geom) geom")
  } else {
    cols.new = cols[cols != "geom"]
  }
  col.string <-  paste(cols.new, collapse = ", ")
  select <- paste0("SELECT ", col.string, "\n  FROM ", schema, ".", table, ";")
  return(list(sql = select, cols = cols.new))
}


#' This function creates an SQLite INSERT statement based on the table 
#' definition stored in the Postgres instance.
#'
#' @param schema A string. The name of the schema in the Postgres instance.
#' @param table A string. The name of the table in the Postgres instance. 
#' @param update logical. A flag telling the function to create an UPSERT 
#'   statement rather than an INSERT statement.
#' @param ins.cols A string vector which contains columns names to use in the
#'   INSERT statement.
#' @param srid An integer which is the srid/EPSG code representing the GPS 
#'   coordinate system/datum to store spatial data in.
#'
#' @return A list containing the INSERT SQL statement as well as a vector of 
#'   column names used to construct it.
#' @export
create.sqlite.insert <- function(schema, table, update = FALSE, 
                                 ins.cols = NULL, srid = 4326){
  info <- get.dest.info(schema, table)
  # gets the pkey constraint name or first unique constraint name
  # on conflict statement only allows one constraint check
  if (is.null(ins.cols)){
    cols <- info$col.info$column_name
  } else {
    restricted <- info$col.info %>% 
      inner_join(as_tibble(list(column_name = ins.cols)), 
                 by = c("column_name" = "column_name")) 
    cols <- restricted$column_name
  }
  if (update == FALSE){
    update.sql <- "OR IGNORE"
  } else {
    update.sql <- "OR REPLACE"
  }
  colstring <- paste(paste0('"', cols,'"'), collapse = ", ")
  params <- rep("?", length(cols))
  geom.col <- which(cols == "geom")
  if (length(geom.col == 1)){
    params[geom.col] <- paste0("ST_GeomFromText(?, ", srid, ")")
  }
  # for convsersion to character datetime
  # dt.cols <- !is.na(as.vector(info$col.info$data_type %>% 
  #   str_match(regex("(?:timestamp|date)", ignore_case = TRUE))))
  # params.dt <- ifelse(dt.cols, paste0(params, "::varchar"), params)
  
  paramstring <- paste(params, collapse = ", ")  
  insert.sql <- glue(paste(
    "INSERT {update.sql} INTO \"{table}\" ({colstring})",
    "VALUES ({paramstring});",
    sep = "\n"))
  return(list(sql = insert.sql, cols = cols))
}


#' Queries the Postgres instance for table data and inserts data into an SQLite 
#' table.
#'
#' @param schema A string. The name of the schema in the Postgres instance.
#' @param spatial logical. A flag tell the function that \code{table} contains
#'   PostGIS geometry columns. 
#'
#' @return Nothing.
#' @export
copy.post.tables <-  function(schema, spatial){
  cat("Copying data...\n")
  tbl.order <- insert.order(schema)
  for (i in rownames(tbl.order)){
    tbl.name <- tbl.order[i,1]$tblname
    
    # get data
    query <- create.post.select(schema = schema, table = tbl.name, 
                                spatial = spatial)
    result <- DBI::dbSendQuery(con, query$sql)
    tbl <- dbFetch(result)
    dbClearResult(result)
    
    if (nrow(tbl) > 0){
      # do insert
      cat(paste0("Inserting into ", tbl.name, " ... "))
      insert <- create.sqlite.insert(schema = schema, table = tbl.name, 
                                     ins.cols = query$cols)
      send.data <- as.list(select(tbl, insert$cols))
      names(send.data) <- NULL
      result <- dbSendStatement(con.f, insert$sql)
      dbBind(result, send.data)
      rows.affected <- dbGetRowsAffected(result)
      dbClearResult(result)
      cat(paste0(min(rows.affected, nrow(tbl)), "/", nrow(tbl), 
                 " rows affected.\n"))
    }
  }
}


#' The main processing function.
#'
#' @param pg_db A string. The database to connect to in the Postgres instance.
#' @param pg_user A string. The user name to connect to the Postgres instance 
#' with. 
#' @param password A string. The password to connect to the Postgres instance 
#'   with. 
#' @param dbpath A string file path that points to the location to create and 
#'   store data from the Postgres \code{schema} in an SQLite/SpatiaLite 
#'   database.
#'   
#' @param port An integer. The port which the Postgres instance uses to monitor
#'   connection requests.
#' @param host A string. The IP address or DNS name which hosts the database. 
#' @param schema A string. The name of the schema in the Postgres instance.
#'
#' @return Nothing.
#' @export
main <- function(pg_db, pg_user, password, dbpath, port = 5432,
                 host = "localhost", schema = "eco"){
  con.f <<- dbConnect(RSQLite::SQLite(), dbpath)
  res <- dbExecute(con.f, "PRAGMA foreign_keys = on;")
  con <- dbConnect(RPostgres::Postgres(), dbname = pg_db, 
                    host = host, port = port, 
                    user = pg_user, password = password)
  res <- dbExecute(con, "SET client_min_messages TO WARNING;")
  
  spatial <- tryCatch(
    expr = {
      res <- dbExecute(con.f, "SELECT load_extension('mod_spatialite');")
      TRUE
    },
    error = function(e){
      print("Spatialite module not found. Creating non-spatial db.")
      return(FALSE)
    }
  )
  create.sqlite(con = con.f, spatial = spatial, 
                sql.path = "sql/create_eco_tables.sql")
  copy.post.tables(schema = schema, spatial = spatial)
  
  dbDisconnect(con)
  rm(con, envir = .GlobalEnv)
  dbDisconnect(con.f)
  rm(con.f, envir = .GlobalEnv)
}

# run only if called from a script.
if (sys.nframe() == 0) {
  args = commandArgs(trailingOnly = TRUE)
  
  option_list <-  list (
    make_option(opt_str = c("-p", "--port"), default = 5432, type = "integer",
                help = paste0("The Postgres connection port")),
    make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    make_option(opt_str = c("-w", "--password"),
                help = paste0("The password for the user provided.")),
    make_option(opt_str = c("-s", "--schema"), default = "eco",
                help = paste0("The schema to export (eco, dima, lmf, public)."))
  )
  
  description <- paste0("Exports the eco schema to a sqlite database. \n",
                        "pg_db = The postgres database name. \n",
                        "pg_user = The posgres user name. \n",
                        "out_path = The file path for the export ",
                        "(i.e. *.sqlite, *.db)\n")
  opt_parser = OptionParser(usage = paste0("%prog [options] pg_db ",
                                           "pg_user out_path"), 
                            option_list = option_list, prog = NULL, 
                            description = description)
  
  opt = parse_args(opt_parser, positional_arguments = 3, args = args)
  if (is.null(opt$options$password)){
    opt$options$password = getPass()
  }
  
  main(pg_db = opt$args[1], pg_user = opt$args[2], port = opt$options$port,
       host = opt$options$host, password = opt$options$password, 
       dbpath = opt$args[3], schema = opt$options$schema)
  cat("\nScript finished.\n")
}