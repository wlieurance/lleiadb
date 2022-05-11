#!/usr/bin/env Rscript

# github_libraries = c(list(name = "parsesql",
#                           location = "wlieurance/parsesql/R")
#                      )
#
# libraries = c("DBI", "dplyr", "getPass", "glue", "optparse", "pool",
#               "readr", "RPostgres", "sf", "stringr", "XML")
#
# github_names = subset(unlist(github_libraries),
#                       names(unlist(github_libraries)) == "name")
# for (lib in c(libraries, github_names)){
#   suppressMessages(library(lib, character.only = TRUE))
# }



#' Creates a pooled database object that other functions use to interact with
#' the database.
#'
#' @param dbname A character vector. The database name to connect to in the postgres
#'   instance.
#' @param host A character vector. The IP address or DNS name which hosts the database.
#' @param port An integer. The port which the postgres service monitors for
#'   connections.
#' @param user A character vector. The database user used to connect to the database.
#' @param password A character vector. The password used to connect to the database.
create.pool <- function(dbname, host, port, user, password){
  tryCatch(
    expr = {
      pool::dbPool(RPostgres::Postgres(), dbname = dbname, host = host,
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
          DBI::dbExecute(con, stmt)
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
#' @param path A character vector directory path which points to the location
#'   where SQL statements to be executed on the PostGIS instance are stored.
#' @param params A named list of variables, which can be used with
#'   glue syntax to replace parameters in \code{path}.
#' @param verbose Boolean. If TRUE will direct the function to print
#'   status messages.
#' @param raw Boolean. If TRUE will direct the function to load the
#'   SQL from the SQL files directly instead of using the \code{sql.list}
#'   variable.
execute.sql <- function(path, params = NA, verbose = FALSE, raw = FALSE){
  if (verbose == TRUE & raw == TRUE){
    cat("\tReading in SQL from file...\n")
  }
  if (raw == TRUE){
    sql.obj <- parsesql::sql_parser$new(
      file = path, params = params, standard = 'PostgreSQL', verbose = verbose,
      fast = TRUE)
    sql <- sql.obj$sql
  } else {

    sql <- lleiadb::sql.list[[basename(path)]][["sql"]]
  }
  if (verbose == TRUE){
    cat("\tExecuting SQL statements...\n\t")
  }
  no_stmts = length(sql)
  n = 1
  for (stmt in sql){
    # cat(stmt)
    if (verbose == TRUE){
      complete_pct = round(n/no_stmts * 100, 1)
      cat(paste0(complete_pct, "%..."))
    }
    res <- tryCatch(
      expr = {
        DBI::dbExecute(pool, stmt)
      },
      error = function(e){
        cat(stmt)
        print(e)
        stop(e)
      })
    # print(res)
    n <- n + 1
  }
  if (verbose == TRUE){
    cat("\n")
  }
}


#' Creates extensions on the public schema of the Postgres instance.
#'
#' @param sql.path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create.exts <-  function(sql.path){
  cat("PUBLIC: creating extensions...\n")
  execute.sql(path = file.path(sql.path, "execute_init.sql"))
}


#' Creates, loads data into, and executes other statements within dima schema
#' of the Postgres instance.
#'
#' @param sql.path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create.dima <- function(sql.path){
  cat("DIMA: creating tables...\n")
  execute.sql(path = file.path(sql.path, "create_dima_tables.sql"))
  cat("DIMA: inserting base data...\n")
  execute.sql(path = file.path(sql.path, "create_dima_data.sql"))
  cat("DIMA: creating triggers...\n")
  execute.sql(path = file.path(sql.path, "create_dima_triggers.sql"))
  cat("DIMA: commenting...\n")
  execute.sql(path = file.path(sql.path, "create_dima_comments.sql"))
}


#' Creates, loads data into, and executes other statements within lmf schema
#' of the Postgres instance.
#'
#' @param sql.path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create.lmf <- function(sql.path){
  cat("LMF: creating tables...\n")
  execute.sql(path = file.path(sql.path, "create_lmf_tables.sql"))
  cat("LMF: commenting...\n")
  execute.sql(path = file.path(sql.path, "create_lmf_comments.sql"))
}


#' Creates, loads data into, and executes other statements within eco schema
#' of the Postgres instance.
#'
#' @param sql.path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create.eco <- function(sql.path){
  cat("ECO: creating tables...\n")
  execute.sql(path = file.path(sql.path, "create_eco_tables.sql"))
}


#' Creates, loads data into, and executes other statements within public schema
#' of the Postgres instance.
#'
#' @param sql.path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create.public <- function(sql.path){
  cat("PUBLIC: creating tables...\n")
  execute.sql(path = file.path(sql.path, "create_public_tables.sql"))
  cat("PUBLIC: inserting base data...\n")
  execute.sql(path = file.path(sql.path, "create_public_data.sql"), verbose = T)
  cat("PUBLIC: creating views...\n")
  execute.sql(path = file.path(sql.path, "create_public_views.sql"))
  cat("PUBLIC: executing statements...\n")
  execute.sql(path = file.path(sql.path, "execute_public_statements.sql"))
}


#' Imports spatial features into the PostGIS database.
#'
#' @param spatial.path A character vector directory path which points to the
#'   location where spatial data to be imported into the PostGIS instance is
#'   stored.
create.spatial <- function(spatial.path){
  cat("PUBLIC: importing spatial features...\n")
  stmt.d = "DROP TABLE IF EXISTS public.timezone CASCADE;"
  stmt.c = paste0("CREATE TABLE public.timezone (tzid VARCHAR(30) PRIMARY KEY,",
                " geom geometry(MULTIPOLYGON, 4326));")
  DBI::dbExecute(pool, stmt.d)
  DBI::dbExecute(pool, stmt.c)
  timezones <- sf::read_sf(file.path(spatial.path, "tz_world_mp.shp")) |>
    dplyr::rename_all(tolower) |>
    dplyr::rename(geom = geometry) |>
    sf::st_set_crs(4326)
  suppressMessages(sf::st_write(obj = timezones, dsn = pool, layer = "timezone",
                           quiet = TRUE, append = TRUE))
}


#' An SQL execution function which uses a try-catch statement to skip/print
#' errors if found in the comment SQL.
#'
#' @param sql character vector. The SQL to execute.
execute.comment <- function(sql){
  res <- tryCatch(
    expr = {
      DBI::dbExecute(pool, sql)
    },
    error = function(e){
      print(e)
      cat(paste0("FAILED: ", sql, " skipping...\n\n"))
    })
}

#' Creates comment SQL.
#'
#' @param tbl.type character vector. The type of comment to be created. One of
#'   c("table", "view", "column")
#' @param schema character vector. The name of the schema on which to comment.
#' @param name character vector. The name of the table or view on which to
#'   comment.
#' @param def character vector. The comment text.
#' @param col character vector. The name of the column on which to
#'   comment. Can be NULL to make a table or view comment.
#'
#' @return character vector. An SQL statement that can be executed by the
#'   database.
create.comment.sql <- function(tbl.type, schema, name, def, col = NULL){
  type <- stringr::str_to_upper(tbl.type)
  name.frmt <- paste0('"', name, '"')
  if (!is.null(col)){
    col.frmt <- paste0('"', col, '"')
  } else {
    col.frmt <- col
  }
  def.frmt <- def |>
    stringr::str_replace_all("&#60;|&lt;", "<") |>
    stringr::str_replace_all("&#62;|&gt;", ">") |>
    stringr::str_replace_all("&#38;|&amp;", "&") |>
    stringr::str_replace_all("&#39;|&apos;|'", "''") |>
    stringr::str_replace_all("&#34;|&quot;", '"')
  fullname <- paste(c(schema, name.frmt, col.frmt), collapse = ".")
  sql <- glue::glue("COMMENT ON {type} {fullname} IS '{def.frmt}';")
  return(sql)
}


#' Main processing function for inserting iso19110-2016 catalog metadata in XML
#' format into the database as comments.
#'
#' @param xml.path A character vector. Path to the xml metadata for the
#'   database.
comment.from.xml <- function(xml.path){
  cat("Writing table/view and field COMMENTs to database from metadata...\n")
  data <- XML::xmlParse(xml.path)
  xml_data <- XML::xmlToList(data)

  # extract xml FC_FeatureType and FC_FeatureAttribute to list
  # and check for xlink mismatches in xml.
  comment.list <- list()
  i <- 0  # a counter to keep track of iterations in the outer loop
  for (tag in xml_data){
    i <-  i + 1
    if (class(tag) == "list"){
      if (!is.null(tag$FC_FeatureType)){
        name <- tag$FC_FeatureType$typeName
        def <- tag$FC_FeatureType$definition$CharacterString
        abstract <- tag$FC_FeatureType$isAbstract$Boolean
        if (abstract == "false"){
          type <- "table"
        } else {
          type <- "view"
        }
        id <- tag$FC_FeatureType$.attrs[["id"]]
        if (id != name){
          print(paste0("mismatch: id: ", id, " name: ", name))
        }
        j <- 0  # a counter to keep track of iterations in the inner loop
        field.list <- list()
        for (tag2 in tag$FC_FeatureType){
          j <-  j + 1
          if (!is.null(tag2) & class(tag2) == "list"){
            if (!is.null(tag2$FC_FeatureAttribute)){
              if (!is.null(tag2$FC_FeatureAttribute$featureType)){
                link <- tag2$FC_FeatureAttribute$featureType[["href"]]
                memname <- tag2$FC_FeatureAttribute$memberName
                memdef <- tag2$FC_FeatureAttribute$definition$CharacterString
              }
              if (paste0("#", id) != link){
                print(paste0("in table ", id, ": xlink mismatch: ", link,
                             " for memberName: ", memname))
              }
              field.sub <- list(type="column", name=memname, def=memdef)
              field.list[[length(field.list) + 1]] <- field.sub
            }
          }
        }
        comment.sub <- list(type=type, name=name, def=def, fields=field.list)
        comment.list[[length(comment.list) + 1]] <- comment.sub
      }
    }
  }

  # construct comment sql for each table/view and column within.
  sql.v = character()
  for (comment in comment.list){
    element <- stringr::str_split(comment$name, "\\.")[[1]]
    schema <- element[1]
    tbl <- element[2]
    sql <- create.comment.sql(tbl.type = comment$type, schema = schema,
                              name = tbl, def = comment$def)
    # print(sql)
    sql.v <- c(sql.v, sql)
    for (field in comment$fields){
      sql.f <- create.comment.sql(tbl.type = field$type, schema = schema,
                                  name = tbl, col = field$name, def = field$def)
      # print(sql.f)
      sql.v <- c(sql.v, sql.f)
    }
  }

  # execute the comment sql
  for (sql in sql.v){
    execute.comment(sql)
  }
}

#' Creates a PostGIS database and populates it with default data.
#'
#' @param dbname character vector. The database name to connect to in the
#'   postgres instance.
#' @param user character vector. The database user used to connect to the
#'   database.
#' @param password character vector. The password used to connect to
#'   the database. If no password is provided, user will be prompted upon
#'   execution.
#' @param sql.path character vector directory path which points to
#'   the location where the SQL files for DB creation are stored.
#' @param spatial.path character vector directory path which
#'   points to the location where spatial data to be imported into the PostGIS
#'   instance is stored.
#' @param host character vector. The IP address or DNS name which
#'   hosts the database.
#' @param port An integer. The port which the postgres service
#'   monitors for connections.
#' @export
create.lleiadb <-  function(dbname, user, password = getPass::getPass(),
                            sql.path = "sql", spatial.path = "spatial",
                            host = "localhost", port = 5432){
  # <<- creates global pool (parent scope of main())
  pool <<- create.pool(dbname = dbname, host = host,
                      port = port, user = user,
                      password = password)
  # assign("pool", pool, envir = .GlobalEnv)
  res <- DBI::dbExecute(pool, "SET client_min_messages TO WARNING;")
  create.exts(sql.path)
  create.dima(sql.path)
  create.lmf(sql.path)
  create.eco(sql.path)
  create.spatial(spatial.path)
  create.public(sql.path)
  comment.from.xml(xml.path = "./metadata/lleiadb_metadata_iso19110-2016.xml")

  pool::poolClose(pool)
  rm(pool, envir = .GlobalEnv)
  cat("\nScript finished.\n")
}

# run only if called from a script.
if (sys.nframe() == 0) {
  option_list = list (
    optparse::make_option(opt_str = c("-p", "--port"), default = 5432,
                          type = "integer",
                help = paste0("The Postgres connection port")),
    optparse::make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    optparse::make_option(opt_str = c("-w", "--password"),
                help = paste0("The password for the user provided.")),
    optparse::make_option(opt_str = c("-s", "--sql_dir"), default = "sql",
                help = paste0("The directory where the database creation SQL ",
                              "files are stored.")),
    optparse::make_option(opt_str = c("-S", "--spatial_dir"),
                default = "spatial",
                help = paste0("The directory where the spatial data to be
                              imported into the PostGIS instance is stored."))
  )

  opt_parser = optparse::OptionParser(
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
  opt = optparse::parse_args(opt_parser, positional_arguments = 2, args = args)
  if (is.null(opt$options$password)){
    opt$options$password = getPass::getPass()
  }

  create.lleiadb(
    dbname = opt$args[1], user = opt$args[2],
    password = opt$options$password, sql.path = opt$options$sql_dir,
    spatial.path = opt$options$spatial_dir, host = opt$options$host,
    port = opt$options$port)
}
