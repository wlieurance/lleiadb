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


#' Loads SQL statements from \code{path} and executes them, glueing parameters
#' as necessary. Can use the \code{parsesql} library to separate statements
#' and execute them individually if the DBI backend does not support block
#' execute via DBI::dbExecute(... immediate = TRUE), though parseql is much
#' slower for large sql files with many statements.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param path A character vector directory path which points to the location
#'   where SQL statements to be executed on the PostGIS instance are stored.
#' @param params A named list of variables, which can be used with
#'   glue syntax to replace parameters in \code{path}.
#' @param verbose Boolean. If TRUE will direct the function to print
#'   status messages.
#' @param immediate Boolean. If FALSE will direct the function to load the
#'   SQL from the SQL files and parse the indivually as a list to execute them
#'   individually. Else all statements will be sent to the PostgreSQL backend
#'   to be executed as a block statement
execute_sql <- function(con, path, params = NA, verbose = FALSE,
                        immediate = TRUE) {
  if (verbose == TRUE) {
    # consider using message() here and for other cat() when packaging
    cat("\tReading in SQL from file...\n")
  }
  if (immediate == TRUE) {
    sql_raw <- readr::read_file(path)
    if (!is.na(params)) {
      sql <- glue::glue_data(.x = params, sql_raw)
    } else {
      sql <- sql_raw
    }
    DBI::dbExecute(con, sql, immediate = TRUE)
  } else {
    sql_obj <- parsesql::sql_parser$new(
      file = path, params = params, standard = "PostgreSQL", verbose = verbose,
      fast = TRUE
    )
    sql <- sql_obj$sql

    if (verbose == TRUE) {
      cat("\tExecuting SQL statements...\n\t")
    }
    no_stmts <- length(sql)
    n <- 1
    for (stmt in sql) {
      # cat(stmt)
      if (verbose == TRUE) {
        complete_pct <- round(n / no_stmts * 100, 1)
        cat(paste0(complete_pct, "%..."))
      }
      tryCatch(
        expr = {
          DBI::dbExecute(con, stmt)
        },
        error = function(e) {
          cat(stmt)
          print(e)
          stop(e)
        }
      )
      n <- n + 1
    }
  }
  if (verbose == TRUE) {
    cat("\n")
  }
}


#' Creates extensions on the public schema of the Postgres instance.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param sql_path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create_exts <- function(con, sql_path) {
  cat("PUBLIC: creating extensions...\n")
  execute_sql(con = con, path = file.path(sql_path, "execute_init.sql"))
}


#' Creates, loads data into, and executes other statements within dima schema
#' of the Postgres instance.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param sql_path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create_dima <- function(con, sql_path) {
  cat("DIMA: creating tables...\n")
  execute_sql(con, path = file.path(sql_path, "create_dima_tables.sql"))
  cat("DIMA: inserting base data...\n")
  execute_sql(con, path = file.path(sql_path, "create_dima_data.sql"))
  cat("DIMA: creating triggers...\n")
  execute_sql(con, path = file.path(sql_path, "create_dima_triggers.sql"))
  cat("DIMA: commenting...\n")
  execute_sql(con, path = file.path(sql_path, "create_dima_comments.sql"))
}


#' Creates, loads data into, and executes other statements within lmf schema
#' of the Postgres instance.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param sql_path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create_lmf <- function(con, sql_path) {
  cat("LMF: creating tables...\n")
  execute_sql(con, path = file.path(sql_path, "create_lmf_tables.sql"))
  cat("LMF: commenting...\n")
  execute_sql(con, path = file.path(sql_path, "create_lmf_comments.sql"))
}


#' Creates, loads data into, and executes other statements within eco schema
#' of the Postgres instance.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param sql_path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create_eco <- function(con, sql_path) {
  cat("ECO: creating tables...\n")
  execute_sql(con, path = file.path(sql_path, "create_eco_tables.sql"))
  cat("ECO: commenting...\n")
  execute_sql(con, path = file.path(sql_path, "create_eco_comments.sql"))
}

#' Creates, loads data into, and executes other statements within aim_lotic
#' schema of the Postgres instance.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param sql_path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create_aim_lotic <- function(con, sql_path) {
  cat("AIM_LOTIC: creating tables...\n")
  execute_sql(con, path = file.path(sql_path, "create_aim_lotic_tables.sql"))
  cat("AIM_LOTIC: commenting...\n")
  execute_sql(con, path = file.path(sql_path, "create_aim_lotic_comments.sql"))
}


#' Creates, loads data into, and executes other statements within public schema
#' of the Postgres instance.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param sql_path A character vector directory path which points to the
#'   location where SQL statements to be executed on the PostGIS instance are
#'   stored.
create_public <- function(con, sql_path) {
  cat("PUBLIC: creating tables...\n")
  execute_sql(con = con, path = file.path(sql_path, "create_public_tables.sql"))
  cat("PUBLIC: inserting base data...\n")
  execute_sql(con = con, path = file.path(sql_path, "create_public_data.sql"))
  cat("PUBLIC: creating views...\n")
  execute_sql(con = con, path = file.path(sql_path, "create_public_views.sql"))
  cat("PUBLIC: commenting...\n")
  execute_sql(con = con,
              path = file.path(sql_path, "create_public_comments.sql"))
  cat("PUBLIC: executing statements...\n")
  execute_sql(con  = con,
              path = file.path(sql_path, "execute_public_statements.sql"))
}


#' Imports spatial features into the PostGIS database.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param spatial.path A character vector directory path which points to the
#'   location where spatial data to be imported into the PostGIS instance is
#'   stored.
create_spatial <- function(con, spatial_path) {
  cat("PUBLIC: importing spatial features...\n")
  stmt_d <- "DROP TABLE IF EXISTS public.timezone CASCADE;"
  stmt_c <- paste0(
    "CREATE TABLE public.timezone (tzid VARCHAR(30) PRIMARY KEY,",
    " geom geometry(MULTIPOLYGON, 4326));"
  )
  DBI::dbExecute(con, stmt_d)
  DBI::dbExecute(con, stmt_c)
  timezones <- sf::read_sf(file.path(spatial_path, "tz_world_mp.shp")) |>
    dplyr::rename_all(tolower) |>
    dplyr::rename(geom = "geometry") |>
    sf::st_set_crs(4326)
  suppressMessages(
    sf::st_write(obj = timezones, dsn = con, layer = "timezone",
                 quiet = TRUE, append = TRUE)
  )
}


#' Creates a PostGIS database and populates it with default data.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @export
create_lleiadb <-  function(con) {

  # these need to be retooled when this is (maybe) put into an R package
  sql_path <- here::here("sql")
  spatial_path <- here::here("spatial")

  DBI::dbExecute(con, "SET client_min_messages TO WARNING;")
  create_exts(con = con, sql_path = sql_path)
  create_dima(con = con, sql_path = sql_path)
  create_lmf(con = con, sql_path = sql_path)
  create_eco(con = con, sql_path = sql_path)
  create_aim_lotic(con = con, sql_path = sql_path)
  create_spatial(con = con, spatial_path = spatial_path)
  create_public(con = con, sql_path = sql_path)
}

# run only if called from a script.
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  source(here::here("common.R"))

  # change this when packaging to native package namespace resolution
  spatial_dir <- here::here("spatial")
  sql_dir <- here::here("sql")

  option_list <- list(
    optparse::make_option(
      opt_str = c("-p", "--port"), default = 5432,
      type = "integer",
      help = "The Postgres connection port [default: %default]."
    ),
    optparse::make_option(
      opt_str = c("-H", "--host"), default = "localhost",
      help = paste0("The host name or ip address of the connection ",
                    "[default: %default].")
    ),
    optparse::make_option(
      opt_str = c("-u", "--user"),
      help = "The user name for the database connection"
    ),
    optparse::make_option(
      opt_str = c("-w", "--password"),
      help = paste0("The password for the user [the user will be ",
                    "prompted if no password is supplied and is required].")
    ),
    optparse::make_option(
      opt_str = c("-c", "--allow_create"), action = "store_true",
      default = FALSE,
      help = paste0("Allows for database creation if the database does not ",
                    "already exist in the PostgreSQL isntance ",
                    "[default: %default].")
    ),
    optparse::make_option(
      opt_str = c("-m", "--maint_db"),
      help = paste0("If database creation is allowed, which existing ",
                    "maintenance database should be used for connection and ",
                    "execution of CREATE DATABASE statement. Will ",
                    "default to database of same name as `USER` if not ",
                    "supplied.")
    )
  )

  opt_parser <- optparse::OptionParser(
    usage = paste0("usage: %prog [options] dbname"),
    option_list = option_list,
    prog = NULL,
    description = paste(
      "Will create an empty PostgreSQL database to store data",
      "from the following data sources:\n",
      "1) USDA-ARS Jornada's Database for Inventory Monitoring",
      "and Assessment (DIMA),\n",
      "2) U.S. DOI Bureau of Land Management's Landscape",
      "Monitoring Framework (LMF) database (should also be",
      "compatible with USDA-NRCS National Resources Inventory",
      "(NRI) data).\n",
      "3) Native format.",
      sep = " "
    )
  )
  opt <- optparse::parse_args(opt_parser, positional_arguments = 1, args = args)

  con <- connect_pg(
    dbname = opt$args[1],
    host = opt$options$host,
    port = opt$options$port,
    user = opt$options$user,
    password = opt$options$password,
    allow_create = opt$options$allow_create,
    maintenance_db = opt$options$maint_db
  )

  create_lleiadb(con = con)

  DBI::dbDisconnect(con)
  rm(con, envir = .GlobalEnv)

  cat("\nScript finished.\n")

}
