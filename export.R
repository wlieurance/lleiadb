#!/usr/bin/env Rscript

# libraries = c("DBI", "dbplyr", "dplyr", "optparse",
#               "RPostgres", "RSQLite", "stringr", "here")
#
# for (lib in libraries){
#   suppressMessages(library(lib, character.only = TRUE))
# }

# custom sources
# suppressMessages(source("import.R"))


#' Takes a CREATE TABLE SQL statement for a Postgres instance and converts it
#' to A CREATE TABLE statement that will work for SQLite.
#'
#' @param sql A string CREATE TABLE statement for a Postgres instance.
#'
#' @return A list containing a string CREATE TABLE statement for an SQLite
#'   database, the name of the table, and geometry type and srid variables if
#'   present.
#' @export
convert_sqlite <- function(sql) {
  table_match <- sql |> stringr::str_match(
    stringr::regex(
      paste0("(?:CREATE|DROP) TABLE (?:IF (?:NOT ){0,1}EXISTS )",
             "{0,1}([^\\.]+)\\.([^\\s]+)"),
      ignore_case = TRUE
    )
  )
  if (is.na(table_match[1])) {
    table <-  NULL
  } else {
    table <- table_match[3]
  }
  geom_match <- sql |> stringr::str_match(
    stringr::regex(
      "geom geometry\\s*\\(([^\\s,]+)[,\\s]+(\\d+)\\s*\\)[,\\s]*",
      ignore_case = TRUE
    )
  )

  if (is.na(geom_match[1])) {
    geom <- NULL
  } else {
    geom <-  c(geom_match[2], as.integer(geom_match[3]))
  }

  sql_new <- sql |>
    stringr::str_replace_all(
      stringr::regex(
        paste0("(\\s+(?:FROM|JOIN|VIEW|TABLE|EXISTS|REFERENCES|",
               "ON|INTO)\\s+)[A-Za-z_\\d]+\\."),
        ignore_case = TRUE
      ), "\\1"
    ) |>
    stringr::str_replace_all(
      stringr::regex("CREATE SCHEMA IF NOT EXISTS [A-Za-z]+;",
                     ignore_case = TRUE),
      ""
    ) |>
    stringr::str_replace_all(
      stringr::regex("(DROP TABLE IF EXISTS [a-z_A-Z\\.]+)\\s+CASCADE;",
                     ignore_case = TRUE),
      "\\1;"
    ) |>
    stringr::str_replace_all(
      stringr::regex(",\\s+geom geometry\\([^\\)]+\\)",
                     ignore_case = TRUE),
      ""
    ) |>
    stringr::str_replace_all(
      stringr::regex("CREATE INDEX .*? USING gist .*?;",
                     ignore_case = TRUE),
      ""
    ) |>
    stringr::str_replace_all(
      stringr::regex("\\s+USING btree", ignore_case = TRUE),
      ""
    )

  list(sql = sql_new, table = table, geom = geom)
}


#' Attempts to execute SQL statement on a db connection and prints/logs the
#' SQL statement and error to stdout and/or log file in case of failure.
#'
#' @param dbcon A database connection created with DBI::dbConnect().
#' @param sql character string. An sql statement to execute.
#'
#' @return A scalar numeric that specifies the number of rows affected by the
#'   statement.
try_execute_sql <- function(dbcon, sql) {
  for (stmt in sql){
    # cat(stmt)
    tryCatch(
      expr = {
        DBI::dbExecute(dbcon, stmt)
      },
      error = function(e) {
        cat(stmt)
        print(e)
        stop(e)
      }
    )
    # print(res)
  }
}


#' Loads CREATE TABLE statements from a file, separates them, converts them to
#' SQlite statements, executes them and adds spatial metadata and columns if
#' necessary
#'
#' @param con_f An RSQLite database connection.
#' @param spatial logical. A flag telling the function to initialize SpatiaLite
#'   metadata, converting the database from SQLite to SpatialLite.
#' @param sql_path A string file path to a file containing Postgres CREATE TABLE
#'   statements.
create_sqlite <- function(con_f, spatial, sql_path) {
  cat("Creating sqlite database...\n")
  if (spatial == TRUE) {
    cat("Intializing spatial metadata...\n")
    DBI::dbExecute(con_f, "SELECT InitSpatialMetadata(1);")
  }
  sql_obj <- parsesql::sql_parser$new(
    file = sql_path, standard = "PostgreSQL", fast = TRUE
  )
  sql_list <- sql_obj$sql
  new_sql_list <- lapply(X = sql_list, FUN = convert_sqlite)
  for (s in new_sql_list){
    stmt <-  s$sql
    geom <- s$geom
    table <- s$table
    if (!trimws(stmt) %in% c("", ";")) {
      try_execute_sql(dbcon = con_f, sql = stmt)
      if (!is.null(geom) && spatial == TRUE) {
        geom_stmt <- paste0("SELECT AddGeometryColumn('", table,
                            "', 'geom', ", geom[2], ", '", geom[1], "');")
        DBI::dbExecute(con_f, geom_stmt)
      }  # end if
    }  # end if
  }  # end for loop
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
create_post_select <- function(con, schema, table, spatial) {
  # for packaging change this to:
  # lleiadb:::get_dest_info(con = con, schema = schema, table = table)
  tbl_info <- get_dest_info(con = con, schema = schema, table = table)
  cols <- tbl_info$col_info$column_name
  cols_quoted <- paste0('"', cols, '"')
  if (spatial == TRUE) {
    cols_new <- cols_quoted |>
      stringr::str_replace('^"geom"$', 'st_astext("geom") geom')
  } else {
    cols_new <- cols_quoted[cols_quoted != '"geom"']
  }
  col_string <-  paste(cols_new, collapse = ", ")
  select <- paste0(
    "SELECT ", col_string, "\n  FROM ", schema, '."', table, '";'
  )

  list(sql = select, cols = cols)
}


#' This function creates an SQLite INSERT statement based on the table
#' definition stored in the Postgres instance.
#'
#' @param con A DBI connection object to the source PostgreSQL database via
#'    dbConnect
#' @param schema A string. The name of the schema in the Postgres instance.
#' @param table A string. The name of the table in the Postgres instance.
#' @param update logical. A flag telling the function to create an UPSERT
#'   statement rather than an INSERT statement.
#' @param ins_cols A string vector which contains columns names to use in the
#'   INSERT statement.
#' @param srid An integer which is the srid/EPSG code representing the GPS
#'   coordinate system/datum to store spatial data in.
#'
#' @return A list containing the INSERT SQL statement as well as a vector of
#'   column names used to construct it.
create_sqlite_insert <- function(con, schema, table, update = FALSE,
                                 ins_cols = NULL, srid = 4326) {
  # convert to package, use:
  # info <- lleiadb:::get.dest.info(schema, table)
  info <- get_dest_info(con = con,  schema = schema, table = table)
  # gets the pkey constraint name or first unique constraint name
  # on conflict statement only allows one constraint check
  if (is.null(ins_cols)) {
    cols <- info$col_info$column_name
  } else {
    restricted <- info$col_info |>
      dplyr::inner_join(tibble::as_tibble(list(column_name = ins_cols)),
                        by = c("column_name" = "column_name"))
    cols <- restricted$column_name
  }
  if (update == FALSE) {
    update_sql <- "OR IGNORE"
  } else {
    update_sql <- "OR REPLACE"
  }
  colstring <- paste(paste0('"', cols, '"'), collapse = ", ")
  params <- rep("?", length(cols))
  geom_col <- which(cols == "geom")
  if (length(geom_col == 1)) {
    params[geom_col] <- paste0("ST_GeomFromText(?, ", srid, ")")
  }
  # for convsersion to character datetime
  # dt.cols <- !is.na(as.vector(info$col.info$data_type |>
  #   stringr::str_match(stringr::regex("(?:timestamp|date)",
  #                                     ignore_case = TRUE))))
  # params.dt <- ifelse(dt.cols, paste0(params, "varchar"), params)

  paramstring <- paste(params, collapse = ", ")
  insert_sql <- glue::glue(paste(
    "INSERT {update_sql} INTO \"{table}\" ({colstring})",
    "VALUES ({paramstring});",
    sep = "\n"
  ))

  list(sql = insert_sql, cols = cols)
}


#' Queries the Postgres instance for table data and inserts data into an SQLite
#' table.
#'
#' @param con A DBI connection object to the source PostgreSQL database via
#'    dbConnect
#' @param con_f A DBI connection object to the destination SQLite database via
#'    dbConnect
#' @param schema A string. The name of the schema in the Postgres instance.
#' @param spatial logical. A flag tell the function that \code{table} contains
#'   PostGIS geometry columns.
copy_post_tables <-  function(con, con_f, schema, spatial) {
  cat("Copying data...\n")
  # for packaging: tbl_order <- lleiadb:::insert_order(con, schema)
  tbl_order <- insert_order(con = con, schema = schema)
  for (i in rownames(tbl_order)){
    tbl_name <- tbl_order[i, 1]$tblname

    # get data
    query <- create_post_select(con = con, schema = schema, table = tbl_name,
                                spatial = spatial)
    tbl <- DBI::dbGetQuery(con, query$sql)

    if (nrow(tbl) > 0) {
      # do insert
      cat(paste0("Inserting into ", tbl_name, " ... "))
      insert <- create_sqlite_insert(con = con, schema = schema,
                                     table = tbl_name, ins_cols = query$cols)
      send_data <- as.list(dplyr::select(tbl, insert$cols))
      names(send_data) <- NULL
      rows_affected <- DBI::dbExecute(con_f, insert$sql, params = send_data)
      cat(paste0(min(rows_affected, nrow(tbl)), "/", nrow(tbl),
                 " rows affected.\n"))
    }
  }
}


#' The main processing function used to copy PostGIS tables to the SQLite
#' database.
#'
#' @param pg_db A string. The database to connect to in the Postgres instance
#' @param con A DBI connection object to the source PostgreSQL database via
#'    dbConnect
#' @param dbpath A string file path that points to the location to create and
#'   store data from the Postgres \code{schema} in an SQLite/SpatiaLite
#'   database
#' @param schema A string. The name of the schema in the Postgres instance
#'
#' @export
post_to_sqlite <- function(con, pg_db, dbpath, schema = "eco") {
  con_f <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  DBI::dbExecute(con_f, "PRAGMA foreign_keys = ON;")
  DBI::dbExecute(con, "SET client_min_messages TO WARNING;")

  spatial <- tryCatch(
    expr = {
      DBI::dbExecute(con_f, "SELECT load_extension('mod_spatialite');")
      TRUE
    },
    error = function(e) {
      print("Spatialite module not found. Creating non-spatial db.")
      FALSE
    }
  )

  # Project-local SQL path.
  # NOTE: If this code is moved into an R package, move SQL files to inst/sql/
  # and replace here::here(...) with system.file("sql", "create_eco_tables",
  # package = "my_pkg_name").
  sql_path <- here::here("sql", "create_eco_tables.sql")
  create_sqlite(con = con_f, spatial = spatial,
                sql_path = sql_path)
  copy_post_tables(con = con, con_f = con_f, schema = schema,
                   spatial = spatial)

  DBI::dbDisconnect(con_f)
}

# run only if called from a script.
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  source(here::here("common.R"))
  source(here::here("import.R"))

  option_list <-  list(
    optparse::make_option(
      opt_str = c("-p", "--port"), default = 5432, type = "integer",
      help = paste0("The Postgres connection port")
    ),
    optparse::make_option(
      opt_str = c("-H", "--host"), default = "localhost",
      help = paste0("The host name or ip address of the connection")
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
      opt_str = c("-s", "--schema"), default = "eco",
      help = paste0("The schema to export (dima, eco, lmf, public).")
    )
  )

  description <- paste("Exports the eco schema to a sqlite database.",
                       "pg_db = The postgres database name,",
                       "out_path = The file path for the export",
                       "(i.e. *.sqlite, *.db)", sep = " ")
  opt_parser <- optparse::OptionParser(
    usage = paste0("%prog [options] pg_db out_path"),
    option_list = option_list, prog = NULL,
    description = description
  )

  opt <- optparse::parse_args(opt_parser, positional_arguments = 2, args = args)

  con <- connect_pg(
    dbname = opt$args[1],
    host = opt$options$host,
    port = opt$options$port,
    user = opt$options$user,
    password = opt$options$password
  )

  post_to_sqlite(con = con, pg_db = opt$args[1], dbpath = opt$args[2],
                 schema = opt$options$schema)

  DBI::dbDisconnect(con)
  cat("\nScript finished.\n")
}
