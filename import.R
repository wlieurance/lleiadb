#!/usr/bin/env Rscript
# libraries = c("optparse", "dplyr", "tibble", "digest", "tictoc", "DBI",
#               "pool", "RPostgres", "RSQLite", "odbc", "sf", "getPass",
#               "tools", "stringr", "glue", "here")
#
# for (lib in libraries) {
#   suppressMessages(library(lib, character.only = TRUE))
# }

#' load special binary operators
`%do%` <- foreach::`%do%`
`%dopar%` <- foreach::`%dopar%`
.data <- rlang::.data
.env <- rlang::.env

#' This vectorizes the digest function from the digest package
vdigest <- Vectorize(digest::digest)

#' This function will take a log message and write it to stdout and/or a log
#' file.
#'
#' @param msg A string. The message to write.
#' @param log A string file path to a log wehre messages are to be written.
#' @param sep A string, the separator to paste after the message (e.g. an EOL
#'   character)
#' @param prnt logical. A flag indicating whether or not to print the msg to
#'   stdout
msg_out <- function(msg, log = NULL, sep = "\n", prnt = TRUE) {
  if (prnt == TRUE) {
    cat(paste0(msg, sep))
  }
  if (!is.null(log)) {
    log_file <- file(log, "a", encoding = "UTF-8")
    writeLines(msg, log_file, sep = sep)
    close(log_file)
  }
}

#' This function is a cheap statement splitter in lieu of a more time consuming
#' SQL parser.  Will break pretty easily so use with caution.
#'
#' @param string A string. A string of (optionally) multiple statements to
#'    split.
#' @param pattern. A stringr regex pattern (optional). Use in case the default
#'  of splitting at semicolon and INSERT, SELECT, UPDATE, DELETE, and CREATE is
#'  not adequate.
statement_split <- function(string, pattern = NULL) {
  if (is.null(pattern)) {
    pattern <- stringr::regex(r"{;\s*(?:INSERT|SELECT|UPDATE|DELETE|CREATE)\s}",
                              ignore_case = TRUE)
  }
  breaks <- stringr::str_locate_all(string = string, pattern = pattern)[[1]]
  if (nrow(breaks) > 0) {
    start <- 1
    new_mat <- matrix(nrow = nrow(breaks) + 1, ncol = 2,
                      dimnames = list(NULL, c("start", "end")))
    for (i in seq_along(breaks)) {
      new_mat[i, 1] <- start
      brk <- breaks[[i, 1]]
      new_mat[i, 2] <- brk
      start <- brk + 1
    }
    if (start < nchar(string)) {
      new_mat[nrow(new_mat), 1] <- start
      new_mat[nrow(new_mat), 2] <- nchar(string)
    }
    split <- mapply(FUN = stringr::str_sub, start = new_mat[, 1],
                    end = new_mat[, 2],
                    MoreArgs = list(string = string))
  } else {
    split <- string
  }
  return(split)
}

#######################################
## retrieving info related functions ##
#######################################


#' Queries the Postgres LLEIA db instance for tables in a specific schema
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The schema to query for tables.
#'
#' @return A table containing the table names in \code{schema}.
get_dest_tables <- function(con, schema) {
  tables_sql <- glue::glue(paste(
    "SELECT table_name ",
    "  FROM information_schema.tables ",
    " WHERE table_schema = '{schema}' ",
    "   AND table_type = 'BASE TABLE';",
    sep = "\n"
  ))

  DBI::dbGetQuery(con, tables_sql)
}

#' Retrieves Foreign keys for a table in Postgres
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The name of the schema to be queried in the Postgres
#'   connection.
#' @param tbl_name A string. The name of the table to retrieve information about
#'   from the postgres connection.
get_foreign_keys <- function(con, schema, tbl_name) {
  # query provided by user @martin on StackOverflow
  # https://stackoverflow.com/questions/1152260/how-to-list-table-foreign-keys
  fk_sql  <- glue::glue(paste(
    "WITH con AS (",
    "SELECT unnest(con1.conkey) AS parent,",
    "           unnest(con1.confkey) AS child,",
    "           con1.confrelid, con1.conrelid, con1.conname",
    "  FROM pg_class cl",
    " INNER JOIN pg_namespace ns ON cl.relnamespace = ns.oid",
    " INNER JOIN pg_constraint con1 ON con1.conrelid = cl.oid",
    " WHERE cl.relname = '{tbl_name}'",
    "   AND ns.nspname = '{schema}'",
    "   AND con1.contype = 'f'",
    ")",
    "",
    "SELECT att2.attname as child_column, cl.relname as parent_table,",
    "       att.attname as parent_column, conname",
    "  FROM con",
    " INNER JOIN pg_attribute att",
    "    ON att.attrelid = con.confrelid AND att.attnum = con.child",
    " INNER JOIN pg_class cl",
    "    ON cl.oid = con.confrelid",
    " INNER JOIN pg_attribute att2",
    "    ON att2.attrelid = con.conrelid AND att2.attnum = con.parent;",
    "", sep = "\n"
  ), .trim = FALSE)

  tibble::as_tibble(DBI::dbGetQuery(con, fk_sql))
}


#' Retrieves Primary keys for a table in Postgres
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The name of the schema to be queried in the Postgres
#'   connection.
#' @param tbl_name A string. The name of the table to retrieve information about
#'   from the postgres connection.
get_primary_keys <- function(con, schema, tbl_name) {
  pk_sql <- glue::glue(paste(
    "SELECT tc.constraint_name, tc.constraint_type, c.column_name, c.data_type",
    "  FROM information_schema.table_constraints tc",
    " INNER JOIN information_schema.constraint_column_usage AS ccu",
    " USING (constraint_schema, constraint_name)",
    " INNER JOIN information_schema.columns AS c",
    "    ON c.table_schema = tc.constraint_schema",
    "   AND tc.table_name = c.table_name AND ccu.column_name = c.column_name",
    " WHERE constraint_type IN ('PRIMARY KEY', 'UNIQUE')",
    "   AND tc.table_schema = '{schema}' AND tc.table_name = '{tbl_name}';",
    "", sep = "\n"
  ), .trim = FALSE)

  tibble::as_tibble(DBI::dbGetQuery(con, pk_sql))
}


#' Retrieves Primary and Foreign keys for a table in Postgres
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The name of the schema to be queried in the Postgres
#'   connection.
#' @param tbl_name A string. The name of the table to retrieve information about
#'   from the postgres connection.
get_pg_tbl_constraints <- function(con, schema, tbl_name) {
  fk <- get_foreign_keys(con = con, schema = schema, tbl_name = tbl_name)
  pk <- get_primary_keys(con = con, schema = schema, tbl_name = tbl_name)

  list(fk = fk, pk = pk)
}

#' This function generates an md5hash for database files or ESRI file
#' geodatabase folders.
#'
#' @param src A string file path to a file with one of the following extensions:
#'   (.mdb, .accdb, .sqlite, .db, .gpkg) or a folder path with the .gdb
#'   extension.
#'
#' @return An md5hash of the file, or in the case of a folder, a hash of the
#'   vector of hashes for each of the files in the folder, minus certain files
#'   in the gdb folder which are mutable or transient.
get_key <- function(src) {
  allowed_exts <- c("mdb", "accdb", "gdb", "sqlite", "db", "gpkg")
  ext <- tools::file_ext(src)
  if (stringr::str_to_lower(ext) == "gdb" && dir.exists(src)) {
    exists <-  TRUE
    in_type <- "folder"
  } else if (stringr::str_to_lower(ext) != "gdb" && file.exists(src)) {
    exists <-  TRUE
    in_type <- "file"
  } else {
    exists <- FALSE
  }

  if (!exists) {
    stop("Source path does not exist. Quitting...")
  }

  if (!(ext %in% allowed_exts)) {
    stop(paste0("Source is not of type (", paste(allowed_exts, collapse = ", "),
                "). Quitting..."))
  }

  cat("Calculating md5 hash...\n")
  if (in_type == "file") {
    key <- digest::digest(src, algo = "md5", file = TRUE)
  } else if (in_type == "folder") {
    files <- list.files(path = src, recursive = TRUE, all.files = TRUE,
                        full.names = FALSE) |>
      # removes anything which ends in .lock
      stringr::str_subset(pattern = ".*(?<!\\.lock)$") |>
      # removes a00000004.gdbtable and a00000004.gdbtablx, a00000004.freelist,
      # and timestamps, which seem to change even when the only file operation
      # is copy
      stringr::str_subset(pattern = stringr::fixed("a00000004.gdbtabl"),
                          negate = TRUE) |>
      stringr::str_subset(pattern = stringr::fixed("a00000004.freelist"),
                          negate = TRUE) |>
      stringr::str_subset(pattern = stringr::fixed("timestamps"), negate = TRUE)

    full_files <- sapply(files, FUN = function(x) file.path(src, x))
    hashes <- sapply(
      full_files,
      FUN = function(x) digest::digest(x, algo = "md5", file = TRUE)
    )
    key <- digest::digest(hashes, algo = "md5")
  }

  key
}


#' Recursive function which returns an ordered table of schema table names
#' where tables that are foreign key parents are listed in order before their
#' children
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The schema to be queried.
#' @param level An integer. An internal parameter that the function uses to keep
#'   track of how many times it has self called.
#' @param processed.tables A tibble. An internal parameter that the function
#'   uses to keep track of which tables have been processed and stored in the
#'   return variable.
#' @param tables A string vector. An internal parameter that the function uses
#'   to keep keep track of unprocessed table names.
#'
#' @return A tibble that gives each table in \code{schema} and a proper insert
#'   order that will prevent improper foreign key violations.
insert_order <- function(con, schema, level = 0, processed_tables = NULL,
                         tables = NULL) {
  # print(level)
  if (is.null(tables)) {
    tables <- get_dest_tables(con, schema)$table_name
  }

  level_tables <- tibble::tibble(
    tblname = character(),
    level = integer()
  )

  if (is.null(processed_tables)) {
    processed_tables <- level_tables
  }

  for (tbl in tables) {
    parents <- get_foreign_keys(con = con, schema = schema, tbl_name = tbl)
    parent_names <- unique(parents$parent_table)
    parents_processed <- intersect(unique(parent_names),
                                   unique(processed_tables$tblname))
    if (isTRUE(all.equal(sort(unique(parents_processed)),
                         sort(unique(parent_names))))) {
      #if (length(parents.processed) == length(parent.names)) {
      level_tables <- level_tables |>
        tibble::add_row(tblname = tbl, level = level)
    }
  }

  processed_tables <- processed_tables |>
    tibble::add_row(level_tables)

  remaining_tables <- setdiff(tables, level_tables$tblname)
  # print(level.tables)
  if (nrow(level_tables) == 0) {
    return(processed_tables)
  } else {
    insert_order(con = con, schema = schema, level = level + 1,
                 processed_tables = processed_tables, tables = remaining_tables)
  }
}


#' Gets column names from a Postgres schema/table.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The schema that contains the table to return info
#'   about.
#' @param table A string. The table name to return info about.
#'
#' @return A list of tibbles, one containing column information, and another
#'   containing constraint information.
get_dest_info <- function(con, schema, table) {
  info_sql <- glue::glue(paste(
    "SELECT column_name, data_type, character_maximum_length ",
    "  FROM information_schema.columns ",
    " WHERE table_schema = '{schema}' ",
    "   AND table_name = '{table}' ",
    " ORDER BY ordinal_position; ",
    sep = "\n"
  ))
  col_info <-  DBI::dbGetQuery(con, info_sql)

  constraint_sql <- glue::glue(paste(
    "SELECT con.conname, con.contype",
    "  FROM pg_catalog.pg_constraint con",
    " INNER JOIN pg_catalog.pg_class rel",
    "    ON rel.oid = con.conrelid",
    " INNER JOIN pg_catalog.pg_namespace nsp",
    "    ON nsp.oid = connamespace",
    " WHERE nsp.nspname = '{schema}'",
    "   AND rel.relname = '{table}';",
    sep = "\n"
  ))

  constraints <-  DBI::dbGetQuery(con, constraint_sql)

  list(col_info = col_info, constraints = constraints)
}


#' Returns information about field types for imported source data.
#'
#' @param tbl A tibble containing the source data.
#'
#' @return A tibble containing column information for \code{tbl}.
get_src_info <- function(tbl) {
  col_names <- tibble::as_tibble(list(column_name = colnames(tbl)))
  src_types <- tbl |>
    dplyr::summarise(dplyr::across(
      .cols = tidyselect::everything(),
      .fns = ~class(.x)[1]  # dttm cols return vector c("POSIXct" "POSIXt")
    )) |>
    dplyr::slice(1) |>
    tidyr::gather("variable", "class")

  info <- col_names |>
    dplyr::left_join(src_types, by = c("column_name" = "variable")) |>
    dplyr::rename(data_type = "class")

  info
}


#' Determines which columns have different types between a data source and
#' destination.
#'
#' @param src_cols A tibble with two fields, column_name (string) and
#'   data_type (string), the former containing column names from source tibble
#'   tables and the latter denoting their R data type (e.g. integer, logical,
#'   etc.)
#' @param dest_cols A tibble with two fields, column_name (string) and
#'   data_type (string), the former containing column names from Postgres
#'   destination table and the latter denoting their Postgres data type (e.g.
#'   character varying, double precision, etc.)
#'
#' @return A logical vector with TRUE indicating same column type and FALSE
#'   indicating otherwise.
compare_types <- function(src_cols, dest_cols) {
  type_match <- tibble::tibble(ptype = character(), rtype = character()) |>
    tibble::add_row(ptype = "character varying", rtype = "character") |>
    tibble::add_row(ptype = "text", rtype = "character") |>
    tibble::add_row(ptype = "double precision", rtype = "numeric") |>
    tibble::add_row(ptype = "integer", rtype = "integer") |>
    tibble::add_row(ptype = "smallint", rtype = "integer") |>
    tibble::add_row(ptype = "bigint", rtype = "integer") |>
    tibble::add_row(ptype = "boolean", rtype = "logical") |>
    tibble::add_row(ptype = "timestamp without time zone",
                    rtype = "POSIXct, POSIXt, Date") |>
    tibble::add_row(ptype = "timestamp with time zone",
                    rtype = "POSIXct, POSIXt, Date") |>
    tibble::add_row(ptype = "timestamp", rtype = "POSIXct, POSIXt, Date")

  src_cols <- src_cols |>
    dplyr::rename(src_type = "data_type")

  dest_cols <- dest_cols |>
    dplyr::rename(dest_type = "data_type")

  compare <- dest_cols |>
    dplyr::inner_join(src_cols, by = c("column_name" = "column_name")) |>
    dplyr::left_join(type_match, by = c("dest_type" = "ptype")) |>
    dplyr::mutate(matched = stringr::str_detect(.data$rtype, .data$src_type)) |>
    dplyr::mutate(matched = ifelse(is.na(.data$matched), FALSE, .data$matched))

  compare
}


#' Gathers information about both source and destination tables and determines
#' which fields are matched in name and type.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The name of the schema to be queried in the Postgres
#'   connection.
#' @param tbl_name A string. The name of the table to retrieve information about
#'   from the postgres connection.
#' @param src_data A tibble containing the source data having the name of
#'   \code{tbl_name}
#'
#' @return A list of vectors containing information about missing columns in the
#'   destination that are in the source, missing columns in the source that are
#'   in the destination, columns which match in both source and destination,
#'   and whether or not those columns need to be CAST during insert.
get_info <- function(con, schema, tbl_name, src_data) {
  dest_info <- get_dest_info(con = con, schema = schema, table = tbl_name)
  src_info <- get_src_info(tbl = src_data)
  cols_src <- src_info$column_name
  cols_dest <- dest_info$col_info$column_name
  compare_info <- compare_types(src_cols = src_info,
                                dest_cols = dest_info$col_info)
  import_cols <-  compare_info$column_name
  cast <- !(compare_info$matched | compare_info$dest_type == "USER-DEFINED")
  missing_dest <-  setdiff(cols_src, cols_dest)
  missing_src <- setdiff(cols_dest, cols_src)

  list(import_cols = import_cols, cast = cast,
       missing_dest = missing_dest, missing_src = missing_src)
}

#########################
## INSERTING functions ##
#########################

#' Will create an INSERT SQL statement for a Postgres table based on the
#' contents of the data source.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The name of the schema in the destination.
#' @param table_name A name of the table in the destination for which to build
#    the INSERT statement
#' @param named logical. A flag that tells the function to build the SQL based
#'   on glue::glue syntax (e.g. {my.parameter}) or parameter position syntax
#'   (e.g. $1, $2, etc.)
#' @param update logical. A flag that instructs the function to construct an
#'   UPSERT statement instead of an INSERT statement.
#' @param ins_cols A string vector containing the names of columns to use for
#'   the insert statement.
#' @param update_cols A string vector containing the names of columns to UPDATE
#'   in the case of an upsert statement.
#' @param cast A logical vector telling the function whether to encapsulate the
#'   parameter in the SQL in a CAST function, hopefully mitigating type errors.
#' @param srid An integer denoting the SRID (EPSG code) to use when inserting
#'   geometry data into tables that support PostGIS geometry.
#'
#' @return a list containing the built INSERT SQL and the list of columns used
#'   to build it.
create_insert <- function(con, schema, table_name, named = FALSE,
                          update = FALSE, ins_cols = NULL, update_cols = NULL,
                          cast = NULL, srid = 4326) {
  if (is.null(cast)) {
    cast <- rep(FALSE, times = length(ins_cols))
  }

  info <- get_dest_info(con = con, schema = schema, table = table_name)
  # gets the pkey constraint name or first unique constraint name
  # on conflict statement only allows one constraint check
  constraint_name <- (
    info$constraints |>
      dplyr::filter(.data$contype %in% c("u", "p")) |>
      dplyr::arrange(.data$contype, .data$conname)
  )$conname[1]

  if (is.null(ins_cols)) {
    cols <- info$col_info$column_name
    dtypes <- info$col_info$data_type
  } else {
    restricted <- info$col_info |>
      dplyr::inner_join(tibble::as_tibble(list(column_name = ins_cols)),
                        by = c("column_name" = "column_name"))
    cols <- restricted$column_name
    dtypes <- restricted$data_type
  }
  if (is.null(update_cols)) {
    u_cols <- cols
  } else {
    u_cols <- intersect(cols, update_cols)
  }
  # geometry
  geom_col <- cols == "geom"

  if (update == FALSE || is.na(constraint_name)) {
    update_sql <- "ON CONFLICT DO NOTHING"
  } else {
    l <- character()
    for (i in seq_along(u_cols)) {
      l[i] <- paste0('"', u_cols[i], '"', " = EXCLUDED.", '"', u_cols[i], '"')
    }
    update_sql <-  paste0(
      glue::glue(
        "ON CONFLICT ON CONSTRAINT \"{constraint_name}\" \nDO UPDATE SET "
      ),
      paste(l, collapse = ", ")
    )
  }
  colstring <- paste(paste0('"', cols, '"'), collapse = ", ")
  if (named == TRUE) {
    # we have to replace spaces in param names because glue::glue_data_sql()
    # can't handle them
    params <- paste0(
      ifelse(cast, "CAST({", "{"),
      stringr::str_replace_all(cols, " ", "_"),
      ifelse(cast, paste0("} AS ", dtypes, ")"), "}")
    )
    # for geometry

  } else {
    params <- paste0(
      ifelse(cast, "CAST($", "$"),
      seq(1, length(cols)),
      ifelse(cast, paste0(" AS ", dtypes, ")"), "")
    )
  }
  params_geom <- paste0(
    ifelse(geom_col, "ST_GeomFromText(", ""),
    params,
    ifelse(geom_col, paste0(", ", srid, ")"), "")
  )

  paramstring <- params_geom |> paste(collapse = ", ")
  insert_sql <- glue::glue(paste(
    "INSERT INTO {schema}.\"{table_name}\" ({colstring})",
    "VALUES ({paramstring}) ",
    "{update_sql};",
    sep = "\n"
  ))

  list(sql = insert_sql, cols = cols)
}


#' Used to insert individual rows via apply() family function or iterative loop
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param row A named list or tibble containing the values to bind to the INSERT
#'   statement via parameter substitution.
#' @param stmt A string containing the INSERT statement with named parameters to
#'   use with glue::glue syntax (e.g. {my.parameter}).
#' @param log A string file path location of the log file to write
#'   results to.
#'
#' @return The number of rows affected by the insert.
insert_row <- function(con, row, stmt, log = NULL, verbose = FALSE) {
  # print(row)
  sql <- glue::glue_data_sql(.x = row, .con = con, stmt)
  # print(sql)
  affected <- tryCatch({
    a <- DBI::dbExecute(con, sql)
    # cat(paste0("row-wise non-error affected: ", a, "\n"))
    a
  },
  error = function(e) {
    values <- stringr::str_match(sql, "VALUES (.+)")[2]
    e_short <- stringr::str_replace(
      e$message,
      stringr::regex("CONTEXT:.+", dotall = TRUE),
      ""
    )
    msg <- paste0("\nFailed on row: ", values, "\nError: ", e_short)
    msg_out(msg, log, sep = "", prnt = verbose)
    last_error <<- e
    # cat(paste0("row-wise ERROR affected: ", 0, "\n"))

    0
  },
  warning = function(w) {
    values <- stringr::str_match(sql, "VALUES (.+)")[2]
    msg <- paste0("\nWarned on row: ", values, "\nWarning: ", w$message)
    msg_out(msg, log, sep = "", prnt = verbose)
    last_warning <<- w

    a
  })

  return(affected)
}


#' Inserts the source data in a source table into the Postgres destination
#' using row-wise inserts (one row at a time.)  This is slower than a multi-row
#' parameter bind, but allows the insert process to capture single row insert
#' errors, log them, and continue processing.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param update logical. A flag that instructs the function to construct an
#'   UPSERT statement instead of an INSERT statement.
#' @param schema A string. The name of the schema in the destination.
#' @param table A tibble containing the source data to be inserted.
#' @param table_name A name of the table in the destination for which to build
#'   the INSERT statement.
#' @param cols A string vector containing the names of columns to use for
#'   the insert statement.
#' @param cast A logical vector telling the function whether to encapsulate the
#'   parameter in the SQL in a CAST function, hopefully mitigating type errors.
#' @param log A string file path location of the log file to write
#'   results to.
#'
#' @return integer. Total number of rows affected by the insert.
rowwise_insert <- function(con, update, schema, table, table_name, cols, cast,
                           log, verbose = FALSE) {
  insert <- create_insert(con = con, update = update, schema = schema,
                          table_name = table_name,
                          ins_cols = cols, cast = cast, named = TRUE)
  # need to remove spaces in column names to deal with glue::glue_sql_data()
  # inadequacies in this matter
  table <- table |> dplyr::rename_all(~gsub(" ", "_", .))
  # pcts = seq(from = 0, to = 1, by = 0.1)
  tot_rows <- nrow(table)
  # old.status <- 0
  total_affected <- 0
  for (i in 1:tot_rows) {
    affected <- insert_row(con = con, row = table[i, ], stmt = insert$sql,
                           log = log, verbose = verbose)
    total_affected <- total_affected + affected
  }

  total_affected
}


#' This is an alternative row-wise insert function using \code{DBI::dbBind()}
#' instead of glue::glue functionality like \code{rowwise.insert()}.
#' CURRENTLY NOT IMPLEMENTED but kept for reference.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param table A tibble containing the source data to be inserted.
#' @param insert A list constructed via \code{create.insert()}.
#'
#' @return The number of rows affected by the insert.
dbbind_insert_rw <- function(con, table, insert) {
  rowwise_affected <- 0
  for (i in rownames(table)) {
    row <- table[i, ]
    send_data <- as.list(dplyr::select(row, insert$cols))
    names(send_data) <- NULL
    a <- tryCatch(
      {
        send <- DBI::dbSendStatement(con, insert$sql)
        DBI::dbBind(send, row)
        rw_bind_affected <- DBI::dbGetRowsAffected(send)
        DBI::dbClearResult(send)
        # cat(paste0("rowwise bind non-error affected: ",
        #            rw.bind.affected, "\n"))
        rw_bind_affected
      },
      error = function(e) {
        DBI::dbClearResult(send)

        0
      }
    )
    rowwise_affected <- rowwise_affected + a
  }

  rowwise_affected
}


#' This is the top-table level inserting function which attempts, be default to
#' write source data to the destination in chunks of \code{chunk.size} rows. It
#' manages errors in the chunk insert by moving to a row-wise insert for that
#' particular chunk. A smaller \code{chunk.size} will result in slower inserts
#' for data without foreign key violations (or other errors) but decreasing
#' \code{chunk.size} can speed up inserts for data where foreign key issues
#' are common.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param update logical. A flag that instructs the function to construct an
#'   UPSERT statement instead of an INSERT statement.
#' @param schema A string. The name of the schema in the destination.
#' @param table A tibble containing the source data to be inserted.
#' @param table_name A name of the table in the destination for which to build
#'   the INSERT statement.
#' @param cols A string vector containing the names of columns to use for
#'   the insert statement.
#' @param cast A logical vector telling the function whether to encapsulate the
#'   parameter in the SQL in a CAST function, hopefully mitigating type errors.
#' @param log A string file path location of the log file to write
#'   results to.
#' @param verbose logical. A flag telling the function to be more verbose in
#'   its messaging.
#' @param chunk_size An integer.This tells the function how many rows to
#'   attempt to insert at once. Failure on a chunk will cause the function
#'   to default to row-wise inserts for the entire chunk.
#'
#' @return The number of rows affected by the insert.
dbbind_insert <- function(con, update, schema, table, table_name, cols, cast,
                          log, verbose = FALSE, chunk_size = 1000) {
  # this whole function needs to be rewritten to use chunk of the data table
  # into a list of chunk sub tables absed on size and then use lapply to bind
  # each chunk. the current way works... but useing row indices in a while loop
  # is not great for debugginf insert issues.
  insert <- create_insert(con = con, update = update, schema = schema,
                          table_name = table_name,
                          ins_cols = cols, cast = cast, named = FALSE)

  # we need to de-tibble and de-name our data to use positional args
  # with dbBind()
  processed <- 0
  total_affected <- 0
  affected <- NA
  tbl_nrow <- nrow(table)
  chunk_no <- ceiling(tbl_nrow / chunk_size)
  chunks_completed <- 0
  pct_done_chunks <- max(as.integer(round(chunk_no * 0.1, 0)), 1)
  if (verbose == TRUE) {
    msg <- paste0("Insert SQL: \n", insert$sql)
    msg_out(msg, log, prnt = FALSE)
  }
  while (processed < tbl_nrow) {
    if (verbose == TRUE) {
      cat(paste0(processed, "/", tbl_nrow, "..."))
    } else {
      if (chunks_completed %% pct_done_chunks == 0) {
        cat(paste0(
          as.integer(round(chunks_completed / chunk_no * 100, 0)), "%..."
        ))
      }
    }
    end_row <-  processed + chunk_size
    sub_tbl <- dplyr::slice(table, (processed + 1):(end_row))
    send_data <- as.list(dplyr::select(sub_tbl, insert$cols))
    names(send_data) <- NULL
    affected <- tryCatch(
      {
        send <- DBI::dbSendStatement(con, insert$sql)
        DBI::dbBind(send, send_data)
        bind_affected <- DBI::dbGetRowsAffected(send)
        DBI::dbClearResult(send)
        # cat(paste0("dbBind non-error affected: ", bind.affected, "\n"))
        bind_affected
      },
      error = function(e) {
        # cat(paste0("\n", e$message))
        cat(glue::glue(paste0(
          "\nFailed on dbBind @ chunk {chunks_completed + 1}, ",
          "attempting rowwise insert...\n"
        ), .trim = FALSE))
        error_affected <- DBI::dbGetRowsAffected(send)
        DBI::dbClearResult(send)
        rowwise_affected <- rowwise_insert(
          con = con, update = update, schema = schema, table = sub_tbl,
          table_name = table_name, cols = cols, cast = cast, log = log,
          verbose = verbose
        )
        tot_error_affected <- error_affected + rowwise_affected
        # cat(paste0("dbbind ERROR affected: ", tot.error.affected, "\n"))

        tot_error_affected
      }
    )
    processed <- processed + nrow(sub_tbl)
    total_affected <- total_affected + affected
    chunks_completed <- chunks_completed + 1
    rm(affected)
    # for safety
    if (nrow(sub_tbl) == 0)
      break
  }
  cat("done\n")

  total_affected
}

#' This function removes records in the source tables which have and unmatched
#' foreign key in the referenced parent table(s). It utilizes the foreign key
#' definitions found in the postgres database passed with dbname and created
#' via the create_db.R script.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The schema of the source table in the Postgres
#'   database.
#' @param tbl_name A string. The name of the source table in the PostgreSQL
#'   database.
#' @param tbl A tibble. The source data to be filtered with name
#'   \code{tbl_name}. May be different from the raw imported table in
#'   \code{tbl_set}.
#' @param tbl_set A list of tables produced by \code{get_src_tables()}
#' @return A list containing the number of orphaned records found as well as
#'   the filtered table with orphans removed.
remove_orphans <- function(con, schema, tbl_name, tbl, tbl_set) {
  filtered_table <- tbl
  keys <- get_foreign_keys(con = con, schema = schema, tbl_name = tbl_name)
  constraints <- unique(keys$conname)
  for (constraint in constraints) {
    fk_con <- keys |>
      dplyr::filter(.data$conname == constraint)
    parent_name <- fk_con[[1, "parent_table"]]
    # parent_tbl <- tbl_set[[schema]][[parent_name]]
    fk <- fk_con$parent_column
    names(fk) <- fk
    key_string <- paste(paste0('"', fk, '"'), collapse = ", ")
    sql <- glue::glue(paste(
      'SELECT {key_string} FROM "{schema}"."{parent_name}"',
      " GROUP BY {key_string};",
      "", sep = "\n"
    ), .trim = FALSE)
    parent_tbl <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
    if (!is.null(parent_tbl)) {
      child_col <- fk_con$child_column
      parent_col <- fk_con$parent_column
      names(parent_col) <- child_col
      # # distinct() here needed due to differences between source and dest
      # # schemas, e.g. STATENM in lmf  has multiple state instances
      # # and is not a primary key in the source table,
      # # but is in the dest table.
      # parent_tbl_keys <- parent_tbl |>
      #   dplyr::select(tidyselect::all_of(parent_col)) |>
      #   dplyr::distinct()
      filtered_table <- filtered_table |>
        dplyr::inner_join(parent_tbl, by = parent_col)
    }
  }
  orphan_no <- nrow(tbl) - nrow(filtered_table)

  list(orphan_no = orphan_no, filtered_table = filtered_table)
}


#' This function removes records in the source tables which have primary key
#' violations within the postgres databse passed with dbname and created via the
#' create_db.R script.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param schema A string. The schema of the source table in the Postgres
#'   database
#' @param tbl_name A string. The name of the source table in the Postgres
#'   database
#' @param tbl A tibble. The source data to be filtered with name
#'   \code{tbl_name}
#' @return A list containing the number of duplicate records found as well as
#'   the filtered table with duplicates removed
remove_duplicates <- function(con, schema, tbl_name, tbl) {
  filtered_table <- tbl |>

  keys <- get_primary_keys(con = con, schema = schema, tbl_name = tbl_name)
  constraints <- unique(keys$constraint_name)
  for (constraint in constraints) {
    pk_con <- keys |>
      dplyr::filter(.data$constraint_name == constraint)
    pk <- pk_con$column_name
    names(pk) <- pk
    key_string <- paste(paste0('"', pk, '"'), collapse = ", ")
    sql <- glue::glue(paste(
      'SELECT {key_string} FROM "{schema}"."{tbl_name}"',
      " GROUP BY {key_string};",
      "", sep = "\n"
    ), .trim = FALSE)
    existing_data <- tibble::as_tibble(DBI::dbGetQuery(con, sql)) |>
      dplyr::mutate(del_me = "x")
    filtered_table <- filtered_table |>
      dplyr::left_join(existing_data, by = pk)  |>
      dplyr::filter(is.na(.data$del_me)) |>
      dplyr::select(-"del_me")
  }
  dup_no <- nrow(tbl) - nrow(filtered_table)

  list(dup_no = dup_no, filtered_table = filtered_table)
}


#' This is the top-level inserting function for all source data. It determines
#' insert order and constructs parameters needed for the table-level insert
#' functions \code{dbbind_insert} and \code{rowwise_insert}
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param tbls A list of tables produced by \code{get_src_tables()}
#' @param update logical. A flag that instructs the function to construct an
#'   UPSERT statement instead of an INSERT statement.
#' @param dbkey A string. Serves as a unique key for the database source which
#'   will identify data in the destination DB as coming from a specific source.
#' @param desc A string. The description the will be used to describe the source
#'   database contents which are imported into the Postgres destination.
#' @param path A string file path pointing to the the source database to import.
#' @param hash A string. The md5hash of the database, produced via
#'   \code{get_key()}
#' @param verbose logical. A flag telling the function to be more verbose in
#'   its messaging.
#' @param log A string file path location of the log file to write
#'   results to.
#' @param named logical. A flag that tells the function to default to construct
#'   the INSERT statement using the glue::glue syntax and insert the data into
#'   the database row-wise (one row at a time).
#' @param chunk_size An integer.This tells the function how many rows to
#'   attempt to insert at once. Failure on a chunk will cause the function
#'   to default to row-wise inserts for the entire chunk.
to_db <- function(con, tbls, update, dbkey, desc, path, hash, verbose = FALSE,
                  log = NULL, named = FALSE, chunk_size = 1000) {

  # this whole function needs to be rewritten at some point.
  # 1) filter whole table set such that only records not in destination are
  #   available to insert (pk filter, assuming update = FALSE)
  # 2) apply foreign key filter to whole table set, in order, making sure that
  #    the next table in line is using the fk filtered parent table(s) (or the 
  #    destination tables maybe since inserting is happening is order), not the
  #    original or just pk filtered parents. Otherwise we get cascading failures
  #    with trying to bind insert whole chunks of data.
  # 3) since we have to insert db_site, db_plot and db_line tables into the
  #    table set for orphan checking... we might as well now get rid of the
  #    special tables language and just do the insert table by table

  # tic()

  special_tables <- list(
    dima = list(
      tblSites = "db_site",
      tblPlots = "db_plot",
      tblLines = "db_line"
    )
  )
  schemas <- names(tbls)
  for (schema in schemas) {
    # creating record in table db before anything else
    if (is.null(tbls[[schema]][["db"]])) {
      db_table <- tibble::as_tibble(
        list(dbkey = dbkey, md5hash = hash, dbpath = path, description = desc)
      )
      tbls[[schema]][["db"]] <- db_table
    }
    table_names <- names(tbls[[schema]])
    order <- insert_order(con = con, schema = schema)
    order_source <- order |>
      dplyr::inner_join(tibble::as_tibble(list(tblname = table_names)),
                        by = c("tblname" = "tblname")) |>
      dplyr::mutate(level = ifelse(.data$tblname == "db", -1, .data$level)) |>
      dplyr::arrange(.data$level, .data$tblname)

    # populated after order is established as these are handled with special
    # these need to be in the table set for orphan record filtering
    if (schema == "dima") {
      if (is.null(tbls[[schema]][["db_site"]])) {
        site_shim <- tbls[[schema]][["tblPlots"]] |>
          dplyr::select("PlotKey", "dbkey") |>
          dplyr::distinct()
        tbls[[schema]][["db_plot"]] <- site_shim
      }
      if (is.null(tbls[[schema]][["db_plot"]])) {
        plot_shim <- tbls[[schema]][["tblSites"]] |>
          dplyr::select("SiteKey", "dbkey") |>
          dplyr::distinct()
        tbls[[schema]][["db_plot"]] <- plot_shim
      }
      if (is.null(tbls[[schema]][["db_line"]])) {
        line_shim <- tbls[[schema]][["tblLines"]] |>
          dplyr::select("LineKey", "dbkey") |>
          dplyr::distinct()
        tbls[[schema]][["db_line"]] <- line_shim
      }
    }

    # loop through source tables and import
    for (tbl in order_source$tblname) {
      current_table <- tbls[[schema]][[tbl]]
      special_table <- special_tables[[schema]][[tbl]]
      iter_tables <- c(special_table, tbl)
      for (i_tbl in iter_tables) {
        if (nrow(current_table) > 0) {
          info <- get_info(con = con, schema = schema, tbl_name = i_tbl,
                           src_data = current_table)
          if (verbose == TRUE &&
              !(i_tbl %in% unlist(special_tables[[schema]]))
          ) {
            msg <- paste0("\nFor table ", i_tbl, ":")
            msg_out(msg, log)
            msg <- paste0(length(info$import_cols),
                          " columns match in source and destination.")
            msg_out(msg, log)
            if (length(info$missing_dest) > 0) {
              msg <- paste0("The following source columns have no match in ",
                            "the destination database:\n\t",
                            paste(info$missing_dest, collapse = ", "))
              msg_out(msg, log)
            }
            if (length(info$missing_src) > 0) {
              msg <- paste0("The following destination columns have no match",
                            " in the source database:\n\t",
                            paste(info$missing_src, collapse = ", "))
              msg_out(msg, log)
            }
          }
          if ("dbkey" %in% info$missing_src) {
            current_table <- current_table |> dplyr::mutate(dbkey = dbkey)
            info$import_cols <- c("dbkey", info$import_cols)
            info$cast <- c(FALSE, info$cast)
          }
          msg <-  paste0("Filtering orphaned records from ", schema, ".",
                         i_tbl, "...")
          msg_out(msg, log)
          fk_filtered <- remove_orphans(con = con, schema = schema,
                                        tbl_name = i_tbl, tbl = current_table,
                                        tbl_set = tbls)
          filtered_table <- fk_filtered$filtered_table
          if (fk_filtered$orphan_no > 0) {
            msg <-  paste0(fk_filtered$orphan_no, " orphans found in ",
                           schema, ".", i_tbl, "...")
            msg_out(msg, log)
          }
          if (update == FALSE) {
            msg <-  paste0("Filtering records from ", schema, ".",
                           i_tbl, " already in destination...")
            msg_out(msg, log)
            pk_filtered <- remove_duplicates(con = con, schema = schema,
                                             tbl_name = i_tbl,
                                             tbl = filtered_table)

            filtered_table <- pk_filtered$filtered_table
            if (pk_filtered$dup_no > 0) {
              msg <- paste0(pk_filtered$dup_no, " already in destination for ",
                            schema, ".", i_tbl, "...")
              msg_out(msg, log)
            }
          }
          msg <-  paste0("Inserting ", nrow(filtered_table), " rows into ",
                         schema, ".", i_tbl, "...")
          msg_out(msg, log, sep = " ")
          if (named == TRUE) {
            affected <- rowwise_insert(
              con = con, update = update, schema = schema,
              table = filtered_table, table_name = i_tbl,
              cols = info$import_cols, log = log, verbose = verbose,
              cast = info$cast
            )
          } else {
            affected <- dbbind_insert(
              con = con, update = update, schema = schema,
              table = filtered_table, table_name = i_tbl,
              cols = info$import_cols, log = log,
              verbose = verbose, cast = info$cast, chunk_size = chunk_size
            )
          }
          t <- tictoc::toc(quiet = TRUE)
          # print(affected)
          msg <- paste0(sum(affected), "/", nrow(filtered_table),
                        " rows affected.\n")
          if (i_tbl %in% c("tblSpecies", "tblSpeciesGeneric", "tblEcolSites")) {
            msg <- paste0(msg, "Rows with values divergent from base table ",
                          "inserted into ", i_tbl, "_delta.\n")
          }
          msg <- paste0(msg, "END ", i_tbl, "\n")
          msg_out(msg, log)
        }  # end if (nrow(current_table) > 0)
      }  # end for (i_tbl in iter_tables)
    }  # end for (tbl in order_source$tblname)
  }  # end for (schema in schemas)

  # msg_out(paste0(t$toc - t$tic, " sec elapsed during insert."), log)
}


#' A recursive function which can be used to determine the proper order in which
#' to refresh materialized views, i.e. materialized views which depend on other
#' materialized views will be refreshed after the view it depends on is
#' refreshed.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param done A string vector. An internal parameter which the function uses to
#'   keep track of which views have already been refreshed.
#' @param level An integer. An internal parameter which the function uses to
#'   keep track of how many times it has been called.
refresh_views <- function(con, done = character(0), level = 0) {
  # cat(paste0("level = ", level, "\n"))
  views <- (DBI::dbGetQuery(
    con, "SELECT relname FROM pg_class WHERE relkind = 'm';"
  ))$relname

  undone_views <- views[!(views %in% done)]
  # cat(paste0("undone views: ", paste(undone.views, collapse = ", "), "\n"))
  if (length(undone_views != 0)) {
    depend_sql <- "
    SELECT dependent_ns.nspname as dependent_schema,
           dependent_view.relname as dependent_view,
	         source_ns.nspname as source_schema,
	         source_table.relname as source_table,
	         source_table.relkind as source_kind,
	         COUNT(pg_attribute.attname) as column_n
      FROM pg_depend
      JOIN pg_rewrite ON pg_depend.objid = pg_rewrite.oid
      JOIN pg_class as dependent_view 
        ON pg_rewrite.ev_class = dependent_view.oid
      JOIN pg_class as source_table ON pg_depend.refobjid = source_table.oid
      JOIN pg_attribute ON pg_depend.refobjid = pg_attribute.attrelid
       AND pg_depend.refobjsubid = pg_attribute.attnum
      JOIN pg_namespace dependent_ns
        ON dependent_ns.oid = dependent_view.relnamespace
      JOIN pg_namespace source_ns ON source_ns.oid = source_table.relnamespace
     WHERE source_table.relkind = 'm'
       AND dependent_ns.nspname = 'public'
       AND dependent_view.relname = '{view}'
       AND pg_attribute.attnum > 0
     GROUP BY dependent_ns.nspname, dependent_view.relname, source_ns.nspname,
              source_table.relname, source_table.relkind
     ORDER BY 1,2;
    "
    completed <-  done
    for (view in undone_views) {
      depends <- (DBI::dbGetQuery(con, glue::glue(depend_sql)))$source_table
      if (all(depends %in% completed)) {
        sql <- paste0("REFRESH MATERIALIZED VIEW public.", view, ";")
        cat(paste0("Refreshing materialized view: public.", view, "...\n"))
        DBI::dbExecute(con, sql)
        completed <- c(completed, view)
      }
    }
    refresh_views(con, done = completed, level = level + 1)
  }
}


#####################################
## SELECTING and reading functions ##
#####################################

#' Constructs a SELECT statment which can be used to retrieve source data from
#' A spatially enabled sqlite database by converting table geometry to WKT
#' format .
#'
#' @param sqlite_con An \code{Rsqlite} DBI connection object.
#' @param table A string. The name of the table to build the select statement
#'   for.
#' @param spatial logical. A flag which tells the function to convert geometry
#'   columns to WKT format for import.  The geometry must be named "geom" in
#'   the source data.
#'
#' @return A list consisting of the constructed SELECT statement and the fields
#'   used to construct it.
create_sqlite_select <- function(sqlite_con, table, spatial) {
  info <- DBI::dbGetQuery(sqlite_con, paste0("PRAGMA table_info(", table, ");"))
  cols <- info$name
  if (spatial == TRUE) {
    cols_new <- cols |> stringr::str_replace("^geom$", "st_astext(geom) geom")
  } else {
    cols_new <- cols[cols != "geom"]
  }


  col_string <-  paste(cols_new, collapse = ", ")
  select <- paste0("SELECT ", col_string, "\n  FROM ", table, ";")

  list(sql = select, cols = cols_new, pragma = info)
}


#' Corrects DBI imports from SQLite in the case where the ambiguous typing or 
#' affinities can cause issues (esp. for all null columns)
#'
#' @param tbl A dataframe returned from 
#'   DBI::GetQuery("SELECT * FROM my_table;").
#' @param pragma A dataframe returned from 
#'   DBI::GetQuery("PRAGMA table_info(my_table);")
#'
#' @return A version of \code{tbl} where date, datetime, timestamp, boolean, and
#'   geometry columns have been explicitly typed.
fix_sqlite_typing <- function(tbl, pragma) {
  # convert date/time to posixct
  new_tbl <- tbl
  date_cols <- pragma |>
    dplyr::filter(stringr::str_detect(type, "(?i)^date$")) |>
    dplyr::pull("name")
  if (length(date_cols) > 0) {
    new_tbl <- new_tbl |>
      dplyr::mutate(across(.cols = all_of(date_cols), 
                           .fns = ~lubridate::as_date(.x)))
  }
  datetime_cols <- pragma |>
    dplyr::filter(stringr::str_detect(
      type, "(?i)(?:datetime|timestamp)")
  ) |>
    dplyr::pull("name")
  if (length(datetime_cols) > 0) {
    new_tbl <- new_tbl |>
      dplyr::mutate(across(.cols = all_of(datetime_cols), 
                           .fns = ~lubridate::as_datetime(.x)))
  }
  
  # convert boolean to logical
  boolean_cols <- pragma |>
    dplyr::filter(stringr::str_detect(type, "(?i)^bool")) |>
    dplyr::pull("name")
  if (length(boolean_cols) > 0) {
    new_tbl <- new_tbl |>
      dplyr::mutate(across(.cols = all_of(boolean_cols), 
                           .fns = ~as.logical(.x)))
  }

  # make sure geometry columns are text
  geom_cols <- pragma |>
    dplyr::filter(stringr::str_detect(
      type, 
      paste0("(?i)^(?:multi)?",
             "(?:point|linestring|polygon|geometry(?:collection)?)[zm]*")
    )) |>
    dplyr::pull("name")
  if (length(geom_cols) > 0) {
    new_tbl <- new_tbl |>
      dplyr::mutate(across(.cols = all_of(geom_cols), 
                           .fns = ~as.character(.x)))
  }

  new_tbl
}

#' Retrieves data tables from data source and returns them as a list of tibbles.
#'
#' @param con A DBI connection. A database connection object to the LLEIA
#'    PostgreSQL database
#' @param path A string file path pointing to the source database. File name
#'   extensions must be one of (.mdb, .accdb, .sqlite, .db) and folder paths
#'   must have the extension of (.gdb).
#' @param md5hash A string. The md5hash of the database, produced via
#'   \code{get_key()}
#' @param key A string. Serves as a unique key for the database source which
#'   will identify data in the destination DB as coming from a specific source.
#' @param desc A string. The description the will be used to describe the source
#'   database contents which are imported into the Postgres destination.
#'
#' @return A list of schema names, and within each a list of tibbles containing
#'   source data.
get_src_tables <- function(con, path, md5hash, key, desc = NULL) {
  terradat <- FALSE
  dima_tbls_sql <- paste0("SELECT table_name FROM information_schema.tables ",
                          "WHERE table_schema='dima' AND table_name NOT LIKE ",
                          "'tblMaint%';")
  dima_table_names <- (DBI::dbGetQuery(con, dima_tbls_sql))$table_name
  lmf_tbls_sql <- paste0("SELECT table_name FROM information_schema.tables ",
                         "WHERE table_schema='lmf';")
  lmf_table_names <- (DBI::dbGetQuery(con, lmf_tbls_sql))$table_name
  eco_tbls_sql <- paste0("SELECT table_name FROM information_schema.tables ",
                         "WHERE table_schema='eco';")
  eco_table_names <- (DBI::dbGetQuery(con, eco_tbls_sql))$table_name
  aim_lotic_tbls_sql <- paste0(
    "SELECT table_name FROM information_schema.tables ",
    "WHERE table_schema='aim_lotic';"
  )
  aim_lotic_table_names <- (DBI::dbGetQuery(con, aim_lotic_tbls_sql))$table_name
  compat <- check_compatible(path)
  if (compat$compatible == FALSE) {
    cat("Data source not readable.\n")
    cat(compat$error_msg)
    cat("\n")
    quit()
  }
  if (compat$con_type == "access") {
    if (compat$con_sub == "odbc") {
      access_head <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ="
      access_constring <- paste0(access_head, path)
      access_con <- odbc::dbConnect(odbc::odbc(),
                                    .connection_string = access_constring,
                                    encoding = "latin1")

      avail_tables <- odbc::dbListTables(access_con)
    } else if (compat$con_sub == "mdbtools") {
      cmd_str <- 'mdb-tables -1 "{path}"'
      cmd <- glue::glue(cmd_str)
      avail_tables <- system2(command = "mdb-tables",
                              args = c("-1", shQuote(path)), stdout = TRUE)
      memory_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    } else {
      stop(paste0("Error: Return of check.compatible() should have been either",
                  " 'odbc' or 'mdbtools'."))
    }
  } else if (compat$con_type == "gdb") {
    avail_layers <- sf::st_layers(dsn = path)
    avail_tables <- avail_layers$name
    if ("terradat" %in% stringr::str_to_lower(avail_tables)) {
      terradat <- TRUE
    }
  } else if (compat$con_type == "sqlite") {
    sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), path)
    avail_tables <- DBI::dbListTables(sqlite_con)
    DBI::dbExecute(sqlite_con, "PRAGMA foreign_keys = on;")
    if (compat$con_sub == "spatialite") {
      DBI::dbExecute(sqlite_con, "SELECT load_extension('mod_spatialite');")
    }
  }

  shared_tables <- c("db")
  dima_tables_get <- intersect(avail_tables, dima_table_names)
  lmf_tables_get <- intersect(avail_tables, lmf_table_names)
  eco_tables_get <- intersect(avail_tables, eco_table_names)
  aim_lotic_tables_get <- intersect(avail_tables, aim_lotic_table_names)


  # remove a schema if they are JUST tables in shared_tables
  if (length(dima_tables_get) > 0 && setequal(shared_tables, dima_tables_get)) {
    dima_tables_get <- character(0)
  }
  if (length(lmf_tables_get) > 0 && setequal(shared_tables, lmf_tables_get)) {
    lmf_tables_get <- character(0)
  }
  if (length(eco_tables_get) > 0 && setequal(shared_tables, eco_tables_get)) {
    eco_tables_get <- character(0)
  }
  if (length(aim_lotic_tables_get) > 0 &&
        setequal(shared_tables, aim_lotic_tables_get)) {
    aim_lotic_tables_get <- character(0)
  }

  # list append in R is as inefficient as R itself is ugly,
  # but like many other ugly things, we'll do them anyway
  tables <- list()
  for (schema in c("dima", "lmf", "eco", "aim_lotic")) {
    cat(paste0("\nimporting ", schema, " tables...\n\n"))
    for (t in get(paste0(schema, "_tables_get"))) {
      cat(paste0("importing ", t, "...\n"))
      if (compat$con_type == "access") {
        if (compat$con_sub == "odbc") {
          tbl <- tibble::as_tibble(dplyr::tbl(access_con, t))
        } else if (compat$con_sub == "mdbtools") {
          ddl <- paste(system2(
            command = "mdb-schema",
            args = c(shQuote(path), "-T", shQuote(t), "sqlite"),
            stdout = TRUE
          ), collapse = "\n")

          result <- DBI::dbExecute(memory_con, ddl)
          insert <- paste(system2(
            command = "mdb-export",
            args = c(
              "-D", shQuote("%Y-%m-%d"), "-T", shQuote("%Y-%m-%d %H:%M:%S"),
              "-I", "sqlite", "-q", shQuote("'"), shQuote(path), shQuote(t)
            ),
            stdout = TRUE
          ), collapse = "\n")

          insert_stmts <- statement_split(insert)
          no_rows <- 0
          for (stmt in insert_stmts) {
            if (stringr::str_trim(stmt) != "") {
              cnt <- DBI::dbExecute(memory_con, stmt)
              no_rows <- no_rows + cnt
            }
          }
          tbl <- tibble::as_tibble(dplyr::tbl(memory_con, t))
        } else {
          stop(paste0("Error: Return of check.compatible() should have been ",
                      "either 'odbc' or 'mdbtools'."))
        }
      } else if (compat$con_type == "gdb") {
        tbl <- tibble::as_tibble(sf::st_read(dsn = path,
                                             layer = t, quiet = TRUE))
      } else if (compat$con_type == "sqlite") {
        if (compat$con_sub == "spatialite") {
          spatial <- TRUE
        } else {
          spatial <- FALSE
        }
        query <- create_sqlite_select(sqlite_con = sqlite_con, table = t,
                                      spatial = spatial)
        result <- DBI::dbSendQuery(sqlite_con, query$sql)
        raw_tbl <- DBI::dbFetch(result)
        DBI::dbClearResult(result)
       
        tbl <- fix_sqlite_typing(tbl = raw_tbl, pragma = query$pragma)
      }
      if (!is.null(tbl)) {
        tbl_processed <- tbl |>
          dplyr::rename_at(dplyr::vars(tidyselect::matches("dbkey")),
                           stringr::str_to_lower)

        if ("dbkey" %in% colnames(tbl_processed)) {
          tbl_processed <- tbl_processed |>
            dplyr::mutate(dbkey = ifelse(is.na(.data$dbkey), key, .data$dbkey))
        }
        tables[[schema]][[t]] <- tbl_processed
      }
    }
  }

  if (compat$con_type == "access" && compat$con_sub == "odbc") {
    DBI::dbDisconnect(access_con)
  } else if (compat$con_type == "access" && compat$con_sub == "mdbtools") {
    DBI::dbDisconnect(memory_con)
  } else if (compat$con_type == "sqlite") {
    DBI::dbDisconnect(sqlite_con)
  }

  # post import modifications
  schemas <- names(tables)
  for (schema in schemas) {
    # deals with existing but potentially inaccurate eco.db table
    if (!is.null(tables[[schema]][["db"]])) {
      old_db <- tables[[schema]][["db"]]
      if (nrow(old_db) == 1) {
        new_db <- old_db |>
          dplyr::mutate(
            dbpath = path,
            md5hash = .env$md5hash
          )
        tables[[schema]][["db"]] <- new_db
      }
    }

    # deals with data with existing dbkeys in their plot-level table
    plot <-  tables[[schema]][c("tblPlots", "POINT", "point")]
    # checks to makes there is at list 1 plot table
    if (!all(is.na(names(plot)))) {
      plot <- plot[lengths(plot) != 0][[1]]
      if ("dbkey" %in% stringr::str_to_lower(colnames(plot))) {
        db_sep <- plot |>
          dplyr::select(tidyselect::matches("dbkey")) |>
          dplyr::rename_all(stringr::str_to_lower) |>
          dplyr::group_by(.data$dbkey) |>
          dplyr::summarize(.groups = "drop") |>
          dplyr::mutate(dbkey = ifelse(is.na(.data$dbkey), key, .data$dbkey)) |>
          dplyr::mutate(dbpath = path,
                        md5hash = md5hash,
                        description = desc)
        tables[[schema]][["db"]] <- db_sep
      }
    }
  }

  list(terradat = terradat, processed = FALSE, path = path,
       md5hash = md5hash, key = key, desc = desc,
       tables = tables)
}


#' Data in TerrAdat has some specials cases, namely that many of the fields,
#' including plotkey, linekey, etc are too long for DIMA due to some of the
#' source data having been converted from another schema type to resemble DIMA
#' data. We are forced to rekey or truncate those fields, as one of the goals
#' of constructing LLEIA in the manner it is, is to be able to "re-DIMA" the
#' enterprise level data into a blank DIMA if we so chose. This function looks
#' at the source data and performs some checks and conversions if it is found to
#' be from a TerrAdat source.
#'
#' @param imported A list of lists of tibbles, constructed via
#'   \code{get_src_tables()}
#'
#' @return A processed list of lists of tibbles, which is compatible with the
#'   DIMA schema, as well as other data such as database keys found in TerrAdat,
#'   and other values (source path, md5hash, key, description and table name
#'   list)
process_terradat <-  function(con, imported) {

  # this function needs to be superceded by a terradat specfic schema.
  # there are now just too many differnces between publically available terradat
  # and dima schema to have to keep this conversion function around.
  # might still be useful to shunt terradat data back into a dima schema though
  # as is is still a mostly functional schema converter

  tbls <- imported$tables
  cat(paste0("Converting TerrADat data to fit within DIMA schema...\n"))
  key_fields <- c("SiteKey", "PlotKey", "LineKey", "RecKey", "SoilKey",
                  "CommentID")
  process_schemas <- c("dima")
  for (schema in process_schemas) {

    # replace missing tblSites since newer versions of public TerrAdat dont
    # have it
    if (is.null(tbls[[schema]][["tblSites"]])) {
      tbls[[schema]][["tblSites"]] <- tbls[[schema]][["tblPlots"]] |>
        dplyr::group_by(.data$SiteKey, .data$dbkey) |>
        dplyr::summarize(DateModified = max(.data$DateModified),
                         .groups = "drop") |>
        dplyr::mutate(SiteKey = ifelse(is.na(.data$SiteKey), "Unknown",
                                       .data$SiteKey))
    }

    tbl_names <- names(tbls[[schema]])
    for (name in tbl_names) {
      cat(paste0("Post-processing ", schema, ".", name))
      dest_info <- get_dest_info(con = con, schema = schema, table = name)
      char_fields <- dest_info$col_info |>
        dplyr::filter(!is.na(.data$character_maximum_length))
      src_col_names <- names(tbls[[schema]][[name]])
      matched_fields <- intersect(colnames(tbls[[schema]][[name]]), key_fields)

      if (length(matched_fields) > 0) {
        # re-keys key.fields which are > 20 based on their hash
        cat("\n\t... re-keying keys > length 20 ")

        new_keys <- tbls[[schema]][[name]] |>
          dplyr::select(tidyselect::any_of(matched_fields)) |>
          dplyr::distinct() |>
          dplyr::mutate(dplyr::across(
            .cols = tidyselect::any_of(matched_fields),
            .fns  = ~ dplyr::case_when(
              nchar(.) > 20 ~
                substr(vdigest(., algo = "md5", serialize = FALSE), 1, 16),
              TRUE ~ .
            ),
            .names = "{.col}_new"
          ))

        tbl <- tbls[[schema]][[name]] |>
          dplyr::left_join(new_keys, by = matched_fields) |>
          dplyr::rename_with(
            .fn = ~ paste0(.x, "_orig"),
            .cols = tidyselect::any_of(matched_fields)
          ) |>
          dplyr::rename_with(
            .fn = ~ stringr::str_replace(.x, "_new$", ""),
            .cols = tidyselect::any_of(paste0(matched_fields, "_new"))
          )
      } else {
        tbl <- tbls[[schema]][[name]]
      }

      # process fields which have text entries too long (truncate)
      cat("\n\t... Trimming fields:")
      for (i in rownames(char_fields)) {
        colname <- char_fields[i, "column_name"]
        maxlen <- char_fields[i, "character_maximum_length"]
        if (colname %in% src_col_names && !(colname %in% key_fields)) {
          cat(paste0("\n\t\t... ", colname, " @ ", maxlen))
          tbl[[colname]] <-
            stringr::str_sub(tbl[[colname]], 1, maxlen)
        }
      }
      cat("\n")
      tbls[[schema]][[name]] <- tbl

      # terradat data in tblSpecRichDetail has been split out and needs to be
      # recombined
      if (name == "tblSpecRichDetail") {
        cat("Flattening tblSpecRichDetail...\n")
        tbl <- tbls[[schema]][[name]] |>
          dplyr::arrange(.data$dbkey, .data$RecKey, .data$subPlotID,
                         .data$SpeciesList) |>
          dplyr::group_by(.data$dbkey, .data$RecKey, .data$subPlotID,
                          .data$subPlotDesc) |>
          dplyr::summarize(
            SpeciesCount = dplyr::n(),
            SpeciesList = paste0(.data$SpeciesList, collapse = ";"),
            .groups = "drop"
          )
        tbls[[schema]][[name]] <- tbl
      }
      if (name == "tblPlots") {
        cat(paste0("Replacing missing SiteKeys in tblPlots with 'Unknown'.\n"))
        tbl <- tbls[[schema]][["tblPlots"]] |>
          dplyr::mutate(SiteKey = ifelse(is.na(.data$SiteKey), "Unknown",
                                         .data$SiteKey))
        tbls[[schema]][["tblPlots"]] <- tbl
      }
      if (name == "tblPlotNotes") {
        cat(paste0("Populating missing CommentIDs in tblPlotNotes.\n"))
        tbl <- tbls[[schema]][["tblPlotNotes"]] |>
          dplyr::mutate(
            CommentID = ifelse(
              is.na(.data$CommentID),
              substr(vdigest(paste0(
                .data$PlotKey, .data$NoteDate, .data$Recorder, .data$Notes
              ), algo = "md5", serialize = FALSE), 1, 16),
              .data$CommentID
            )
          ) |>
          dplyr::rename(Note = "Notes")
        tbls[[schema]][["tblPlotNotes"]] <- tbl
      }
      if (name == "tblSites") {
        cat(paste0("Populating missing sites in tblSites with values",
                   " from tblPlots...\n"))

        site_sitekeys <- tbls[[schema]][["tblSites"]] |>
          dplyr::group_by(.data$SiteKey) |>
          dplyr::summarize(.groups = "drop")

        sitekeys <- tbls[[schema]][["tblPlots"]] |>
          dplyr::mutate(
            SiteKey = ifelse(is.na(.data$SiteKey), "Unknown", .data$SiteKey)
          ) |>
          dplyr::anti_join(site_sitekeys,
                           by = c("SiteKey" = "SiteKey")) |>
          dplyr::group_by(.data$SiteKey, .data$dbkey) |>
          dplyr::summarize(.groups = "drop") |>
          dplyr::mutate(
            Notes = paste0("Missing, populated", " from tblPlots.")
          )


        tbl <- dplyr::bind_rows(tbls[[schema]][["tblSites"]], sitekeys)
        tbls[[schema]][["tblSites"]] <- tbl
      }
      if (name == "tblGapDetail") {
        cat(paste0("Populating missing SeqNos in tblGapDetail.\n"))
        tbl <- tbls[[schema]][["tblGapDetail"]] |>
          dplyr::group_by(.data$RecKey, .data$RecType) |>
          dplyr::mutate(
            n = sum(as.double(.data$GapEnd) - as.double(.data$GapStart))
          ) |>
          dplyr::mutate(
            rn = dplyr::case_when(
              n <= 0 ~ dplyr::row_number(dplyr::desc(GapStart)),
              TRUE ~ dplyr::row_number(GapStart)
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            SeqNo = dplyr::case_when(is.na(.data$SeqNo) ~ rn,
                                     TRUE ~ .data$SeqNo)
          ) |>
          dplyr::select(-"n", -"rn")
        tbls[[schema]][["tblGapDetail"]] <- tbl
      }
    }
  }

  list(terradat = imported$terradat, processed = TRUE,
       path = imported$path, md5hash = imported$md5hash,
       key = imported$key, desc = imported$desc,
       tables = tbls)
}

#' Checks if a data source can be read by the import functions.
#'
#' @param path character string. The file path to the data source.
#'
#' @return A list with compatibility characteristics.
#' @export
check_compatible <- function(path) {
  ret <- list(compatible = FALSE, con_type = NA, con_sub = NA, error_msg = NA,
              warn_msg = NA)
  if (stringr::str_to_lower(tools::file_ext(path)) %in% c("mdb", "accdb")) {
    if (!file.exists(path)) {
      ret$error_msg <- "File does not exist."
      return(ret)
    }
    try_mdbtools <- FALSE
    mdbtools <- FALSE
    ret$con_type <- "access"
    access_head <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ="
    access_constring <- paste0(access_head, path)
    connect <- tryCatch({
      odbc::dbConnect(odbc::odbc(), .connection_string = access_constring,
                      encoding = "latin1")

    },
    error = function(cond) {
      ret$error_msg <<- cond
      try_mdbtools <<- TRUE
      cond
    })
    if (inherits(connect, "DBIConnection")) {
      tables <- tryCatch({
        odbc::dbListTables(connect)
      },
      error = function(cond) {
        ret$error.msg <<- cond
        try_mdbtools <<- TRUE
        cond
      })
    }
    if (inherits(connect, "DBIConnection")) {
      odbc::dbDisconnect(connect)
    }
    # try mdbtools
    if (try_mdbtools == TRUE) {
      try <- suppressWarnings(
        system("mdb-json --version > /dev/null 2>&1")
      )
      if (try == 0) {
        mdbtools <- TRUE
      }
    }
    if (!("error" %in% class(connect))) {
      if (!("error" %in% class(tables))) {
        ret$compatible <- TRUE
        ret$con_sub <- "odbc"
      } else if (mdbtools == TRUE) {
        ret$compatible <- TRUE
        ret$con_sub <- "mdbtools"
      }
    } else if (mdbtools == TRUE) {
      ret$compatible <- TRUE
      ret$con_sub <- "mdbtools"
    }
  } else if (
    stringr::str_to_lower(
      tools::file_ext(stringr::str_remove(path, "[/\\\\]+$"))
    ) == "gdb"
  ) {
    if (!dir.exists(path)) {
      ret$error_msg <- "Directory does not exist."
      return(ret)
    }
    ret$con_type <- "gdb"
    tables <- tryCatch({
      sf::st_layers(dsn = path)
    },
    error = function(cond) {
      ret$error_msg <<- cond
      cond
    })
    if (!("error" %in% class(tables))) {
      ret$compatible <- TRUE
      ret$con_sub <- "gdal"
    }
  } else if (
    stringr::str_to_lower(tools::file_ext(path)) %in%
      c("db", "sqlite", "gpkg")
  ) {
    if (!file.exists(path)) {
      ret$error_msg <- "Path does not exist."
      return(ret)
    }
    no_tables <- 0
    try_spatial <- TRUE
    ret$con_type <- "sqlite"
    connect <- tryCatch({
      DBI::dbConnect(RSQLite::SQLite(), path)
    },
    error = function(cond) {
      ret$error.msg <<- cond
      try_spatial <<- FALSE
      cond
    })

    if (inherits(connect, "DBIConnection")) {
      tables <- tryCatch({
        DBI::dbListTables(connect)
      },
      error = function(cond) {
        try_spatial <<- FALSE
        ret$error_msg <<- cond
        cond
      })

      if (!("error" %in% class(tables))) {
        no_tables <- length(tables)
      }
    }
    if (try_spatial == TRUE) {
      spatial <- tryCatch(
        expr = {
          DBI::dbExecute(connect,
                         "SELECT load_extension('mod_spatialite');")
          TRUE
        },
        error = function(cond) {
          ret$error.msg <<- cond
          cond
        },
        warning = function(cond) {
          ret$warn.msg <<- cond
          NULL
        }
      )
      if (spatial == TRUE) {
        ret$con_sub <- "spatialite"
      } else {
        ret$con_sub <- "sqlite"
      }
    }
    if (inherits(connect, "DBIConnection")) {
      odbc::dbDisconnect(connect)
    }
    if (!inherits(connect, "DBIConnection")) {
      ret$compatible <- FALSE
    } else if ("error" %in% class(tables)) {
      ret$compatible <- FALSE
    } else if (no_tables == 0) {
      ret$compatible <- FALSE
      ret$error_msg <- "Database is empty."
    } else {
      ret$compatible <- TRUE
    }
  } else {
    ret$error_msg <- paste0("File extension not one of (mdb, accdb, gdb, db, ",
                            "sqlite, gpkg)")
  }

  ret
}


#' The main processing function used to import data from source data into a
#' PostGIS database
#'
#' @param con A DBI connection. A database connection object to the already
#'    created (via create_db) LLEIA PostgreSQL database
#' @param src_path A string file path pointing to the source database or saved
#'   RDS file. File name extensions must be one of (.mdb, .accdb, .sqlite, .db,
#'   .rds) and folder paths must have the extension of (.gdb).
#' @param key A string. Serves as a unique key for the database source which
#'   will identify data in the destination DB as coming from a specific source.
#' @param desc A string. The description the will be used to describe the source
#'   database contents which are imported into the Postgres destination.
#' @param update logical. A flag that instructs the function to construct an
#'   UPSERT statement instead of an INSERT statement.
#' @param log logical. A flag that tells the function to create a log file of
#'   the import, saved in the current working directory in the format of
#'   import_YYYMMDDHHMMSS.log
#' @param save_raw A string file path to which to save data imported data from
#'   \code{src_path} via \code{get_src_tables()}. This file can be used in
#'   as a future \code{src_path}. Primarily used for testing purposes.
#' @param save_processed A string file path to which to save data imported data
#'   from \code{src_path} via \code{get_src_tables()} which has been processed
#'   via \code{process_terradat()}. This file can be used in
#'   as a future \code{src_path}. Primarily used for testing purposes.
#' @param verbose logical. A flag telling the function to be more verbose in
#'   its messaging.
#' @param chunk_size An integer.This tells the function how many rows to
#'   attempt to insert at once. Failure on a chunk will cause the function
#'   to default to row-wise inserts for the entire chunk.
#' @param skip_refresh logical. A flag telling the function to skip refreshing
#'   materialized views. This can be helpful in speeding up the importing of
#'   multiple databases.
#' @export
import_to_post <-  function(
  con, src_path, key, desc, host = "localhost", port = 5432, update = FALSE,
  log = NULL, save_raw = NULL, save_processed = NULL, verbose = FALSE,
  chunk_size = 1000, skip_refresh = FALSE
) {
  # compat <- check.compatible(path = src_path)
  DBI::dbExecute(con, "SET client_min_messages TO WARNING;")

  log_name <- log

  # in case the tables have already been saved in RDS format
  if (stringr::str_to_lower(tools::file_ext(src_path)) == "rds") {
    imported <- readRDS(src_path)
    key <- imported$key
    hash <- imported$md5hash

    # in case a fresh import is needed
  } else {
    if (is.null(key)) {
      key <- stringr::str_sub(digest::digest(src_path, algo = "md5"),
                              start = -6)
    }
    hash <- get_key(src_path)
    imported <- get_src_tables(con = con, path = src_path, md5hash = hash,
                               key = key, desc = desc)
    if (!is.null(save_raw)) {
      cat(paste0("Saving raw tables as ", save_raw),
          "\n")
      saveRDS(imported, save_raw)
    }
  }
  # process the data if it is in terradat format so we can place it into
  # the dima schema.
  if (imported$terradat == TRUE && imported$processed == FALSE) {
    post_processed <- process_terradat(con = con, imported = imported)
    if (!is.null(save_processed)) {
      cat(paste0("Saving processed tables as ", save_processed), "\n")
      saveRDS(post_processed, save_processed)
    }
  } else {
    post_processed <- imported
  }
  to_db(con = con, tbls = post_processed$tables, update = update, dbkey = key,
        desc = desc, path = src_path, hash = hash, log = log_name,
        verbose = verbose, named = FALSE,
        chunk_size = chunk_size)

  if (skip_refresh == FALSE) {
    refresh_views(con = con)
  }
}

# run only if called from a script.
if (sys.nframe() == 0) {
  source(here::here("common.R"))
  args <- commandArgs(trailingOnly = TRUE)

  option_list <-  list(
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
      opt_str = c("-U", "--update"), action = "store_true", default = FALSE,
      help = paste0("A flag telling the script to update existing ",
                    "data from the source data instead of inserting ",
                    "only new records [default: %default].")
    ),
    optparse::make_option(
      opt_str = c("-k", "--key"),
      help = paste0("A short/unique code for this database. [Will be",
                    " generated automatically if not provided].")
    ),
    optparse::make_option(
      opt_str = c("-d", "--desc"),
      help = "Descriptive text for documenting the source data."
    ),
    optparse::make_option(
      opt_str = c("-l", "--log"),
      help = paste0("Path at which to log the results of the import")
    ),
    optparse::make_option(
      opt_str = c("-v", "--verbose"), action = "store_true", default = FALSE,
      help = "Increase the level of script reporting [default: %default]."
    ),
    optparse::make_option(
      opt_str = c("-s", "--save_raw"),
      help = paste0("Path at which to save a raw version of the ",
                    "imported tables in RDS format.")
    ),
    optparse::make_option(
      opt_str = c("-S", "--save_processed"),
      help = paste0("Path at which to save a processed version of ",
                    "the imported tables in RDS format. Script will ",
                    "only export if tables they have been processed.")
    ),
    optparse::make_option(
      opt_str = c("-c", "--chunk_size"), default = 1000,
      help = paste0("The number of rows to insert into the db at ",
                    "once. If there is an insert error in a chunk, ",
                    "this number of records will be inserted row-",
                    "wise instead, so keeping this number smaller ",
                    "for databases with many probable constraint and",
                    " value errors is a good idea ",
                    "[default: %default].")
    ),
    optparse::make_option(
      opt_str = c("-r", "--skip_refresh"), action = "store_true",
      default = FALSE,
      help = paste0("Skip refreshing materialized views. This can ",
                    "save time when uploading multiple databases ",
                    "[default: %default].")
    )
  )

  description <- paste0(
    "\nThis script will import data from DIMA, LMF, or ",
    "Terradat data sources into a (already created) PostGIS ",
    "database.\n\n", "dbname: The name of the postgres ",
    "database into which to import.\nsource_data: The file path to the ",
    "database file (.mdb, .accdb, .sqlite, .db) or folder (.gdb).\n"
  )

  opt_parser <- optparse::OptionParser(
    usage = "usage: %prog [options] dbname source_data",
    option_list = option_list, prog = NULL,
    description = description
  )

  opt <- optparse::parse_args(opt_parser, positional_arguments = 2, args = args)

  if (!is.null(opt$options$log)) {  # test to see if log is writable
    msg_out("\nStarting import.", log = opt$options$log)
  }

  con <- connect_pg(
    dbname = opt$args[1],
    host = opt$options$host,
    port = opt$options$port,
    user = opt$options$user,
    password = opt$options$password
  )

  import_to_post(
    con = con,
    src_path = opt$args[2],
    key = opt$options$key,
    desc = opt$options$desc,
    update = opt$options$update,
    log = opt$options$log,
    save_raw = opt$options$save_raw,
    save_processed = opt$options$save_processed,
    verbose = opt$options$verbose,
    chunk_size = opt$options$chunk_size,
    skip_refresh = opt$options$skip_refresh
  )

  DBI::dbDisconnect(con)
  rm(con, envir = .GlobalEnv)
  msg_out("\nScript finished.", log = opt$options$log)
}
