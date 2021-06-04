#!/usr/bin/env Rscript
libraries = c("optparse", "dplyr", "tibble", "digest", "tictoc", "DBI", "pool", 
              "RPostgres", "RSQLite", "odbc", "rgdal", "getPass", "tools", 
              "stringr", "glue")

for (lib in libraries){
  suppressMessages(library(lib, character.only = TRUE))
}
vdigest <- Vectorize(digest)



#' This function will take a log message and write it to stdout and/or a log 
#' file.
#'
#' @param msg A string. The message to write. 
#' @param log A string file path to a log wehre messages are to be written.
#' @param sep A string, the separator to paste after the message (e.g. an EOL 
#'   character)
#' @param prnt logical. A flag indicating whether or not to print the msg to 
#'   stdout 
#'
#' @return Nothing
#' @export
msg.out <- function(msg, log = NULL, sep = "\n", prnt = TRUE){
  if (prnt == TRUE){
    cat(paste0(msg, sep))
  }
  if (!is.null(log)){
    log.file <- file(log, "a", encoding = "UTF-8") 
    writeLines(msg, log.file, sep = sep)
    close(log.file) 
  }
}

#######################################
## retrieving info related functions ##
#######################################


#' Queries the Postgres LLEIA db instance for tables in a specific schema
#'
#' @param schema A string. The schema to query for tables.
#'
#' @return A table containing the table names in \code{schema}.
#' @export
get.dest.tables <- function(schema){
  tables.sql <- glue(paste(
    "SELECT table_name ", 
    "  FROM information_schema.tables ", 
    " WHERE table_schema = '{schema}' ",
    "   AND table_type = 'BASE TABLE';",
    sep = "\n"
  ))
  tables <- dbGetQuery(con, tables.sql)
  
}


#' This function generates an md5hash for database files or ESRI file 
#' geodatabase folders.
#'
#' @param src A string file path to a file with one of the following extensions:
#'   (.mdb, .accdb, .sqlite, .db) or a folder path with the .gdb extension.
#'
#' @return An md5hash of the file, or in the case of a folder, a hash of the 
#'   vector of hashes for each of the files in the folder, minus certain files
#'   in the gdb folder which are mutable or transient.
#' @export
get.key <- function(src){
  allowed.exts <- c("mdb", "accdb", "gdb", "sqlite", "db")
  ext <- file_ext(src)
  base <- basename(src)
  dir <- dirname(src)
  if (str_to_lower(ext) == "gdb" & dir.exists(src)){
    exists <-  TRUE
    in.type <- "folder"
  } else if (str_to_lower(ext) != "gdb" & file.exists(src)){
    exists <-  TRUE
    in.type <- "file"
  } else {
    exists <- FALSE
  }
  if(!exists){
    stop("Source path does not exist. Quitting...")
  }
  if(!(ext %in% allowed.exts)){
    stop(paste0("Source is not of type (", paste(allowed.exts, collapse = ", "),
                "). Quitting..."))
  }
  cat("Calculating md5 hash...\n")
  if (in.type == "file"){
    key <- digest(src, algo="md5", file=TRUE)
  } else if (in.type == "folder"){
    files <- list.files(path = src, recursive = TRUE, all.files = TRUE,
                        full.names = FALSE) %>%
      # removes anything which ends in .lock
      str_subset(pattern = ".*(?<!\\.lock)$") %>%
      # removes a00000004.gdbtable and a00000004.gdbtablx, a00000004.freelist, 
      # and timestamps, which seem to change even when the only file operation 
      # is copy
      str_subset(pattern = fixed("a00000004.gdbtabl"), negate = TRUE) %>%
      str_subset(pattern = fixed("a00000004.freelist"), negate = TRUE) %>%
      str_subset(pattern = fixed("timestamps"), negate = TRUE)
    
    full.files <- sapply(files, FUN=function(x) file.path(src, x))
    hashes <- sapply(full.files, FUN=function(x) digest(x, algo="md5", 
                                                        file=TRUE))
    key <- digest(hashes, algo="md5")
  }
  return(key)
}


#' Recursive function which returns an ordered table of schema table names 
#' where tables that are foreign key parents are listed in order before their 
#' children
#' 
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
#' @export
insert.order <- function(schema, level = 0, processed.tables = NULL, 
                         tables = NULL){
  # print(level)
  if (is.null(tables)){
    tables <- get.dest.tables(schema)$table_name
  }
  
  # sql courtesy of user martin
  # https://stackoverflow.com/questions/1152260/postgres-sql-to-list-table-foreign-keys
  find.parent.sql <- paste(
    "SELECT att2.attname as \"child_column\", ", 
    "       cl.relname as \"parent_table\", ", 
    "       att.attname as \"parent_column\", ",
    "       conname ",
    "  FROM (",
    "       SELECT unnest(con1.conkey) as \"parent\", ", 
    "              unnest(con1.confkey) as \"child\", ", 
    "              con1.confrelid, con1.conrelid, con1.conname ",
    "         FROM pg_class cl ",
    "         JOIN pg_namespace ns on cl.relnamespace = ns.oid ",
    "         JOIN pg_constraint con1 on con1.conrelid = cl.oid ",
    "        WHERE cl.relname = '{tbl}' ",
    "          AND ns.nspname = '{schema}' ",
    "          AND con1.contype = 'f' ",
    "       ) con ",
    "  JOIN pg_attribute att ON ",
    "       att.attrelid = con.confrelid AND att.attnum = con.child ",
    "  JOIN pg_class cl ON ",
    "       cl.oid = con.confrelid ",
    "  JOIN pg_attribute att2 ON ",
    "       att2.attrelid = con.conrelid and att2.attnum = con.parent;",
    sep = "\n")
  
  
  level.tables <- tibble(
    tblname = character(),
    level = integer())
  if (is.null(processed.tables)){
    processed.tables = level.tables
  }
  
  for (tbl in tables){
    sql  <- glue(find.parent.sql)
    parents <- dbGetQuery(con, sql)
    parent.names <- unique(parents$parent_table)
    parents.processed <- intersect(unique(parent.names), 
                                   unique(processed.tables$tblname))
    if (isTRUE(all.equal(sort(unique(parents.processed)), 
                         sort(unique(parent.names))))){
      #if (length(parents.processed) == length(parent.names)){
      level.tables <- level.tables %>% 
        add_row(tblname = tbl, level = level)
    }
  }
  
  processed.tables <- processed.tables %>% add_row(level.tables)
  remaining.tables <- setdiff(tables, level.tables$tblname)
  # print(level.tables)
  if (nrow(level.tables) == 0){
    return(processed.tables)
  } else {
    insert.order(schema, level = level + 1, processed.tables = processed.tables,
                 tables = remaining.tables)
  }
}



#' Gets column names from a Postgres schema/table.
#'
#' @param schema A string. The schema that contains the table to return info 
#'   about.
#' @param table A string. The table name to return info about. 
#'
#' @return A list of tibbles, one containing column information, and another
#'   containing constraint information.
#' @export
get.dest.info <- function(schema, table){
  info.sql <- glue(paste(
    "SELECT column_name, data_type, character_maximum_length ",
    "  FROM information_schema.columns ",
    " WHERE table_schema = '{schema}' ",
    "   AND table_name = '{table}' ", 
    " ORDER BY ordinal_position; ",
    sep = "\n"))
  col.info <-  dbGetQuery(con, info.sql)
  
  constraint.sql = glue(paste(
    "SELECT con.conname, con.contype",
    "  FROM pg_catalog.pg_constraint con",
    " INNER JOIN pg_catalog.pg_class rel",
    "    ON rel.oid = con.conrelid",
    " INNER JOIN pg_catalog.pg_namespace nsp",
    "    ON nsp.oid = connamespace",
    " WHERE nsp.nspname = '{schema}'",
    "   AND rel.relname = '{table}';",
    sep = "\n"))
  constraints <-  dbGetQuery(con, constraint.sql)
  return(list(col.info = col.info, constraints = constraints))
}



#' Returns information about field types for imported source data.
#'
#' @param tbl A tibble containing the source data.
#'
#' @return A tibble containing column information for \code{tbl}.
#' @export
get.src.info <- function(tbl){
  col.names <- as_tibble(list(column_name = colnames(tbl)))
  src.types <- tbl %>% 
    dplyr::summarise_all(class) %>% slice(1) %>% 
    tidyr::gather(variable, class)
  info <- col.names %>% 
    left_join(src.types, by=c("column_name" = "variable")) %>%
    rename(data_type = class)
  return (info)
}


#' Determines which columns have different types between a data source and 
#' destination. and 
#'  type
#'
#' @param src.cols A tibble with two fields, column_name (string) and 
#'   data_type (string), the former containing column names from source tibble
#'   tables and the latter denoting their R data type (e.g. integer, logical, 
#'   etc.)
#' @param dest.cols A tibble with two fields, column_name (string) and 
#'   data_type (string), the former containing column names from Postgres 
#'   destination table and the latter denoting their Postgres data type (e.g. 
#'   character varying, double precision, etc.)
#'
#' @return A logical vector with TRUE indicating same column type and FALSE 
#'   indicating otherwise.
#' @export
compare.types <- function(src.cols, dest.cols){
  type.match <- tibble(ptype = character(), rtype = character()) %>%
    add_row(ptype = "character varying", rtype = "character") %>%
    add_row(ptype = "text", rtype = "character") %>%
    add_row(ptype = "double precision", rtype = "numeric") %>%
    add_row(ptype = "integer", rtype = "integer") %>%
    add_row(ptype = "smallint", rtype = "integer") %>%
    add_row(ptype = "bigint", rtype = "integer") %>%
    add_row(ptype = "boolean", rtype = "logical") %>%
    add_row(ptype = "timestamp without time zone", 
            rtype = "POSIXct, POSIXt, Date") %>%
    add_row(ptype = "timestamp with time zone", 
            rtype = "POSIXct, POSIXt, Date") %>%
    add_row(ptype = "timestamp", rtype = "POSIXct, POSIXt, Date")
  
  src.cols <-  src.cols %>% rename(src_type = data_type)
  dest.cols <-  dest.cols %>% rename(dest_type = data_type)
  compare <- dest.cols %>% 
    inner_join(src.cols, by = c("column_name" = "column_name")) %>% 
    left_join(type.match, by = c("dest_type" = "ptype")) %>% 
    mutate(matched = str_detect(rtype, src_type)) %>%
    mutate(matched = ifelse(is.na(matched), FALSE, matched))
  return (compare)
}


#' Gathers information about both source and destination tables and determines
#' which fields are matched in name and type.
#'
#' @param schema A string. The name of the schema to be queried in the Postgres
#'   connection.
#' @param tbl.name A string. The name of the table to retrieve information about
#'   from the postgres connection.
#' @param src.data A tibble containing the source data having the name of 
#'   \code{tbl.name}
#'
#' @return A list of vectors containing information about missing columns in the 
#'   destination that are in the source, missing columns in the source that are 
#'   in the destination, columns which match in both source and destination, 
#'   and whether or not those columns need to be CAST during insert.
#' @export
get.info <- function(schema, tbl.name, src.data){
  dest.info <- get.dest.info(schema, tbl.name)
  src.info <- get.src.info(src.data)
  cols.src <- src.info$column_name
  cols.dest <- dest.info$col.info$column_name
  compare.info <- compare.types(src.cols = src.info, 
                                dest.cols = dest.info$col.info)
  import.cols <-  compare.info$column_name
  cast <- !(compare.info$matched | compare.info$dest_type == "USER-DEFINED")
  missing.dest <-  setdiff(cols.src, cols.dest)
  missing.src <- setdiff(cols.dest, cols.src)
  return(list(import.cols = import.cols, cast = cast, 
              missing.dest = missing.dest, missing.src = missing.src))
}

#########################
## INSERTING functions ##
#########################


#' Will create an INSERT SQL statement for a Postgres table based on the 
#' contents of the data source.
#'
#' @param schema A string. The name of the schema in the destination. 
#' @param table.name A name of the table in the destination for which to build the INSERT 
#'   statement
#' @param named logical. A flag that tells the function to build the SQL based 
#'   on glue syntax (e.g. {my.parameter}) or parameter position syntax (e.g. $1, 
#'   $2, etc.) 
#' @param update logical. A flag that instructs the function to construct an 
#'   UPSERT statement instead of an INSERT statement.
#' @param ins.cols A string vector containing the names of columns to use for 
#'   the insert statement.
#' @param update.cols A string vector containing the names of columns to UPDATE 
#'   in the case of an upsert statement.
#' @param cast A logical vector telling the function whether to encapsulate the
#'   parameter in the SQL in a CAST function, hopefully mitigating type errors.
#' @param srid An integer denoting the SRID (EPSG code) to use when inserting 
#'   geometry data into tables that support PostGIS geometry.
#'
#' @return a list containing the built INSERT SQL and the list of columns used
#'   to build it.
#' @export
create.insert <- function(schema, table.name, named = FALSE, update = FALSE, 
                          ins.cols = NULL, update.cols = NULL, cast = NULL,
                          srid = 4326){
  if (is.null(cast)){
    cast <- rep(FALSE, times = length(ins.cols))
  }
  info <- get.dest.info(schema, table.name)
  # gets the pkey constraint name or first unique constraint name
  # on conflict statement only allows one constraint check
  constraint.name <- (info$constraints %>% filter(contype %in% c("u", "p")) %>% 
                        arrange(contype, conname))$conname[1]
  if (is.null(ins.cols)){
    cols <- info$col.info$column_name
    dtypes <- info$col.info$data_type
  } else {
    restricted <- info$col.info %>% 
      inner_join(as_tibble(list(column_name = ins.cols)), 
                 by = c("column_name" = "column_name")) 
    cols <- restricted$column_name
    dtypes <- restricted$data_type
  }
  if (is.null(update.cols)){
    u.cols <- cols
  } else {
    u.cols <- intersect(cols, update.cols)
  }
  # geometry
  geom.col <- cols == "geom"
  
  if (update == FALSE | is.na(constraint.name)){
    update.sql <- "ON CONFLICT DO NOTHING"
  } else {
    l = character()
    for (i in 1:length(u.cols)){
      l[i] <- paste0('"', u.cols[i], '"', " = EXCLUDED.", '"', u.cols[i], '"')
    }
    update.sql <-  paste0(
      glue("ON CONFLICT ON CONSTRAINT \"{constraint.name}\" \nDO UPDATE SET "), 
      paste(l, collapse = ", "))
  }
  colstring <- paste(paste0('"', cols,'"'), collapse = ", ")
  if (named == TRUE){
    # we have to replace spaces in param names because glue_data_sql() can't
    # handle them
    params <- paste0(ifelse(cast,"CAST({", "{"), 
                          str_replace_all(cols, " ", "_"), 
                          ifelse(cast, paste0("} AS ", dtypes, ")"), "}"))
    # for geometry 

  } else {
    params <- paste0(ifelse(cast,"CAST($", "$"), seq(1, length(cols)), 
                          ifelse(cast, paste0(" AS ", dtypes, ")"), ""))
  }
  params.geom <- paste0(ifelse(geom.col,"ST_GeomFromText(", ""), params, 
                        ifelse(geom.col, paste0(", ", srid, ")"), ""))
  paramstring <- params.geom %>% paste(collapse = ", ")
  insert.sql <- glue(paste(
    "INSERT INTO {schema}.\"{table.name}\" ({colstring})",
    "VALUES ({paramstring}) ",
    "{update.sql};", 
    sep = "\n"))
  return(list(sql = insert.sql, cols = cols))
}



#' Used to insert individual rows via apply() family function or iterative loop
#'
#' @param row A named list or tibble containing the values to bind to the INSERT
#'   statement via parameter substitution.
#' @param stmt A string containing the INSERT statement with named parameters to
#'   use with glue syntax (e.g. {my.parameter}). 
#' @param log A string file path location of the log file to write 
#'   results to.
#'
#' @return The number of rows affected by the insert.
#' @export
insert.row <- function(row, stmt, log = NULL){
  # print(row)
  sql <- glue_data_sql(.x = row, .con = con, stmt)
  # print(sql)
  affected <- tryCatch({
    a <- dbExecute(con, sql)
    # cat(paste0("row-wise non-error affected: ", a, "\n"))
    a
  },
  error = function(e) {
    values = str_match(sql, "VALUES (.+)")[2]
    e.short <- str_replace(e$message, regex("CONTEXT:.+", dotall = TRUE), "")
    msg <- paste0("\nFailed on row: ", values, "\nError: ", e.short)
    msg.out(msg, log, sep = "")
    last.error <<- e
    # cat(paste0("row-wise ERROR affected: ", 0, "\n"))
    return(0)
  },
  warning = function(w) {
    values = str_match(sql, "VALUES (.+)")[2]
    msg <- paste0("\nWarned on row: ", values, "\nWarning: ", w$message)
    msg.out(msg, log, sep = "")
    last.warning <<- w
    return(a)
  }
  )
  return(affected)
}


#' Inserts the source data in a source table into the Postgres destination
#' using row-wise inserts (one row at a time.)  This is slower than a multi-row
#' parameter bind, but allows the insert process to capture single row insert 
#' errors, log them, and continue processing.
#'
#' @param update logical. A flag that instructs the function to construct an 
#'   UPSERT statement instead of an INSERT statement.
#' @param schema A string. The name of the schema in the destination. 
#' @param table A tibble containing the source data to be inserted.
#' @param table.name A name of the table in the destination for which to build 
#'   the INSERT statement.
#' @param cols A string vector containing the names of columns to use for 
#'   the insert statement.
#' @param cast A logical vector telling the function whether to encapsulate the
#'   parameter in the SQL in a CAST function, hopefully mitigating type errors.
#' @param log A string file path location of the log file to write 
#'   results to.
#'
#' @return
#' @export
rowwise.insert <- function(update, schema, table, table.name, cols, cast, log){
  insert <- create.insert(update = update, schema = schema, 
                          table.name = table.name, 
                          ins.cols = cols, cast = cast, named = TRUE)
  # need to remove spaces in column names to deal with glue_sql_data()
  # inadequacies in this matter
  table <- table %>% rename_all(~gsub(" ", "_", .))
  # pcts = seq(from = 0, to = 1, by = 0.1)
  tot.rows <- nrow(table)
  # old.status <- 0
  total.affected <- 0
  for (i in 1:tot.rows) {
    affected <-  insert.row(row = table[i,], stmt = insert$sql, 
                   log = log)
    total.affected <- total.affected + affected
  }
  return(total.affected)
}


#' This is an alternative row-wise insert function using \code{DBI::dbBind()} 
#' instead of glue functionality like \code{rowwise.insert()}.
#' CURRENTLY NOT IMPLEMENTED but kept for reference.
#'
#' @param table A tibble containing the source data to be inserted.
#' @param insert A list constructed via \code{create.insert()}.
#'
#' @return The number of rows affected by the insert.
#' @export
dbbind.insert.rw <- function(table, insert){
  rowwise.affected = 0
  for (i in rownames(table)){
    row <- table[i,]
    send.data <- as.list(select(row, insert$cols))
    names(send.data) <- NULL
    a <- tryCatch(
      {
        send <- dbSendStatement(con, insert$sql)
        dbBind(send, row)
        rw.bind.affected <- dbGetRowsAffected(send)
        dbClearResult(send)
        # cat(paste0("rowwise bind non-error affected: ", 
        #            rw.bind.affected, "\n"))
        rw.bind.affected
      },
      error=function(e) {
        dbClearResult(send)
        return(0)
      }
    )
    rowwise.affected <- rowwise.affected + a
  }
  return(rowwise.affected)
}


#' This is the top-table level inserting function which attempts, be default to 
#' write source data to the destination in chunks of \code{chunk.size} rows. It
#' manages errors in the chunk insert by moving to a row-wise insert for that
#' particular chunk. A smaller \code{chunk.size} will result in slower inserts
#' for data without foreign key violations (or other errors) but decreasing 
#' \code{chunk.size} can speed up inserts for data where foreign key issues
#' are common.
#'
#' @param update logical. A flag that instructs the function to construct an 
#'   UPSERT statement instead of an INSERT statement.
#' @param schema A string. The name of the schema in the destination.
#' @param table A tibble containing the source data to be inserted.
#' @param table.name A name of the table in the destination for which to build 
#'   the INSERT statement.
#' @param cols A string vector containing the names of columns to use for 
#'   the insert statement.
#' @param cast A logical vector telling the function whether to encapsulate the
#'   parameter in the SQL in a CAST function, hopefully mitigating type errors.
#' @param log A string file path location of the log file to write 
#'   results to.
#' @param verbose logical. A flag telling the function to be more verbose in 
#'   its messaging.
#' @param chunk.size An integer.This tells the function how many rows to 
#'   attempt to insert at once. Failure on a chunk will cause the function
#'   to default to row-wise inserts for the entire chunk. 
#'
#' @return The number of rows affected by the insert.
#' @export
dbbind.insert <- function(update, schema, table, table.name, cols, cast, log, 
                          verbose = FALSE, chunk.size = 1000){
  insert <- create.insert(update = update, schema = schema, 
                          table.name = table.name, 
                          ins.cols = cols, cast = cast, named = FALSE)
  
  # we need to de-tibble and de-name our data to use positional args
  # with dbBind()
  processed <- 0
  total.affected <- 0
  affected <- NA
  tbl.nrow <- nrow(table)
  if (verbose == TRUE){
    msg <- paste0("Insert SQL: \n", insert$sql)
    msg.out(msg, log, prnt = FALSE)
  }
  while (processed < tbl.nrow) {
    if (verbose == TRUE){
      cat(paste0(processed,"/",tbl.nrow, "..."))
    } else {
      cat(paste0(as.integer(round(processed/tbl.nrow*100, 0)), "%..."))
    }
    end.row <-  processed + chunk.size
    sub.tbl <- slice(table, (processed + 1):(end.row))
    send.data <- as.list(select(sub.tbl, insert$cols))
    names(send.data) <- NULL
    affected <- tryCatch(
      {
        send <- dbSendStatement(con, insert$sql)
        dbBind(send, send.data)
        bind.affected <- dbGetRowsAffected(send)
        dbClearResult(send)
        # cat(paste0("dbBind non-error affected: ", bind.affected, "\n"))
        bind.affected
      },
      error=function(e) {
        # cat(paste0("\n", e$message))
        cat("\nFailed on dbBind(), attempting rowwise insert...")
        error.affected <- dbGetRowsAffected(send)
        dbClearResult(send)
        rowwise.affected <- rowwise.insert(update = update, schema = schema,
                                   table = sub.tbl,
                                   table.name = table.name,
                                   cols = cols, cast = cast, log = log)
        tot.error.affected <- error.affected + rowwise.affected
        # cat(paste0("dbbind ERROR affected: ", tot.error.affected, "\n"))
        return(tot.error.affected)
      }
    )
    processed <- processed + nrow(sub.tbl)
    total.affected <- total.affected + affected
    rm(affected)
    # for safety
    if (nrow(sub.tbl) == 0)
      break
  }
  cat("100%\n")
  return (total.affected)
}



#' This is the top-level inserting function for all source data. It determines
#' insert order and constructs parameters needed for the table-level insert 
#' functions \code{dbbind.insert} and \code{rowwise.insert}
#'
#' @param tbls A list of tables produced by \code{get.src.tables()}
#' @param update logical. A flag that instructs the function to construct an 
#'   UPSERT statement instead of an INSERT statement.
#' @param dbkey A string. Serves as a unique key for the database source which
#'   will identify data in the destination DB as coming from a specific source.
#' @param desc A string. The description the will be used to describe the source
#'   database contents which are imported into the Postgres destination.
#' @param path A string file path pointing to the the source database to import. 
#' @param hash A string. The md5hash of the database, produced via 
#'   \code{get.key()}
#' @param verbose logical. A flag telling the function to be more verbose in 
#'   its messaging.
#' @param log A string file path location of the log file to write 
#'   results to.
#' @param named logical. A flag that tells the function to defualt to constuct
#'   the INSERT statement using the glue syntax and insert the data into the 
#'   database row-wise (one row at a time).
#' @param chunk.size An integer.This tells the function how many rows to 
#'   attempt to insert at once. Failure on a chunk will cause the function
#'   to default to row-wise inserts for the entire chunk.  
#'
#' @return Nothing.
#' @export
to.db <- function(tbls, update, dbkey, desc, path, hash, verbose = FALSE, 
                  log = NULL, named = FALSE, chunk.size = 1000){
  # tic()
  special.tables = list(dima = list(
    tblSites = "db_site",
    tblPlots = "db_plot",
    tblLines = "db_line"))
  schemas <- names(tbls)
  for (schema in schemas){
    # creating record in table db before anything else
    if (is.null(tbls[[schema]][["db"]])){
      db.table <- as_tibble(list(dbkey = dbkey, md5hash = hash, dbpath = path, 
                                 description = desc))
      tbls[[schema]][["db"]] <- db.table
    }
    
    table.names <- names(tbls[[schema]])
    order <- insert.order(schema)
    order.source <- order %>% inner_join(as_tibble(list(tblname=table.names)),
                                         by=c("tblname" = "tblname")) %>%
      mutate(level = ifelse(tblname == "db", -1, level)) %>%
      arrange(level, tblname)
    
    
    # loop through source tables and import
    for (tbl in order.source$tblname){
      current.table <- tbls[[schema]][[tbl]]
      special.table <- special.tables[[schema]][[tbl]]
      for (i.tbl in c(special.table, tbl)){
        if (nrow(current.table) > 0){
          info <- get.info(schema = schema, tbl.name = i.tbl, 
                           src.data = current.table)
  
          if (verbose == TRUE){
            msg <- paste0("\nFor table ", i.tbl, ":")
            msg.out(msg, log)
            msg <- paste0(length(info$import.cols), 
                          " columns match in source and destination.")
            msg.out(msg, log)
            if (length(info$missing.dest) > 0){
              msg <- paste0("The following source columns have no match in the",
                            " destination database: ", paste(info$missing.dest, 
                                                            collapse = ", "))
              msg.out(msg, log)
            }
            if (length(info$missing.src) > 0){
              msg <- paste0("The following destination columns have no match ",
                            "in the source database: ", paste(info$missing.src, 
                                                           collapse = ", "))
              msg.out(msg, log)
            }
          }
          if ("dbkey" %in% info$missing.src){
            current.table <- current.table %>% mutate(dbkey = dbkey)
            info$import.cols <- c("dbkey", info$import.cols)
            info$cast <- c(FALSE, info$cast)
          }
          msg <-  paste0("Inserting data into ", schema, ".", i.tbl, 
                         "... ")
          msg.out(msg, log, sep = " ")
          if (named == TRUE){
            affected <- rowwise.insert(update = update, schema = schema,
                                       table = current.table, 
                                       table.name = i.tbl,
                                       cols = info$import.cols, log = log, 
                                       cast = info$cast)
          }
          else {
            affected <- dbbind.insert(update = update, schema = schema,
                                      table = current.table, 
                                      table.name = i.tbl,
                                      cols = info$import.cols, log = log,
                                      verbose = verbose, cast = info$cast,
                                      chunk.size = chunk.size)
          }
          t <- toc(quiet=TRUE)
          # print(affected)
          msg <- paste0(sum(affected), "/", nrow(current.table), 
                        " rows affected.\n")
          if (i.tbl %in% c("tblSpecies", "tblSpeciesGeneric", "tblEcolSites")){
            msg <- paste0(msg, "Rows with values divergent from base table ",
                          "inserted into ", i.tbl, "_delta.\n")
          }
          msg <- paste0(msg, "END ", i.tbl, "\n")
          msg.out(msg, log)
        }
      }
    }
  }
  # msg.out(paste0(t$toc - t$tic, " sec elapsed during insert."), log)
}


#' A recursive function which can be used to determine the proper order in which
#' to refresh materialized views, i.e. materialized views which depend on other
#' materialized views will be refreshed after the view it depends on is 
#' refreshed.
#'
#' @param done A string vector. An internal parameter which the function uses to
#'   keep track of which views have already been refreshed.
#' @param level An integer. An internal parameter which the function uses to 
#'   keep track of how many times it has been called.
#'
#' @return Nothing.
#' @export
refresh.views <- function(done = character(0), level = 0){
  # cat(paste0("level = ", level, "\n"))
  views <- (dbGetQuery(
    con, "SELECT relname FROM pg_class WHERE relkind = 'm';"))$relname
  undone.views <- views[!(views %in% done)]
  # cat(paste0("undone views: ", paste(undone.views, collapse = ", "), "\n"))
  if (length(undone.views != 0)){
    depend.sql <- "
    SELECT dependent_ns.nspname as dependent_schema,
           dependent_view.relname as dependent_view, 
	         source_ns.nspname as source_schema, 
	         source_table.relname as source_table,
	         source_table.relkind as source_kind,
	         COUNT(pg_attribute.attname) as column_n
      FROM pg_depend 
      JOIN pg_rewrite ON pg_depend.objid = pg_rewrite.oid 
      JOIN pg_class as dependent_view ON pg_rewrite.ev_class = dependent_view.oid 
      JOIN pg_class as source_table ON pg_depend.refobjid = source_table.oid 
      JOIN pg_attribute ON pg_depend.refobjid = pg_attribute.attrelid 
       AND pg_depend.refobjsubid = pg_attribute.attnum 
      JOIN pg_namespace dependent_ns ON dependent_ns.oid = dependent_view.relnamespace
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
    for (view in undone.views){
      depends <- (dbGetQuery(con, glue(depend.sql)))$source_table
      if (all(depends %in% completed)){
        sql = paste0("REFRESH MATERIALIZED VIEW public.", view, ";")
        cat(paste0("Refreshing materialized view: public.", view, "...\n"))
        dbExecute(con, sql)
        completed = c(completed, view)
      }
    }
    refresh.views(done = completed, level = level + 1)
  }
}


#####################################
## SELECTING and reading functions ##
#####################################

#' Constructs a SELECT statment which can be used to retrieve source data from
#' A spatially enabled sqlite database by converting table geometry to WKT 
#' format and numeric DATETIME columns to TIMESTAMP compatible format. 
#' The function assumes that datetime values are stores numerically in the 
#' sqlite database because that is the field type sqlite assumes when a table 
#' CREATE statement uses a data type of TIMESTAMP, DATE, OR DATETIME.
#'
#' @param sqlite.con An \code{Rsqlite} connection object. 
#' @param table A string. The name of the table to build the select statement 
#'   for.
#' @param spatial logical. A flag which tells the function to convert geometry 
#'   columns to WKT format for import.  The geometry must be named "geom" in 
#'   the source data. 
#'
#' @return A list consisting of the constructed SELECT statement and the fields
#'   used to construct it.
#' @export
create.sqlite.select <- function(sqlite.con, table, spatial){
  info <- dbGetQuery(sqlite.con, paste0("PRAGMA table_info(", table, ");"))
  cols <-  info$name
  if (spatial == TRUE){
    cols.new <- cols %>% str_replace("^geom$", "st_astext(geom) geom")
  } else {
    cols.new = cols[cols != "geom"]
  }
  dt.cols <- !is.na(as.vector(info$type %>% 
                                str_match(regex("(?:timestamp|date)", 
                                                ignore_case = TRUE))))
  cols.dt <- ifelse(dt.cols, paste0("datetime(", cols.new, 
                                    ", 'unixepoch') || '-00'", cols.new), 
                    cols.new)
  col.string <-  paste(cols.dt, collapse = ", ")
  select <- paste0("SELECT ", col.string, "\n  FROM ", table, ";")
  return(list(sql = select, cols = cols.new))
}


#' Retrieves data tables from data source and returns them as a list of tibbles.
#'
#' @param path A string file path pointing to the source database. File name
#'   extensions must be one of (.mdb, .accdb, .sqlite, .db) and folder paths
#'   must have the extension of (.gdb).
#' @param md5hash A string. The md5hash of the database, produced via 
#'   \code{get.key()}
#' @param key A string. Serves as a unique key for the database source which
#'   will identify data in the destination DB as coming from a specific source.
#' @param desc A string. The description the will be used to describe the source
#'   database contents which are imported into the Postgres destination.
#'
#' @return A list of schema names, and within each a list of tibbles containing
#'   source data. 
#' @export

get.src.tables <- function(path, md5hash, key, desc = NULL){
  terradat = FALSE
  dima.tbls.sql <- paste0("SELECT table_name FROM information_schema.tables ",
                          "WHERE table_schema='dima' AND table_name NOT LIKE ",
                          "'tblMaint%';")
  dima.table.names <- (dbGetQuery(con, dima.tbls.sql))$table_name
  lmf.tbls.sql <- paste0("SELECT table_name FROM information_schema.tables ",
                         "WHERE table_schema='lmf';")
  lmf.table.names <- (dbGetQuery(con, lmf.tbls.sql))$table_name
  eco.tbls.sql <- paste0("SELECT table_name FROM information_schema.tables ",
                         "WHERE table_schema='eco';")
  eco.table.names <- (dbGetQuery(con, eco.tbls.sql))$table_name
  
  if (str_to_lower(file_ext(path)) %in% c("mdb", "accdb")) {
    con.type <- "access"
    access.head <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ="
    access.constring <- paste0(access.head, path)
    access.con <- odbc::dbConnect(odbc::odbc(), 
                                  .connection_string = access.constring,
                                  encoding = "latin1")
    
    avail.tables <- dbListTables(access.con)
  } else if (str_to_lower(file_ext(str_remove(path, "[/\\\\]+$"))) == "gdb") {
    con.type <- "gdb"
    avail.tables <- ogrListLayers(dsn = path)
    if ("terradat" %in% str_to_lower(avail.tables)){
      terradat = TRUE
    }
  } else if (str_to_lower(file_ext(path)) %in% c("db", "sqlite")){
    con.type <- "sqlite"
    sqlite.con <- DBI::dbConnect(RSQLite::SQLite(), path)
    avail.tables <- dbListTables(sqlite.con)
    res <- dbExecute(sqlite.con, "PRAGMA foreign_keys = on;")
    spatial <- tryCatch(
      expr = {
        res <- dbExecute(sqlite.con, "SELECT load_extension('mod_spatialite');")
        TRUE
      },
      error = function(e){
        # print("Spatialite module not found. Creating non-spatial db.")
        return(FALSE)
      }
    )
  }
  dima.tables.get <-  intersect(avail.tables, dima.table.names)
  lmf.tables.get <-  intersect(avail.tables, lmf.table.names)
  eco.tables.get <-  intersect(avail.tables, eco.table.names)
  
  # list append in R is as inefficient as R itself is ugly,
  # but like many other ugly things, we'll do them anyway
  tables = list()
  for (schema in c("dima", "lmf", "eco")){
    cat(paste0("\nimporting ", schema, " tables...\n\n"))
    for (t in get(paste0(schema, ".tables.get"))){
      cat(paste0("importing ", t, "...\n"))
      if (con.type == "access"){
        tbl <- as_tibble(tbl(access.con, t))
      } else if (con.type == "gdb"){
        tbl <- as_tibble(
          suppressWarnings(readOGR(dsn = path, layer = t, 
                                   dropNULLGeometries = FALSE, 
                                   stringsAsFactors = FALSE, 
                                   verbose = FALSE, encoding = "UTF-8",
                                   use_iconv = TRUE)))
      } else if (con.type == "sqlite"){
        query <- create.sqlite.select(sqlite.con = sqlite.con, table = t, 
                                      spatial = spatial)
        result <- DBI::dbSendQuery(sqlite.con, query$sql)
        tbl <- dbFetch(result)
        dbClearResult(result)
      }
      tbl.processed <- tbl %>% rename_at(vars(matches("dbkey")), str_to_lower)
      if ("dbkey" %in% colnames(tbl.processed)){
        tbl.processed <- tbl.processed %>%
          mutate(dbkey = ifelse(is.na(dbkey), key, dbkey))
      }
      tables[[schema]][[t]] <- tbl.processed
    }
  }
  if (con.type == "access"){
    dbDisconnect(access.con)
  } else if (con.type == "sqlite"){
    dbDisconnect(sqlite.con)
  }
  
  # dealing with data with existing dbkeys in their plot-level table
  schemas <- names(tables)
  for (schema in schemas){
    plot <-  tables[[schema]][c("tblPlots", "POINT", "point")]
    plot <- plot[lengths(plot) != 0][[1]]
    if ("dbkey" %in% str_to_lower(colnames(plot))){
      db <-  plot %>% select(matches("dbkey")) %>% 
        rename_all(str_to_lower) %>% group_by(dbkey) %>% 
        summarize(.groups = "drop") %>% filter(!is.na(dbkey)) %>%
        mutate(dbpath = path, md5hash = md5hash, description = desc)
      tables[[schema]][["db"]] <- db
    }
  }
  return(list(terradat = terradat, processed = FALSE, path = path, 
              md5hash = md5hash, key = key, desc = desc,
              tables = tables))
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
#'   \code{get.src.tables()}
#'
#' @return A processed list of lists of tibbles, which is compatible with the 
#'   DIMA schema, as well as other data such as database keys found in TerrAdat,
#'   and other values (source path, md5hash, key, description and table name 
#'   list)
#' @export
process.terradat <-  function(imported){
  tbls <- imported$tables
  cat(paste0("Converting TerrADat data to fit within DIMA schema...\n"))
  key.fields = c("SiteKey", "PlotKey", "LineKey", "RecKey", "SoilKey", 
                 "CommentID")
  process.schemas <- c("dima")
  for (schema in process.schemas){
    tbl.names <- names(tbls[[schema]])
    for (name in tbl.names){
      cat(paste0("Post-processing ", schema, ".", name))
      dest.info <- get.dest.info(schema = schema, table = name)
      char.fields <- dest.info$col.info %>% 
        filter(!is.na(character_maximum_length))
      src.col.names <- names(tbls[[schema]][[name]])
      # re-keys key.fields which are > 20 based on their hash
      cat("\n\t... re-keying keys > length 20 ")
      tbl <- tbls[[schema]][[name]] %>% 
        mutate(across(any_of(key.fields), 
                      ~ case_when(
                        str_count(.) > 20 ~ substr(vdigest(., algo = "md5", 
                                                           serialize = FALSE), 
                                                   1, 16), 
                        TRUE ~ .)))
      # process fields which have text entries too long (truncate)
      cat("\n\t... Trimming fields:")
      for (i in rownames(char.fields)){
        colname <-  char.fields[i, "column_name"]
        maxlen <-  char.fields[i, "character_maximum_length"]
        if (colname %in% src.col.names & !(colname %in% key.fields)){
          cat(paste0("\n\t\t... ", colname, " @ ", maxlen))
          tbl[[colname]] <- 
            str_sub(tbl[[colname]], 1, maxlen)
        }
      }
      cat("\n")
      tbls[[schema]][[name]] <- tbl
      
      # terradat data in tblSpecRichDetail has been split out and needs to be
      # recombined
      if (name == "tblSpecRichDetail"){
        cat("Flattening tblSpecRichDetail...\n")
        tbl <- tbls[[schema]][[name]] %>% 
          # arrange(dbkey, RecKey, subPlotID, SpeciesList) %>%
          group_by(dbkey, RecKey, subPlotID, subPlotDesc) %>%
          summarize(SpeciesCount = max(SpeciesCount),
                    SpeciesList = paste0(SpeciesList, collapse = ";"), 
                    .groups = "drop")
        tbls[[schema]][[name]] <- tbl
      }
      if (name == "tblPlots"){
        cat(paste0("Replacing missing SiteKeys in tblPlots with 'Unknown'.\n"))
        tbl <- tbls[[schema]][["tblPlots"]] %>% 
          mutate(SiteKey = ifelse(is.na(SiteKey), "Unknown", SiteKey))
        tbls[[schema]][["tblPlots"]] <- tbl
      }
      if (name == "tblPlotNotes"){
        cat(paste0("Populating missing CommentIDs in tblPlotNotes.\n"))
        tbl <- tbls[[schema]][["tblPlotNotes"]] %>% 
          mutate(CommentID = 
                   ifelse(is.na(CommentID), 
                          substr(vdigest(paste0(PlotKey, NoteDate, Recorder, 
                                                Notes), 
                                         algo = "md5", serialize = FALSE), 
                                 1, 16), CommentID)) %>%
          rename(Note = Notes)
        tbls[[schema]][["tblPlotNotes"]] <- tbl
      }
      if (name == "tblSites"){
        cat(paste0("Populating missing sites in tblSites with values",
                   " from tblPlots...\n"))
        sitekeys <- tbls[[schema]][["tblPlots"]] %>%
          left_join(tbls[[schema]][["tblSites"]], 
                    by = c("SiteKey" = "SiteKey"), keep = TRUE) %>%
          filter(is.na(SiteKey.y)) %>%
          select(SiteKey.x, dbkey.x) %>% 
          rename(SiteKey = SiteKey.x, dbkey = dbkey.x) %>%
          group_by(SiteKey, dbkey) %>% summarize(.groups = "drop") %>%
          mutate(SiteKey = ifelse(is.na(SiteKey), "Unknown", SiteKey),
                 Notes = paste0("Missing, populated",
                                " from tblPlots."))

        
        tbl <- bind_rows(tbls[[schema]][["tblSites"]], sitekeys)
        tbls[[schema]][["tblSites"]] <- tbl
      }
      if (name == "tblGapDetail"){
        cat(paste0("Populating missing SeqNos in tblGapDetail.\n"))
        tbl <- tbls[[schema]][["tblGapDetail"]] %>% 
          group_by(RecKey, RecType) %>%
          mutate(n = sum(as.double(GapEnd) - as.double(GapStart))) %>% 
          mutate(rn = case_when(n <= 0 ~ row_number(desc(GapStart)),
                                TRUE ~ row_number(GapStart))) %>% 
          ungroup() %>%
          mutate(SeqNo = case_when(is.na(SeqNo) ~ rn,
                                   TRUE ~ SeqNo)) %>%
          select(-n, -rn)
        tbls[[schema]][["tblGapDetail"]] <- tbl
      }
    }
  }
  return(list(terradat = imported$terradat, processed = TRUE, 
              path = imported$path, md5hash = imported$md5hash, 
              key = imported$key, desc = imported$desc,
              tables = tbls))
}


#' The main processing function.
#'
#' @param dbname A string. The database name to connect to in the postgres 
#'   instance.
#' @param user A string. The database user used to connect to the database.
#' @param password A string. The password used to connect to the database.
#' @param src.path A string file path pointing to the source database or saved 
#'   RDS file. File name extensions must be one of (.mdb, .accdb, .sqlite, .db, 
#'   .rds) and folder paths must have the extension of (.gdb).
#' @param key A string. Serves as a unique key for the database source which
#'   will identify data in the destination DB as coming from a specific source.
#' @param desc A string. The description the will be used to describe the source
#'   database contents which are imported into the Postgres destination.
#' @param host A string. The IP address or DNS name which hosts the database. 
#' @param port An integer. The port which the postgres service monitors for 
#'   connections.
#' @param update logical. A flag that instructs the function to construct an 
#'   UPSERT statement instead of an INSERT statement.
#' @param log logical. A flag that tells the function to create a log file of 
#'   the import, saved in the current working directory in the format of 
#'   import_YYYMMDDHHMMSS.log
#' @param save.raw A string file path to which to save data imported data from
#'   \code{src.path} via \code{get.src.tables()}. This file can be used in 
#'   as a future \code{src.path}. Primarily used for testing purposes.
#' @param save.processed A string file path to which to save data imported data 
#'   from \code{src.path} via \code{get.src.tables()} which has been processed 
#'   via \code{process.terradat()}. This file can be used in 
#'   as a future \code{src.path}. Primarily used for testing purposes.
#' @param verbose logical. A flag telling the function to be more verbose in 
#'   its messaging.
#' @param chunk.size An integer.This tells the function how many rows to 
#'   attempt to insert at once. Failure on a chunk will cause the function
#'   to default to row-wise inserts for the entire chunk. 
#' @param skip.refresh logical. A flag telling the function to skip refreshing
#'   materialized views. This can be helpful in speeding up the importing of
#'   multiple databases.
#'
#' @return Nothing.
#' @export
main <-  function(dbname, user, password, src.path, key, desc, 
                  host = "localhost", port = 5432, update = FALSE, log = FALSE, 
                  save.raw = NULL, save.processed = NULL, verbose = FALSE, 
                  chunk.size = 1000, skip.refresh = FALSE){
  con <<- dbConnect(RPostgres::Postgres(), dbname = dbname, 
                  host = host, port = port, 
                  user = user, password = password)
  res <- dbExecute(con, "SET client_min_messages TO WARNING;")
  if (log == TRUE){
    log.name <-  paste0("./import_", 
                      strftime(Sys.time(), format="%Y%m%d%H%M%S"), ".log")
  } else{
    log.name <-  NULL
  }
  # in case the tables have already been saved in RDS format
  if (str_to_lower(file_ext(src.path)) == "rds"){
    imported <- readRDS(src.path)
    key <- imported$key
    hash <- imported$md5hash
  # in case a fresh import is needed
  } else {
    if (is.null(key)){
      key <- str_sub(digest(src.path, algo = "md5"), start = -6)
    }
    hash <- get.key(src.path)
    imported <- get.src.tables(path = src.path, md5hash = hash, key = key, 
                             desc = desc)
    if (!is.null(save.raw)){
      cat(paste0("Saving raw tables as ", save.raw), 
          "\n")
      saveRDS(imported, save.raw)
    }
  }
  # process the data if it is in terradat format so we can place it into
  # the dima schema.
  if (imported$terradat == TRUE & imported$processed == FALSE){
      processed <- process.terradat(imported = imported)
      if (!is.null(save.processed)){
        cat(paste0("Saving processed tables as ", save.processed), 
            "\n")
        saveRDS(processed, save.processed)
      }
    } else {
      processed <- imported
  }
  to.db(tbls = processed$tables, update = update, dbkey = key, 
        desc = desc, path = src.path, hash = hash, log = log.name, 
        verbose = verbose, named = FALSE, 
        chunk.size = chunk.size)
  if (skip.refresh == FALSE){
    refresh.views()
  }
  dbDisconnect(con)
  rm(con, envir = .GlobalEnv)
  msg.out("\nScript finished.", log = log.name)
}

# run only if called from a script.
if (sys.nframe() == 0) {
  option_list <-  list (
    make_option(opt_str = c("-p", "--port"), default = 5432, type = "integer",
                help = paste0("The Postgres connection port")),
    make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    make_option(opt_str = c("-w", "--password"),
                help = paste0("The password for the user provided.")),
    make_option(opt_str = c("-u", "--update"), action = "store_true", 
                default = FALSE,
                help = paste0("A flag telling the script to update existing ",
                              "data from the source data instead of inserting ",
                              "only new records.")),
    make_option(opt_str = c("-k", "--key"), 
                help = paste0("A short/unique code for this database. Will be ",
                              "generated automatically if not provided.")),
    make_option(opt_str = c("-d", "--desc"), 
                help = paste0("Descriptive text for documenting the source ",
                              "data.")),
    make_option(opt_str = c("-l", "--log"), action = "store_true", 
                default = FALSE,
                help = paste0("Log the results of the import. Stored in the ", 
                              "executing directory as ",
                              "'import_YYYYMMDDHHMMSS.log'")),
    make_option(opt_str = c("-v", "--verbose"), action = "store_true", 
                default = FALSE,
                help = "Increase the level of script reporting"),
    make_option(opt_str = c("-s", "--save_raw"),
                help = paste0("Path at which to save a raw version of the ",
                              "imported tables in RDS format.")),
    make_option(opt_str = c("-S", "--save_processed"),
                help = paste0("Path at which to save a processed version of the ",
                              "imported tables in RDS format. Script will only",
                              " export if tables they have been processed.")),
    make_option(opt_str = c("-c", "--chunk_size"), default = 1000,
                help = paste0("The number of rows to insert into the db at ",
                              "once. If there is an insert error in a chunk, ",
                              "this number of records will be inserted row-",
                              "wise instead, so keeping this number smaller ", 
                              "for databases with many probable constraint and",
                              " value errors is a good idea.")),
    make_option(opt_str = c("-r", "--skip_refresh"), action = "store_true", 
                default = FALSE,
                help = paste0("Skip refreshing materialized views. This can ",
                              "save time when uploading multiple databases."))
    
  )
  
  description <- paste0(
    "\nThis script will import data from DIMA, LMF, or ",
    "Terradat data sources into a (already created) PostGIS ",
    "database.\n\n", "dbname: The name of the postgres ",
    "database into which to import.\nuser: The user name for the postgres ",
    "database.\nsource_data: The file path to the database file (.mdb, ",
    ".accdb, .sqlite, .db) or folder (.gdb).\n"
  )
  
  opt_parser = OptionParser(usage = paste0("usage: %prog [options] dbname user",
                                           " source_data"), 
                            option_list=option_list, prog = NULL, 
                            description = description
  )
  
  args = commandArgs(trailingOnly = TRUE)
  opt = parse_args(opt_parser, positional_arguments = 3, args = args)
  if (is.null(opt$options$password)){
    opt$options$password = getPass()
  }
  main(dbname = opt$args[1], user = opt$args[2], 
       password = opt$options$password, 
       src.path = opt$args[3], key = opt$options$key, desc = opt$options$desc,
       host = opt$options$host, port = opt$options$port,
       update = opt$options$update, log = opt$options$log, 
       save.raw = opt$options$save_raw, 
       save.processed = opt$options$save_processed, 
       verbose = opt$options$verbose, chunk.size = opt$options$chunk_size, 
       skip.refresh = opt$options$skip_refresh)
}
