#' An SQL execution function which uses a try-catch statement to skip/print
#' errors if found in the comment SQL.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param sql character vector. The SQL to execute.
execute_comment <- function(con, sql) {
  tryCatch(
    expr = {
      DBI::dbExecute(con, sql)
    },
    error = function(e) {
      print(e)
      cat(paste0("FAILED: ", sql, " skipping...\n\n"))
    }
  )
}

#' Creates comment SQL.
#'
#' @param tbl_type character vector. The type of comment to be created. One of
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
create_comment_sql <- function(tbl_type, schema, name, def, col = NULL) {
  type <- stringr::str_to_upper(tbl_type)
  name_frmt <- paste0('"', name, '"')
  if (!is.null(col)) {
    col_frmt <- paste0('"', col, '"')
  } else {
    col_frmt <- col
  }
  def_frmt <- def |>
    stringr::str_replace_all("&#60;|&lt;", "<") |>
    stringr::str_replace_all("&#62;|&gt;", ">") |>
    stringr::str_replace_all("&#38;|&amp;", "&") |>
    stringr::str_replace_all("&#39;|&apos;|'", "''") |>
    stringr::str_replace_all("&#34;|&quot;", '"') |>
    stringr::str_replace_all("[\u00A0\u202F]", " ") |>  # non-breaking spaces
    trimws()
  fullname <- paste(c(schema, name_frmt, col_frmt), collapse = ".")
  sql <- glue::glue("COMMENT ON {type} {fullname} IS '{def_frmt}';")

  sql
}

#' Main processing function for inserting iso19110-2016 catalog metadata in XML
#' format into the database as comments.
#'
#' @param con A DBI connection. A database connection object to the
#'    PostgreSQL database
#' @param xml_path A character vector. Path to the xml metadata for the
#'   database.
comment_from_iso19110 <- function(con, xml_path, out_path = NULL) {
  cat("Writing table/view and field COMMENTs to database from metadata...\n")
  data <- XML::xmlParse(xml_path)
  xml_data <- XML::xmlToList(data)

  # extract xml FC_FeatureType and FC_FeatureAttribute to list
  # and check for xlink mismatches in xml.
  comment_list <- list()
  i <- 0  # a counter to keep track of iterations in the outer loop
  for (tag in xml_data){
    i <- i + 1
    if (class(tag) == "list") {
      if (!is.null(tag$FC_FeatureType)) {
        name <- tag$FC_FeatureType$typeName
        def <- tag$FC_FeatureType$definition$CharacterString
        abstract <- tag$FC_FeatureType$isAbstract$Boolean
        if (abstract == "false") {
          type <- "table"
        } else {
          type <- "view"
        }
        id <- tag$FC_FeatureType$.attrs[["id"]]
        if (id != name) {
          print(paste0("mismatch: id: ", id, " name: ", name))
        }
        j <- 0  # a counter to keep track of iterations in the inner loop
        field_list <- list()
        for (tag2 in tag$FC_FeatureType){
          j <- j + 1
          if (!is.null(tag2) && class(tag2) == "list") {
            if (!is.null(tag2$FC_FeatureAttribute)) {
              if (!is.null(tag2$FC_FeatureAttribute$featureType)) {
                link <- tag2$FC_FeatureAttribute$featureType[["href"]]
                memname <- tag2$FC_FeatureAttribute$memberName
                memdef <- tag2$FC_FeatureAttribute$definition$CharacterString
              }
              if (paste0("#", id) != link) {
                print(paste0("in table ", id, ": xlink mismatch: ", link,
                             " for memberName: ", memname))
              }
              field_sub <- list(type = "column", name = memname, def = memdef)
              field_list[[length(field_list) + 1]] <- field_sub
            }
          }
        }
        comment_sub <- list(type = type, name = name, def = def,
                            fields = field_list)
        comment_list[[length(comment_list) + 1]] <- comment_sub
      }
    }
  }

  # construct comment sql for each table/view and column within.
  sql_v <- character()
  for (comment in comment_list) {
    element <- stringr::str_split(comment$name, "\\.")[[1]]
    schema <- element[1]
    tbl <- element[2]
    sql <- create_comment_sql(tbl_type = comment$type, schema = schema,
                              name = tbl, def = comment$def)
    # print(sql)
    sql_v <- c(sql_v, sql)
    for (field in comment$fields) {
      sql_f <- create_comment_sql(tbl_type = field$type, schema = schema,
                                  name = tbl, col = field$name, def = field$def)
      # print(sql.f)
      sql_v <- c(sql_v, sql_f)
    }
  }

  if (is.null(out_path)) {
    # execute the comment sql
    for (sql in sql_v){
      execute_comment(con, sql)
    }
  } else {
    for (sql in sql_v){
      readr::write_lines(x = sql, file = out_path, append = TRUE)
    }
  }
}


#' Main processing function for inserting Dublin Core+ metadata in XML
#' format into the database as comments.
#'
#' @param xml_path A character vector. Path to the xml metadata for the
#'   database.
#' @param schema A character vector. The database schema on which to comment.
#' @param use_title Boolean. If TRUE, will set the table name to title
#'   within the xml file, othersie will use the file name.
#' @param out_path A character vector. The path to the output file to which to
#'    write the comments.
comment_from_dublin <- function(xml_path, schema, use_title = FALSE,
                                out_path = NULL) {
  cat("Writing table/view and field COMMENTs to database from metadata...\n")
  data <- XML::xmlParse(xml_path)
  xml_data <- XML::xmlToList(data)
  head <- xml_data[["Description"]]
  if (use_title) {
    table <- head[["title"]]
  } else {
    table <- basename(xml_path) |>
      stringr::str_split_i("\\.", i = 1)
  }
  table_desc <- head[["description"]]
  item_type <- head[["type"]]
  if (stringr::str_detect(item_type, stringr::regex("view",
                                                    ignore_case = TRUE))) {
    comment_type <- "VIEW"
  } else {
    comment_type <- "TABLE"
  }

  attributes <- head[["attributes"]][["dictionary"]]
  keep <- which(names(attributes) == "attribute")
  attr_keep <- attributes[keep]
  field_list <- lapply(X = attr_keep, FUN = function(a) {
    fname <- a[["title"]]
    fdesc <- a[["description"]] |>
      stringr::str_replace_all("&#60;|&lt;", "<") |>
      stringr::str_replace_all("&#62;|&gt;", ">") |>
      stringr::str_replace_all("&#38;|&amp;", "&") |>
      stringr::str_replace_all("&#39;|&apos;|'", "''") |>
      stringr::str_replace_all("&#34;|&quot;", '"') |>
      stringr::str_replace_all("[\u00A0\u202F]", " ") |>
      trimws()
    ftype <- a[["type"]]
    list(name = fname, desc = fdesc, type = ftype)
  })

  base_str <- paste(
    "COMMENT ON COLUMN \"{schema}\".\"{table}\".\"{column}\"",
    "IS '{desc}';"
  )
  field_comments <- lapply(X = field_list, FUN = function(a) {
    column <- a$name
    desc <- a$desc
    sql <- glue::glue(base_str)
    sql
  }) |>
    unname() |>
    unlist()

  table_comment <- glue::glue(
    "COMMENT ON {comment_type} \"{schema}\".\"{table}\" IS '{table_desc}';"
  )

  comments <- c(table_comment, field_comments)

  if (!is.null(out_path)) {
    head_comment <- glue::glue("-- {schema}.{table}")
    readr::write_lines(x = head_comment, file = out_path, append = TRUE)
    for (sql in comments) {
      readr::write_lines(x = sql, file = out_path, append = TRUE)
    }
    readr::write_lines(x = "", file = out_path, append = TRUE)
  }
}
