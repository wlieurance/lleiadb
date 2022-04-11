source("R/create_db.R")
files <- list.files("sql", pattern=r"(\.sql)")
files.to.convert <- files[which(files != "dima_qaqc.sql")]
sql.list <- list()
for (f in files.to.convert){
  path = file.path("sql", f)
  sql.obj <- sql_parser$new(
    file = path, standard = 'PostgreSQL', verbose = TRUE,
    fast = FALSE)
  sql.list[[f]][["sql"]] <- sql.obj$sql
  sql.list[[f]][["formatted"]] <- sql.obj$formatted
}

