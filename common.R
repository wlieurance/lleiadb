#' Connects to a PostgreSQL database, if password is invalid, gets it from
#' the user, and if user is not supplied, grabs it from the system.
#'
#' @param dbname A string. The database name to connect to in the postgres
#'   instance.
#' @param host A string. The IP address or DNS name which hosts the database.
#' @param port An integer. The port which the postgres service monitors for
#'   connections.
#' @param user A string. The database user used to connect to the database.
#' @param password A string. The password used to connect to the database.
#' @return A DBI connection object
#' @export
connect_pg <- function(dbname, host = "localhost", port = 5432, user = NULL,
                       password = NULL, maintenance_db = NULL,
                       allow_create = FALSE, recurse = 0) {

  if (recurse > 4) {
    stop("Something went wrong... stopping function recursion...")
  }
  if (is.null(user)) {
    user <- Sys.info()[["user"]]
  }

  if (is.null(maintenance_db)) {
    maintenance_db <- user
  }

  args <- list(
    drv = RPostgres::Postgres(),
    dbname = dbname,
    host = host,
    port = port,
    user = user
  )

  if (!is.null(password)) {
    args$password <- password
  }

  tryCatch(
    do.call(DBI::dbConnect, args),
    error = function(e) {
      msg <- conditionMessage(e)

      # print(msg)

      needs_password <- grepl(
        "no password supplied|password authentication failed",
        msg,
        ignore.case = TRUE
      )

      needs_db <- grepl(
        paste0('database "', dbname, '" does not exist'),
        msg,
        fixed = TRUE
      )

      if (!needs_password && !needs_db) {
        stop(e)
      }

      if (needs_password) {
        password <- getPass::getPass(
          paste0("Password for PostgreSQL user '", user, "': ")
        )
        args$password <- password
      }

      if (needs_db) {
        if (allow_create) {
          message(paste0('Creating database "', dbname, '"...'))
          new_args <- args
          new_args$dbname <- maintenance_db
          con <- do.call(DBI::dbConnect, new_args)
          on.exit(DBI::dbDisconnect(con), add = TRUE)
          DBI::dbExecute(
            con,
            paste("CREATE DATABASE", DBI::dbQuoteIdentifier(con, dbname))
          )
        } else {
          stop(
            paste0('Database "', dbname, '" does not exist and creation ',
                   "disallowed. Stopping.."),
            call. = FALSE  # cleaner error msg w/o function call
          )
        }
      }

      connect_pg(dbname = dbname, host = host, port = port,
                 user = user, password = args$password,
                 maintenance_db = maintenance_db, allow_create = allow_create,
                 recurse = recurse + 1)
    }
  )
}
