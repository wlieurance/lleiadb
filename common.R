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
                       password = NULL) {

  if (is.null(user)) {
    user <- Sys.info()[["user"]]
  }

  try_connect <- function(password = NULL) {
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

    do.call(DBI::dbConnect, args)
  }

  tryCatch(
    try_connect(password),
    error = function(e) {
      msg <- conditionMessage(e)

      needs_password <- grepl(
        "no password supplied|password authentication failed",
        msg,
        ignore.case = TRUE
      )

      if (!needs_password) {
        stop(e)
      }

      password <- getPass::getPass(
        paste0("Password for PostgreSQL user '", user, "': ")
      )

      try_connect(password)
    }
  )
}
