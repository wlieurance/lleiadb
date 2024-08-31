source('import.R')

establish.con <- function(dbname, user, password, host = "localhost",
                          port = 5432){
  con <<- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname,
                  host = host, port = port,
                  user = user, password = password)
  res <- DBI::dbExecute(con, "SET client_min_messages TO WARNING;")
}


# run only if called from a script.
if (sys.nframe() == 0) {
  option_list <-  list(
    optparse::make_option(opt_str = c("-p", "--port"), default = 5432,
                          type = "integer",
                help = paste0("The Postgres connection port ",
                              "[default: %default].")),
    optparse::make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection ",
                              "[default: %default].")),
    optparse::make_option(opt_str = c("-w", "--password"),
                help = paste0("The password for the user [the user will be ",
                              "prompted if no password is supplied]."))
  )

  description <- paste0(
    "\nThis script will refresh the materialized views in your LLEIA database."
  )

  opt_parser = optparse::OptionParser(
    usage = "usage: %prog [options] dbname user",
                            option_list=option_list, prog = NULL,
                            description = description
  )

  args = commandArgs(trailingOnly = TRUE)
  opt = optparse::parse_args(opt_parser, positional_arguments = 2, args = args)
  if (is.null(opt$options$password)){
    opt$options$password = getPass::getPass()
  }

establish.con(dbname = opt$args[1], user = opt$args[2],
              password = opt$options$password, host = opt$options$host,
              port = opt$options$port)

refresh.views()
print("Script finished.")
}

