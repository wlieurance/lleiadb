#' Database creation SQL converted from raw SQL into list format for use in
#' create.lleiadb function.
#'
#' @format A list with one element for each SQL file. Each sub-list has two
#'   character vectors:
#' \describe{
#'   \item{sql}{A vector of SQL statements, suitable for executing within the
#'   database}
#'   \item{formatted}{A vector of formatted SQL statements suitable for
#'   printing but not executing.}
#' }
"sql.list"
