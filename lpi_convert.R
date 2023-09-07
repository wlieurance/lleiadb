#!/usr/bin/env Rscript

# libraries = c("dplyr", "foreach", "getPass", "optparse",
#               "readr", "RPostgres", "sf", "stringr",
#               "tidyr", "tools")
#
# for (lib in libraries){
#   suppressMessages(library(lib, character.only = TRUE))
# }

#' load special binary operators
`%do%` <- foreach::`%do%`
`%dopar%` <- foreach::`%dopar%`

# options
options(readr.show_progress = FALSE)

#' Takes a filter string and applies the filter to the raw input data, in the
#' case of multiple filters, uses dplyr::bind_rows() to combine.
#'
#' @param in.tbl A tibble. Produced by the lpi_calc module and imported.
#' @param indicator.tbl A tibble with the character fields: \code{field},
#'   \code{indicator}, and \code{hit_type}. The \code{field} column must contain
#'   one of \code{c("cover", "height", "dead")}. The indicator field must match
#'   indicators to filter \code{in.tbl} field \code{indicator}, and the hit_type
#'   field must be similarly populated to match \code{in.tbl} field
#'   \code{hit_type}, generally one of (any, all, top, lower, surface,
#'   herbaceous, woody).
#' @param sd Boolean. Use standard deviation field(s) in wide result.
#' @param n Boolean. Use line count (n) field(s) in wide result.
#'
#' @return A tibble which has been pivoted to a wide format via
#'   tidyr::pivot_longer() where each row has a unique plotkey and survey_year
#' @importFrom foreach %do%
filter.widen <- function(in.tbl, indicator.tbl, sd = TRUE, n = TRUE, 
                         simplify = FALSE){
  static.cols <- c("plotkey", "survey_year", "survey_date_min")
  vals <- unique(indicator.tbl$field)
  values.from <- character()
  values.fill <- list()

  filt.df.grp <- indicator.tbl |> dplyr::group_by(indicator, hit_type) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop")

  filt.select <- indicator.tbl |>
    dplyr::mutate(m = paste0("^", paste(indicator, hit_type, 
                                        gsub("cover", "cvr", field), 
                                        sep = "_")))
  if ("cover" %in% vals){
    values.from <- c(values.from, "cvr_pct_mean")
    values.fill[["cvr_pct_mean"]] = 0
    if (sd == TRUE){
      values.from <- c(values.from, "cvr_pct_sd")
      values.fill[["cvr_pct_sd"]] = 0
      }
    if (n == TRUE){
      values.from <- c(values.from, "cvr_n")
      values.fill[["cvr_n"]] = 0
      }
  }
  if ("height" %in% vals){
    values.from <-  c(values.from, "height_cm_mean")
    values.fill[["height_cm_mean"]] = NA_real_
    if (sd == TRUE){
      values.from <- c(values.from, "height_cm_sd")
      values.fill[["height_cm_sd"]] = NA_real_
      }
    if (n == TRUE){
      values.from <- c(values.from, "height_n")
      values.fill[["height_n"]] = 0
      }
  }
  if ("dead" %in% vals){
    values.from = c(values.from, "dead_pct_mean")
    values.fill[["dead_pct_mean"]] = NA_real_
    if (sd == TRUE){
      values.from <- c(values.from, "dead_pct_sd")
      values.fill[["dead_pct_sd"]] = NA_real_
      }
    if (n == TRUE){
      values.from <- c(values.from, "dead_n")
      values.fill[["dead_n"]] = 0
      }
  }

  filt.df <- in.tbl |> 
    dplyr::inner_join(filt.df.grp, 
                      by = c("indicator" = "indicator", 
                             "hit_type" = "hit_type"))

  filt.df.distinct <- filt.df |> dplyr::distinct()

  filt.df.wide <- filt.df.distinct |>
    tidyr::pivot_wider(id_cols = c("plotkey", "survey_year", "survey_date_min"),
                names_from = c(indicator, hit_type),
                values_from = tidyselect::all_of(values.from),
                names_glue = "{indicator}_{hit_type}_{.value}",
                values_fill = values.fill,
                names_sort = TRUE)

  filt.df.sel <- filt.df.wide |>
    dplyr::select(tidyselect::matches(c(static.cols, filt.select$m),
                                         perl = TRUE))

  dynamic.cols <- sort(
    names(filt.df.sel)[which(!(names(filt.df.sel) %in% static.cols))]
    )
  filt.df.sort <- filt.df.sel |> dplyr::select(
    tidyselect::all_of(c(static.cols, dynamic.cols))
    )

 # repair our columns names so they aren't all wacky
  repaired.cols <- dynamic.cols |>
    stringr::str_replace_all("[^A-Za-z_\\d]", "") |>
    stringr::str_replace("^([^A-Za-z])", "n\\1") |>
    stringr::str_to_lower() |> make.unique(sep = "_")
  names(repaired.cols) <- dynamic.cols
  filt.repair <- filt.df.sort |>
    dplyr::rename_with(~repaired.cols)

  return(filt.repair)
}

#' Takes a tibble produced from filter.table(), joins site data and point data
#' and writes to a spatial data set based on the output file extension.
#'
#' @param tbl.out A tibble produced by the filter.table() function.
#' @param out.path A string which denotes the file path to write the output to.
#'   The extension of out.path is used by sf::st_write() to guess the format
#'   of the output using available drivers in sf::st_drivers().
#' @param dbname A string. The database name to connect to in the postgres
#'   instance.
#' @param user A string. The database user used to connect to the database.
#' @param password A string. The password used to connect to the database.
#' @param host A string. The IP address or DNS name which hosts the database.
#' @param port An integer. The port which the postgres service monitors for
#'   connections.
#' @param layer.name A string. The name of the layer to be used in the
#'   destination (\code{out.path}) when the feature is written.
#' @param srid An integer. The SRID/EPSG code that the output should have.
write.spatial <- function(tbl.out, out.path, dbname, user, password, host,
                          port, layer.name, srid){
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname,
                   host = host, port = port,
                   user = user, password = password)
  res <- DBI::dbExecute(con, "SET client_min_messages TO WARNING;")
  site <- tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM public.site;"))
  point <- sf::st_read(con,
                   query = "SELECT * FROM public.point WHERE geom IS NOT NULL")
    # st_zm(point, drop = TRUE, what = "ZM")

  final.tbl <- dplyr::select(point, sitekey, plotkey, plotid) |>
    dplyr::inner_join(
      dplyr::select(site, sitekey, siteid, site_name, source, source_type),
                    by = c("sitekey" = "sitekey")) |>
    dplyr::select(tidyselect::starts_with("site"),
                  tidyselect::starts_with("plot"),
                  tidyselect::starts_with("source")) |>
    dplyr::inner_join(tbl.out, by = c("plotkey" = "plotkey"))
  if (srid != 4326){
    out <- sf::st_transform(final.tbl, srid)
  } else {
    out <- final.tbl
  }
  sf::st_write(out, out.path, layer = layer.name)
}

#' Takes an output from \code{calc.lpi}, coverts it to a gis friendly wide
#' format, and adds geomtery fields.
#'
#' @param dbname A string. The database name to connect to in the postgres
#'   instance.
#' @param user A string. The database user used to connect to the database.
#' @param password A string. The password used to connect to the database.
#' @param in.path A string. The file path to am RDS or CSV file produced from
#'   the lpi_calc module.
#' @param out.path A string which denotes the file path to write the output to.
#'   The extension of out.path is used by sf::st_write() to guess the format
#'   of the output using available drivers in sf::st_drivers().
#' @param host A string. The IP address or DNS name which hosts the database.
#' @param port An integer. The port which the postgres service monitors for
#'   connections.
#' @param layer.name A string. The name of the layer to be used in the
#'   destination (\code{out.path}) when the feature is written.
#' @param srid An integer. The SRID/EPSG code that the output should have.
#' @param sep A character which is used to delimit the input file
#'   (\code{in.path}) in the case that it is in the CSV format.
#' @param filter.df A tibble with the character fields: \code{field},
#'   \code{indicator}, and \code{hit_type}. The \code{field} column must contain
#'   one of \code{c("cover", "height", "dead")}. The indicator field must match
#'   indicators to filter \code{in.tbl} by or NA, which will result in all
#'   indicators of a certain \code{hit_type} being selected. The same logic
#'   applies to the \code{hit_type} field.
#' @param sd Boolean. Use standard deviation field(s) in wide result.
#' @param n Boolean. Use line count (n) field(s) in wide result.
#' @returns Nothing.
#' @export
lpi.to.gis <- function(
    dbname, user, password, in.path, out.path, host = "localhost",
    port = 5432, layer.name = "point", srid = 4326, sep = ",",
    sd = TRUE, n = TRUE,
    filter.df = tibble::tibble(field = character(),
                               indicator = character(),
                               hit_type = character())){
  if (!file.exists(in.path)){
    stop(paste0("Input file ", in.path, " does not exist."))
  }
  if (!(tools::file_ext(in.path) %in% c("rds", "csv"))){
    stop(paste0("Input file extension must have .csv or.rds extension."))
  }
  cat(paste0("Reading in data from ", in.path, "\n"))
  if(tools::file_ext(in.path) == "rds"){
    in.tbl <- readRDS(in.path)
  } else {
    in.tbl <- tibble::as_tibble(readr::read_delim(in.path, delim = sep,
                                                  show_col_types = FALSE))
  }
  indicator.tbl <- create.indicator.tbl(filter.df = filter.df, in.tbl = in.tbl)
  in.wide <- filter.widen(in.tbl = in.tbl, indicator.tbl = indicator.tbl,
                          sd = sd, n = n)



  write.spatial(tbl.out = in.wide, out.path = out.path, dbname = dbname,
                user = user, password = password, host = host,
                port = port, layer.name = layer.name, srid = srid)

}

#' Creates a filter.df tibble to be used in \code{lpi.to.gis} from strings.
#' Primarily used for command line purposes.
#'
#' @param cover_hit character string. A comma delimited string of
#'   \code{hit_type} that gives preference order for \code{hit_type} when
#'   selecting records from a \code{calc.lpi} output which will be used to
#'   create a long -> wide sf data frame for export using cover fields
#'   (cvr_*). Only one \code{hit_type} will be used per indicator and all
#'   indicators will be selected
#' @param height_hit character string. A comma delimited string 
#'   (see cover_filter) for creating height fields (height_*) based on 
#'   \code{hit_type} preference order.
#' @param dead_hit character string. A comma delimited string 
#'   (see cover_filter) for creating height fields (dead_*) based on 
#'   \code{hit_type} preference order.
#' @param cover_filter character string. A semicolon delimited string of comma
#'   delimited pairs consisting of \code{indicator} and \code{hit_type} that
#'   should be selected from a \code{calc.lpi} output which will be used to
#'   create a long -> wide sf data frame for export using cover fields
#'   (cvr_*). Mutually exclusive with \code{cover_hit}.
#' @param height_filter character string. A semicolon delimited string of comma
#'   delimited pairs (see cover_filter) for creating height fields (height_*).
#'   Mutually exclusive with \code{height_hit}.
#' @param dead_filter character string. A semicolon delimited string of comma
#'   delimited pairs (see cover_filter) for creating dead fields (dead_*).
#'   Mutually exclusive with \code{dead_hit}.
#'
#' @return a tibble
#' @export
#'
#' @examples create.filter.df(height_hit = "any, all", cover_filter =
#'   "indicator1, any; indicator2, top; indicator 3")
create.filter.df <- function(cover_hit = NULL, height_hit = NULL,
                             dead_hit = NULL, cover_filter = NULL,
                             height_filter = NULL, dead_filter = NULL) {
  # sanity check
  if (!is.null(cover_hit) && !is.null(cover_filter)) {
    cat(paste("cover_hit and cover_filter are mutually exclusive, please give",
              "only one of these options.\n"))
    quit()
  }
  if (!is.null(height_hit) && !is.null(height_filter)) {
    cat(paste("height_hit and height_filter are mutually exclusive, please",
              "give only one of these options.\n"))
    quit()
  }
  if (!is.null(dead_hit) && !is.null(dead_filter)) {
    cat(paste("dead_hit and dead_filter are mutually exclusive, please give",
              "only one of these options.\n"))
    quit()
  }

  filter.df <- tibble::tibble(field = character(), indicator = character(),
                              hit_type = character(), rank = integer())
  # specific filters
  filter.str <- c(cover = cover_filter,
                  height = height_filter,
                  dead = dead_filter)
  split.1 <- stringr::str_split(filter.str, "\\s*;\\s*")
  filt.1 <- lapply(X = split.1, FUN = function (x) x[which(trimws(x) != "")])
  split.2 <- lapply(X = filt.1, FUN = function (x)
    stringr::str_split(x, "\\s*,\\s*"))
  names(split.2) <- names(filter.str)
  for (cat in names(split.2)){
    for (row in split.2[[cat]]){
      filter.df <- filter.df |>
        tibble::add_row(field = cat, indicator = row[1], hit_type = row[2],
                        rank = 1)
    }
  }

  # hit filters
  hit.str <- c(cover = cover_hit,
               height = height_hit,
               dead = dead_hit)
  hit.split <- sapply(X = hit.str, FUN = function (x)
    stringr::str_split(x, "\\s*,\\s*"))
  for (cat in names(hit.split)){
    rank <- 1
    for (row in hit.split[[cat]]){
      filter.df <- filter.df |>
        tibble::add_row(field = cat, indicator = NA, hit_type = row[1],
                        rank = rank)
      rank <- rank + 1 
    }
  }

  return(filter.df)
}


#' Creates a table of indicator and hit_type combos for each field (cover,
#' height, dead) from what is actually in the data compared to what has been
#' created with create.filter.df()
#'
#' @param filter.df A tibble produced by create.filter.df() or having the same
#'   definition.
#' @param in.tbl A tibble. Produced by the lpi_calc module and imported.
#' @return A tibble with a unique combination of indicators and hit_types which
#'   can be used to filter the main data table.
#' @export
create.indicator.tbl <- function(filter.df, in.tbl){
  indicators <- in.tbl |> dplyr::group_by(indicator, hit_type) |>
    dplyr::summarize(.groups = "drop")
  fields <- filter.df |> dplyr::group_by(field) |> 
    dplyr::summarize(.groups = "drop") |> dplyr::pull(field)

  ind.tbl <- foreach::foreach (f = fields, .combine = dplyr::bind_rows) %do% {
    filter.grp <- filter.df |> dplyr::filter(field == f) |>
      dplyr::group_by(indicator, hit_type, rank) |>
      dplyr::summarize(.groups = "drop")
    specific.ind <- filter.grp |> 
      dplyr::filter((!is.na(indicator) & !is.na(hit_type)))
    general.ind <- filter.grp |> dplyr::filter(is.na(hit_type)) |>
      dplyr::select(indicator, rank) 
    general.ht <- filter.grp |> dplyr::filter(is.na(indicator)) |>
      dplyr::select(hit_type, rank)
    ind.combo <- indicators |> 
      dplyr::inner_join(general.ind, by = c("indicator" = "indicator"))
    hit.combo <- indicators |> 
      dplyr::inner_join(general.ht, by = c("hit_type" = "hit_type"))
    combo <- dplyr::bind_rows(ind.combo, hit.combo, specific.ind)
    combo.min.rank <- combo |> dplyr::group_by(indicator) |> 
      dplyr::summarize(min_rank = min(rank), .groups = "drop")
    combo.filt <- combo |> 
      dplyr::inner_join(combo.min.rank, 
                        by = c("indicator" = "indicator", 
                               "rank" = "min_rank")) |>
      dplyr::mutate(field = f)

    combo.filt
  }
  ind.unique <- ind.tbl |> dplyr::select(-rank) |> dplyr::distinct()
  return(ind.unique)
}
# Run if module is called from Rscript.
if (sys.nframe() == 0) {
  args = commandArgs(trailingOnly = TRUE)
  option_list = list (
    optparse::make_option(opt_str = c("-p", "--port"), default = 5432,
                          type = "integer",
                help = paste0("The Postgres connection port")),
    optparse::make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    optparse::make_option(opt_str = c("-w", "--password")),
    optparse::make_option(opt_str = c("-n", "--name"), default = "point",
                help = "The name of the layer in the output geopackage."),
    optparse::make_option(opt_str = c("-S", "--srid"),
                help = "The epsg/srid of the exported feature.",
                default = 4326),
    optparse::make_option(
      opt_str = c("-s", "--sep"), default = ",",
      help = paste0("Separator used in the input (if delimited). In the ",
                    r"{case of escaped characters (e.g. \t) you must}",
                    " pass the literal character recongnized your ",
                    r"{shell (e.g. $'\t' for bash).}")),
    optparse::make_option(opt_str = c("--cover_hit"),
                help = paste0("string which restricts output fields to a ",
                              "specific hit type for all indicators (1 per ",
                              "indicator) in order of preference. format: ",
                              "\"hit_type1, hit_type2, etc.\". where ",
                              "hit type is one of (any, all, top, lower, ",
                              "surface, herbaceous, woody). If the first ",
                              "hit_type is unavailable for an indicator, the ",
                              "second given is then used and so on. This ",
                              "option is mutually exculsive with the ",
                              "--cover_filter option.")),
    optparse::make_option(opt_str = c("--height_hit"),
                help = paste0("string which restricts output fields to a",
                              "specific hit_type for all height indicators ",
                              "(mutually exclusive with --height_filter, see ",
                              "--cover_hit).")),
    optparse::make_option(opt_str = c("--dead_hit"),
                help = paste0("string which restricts output fields to a",
                              "specific hit_type for all dead indicators ",
                              "(mutually exclusive with --dead_filter, see ",
                              "--cover_hit).")),
    optparse::make_option(opt_str = c("--cover_filter"),
                help = paste0("string which restricts output fields to ",
                              "specific cover indicators using the following ",
                              "format: \"indicator_name1, hit_type1; ",
                              "indicator_name2, hit_type2; etc.\", where ",
                              "hit type is either blank or one of (any, all, ",
                              "top, lower, surface, herbaceous, woody)")),
    optparse::make_option(opt_str = c("--height_filter"),
                help = paste0("string which restricts output fields to ",
                              "specific height indicators.")),
    optparse::make_option(opt_str = c("--dead_filter"),
                help = paste0("string which restricts output fields to ",
                              "specific dead indicators.")),
    optparse::make_option(opt_str = c("--sd"), action = "store_true",
                          default = FALSE,
                          help = paste0("Include standard deviation ",
                                        "calculations for each field.")),
    optparse::make_option(opt_str = c("-N", "--line_n"), action = "store_true",
                          default = FALSE,
                          help = paste0("Include number of lines used to ",
                                        "calculate plot mean for each field."))
  )
  opt_parser = optparse::OptionParser(usage = paste0("usage: %prog [options] ",
                                           "dbname user in_file out_path"),
                            option_list=option_list, prog = NULL,
                            description =
    paste0("\nThis script will read in a csv/rds file produced from the ",
           "lpi_calc module, restrict the indicators according to option ",
           "input, convert the result to wide format, and then output to ",
           "a spatial output based on the file extension (dsn ",
           "needs to be something sf::st_write() can guess).")
  )
  opt = optparse::parse_args(opt_parser, positional_arguments = 4, args = args)

  # option checks
  if (is.null(opt$options$password)){
    opt$options$password = getPass::getPass()
  }
  # convert our string command line inputs to lists
  f.df <- create.filter.df(cover_hit = opt$options$cover_hit,
                           height_hit = opt$options$height_hit,
                           dead_hit = opt$options$dead_hit,
                           cover_filter = opt$options$cover_filter,
                           height_filter = opt$options$height_filter,
                           dead_filter = opt$options$dead_filter)

  lpi.to.gis(
    dbname = opt$args[1], host = opt$options$host, port = opt$options$port,
    user = opt$args[2], password = opt$options$password,
    layer.name = opt$options$name, srid = opt$options$srid,
    filter.df = f.df, in.path = opt$args[3],
    out.path = opt$args[4], sep = opt$options$sep,
    sd = opt$options$sd, n = opt$options$line_n)
  cat("Script finished.\n")
}



