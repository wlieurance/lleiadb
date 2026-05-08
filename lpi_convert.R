#!/usr/bin/env Rscript

# libraries = c("dplyr", "foreach", "getPass", "optparse",
#               "readr", "RPostgres", "sf", "stringr",
#               "tidyr", "tools", "nanoparquet")
#
# for (lib in libraries){
#   suppressMessages(library(lib, character.only = TRUE))
# }

#' load special binary operators
`%do%` <- foreach::`%do%`
`%dopar%` <- foreach::`%dopar%`
.data <- rlang::.data

# options
options(readr.show_progress = FALSE)

#' Takes a filter string and applies the filter to the raw input data, in the
#' case of multiple filters, uses dplyr::bind_rows() to combine.
#'
#' @param in_tbl A tibble. Produced by the lpi_calc module and imported.
#' @param indicator_tbl A tibble with the character fields: \code{field},
#'   \code{indicator}, and \code{hit_type}. The \code{field} column must contain
#'   one of \code{c("cover", "height", "dead")}. The indicator field must match
#'   indicators to filter \code{in_tbl} field \code{indicator}, and the hit_type
#'   field must be similarly populated to match \code{in_tbl} field
#'   \code{hit_type}, generally one of (any, all, top, lower, surface,
#'   herbaceous, woody).
#' @param sd Boolean. Use standard deviation field(s) in wide result.
#' @param n Boolean. Use line count (n) field(s) in wide result.
#'
#' @return A tibble which has been pivoted to a wide format via
#'   tidyr::pivot_longer() where each row has a unique plotkey and survey_year
#' @importFrom foreach %do%
filter_widen <- function(in_tbl, indicator_tbl, sd = TRUE, n = TRUE,
                         simplify = FALSE) {
  static_cols <- c("plotkey", "survey_year", "survey_date_min")
  vals <- unique(indicator_tbl$field)
  values_from <- character()
  values_fill <- list()

  filt_df_grp <- indicator_tbl |>
    dplyr::group_by(.data$indicator, .data$hit_type) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop")

  if ("cover" %in% vals) {
    values_from <- c(values_from, "cvr_pct_mean")
    values_fill[["cvr_pct_mean"]] <- 0
    if (sd == TRUE) {
      values_from <- c(values_from, "cvr_pct_sd")
      values_fill[["cvr_pct_sd"]] <- 0
    }
    if (n == TRUE) {
      values_from <- c(values_from, "cvr_n")
      values_fill[["cvr_n"]] <- 0
    }
  }

  if ("height" %in% vals) {
    values_from <-  c(values_from, "height_cm_mean")
    values_fill[["height_cm_mean"]] <- NA_real_
    if (sd == TRUE) {
      values_from <- c(values_from, "height_cm_sd")
      values_fill[["height_cm_sd"]] <- NA_real_
    }
    if (n == TRUE) {
      values_from <- c(values_from, "height_n")
      values_fill[["height_n"]] <- 0
    }
  }

  if ("dead" %in% vals) {
    values_from <- c(values_from, "dead_pct_mean")
    values_fill[["dead_pct_mean"]] <- NA_real_
    if (sd == TRUE) {
      values_from <- c(values_from, "dead_pct_sd")
      values_fill[["dead_pct_sd"]] <- NA_real_
    }
    if (n == TRUE) {
      values_from <- c(values_from, "dead_n")
      values_fill[["dead_n"]] <- 0
    }
  }

  filt_df <- in_tbl |>
    dplyr::inner_join(filt_df_grp,
                      by = c("indicator" = "indicator",
                             "hit_type" = "hit_type"))

  filt_df_distinct <- filt_df |> dplyr::distinct()

  if (length(unique(filt_df_distinct$hit_type)) == 1) {
    names_glue <- "{indicator}"
  } else {
    names_glue <- "{indicator}_{hit_type}"
  }
  if (length(values_from) > 1) {
    names_glue <- paste0(names_glue, "_{.value}")
  }

  filt_df_wide <- filt_df_distinct |>
    tidyr::pivot_wider(
      id_cols = c("plotkey", "survey_year", "survey_date_min"),
      names_from = c("indicator", "hit_type"),
      values_from = tidyselect::all_of(values_from),
      names_glue = names_glue,
      values_fill = values_fill,
      names_sort = TRUE
    )

  filt_df_sel <- filt_df_wide |>
    dplyr::select(
      tidyselect::all_of(static_cols), tidyselect::everything()
    )

  # # not working cause it creates duplicate cols
  # dynamic_cols <- sort(
  #   names(filt_df_sel)[which(!(names(filt_df_sel) %in% static_cols))]
  # )
  #
  # filt_df_sort <- filt_df_sel |>
  #   dplyr::select(tidyselect::all_of(c(static_cols, dynamic_cols)))

  # # repair our columns names so they aren't all wacky
  # repaired_cols <- dynamic_cols |>
  #   stringr::str_replace_all("[^A-Za-z_\\d]", "") |>
  #   stringr::str_replace("^([^A-Za-z])", "n\\1") |>
  #   stringr::str_to_lower() |>
  #   make.unique(sep = "_")
  # names(repaired_cols) <- dynamic_cols
  #
  # filt_repair <- filt_df_sort |>
  #   dplyr::rename(tidyselect::any_of(repaired_cols))

  filt_df_sel
}

#' Takes a tibble produced from filter_table(), joins site data and point data
#' and writes to a spatial data set based on the output file extension.
#'
#' @param con A DBI connection object to the database containing the spatial
#'   data assocaited with tbl_out
#' @param tbl_out A tibble produced by the filter_table() function.
#' @param out_path A string which denotes the file path to write the output to.
#'   The extension of out_path is used by sf::st_write() to guess the format
#'   of the output using available drivers in sf::st_drivers().
#' @param layer_name A string. The name of the layer to be used in the
#'   destination (\code{out_path}) when the feature is written.
#' @param srid An integer. The SRID/EPSG code that the output should have.
write_spatial <- function(con, tbl_out, out_path, layer_name, srid,
                          schema = NULL) {
  DBI::dbExecute(con, "SET client_min_messages TO WARNING;")
  site <- tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM public.site;"))
  point <- sf::st_read(
    con,
    query = "SELECT * FROM public.point WHERE geom IS NOT NULL"
  )
  # st_zm(point, drop = TRUE, what = "ZM")

  final_tbl <- dplyr::select(point, "sitekey", "plotkey", "plotid") |>
    dplyr::inner_join(
      dplyr::select(site, "sitekey", "siteid", "site_name", "source",
                    "source_type"),
      by = c("sitekey" = "sitekey")
    ) |>
    dplyr::select(tidyselect::starts_with("site"),
                  tidyselect::starts_with("plot"),
                  tidyselect::starts_with("source")) |>
    dplyr::inner_join(tbl_out, by = c("plotkey" = "plotkey"))

  if (srid != 4326) {
    out <- sf::st_transform(final_tbl, srid)
  } else {
    out <- final_tbl
  }

  if (!is.null(out_path)) {
    sf::st_write(obj = out, dsn = out_path, layer = layer_name)
  } else {
    if (is.null(schema)) {
      schema <- "public"
    }
    # max row size is 1600 but row size is too large for postgres with that.
    # keeping it down to ~ 900 for saftey (7200 bytes + other cols <= 8160)
    if (ncol(out) > 900) {
      cat("Too many columns. Filtering down to 900 most present.\n")
      out_sum <- tbl_out |>
        dplyr::select(
          !tidyselect::any_of(c("plotkey", "survey_year", "survey_date_min"))
        ) |>
        dplyr::summarize(dplyr::across(.cols = tidyselect::everything(),
                                       .fns = sum))

      head_cols <- colnames(out)[which(!(colnames(out) %in% colnames(out_sum)))]

      restrict_to <- out_sum |> tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = "name", values_to = "ind_sum"
      ) |>
        dplyr::arrange(dplyr::desc(.data$ind_sum)) |>
        dplyr::mutate(rn = dplyr::row_number()) |>
        dplyr::filter(.data$rn <= 900 - length(head_cols)) |>
        dplyr::arrange(.data$name) |>
        dplyr::pull("name")

      res_out <- out |>
        dplyr::select(
          !tidyselect::any_of(colnames(out_sum)),
          tidyselect::all_of(restrict_to)
        )

    } else {
      res_out <- out
    }
    sf::st_write(obj = res_out, dsn = con,
                 layer = DBI::Id(schema = schema, table = layer_name),
                 driver = "PostgreSQL", delete_layer = FALSE, append = FALSE,
                 quiet = TRUE)
  }
}

#' Takes an output from \code{calc_lpi}, coverts it to a gis friendly wide
#' format, and adds geomtery fields.
#'
#' @param con A DBI connection object to the database from which the
#'   in_path data was calculated
#' @param in_path A string. The file path to am RDS or CSV file produced from
#'   the lpi_calc module.
#' @param out_path A string which denotes the file path to write the output to.
#'   The extension of out.path is used by sf::st_write() to guess the format
#'   of the output using available drivers in sf::st_drivers().
#' @param layer_name A string. The name of the layer to be used in the
#'   destination (\code{out_path}) when the feature is written.
#' @param srid An integer. The SRID/EPSG code that the output should have.
#' @param sep A character which is used to delimit the input file
#'   (\code{in_path}) in the case that it is in the CSV format.
#' @param sd Boolean. Use standard deviation field(s) in wide result.
#' @param n Boolean. Use line count (n) field(s) in wide result.
#' @param schema character. The optional name of the schema to write to instead
#'   when out_path is NULL, using (\code{con})
#' @param only_valid_species Boolean. If TRUE will restrict indicators to just
#'   valid species codes. Useful for cutting down on very wide species-level
#'   indicators.
#' @param filter_df A tibble with the character fields: \code{field},
#'   \code{indicator}, and \code{hit_type}. The \code{field} column must contain
#'   one of \code{c("cover", "height", "dead")}. The indicator field must match
#'   indicators to filter \code{in_tbl} by or NA, which will result in all
#'   indicators of a certain \code{hit_type} being selected. The same logic
#'   applies to the \code{hit_type} field.
#' @return Nothing
#' @export
lpi_to_gis <- function(
  con, in_path = NULL, in_table = NULL, out_path = NULL, layer_name = "point",
  srid = 4326, sep = ",", sd = TRUE, n = TRUE, schema = NULL,
  only_valid_species = FALSE,
  filter_df =
    tibble::tibble(field = character(), indicator = character(),
                   hit_type = character())
) {
  valid_ext <- c("rds", "csv", "tsv", "parquet", "pqt")

  if (!is.null(in_path)) {
    if (!file.exists(in_path)) {
      stop(paste0("Input file ", in_path, " does not exist."))
    }
    if (!(tolower(tools::file_ext(in_path)) %in% valid_ext)) {
      stop(
        paste0(
          "Input file extension must be one of: ",
          paste(valid_ext, collapse = ", "), "."
        )
      )
    }
    cat(paste0("Reading in data from ", in_path, "\n"))
    if (tolower(tools::file_ext(in_path)) == "rds") {
      in_tbl <- readRDS(in_path)
    } else if (tolower(tools::file_ext(in_path)) %in% c("csv", "tsv")) {
      in_tbl <- tibble::as_tibble(
        readr::read_delim(in_path, delim = sep, show_col_types = FALSE)
      )
    } else if (tolower(tools::file_ext(in_path)) %in% c("parquet", "pqt")) {
      in_tbl <- tibble::as_tibble(
        nanoparquet::read_parquet(in_path)
      )
    }
  } else if (!is.null(in_table)) {
    in_sql <- glue::glue("SELECT * FROM {in_table};")
    in_tbl <- DBI::dbGetQuery(con, in_sql)
  } else {
    stop("Either `in_path` or `in_table` must be provided. Exiting...")
  }

  indicator_tbl <- create_indicator_tbl(
    con = con,
    filter_df = filter_df, in_tbl = in_tbl,
    only_valid_species = only_valid_species
  )

  in_wide <- filter_widen(in_tbl = in_tbl, indicator_tbl = indicator_tbl,
                          sd = sd, n = n)

  write_spatial(con = con, tbl_out = in_wide, out_path = out_path,
                layer_name = layer_name, srid = srid, schema = schema)
}

#' Creates a filter_df tibble to be used in \code{lpi_to_gis} from strings.
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
#' @examples
#' create_filter_df(
#'   height_hit = "any, all",
#'   cover_filter = "indicator1, any; indicator2, top; indicator 3"
#' )
create_filter_df <- function(cover_hit = NULL, height_hit = NULL,
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

  filter_df <- tibble::tibble(field = character(), indicator = character(),
                              hit_type = character(), rank = integer())
  # specific filters
  filter_str <- c(cover = cover_filter,
                  height = height_filter,
                  dead = dead_filter)
  split_1 <- stringr::str_split(filter_str, "\\s*;\\s*")
  filt_1 <- lapply(X = split_1, FUN = function(x) x[which(trimws(x) != "")])
  split_2 <- lapply(
    X = filt_1,
    FUN = function(x) stringr::str_split(x, "\\s*,\\s*")
  )
  names(split_2) <- names(filter_str)
  for (cat in names(split_2)) {
    for (row in split_2[[cat]]) {
      filter_df <- filter_df |>
        tibble::add_row(field = cat, indicator = row[1], hit_type = row[2],
                        rank = 1)
    }
  }

  # hit filters
  hit_str <- c(cover = cover_hit,
               height = height_hit,
               dead = dead_hit)
  hit_split <- sapply(
    X = hit_str,
    FUN = function(x) stringr::str_split(x, "\\s*,\\s*")
  )
  for (cat in names(hit_split)) {
    rank <- 1
    for (row in hit_split[[cat]]) {
      filter_df <- filter_df |>
        tibble::add_row(field = cat, indicator = NA, hit_type = row[1],
                        rank = rank)
      rank <- rank + 1
    }
  }

  filter_df
}


#' Creates a table of indicator and hit_type combos for each field (cover,
#' height, dead) from what is actually in the data compared to what has been
#' created with create_filter_df()
#'
#' @param filter_df A tibble produced by create_filter_df() or having the same
#'   definition.
#' @param in_tbl A tibble. Produced by the lpi_calc module and imported.
#' @return A tibble with a unique combination of indicators and hit_types which
#'   can be used to filter the main data table.
#' @export
create_indicator_tbl <- function(con, filter_df, in_tbl,
                                 only_valid_species = FALSE) {
  indicators_raw <- in_tbl |>
    dplyr::group_by(.data$indicator, .data$hit_type) |>
    dplyr::summarize(.groups = "drop")

  if (only_valid_species) {
    valid <- tibble::as_tibble(DBI::dbGetQuery(
      con,
      statement = "SELECT accepted_symbol FROM public.plant;"
    ))
    indicators <- indicators_raw |>
      dplyr::inner_join(valid, by = c("indicator" = "accepted_symbol"))
  } else {
    indicators <- indicators_raw
  }

  fields <- filter_df |>
    dplyr::group_by(.data$field) |>
    dplyr::summarize(.groups = "drop") |>
    dplyr::pull("field")

  ind_tbl <- foreach::foreach(f = fields, .combine = dplyr::bind_rows) %do% {
    filter_grp <- filter_df |>
      dplyr::filter(.data$field == f) |>
      dplyr::group_by(.data$indicator, .data$hit_type, .data$rank) |>
      dplyr::summarize(.groups = "drop")

    specific_ind <- filter_grp |>
      dplyr::filter((!is.na(.data$indicator) & !is.na(.data$hit_type)))

    general_ind <- filter_grp |>
      dplyr::filter(is.na(.data$hit_type)) |>
      dplyr::select("indicator", "rank")

    general_ht <- filter_grp |>
      dplyr::filter(is.na(.data$indicator)) |>
      dplyr::select("hit_type", "rank")

    ind_combo <- indicators |>
      dplyr::inner_join(general_ind, by = c("indicator" = "indicator"))

    hit_combo <- indicators |>
      dplyr::inner_join(general_ht, by = c("hit_type" = "hit_type"))

    combo <- dplyr::bind_rows(ind_combo, hit_combo, specific_ind)

    combo_min_rank <- combo |>
      dplyr::group_by(.data$indicator) |>
      dplyr::summarize(min_rank = min(.data$rank), .groups = "drop")

    combo_filt <- combo |>
      dplyr::inner_join(combo_min_rank,
                        by = c("indicator" = "indicator",
                               "rank" = "min_rank")) |>
      dplyr::mutate(field = f)

    combo_filt
  }

  ind_unique <- ind_tbl |>
    dplyr::select(-"rank") |>
    dplyr::distinct()

  ind_unique
}


# Run if module is called from Rscript.
if (sys.nframe() == 0) {
  source(file.path("common.R"))
  args <- commandArgs(trailingOnly = TRUE)

  option_list <- list(
    optparse::make_option(
      opt_str = c("-u", "--user"),
      help = paste0("The user with which to connect to the PostgreSQL database")
    ),
    optparse::make_option(
      opt_str = c("-p", "--port"), default = 5432, type = "integer",
      help = paste0("The Postgres connection port")
    ),
    optparse::make_option(
      opt_str = c("-H", "--host"), default = "localhost",
      help = paste0("The host name or ip address of the connection")
    ),
    optparse::make_option(opt_str = c("-w", "--password")),
    optparse::make_option(
      opt_str = c("-i", "--in_file"),
      help = paste("The path of the input file to read created by the lpi_calc",
                   "module.")
    ),
    optparse::make_option(
      opt_str = c("-I", "--in_table"),
      help = paste("The database table name to read created by the lpi_calc",
                   "module.")
    ),
    optparse::make_option(
      opt_str = c("-o", "--out_file"),
      help = paste("The path of the output data source name (DSN) which",
                   "st_write() can use to write the output feature. If this",
                   "is omitted, the PostgreSQL connection established via",
                   "`dbname` is used as the DSN.")
    ),
    optparse::make_option(
      opt_str = c("-x", "--schema_out"),
      help = paste("The name of the schema in `dbname` with which to write",
                   "the output features if `out_file` is not provided.")
    ),
    optparse::make_option(
      opt_str = c("-n", "--name"), default = "point",
      help = "The name of the layer/table in the output file/database."
    ),
    optparse::make_option(
      opt_str = c("-S", "--srid"),
      help = "The epsg/srid of the exported feature.",
      default = 4326
    ),
    optparse::make_option(
      opt_str = c("-s", "--sep"), default = ",",
      help = paste0("Separator used in the input (if delimited). In the ",
                    r"{case of escaped characters (e.g. \t) you must}",
                    " pass the literal character recongnized your ",
                    r"{shell (e.g. $'\t' for bash).}")
    ),
    optparse::make_option(
      opt_str = c("--cover_hit"),
      help = paste0("string which restricts output fields to a ",
                    "specific hit type for all indicators (1 per ",
                    "indicator) in order of preference. format: ",
                    "\"hit_type1, hit_type2, etc.\". where ",
                    "hit type is one of (any, all, top, lower, ",
                    "surface, herbaceous, woody). If the first ",
                    "hit_type is unavailable for an indicator, the ",
                    "second given is then used and so on. This ",
                    "option is mutually exculsive with the ",
                    "--cover_filter option.")
    ),
    optparse::make_option(
      opt_str = c("--height_hit"),
      help = paste0("string which restricts output fields to a",
                    "specific hit_type for all height indicators ",
                    "(mutually exclusive with --height_filter, see ",
                    "--cover_hit).")
    ),
    optparse::make_option(
      opt_str = c("--dead_hit"),
      help = paste0("string which restricts output fields to a",
                    "specific hit_type for all dead indicators ",
                    "(mutually exclusive with --dead_filter, see ",
                    "--cover_hit).")
    ),
    optparse::make_option(
      opt_str = c("--cover_filter"),
      help = paste0("string which restricts output fields to ",
                    "specific cover indicators using the following ",
                    "format: \"indicator_name1, hit_type1; ",
                    "indicator_name2, hit_type2; etc.\", where ",
                    "hit type is either blank or one of (any, all, ",
                    "top, lower, surface, herbaceous, woody)")
    ),
    optparse::make_option(
      opt_str = c("--height_filter"),
      help = paste0("string which restricts output fields to ",
                    "specific height indicators.")
    ),
    optparse::make_option(
      opt_str = c("--dead_filter"),
      help = paste0("string which restricts output fields to ",
                    "specific dead indicators.")
    ),
    optparse::make_option(
      opt_str = c("--sd"), action = "store_true",
      default = FALSE,
      help = paste0("Include standard deviation ",
                    "calculations for each field.")
    ),
    optparse::make_option(
      opt_str = c("-N", "--line_n"), action = "store_true",
      default = FALSE,
      help = paste0("Include number of lines used to ",
                    "calculate plot mean for each field.")
    ),
    optparse::make_option(
      opt_str = c("-V", "--valid_species"), action = "store_true",
      default = FALSE,
      help = paste("Restrict indicators only to USDA PLANTS accepted symbols.",
                   "Only use this option where `indicator` in the source",
                   "dataset are species codes.")
    )

  )

  opt_parser <- optparse::OptionParser(
    usage = "usage: %prog [options] dbname",
    option_list = option_list,
    prog = NULL,
    description = paste(
      "\nThis script will read in a csv/rds/parquet file (--in_file) or a",
      "PostgreSQL table (--in_table) produced via the",
      "lpi_calc module, restrict the indicators according to option",
      "input, convert the result to wide format, and then output to",
      "a spatial output based on the DSN given in `out_file`."
    )
  )

  opt <- optparse::parse_args(opt_parser, positional_arguments = 1, args = args)

  if (length(c(opt$options$in_file, opt$options$in_table)) != 1) {
    stop(paste(
      "Either `in_file` or `in_table` must be provided. Exiting..."
    ))
  }


  con <- connect_pg(
    dbname = opt$args[1],
    host = opt$options$host,
    port = opt$options$port,
    user = opt$options$user,
    password = opt$options$password
  )

  # convert our string command line inputs to lists
  f_df <- create_filter_df(
    cover_hit = opt$options$cover_hit,
    height_hit = opt$options$height_hit,
    dead_hit = opt$options$dead_hit,
    cover_filter = opt$options$cover_filter,
    height_filter = opt$options$height_filter,
    dead_filter = opt$options$dead_filter
  )

  lpi_to_gis(
    con = con,
    in_table = opt$options$in_table,
    layer_name = opt$options$name,
    srid = opt$options$srid,
    filter_df = f_df,
    in_path = opt$options$in_file,
    in_table = opt$options$in_table,
    out_path = opt$options$out_file,
    sep = opt$options$sep,
    sd = opt$options$sd,
    n = opt$options$line_n,
    schema = opt$options$schema_out,
    only_valid_species = opt$options$valid_species
  )

  DBI::dbDisconnect(con)
  cat("Script finished.\n")
}
