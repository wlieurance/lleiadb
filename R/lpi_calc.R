#!/usr/bin/env Rscript

# libraries = c("dbplyr", "doParallel", "dplyr", "getPass", "glue",
#               "lubridate", "optparse", "parallel", "readr",
#               "RPostgres", "stringi", "stringr", "tictoc", "tidyr",
#               "tools")
#
# for (lib in libraries){
#   suppressMessages(library(lib, character.only = TRUE))
# }

#' load special binary operators
`%do%` <- foreach::`%do%`
`%dopar%` <- foreach::`%dopar%`

#' This function connects to a Postgres instance of the LLEIA database and reads
#' in relevant tables for Point intercept calculations.
#'
#' @param dbname A string. The database name for the postgres connection.
#' @param host A string. The IP address or DNS name for the postgres connection
#' @param port An integer. The port for the postres connection.
#' @param user A string. The user name for the postgres connection
#' @param password A string. The password for the postgres connection.
#'
#' @return A list of tibbles imported from the connection source.
import.data <- function(dbname, host, port, user, password){
  tictoc::tic.clearlog()
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname,
                   host = host, port = port,
                   user = user, password = password)
  res <- DBI::dbExecute(con, "SET client_min_messages TO WARNING;")
  tictoc::tic(msg = "Loading data from data sources", quiet = TRUE)

  cat("Importing table 'plant'...\n")
  plant <- tibble::as_tibble(dplyr::tbl(con, "plant")) |>
    dplyr::mutate(duration_first = gsub( ",.*$", "", duration),
           growth_habit_first = gsub( ",.*$", "", growth_habit))
  cat("Importing table 'site'...\n")
  site <- tibble::as_tibble(dplyr::tbl(con, "site"))
  cat("Importing table 'point'...\n")
  point <- tibble::as_tibble(dplyr::tbl(con, "point"))
  cat("Importing table 'transect'...\n")
  transect <- tibble::as_tibble(dplyr::tbl(con, "transect"))
  pinterceptmeta <- tibble::as_tibble(dplyr::tbl(con, "pintercept_meta"))
  cat("Importing table 'pintercept'...\n")
  pintercept <- tibble::as_tibble(dplyr::tbl(con, "pintercept"))
  tictoc::toc(log = TRUE, quiet = TRUE)

  tictoc::tic(msg = "Preprocessing imported data", quiet = TRUE)
  cat("Processing code types in table 'plant'...\n")
  genus.family.codes <- dplyr::union(
    dplyr::filter(plant, code_type == "Genus") |>
      dplyr::select(accepted_symbol, code_type),
    dplyr::mutate(plant,
                  test = stringr::str_to_upper(substring(family, 1, 6))) |>
      dplyr::select(test) |>
      dplyr::rename(accepted_symbol = test) |>
      dplyr::group_by(accepted_symbol) |>
      dplyr::summarize(.groups = "drop")  |>
      dplyr::filter(!is.na(accepted_symbol)) |>
      dplyr::mutate(code_type = 'Family'))

  # add species level info to our long table
  cat("Processing generic species codes in table 'pintercept'...\n")
  pintercept.long <- pintercept |>
    # join with plant table in order to evaluate indicator
    dplyr::left_join(plant, by = c("hit" = "accepted_symbol")) |>
    # Some genus/family codes have growth habits attached at the end of them
    # (e.g. LUPINAF) or are unknown species codes (e.g. AF01). The following
    # added fields help tease these out for use in indicator calculations.
    tidyr::extract(
      "hit",
      into = c("tail_id", "tail_gh"),
      regex = "(.+)(AF|PF|AG|PG|SH|TR)$",
      remove = FALSE
    ) |>
    tidyr::extract(
      "hit",
      into = c("head_gh", "head_id"),
      regex = paste0(
        "^(AF|PF|AG|PG|SH|TR|2FA|2FP|2GP|2SHRUB|2SUBS|2GA|",
        "2TREE|2VW|2BRY|2MOSS)(\\d+)$"
      ),
      remove = FALSE
    ) |>
    dplyr::left_join(
      dplyr::rename(genus.family.codes, tail_type = code_type),
      by = c("tail_id" = "accepted_symbol")
    ) |>
    dplyr::mutate(hit_clean = dplyr::case_when(!is.na(tail_id) &
                                                 !is.na(tail_type)
                                               ~ tail_id, TRUE ~ hit)) |>
    dplyr::mutate(
      growth_habit_alt = dplyr::case_when(
        !is.na(tail_type) ~ tail_gh,!is.na(head_gh) ~ head_gh,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::mutate(
      gh_alt = dplyr::case_when(
        growth_habit_alt %in% c("AF", "PF", "2FA", "2FP") |
          hit %in% c("AAFF", "PPFF", "FORB")
        ~ "Forb/herb",
        growth_habit_alt %in% c("AG", "PG", "2GP", "2GA") |
          hit %in% c("AAGG", "PPGG", "GRASS")
        ~ "Graminoid",
        growth_habit_alt %in% c("TR", "2TREE") |
          hit %in% c("PPTR", "TREE") ~ "Tree",
        growth_habit_alt %in% c("SH", "2SHRUB") |
          hit %in% c("PPSH", "SHRUB") ~ "Shrub",
        growth_habit_alt %in% c("2SUBS") |
          hit %in% c("SUBSHRUB") ~ "Subshrub",
        growth_habit_alt %in% c("2VW") ~ "Vine",
        growth_habit_alt %in% c("2BRY", "2MOSS") |
          hit %in% c("MOSS", "LICHEN") ~ "Nonvascular",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::mutate(
      duration_alt = dplyr::case_when(
        growth_habit_alt %in% c("AF", "AG", "2FA", "2GA") |
          hit %in% c("AAFF", "AAGG", "AASU")  ~ "Annual",
        growth_habit_alt %in% c(
          "PF",
          "PG",
          "2FP",
          "2GP",
          "SH",
          "TR",
          "2SHRUB",
          "2SUBS",
          "2TREE",
          "2VW"
        ) |
          hit %in% c("PPFF", "PPGG", "PPSH", "PPTR",
                     "PPSU")  ~ "Perennial",
        growth_habit_alt %in% c("2BRY", "2MOSS") ~
          NA_character_,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::mutate(growth_habit_first = replace(growth_habit_first,
                                        is.na(growth_habit_first),
                                        gh_alt[is.na(growth_habit_first)])) |>
    dplyr::mutate(duration_first = replace(duration_first,
                                    is.na(duration_first),
                                    duration_alt[is.na(duration_first)])) |>
    dplyr::select(-tidyselect::starts_with("tail"),
                  -tidyselect::starts_with("head"),
                  -tidyselect::contains("alt"))

  # arrange the data table for testing/viewing purposes
  # pintercept.long <- arrange(pintercept.long, reckey, mark, hit_order)

  cat("Transposing long format 'pintercept' to wide format...\n")
  pintercept.hit.wide <- pintercept |>
    dplyr::select(reckey, mark, hit_order, hit_sub, hit) |>
    tidyr::pivot_wider(names_from = c(hit_order, hit_sub),
                names_glue ="hit{hit_order}_{hit_sub}",
                values_from = hit, names_sort = TRUE) |>
    dplyr::rename_all(dplyr::recode,
                      "hit1_w" = "hit_woody",
                      "hit2_h" = "hit_herb",
                      "hit3_h" = "hit_herb_lwr") |>
    dplyr::select(reckey, mark,
                  tidyselect::any_of(c("hit1_t", "hit2_l", "hit3_l", "hit4_l",
                                       "hit5_l", "hit6_l", "hit7_l", "hit8_l",
                                       "hit9_s", "hit_woody", "hit_herb",
                                       "hit_herb_lwr")))

  pintercept.dead.wide <- pintercept |>
    dplyr::select(reckey, mark, hit_order, hit_sub, dead) |>
    tidyr::pivot_wider(names_from = c(hit_order, hit_sub),
                names_glue ="dead{hit_order}_{hit_sub}",
                values_from = dead, names_sort = TRUE) |>
    dplyr::rename_all(dplyr::recode,
                      "dead1_w" = "dead_woody",
                      "dead2_h" = "dead_herb",
                      "dead3_h" = "dead_herb_lwr") |>
    dplyr::select(reckey, mark,
                  tidyselect::any_of(c("dead1_t", "dead2_l", "dead3_l",
                                       "dead4_l", "dead5_l", "dead6_l",
                                       "dead7_l", "dead8_l", "dead9_s",
                                       "dead_woody", "dead_herb",
                                       "dead_herb_lwr")))

  pintercept.height.wide <- pintercept |>
    dplyr::select(reckey, mark, hit_order, hit_sub, height_cm) |>
    tidyr::pivot_wider(names_from = c(hit_order, hit_sub),
                names_glue ="heightcm{hit_order}_{hit_sub}",
                values_from = height_cm, names_sort = TRUE) |>
    dplyr::rename_all(dplyr::recode,
                      "heightcm1_w" = "height_woody",
                      "heightcm2_h" = "height_herb",
                      "heightcm3_h" = "height_herb_lwr") |>
    dplyr::select(reckey, mark,
                  tidyselect::any_of(
                    c("heightcm1_t", "heightcm2_l", "heightcm3_l",
                      "heightcm4_l", "heightcm5_l", "heightcm6_l",
                      "heightcm7_l", "heightcm8_l", "heightcm9_s",
                      "heightcm_woody", "heightcm_herb",
                      "heightcm_herb_lwr")))

  pintercept.wide <- pintercept.hit.wide |>
    dplyr::left_join(pintercept.dead.wide,
              by = c("reckey" = "reckey", "mark" = "mark")) |>
    dplyr::left_join(pintercept.height.wide,
              by = c("reckey" = "reckey", "mark" = "mark"))

  # get total number of complete points per reckey in order to evaluate cover
  cat("Calculating number of vaild points per LPI record...\n")
  point.count <- dplyr::filter(pintercept.wide, !is.na(hit1_t),
                               !is.na(hit9_s)) |>
    dplyr::group_by(reckey) |>
    dplyr::summarize(pt_count = dplyr::n(), .groups = "drop")

  tictoc::toc(log = TRUE, quiet = TRUE)
  DBI::dbDisconnect(con)
  return(list(pintercept.long = pintercept.long,
              pintercept.wide = pintercept.wide,
              point.count = point.count,
              site = site, point = point, transect = transect,
              pinterceptmeta = pinterceptmeta))
}


#' This function reads in a tab delimited file containing filter definitions for
#' multiple indicators and returns a tibble version of it.
#'
#' @param indicators A string file path which points to a tab delimited file
#'   (with field names) containing three fields: 1) filter.tbl - this is either
#'   "long" or "wide" and indicates which type of point intercept table is used
#'   in calculation of the indicator. 2) name - this is the name of the
#'   indicator. It is recommended for later processing that these name contain
#'   only alphanumeric characters or underscores. 3) filter.exp - a
#'   \code{dplyr::filter()} expression with the first argument = "hits" (e.g.
#'   dplyr::filter(hits, my_field = "some parameter"). Multiple filter
#'   expressions can by union-ed with the \code{union} operator, and the filter
#'   statement can use the (now-deprecated) \code{filter_if()},
#'   \code{filter_at}, and \code{filter_all} as well as the use the
#'   \code{across()} operator.
#'
#' @return A tibble containing the filter statements from \code{indicators}.
load.indicators <- function(indicators){
  if (!is.null(indicators)){
    indicator.list <- tibble::as_tibble(
      utils::read.delim(indicators, sep = "\t",
                        comment.char = "#",
                        quote = "", stringsAsFactors = FALSE,
                        blank.lines.skip = TRUE)) |>
      dplyr::mutate_if(is.character, .funs = function(x) stringr::str_trim(x))
  } else {
    indicator.list <-  NULL
  }
  return(indicator.list)
}

#' This function will take a point intercept data data (either wide or long)
#' and calculate cover, height, and % dead indicators based on the filter
#' strings evaluated as an expression on the raw table.
#'
#' @param name A string. The name of the indicator.
#' @param filter.exp A string. A \code{dpylr::filter()} expression
#'   (or union-ed set thereof) which is used to filter the table.
#' @param calc.type A string. The table type on which to perform the filter
#'   expression.
#' @param hits A tibble. The table to perform the filter on.
#' @param ptcount A tibble which contains two fields,"reckey" and "pt_count",
#'   the former a string field containing a unique set of record keys and the
#'   latter an integer field containing the number of complete points for that
#'   record.
#'
#' @return A tibble, with the \code{filter.exp} applied to \code{hits}
#' @examples calc.indicators("tree", 'dplyr::filter(hits, growth_habit_first ==
#'   "Tree", "long", pintercept.long')
calc.indicators <- function(name = NULL, filter.exp = NULL, calc.type, hits,
                            ptcount) {
  if(calc.type == "long") {
    # if there is no filter expression, function defaults to calculating
    # species level indicators
    if (is.null(filter.exp)){
      filter.exp = paste0(
        'dplyr::filter(hits, !(hit %in% c("N", "L", "HL", "WL", ',
        '"NL", "DS", "W", "VL", "S", "LC", "M", "D", "R", ',
        '"CY", "EL", "GR", "CB", "ST", "BY", "BR", "None", ',
        '"RF", "AL", "OM", "WA")) &  !is.na(hit) & hit != "")')
    }
    if (is.null(name)){
      name = as.symbol("hit")
    }

    # filter by user defined expression(s) to get marks with indicator present
    # for any cover class
    ind.count.any <- eval(parse(text = filter.exp)) |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::group_by(reckey, mark, hit_type, indicator) |>
      dplyr::summarize(.groups = "drop") |>
      dplyr::group_by(reckey, hit_type, indicator) |>
      dplyr::summarize(ind_count = dplyr::n(), .groups = "drop") |>
      # takes out growth habit hits, since we can't really use them to calc
      # cover but we want to keep them in for later join to height if necessary
      dplyr::mutate(ind_count = ifelse(hit_type == "g", 0, ind_count)) |>
      dplyr::group_by(reckey, indicator) |>
      dplyr::summarize(ind_count = as.integer(sum(ind_count)), .groups = "drop")

    ind.height.any <- eval(parse(text = filter.exp)) |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::filter(!is.na(height_cm)) |>
      dplyr::group_by(reckey, indicator) |>
      dplyr::summarize(height_n = dplyr::n(),
                height_cm_mean = mean(height_cm, na.rm = T),
                height_cm_sd = sd(height_cm, na.rm = T),
                .groups = "drop")

    ind.dead.any <- eval(parse(text = filter.exp)) |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::filter(!is.na(dead) & hit_type == "l") |>
      dplyr::group_by(reckey, mark, indicator) |>
      dplyr::summarize(dead = all(dead), .groups = "drop") |>
      dplyr::group_by(reckey, indicator) |>
      dplyr::summarize(dead_n = dplyr::n(),
                dead = sum(dead),
                .groups = "drop") |>
      dplyr::mutate(dead_pct = ifelse(dead_n == 0, NA_real_, dead/dead_n))

    # join indicator count to total point count per method instance (reckey) and
    # calculate percent cover
    ind.cover.any.pre <- dplyr::left_join(ptcount, ind.count.any,
                                   by = c("reckey"="reckey")) |>
      dplyr::mutate(n = dplyr::case_when(is.na(ind_count) ~ as.integer(0),
                           TRUE ~ ind_count)) |>
      dplyr::mutate(hit_type = "any",
             cvr_pct = as.double(n/pt_count))
    if (class(name) == "character"){
      ind.cover.any <- ind.cover.any.pre |>
        dplyr::mutate(indicator = dplyr::case_when(is.na(indicator) ~ name,
                                     TRUE ~ indicator))
    } else {
      ind.cover.any <- ind.cover.any.pre |>
        dplyr::filter(!is.na(ind_count) & !is.na(indicator))
    }
    ind.any <- ind.cover.any |>
      dplyr::left_join(ind.height.any, by = c("reckey" = "reckey",
                                       "indicator" = "indicator")) |>
      dplyr::left_join(dplyr::select(ind.dead.any, reckey, indicator, dead_pct),
                by = c("reckey" = "reckey", "indicator" = "indicator")) |>
      dplyr::mutate(height_n = ifelse(is.na(height_n), 0, height_n))

  # filter by user defined expression(s) to get marks with indicator present for
  # different cover classes (top, lower, surface/basal)
    ind.count.class <- eval(parse(text = filter.exp)) |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::group_by(reckey, mark, hit_type, hit_sub, indicator) |>
      dplyr::summarize(.groups = "drop") |>
      dplyr::group_by(reckey, hit_type, hit_sub, indicator) |>
      dplyr::summarize(ind_count = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(ind_count = ifelse(hit_type == "g", 0, ind_count)) |>
      dplyr::group_by(reckey, hit_sub, indicator) |>
      dplyr::summarize(ind_count = as.integer(sum(ind_count)), .groups = "drop")

    ind.height.class <- eval(parse(text = filter.exp)) |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::filter(!is.na(height_cm)) |>
      dplyr::group_by(reckey, indicator, hit_sub) |>
      dplyr::summarize(height_n = dplyr::n(), height_cm_mean = mean(height_cm),
                height_cm_sd = sd(height_cm), .groups = "drop")

    ind.dead.class <- eval(parse(text = filter.exp)) |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::filter(!is.na(dead) & hit_type == "l") |>
      dplyr::group_by(reckey, mark, indicator, hit_sub) |>
      dplyr::summarize(dead = all(dead), .groups = "drop") |>
      dplyr::group_by(reckey, indicator, hit_sub) |>
      dplyr::summarize(dead_n = dplyr::n(),
                dead = sum(dead),
                .groups = "drop") |>
      dplyr::mutate(dead_pct = ifelse(dead_n == 0, NA_real_, dead/dead_n))

    # join indicator count to total point count per method instance (reckey) and
    # calculate percent cover
    ind.cover.class.pre <- dplyr::left_join(ptcount, ind.count.class,
                                     by = c("reckey"="reckey")) |>
      dplyr::filter(!is.na(hit_sub)) |>
      dplyr::mutate(n = dplyr::case_when(is.na(ind_count) ~ as.integer(0),
                           TRUE ~ ind_count)) |>
      dplyr::mutate(cvr_pct = as.double(n/pt_count)) |>
      dplyr::mutate(hit_type = dplyr::case_when(hit_sub == "t" ~ "top",
                                  hit_sub == "l" ~ "lower",
                                  hit_sub == "s" ~ "surface",
                                  hit_sub == "w" ~ "woody",
                                  hit_sub == "h" ~ "herbaceous"))
    if (class(name) == "character"){
      ind.cover.class <- ind.cover.class.pre |>
        dplyr::mutate(indicator = dplyr::case_when(is.na(indicator) ~ name,
                                     TRUE ~ indicator))
    } else {
      ind.cover.class <- ind.cover.class.pre |>
        dplyr::filter(!is.na(ind_count) & !is.na(indicator))
    }

    ind.class <- ind.cover.class |>
      dplyr::left_join(ind.height.class, by = c("reckey" = "reckey",
                                       "indicator" = "indicator",
                                       "hit_sub" = "hit_sub")) |>
      dplyr::left_join(dplyr::select(ind.dead.class, reckey, indicator,
                                     hit_sub, dead_pct),
                by = c("reckey" = "reckey", "indicator" = "indicator",
                       "hit_sub" = "hit_sub")) |>
      dplyr::mutate(height_n = ifelse(is.na(height_n), 0, height_n)) |>
      dplyr::select(-hit_sub)

    ind.cover <-  dplyr::union(ind.any, ind.class) |>
      dplyr::select(-pt_count, -ind_count, -n)
  } else if(calc.type == "wide") {
    ind.count.any <- eval(parse(text = filter.exp)) |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::group_by(reckey, mark, indicator) |>
      dplyr::summarize(.groups = "drop") |>
      dplyr::group_by(reckey, indicator) |>
      dplyr::summarize(ind_count = dplyr::n(), .groups = "drop")

    ind.cover.any <- dplyr::left_join(ptcount, ind.count.any,
                                   by = c("reckey"="reckey")) |>
      dplyr::mutate(n = dplyr::case_when(is.na(ind_count) ~ as.integer(0),
                           TRUE ~ ind_count)) |>
      dplyr::mutate(hit_type = "all",
             cvr_pct = as.double(n/pt_count),
             indicator = dplyr::case_when(is.na(indicator) ~ name,
                                   TRUE ~ indicator),
             height_n = NA_integer_,
             height_cm_mean = NA_real_,
             height_cm_sd = NA_real_
             )

    ind.cover <- dplyr::select(ind.cover.any, reckey, indicator, hit_type,
                        cvr_pct, height_n, height_cm_mean, height_cm_sd)
  }
  return(ind.cover)
}


#' This function will filter out a table based on a string evaluated as an
#' expression.
#'
#' @param name A string. The name of the indicator.
#' @param filter.exp A string. A \code{dpylr::filter()} expression (or union-ed
#'   set thereof) which is used to filter the table
#' @param calc.type A string. The table type on which to perform the filter
#'   expression.
#' @param hits A tibble. The table to perform the filter on.
#'
#' @return A tibble, with the \code{filter.exp} applied to \code{hits}
#'
#' @examples test.indicators("tree", 'dplyr::filter(hits, growth_habit_first ==
#'   "Tree", "long", pintercept.long')
test.indicators <- function(name, filter.exp, calc.type, hits) {
  # filter by user defined expression(s) to get marks with indicator present for
  # any cover class
  ind.count.any <- eval(parse(text = filter.exp)) |>
    dplyr::mutate(indicator = name)
  return(ind.count.any)
}

#' This function takes calculated indicators at the survey level (reckey) and
#' adds zeros (cover) or NA's (height) for those surveys which do not have
#' values for specific indicators and then calculates the average for each
#' unique plot/survey year.
#'
#' @param imported  A list of tibbles produced from \code{import.data()}
#' @param indicators A tibble produced from \code{calc.indicators()}. These are
#'   the calculated indicators at the survey (reckey) level.
#' @param indicator.list A tibble produced from \code{load.indicators()}
#'
#' @return A tibble containing the calculated indicators averaged at the plot
#'   level.
calc.plot <-  function(imported, indicators, indicator.list = NULL){
  hit.types <- tibble::as_tibble(list(hit_type = c("any", "top", "lower",
                                                   "surface", "woody",
                                                   "herbaceous", "all"),
                              calc_type = c("long", "long", "long", "long",
                                            "long", "long", "wide")))

  plot.head <- dplyr::select(imported$transect, linekey, lineid, plotkey) |>
    dplyr::inner_join(dplyr::select(imported$pinterceptmeta, reckey,
                                    survey_date, linekey),
               by = c("linekey" = "linekey")) |>
    dplyr::mutate(survey_year = lubridate::year(survey_date))

  plot.indicators <-  plot.head |>
    dplyr::inner_join(indicators, by = c("reckey" = "reckey")) |>
    dplyr::rename(height_cm = height_cm_mean,
           dead = dead_pct)

  # species
  if (is.null(indicator.list)){
    plot.indicators.all <- plot.indicators |>
      dplyr::group_by(plotkey, survey_year, indicator) |>
      dplyr::summarize(survey_date_min = min(survey_date),
                .groups = "drop") |>
      dplyr::mutate(calc_type = "long") |>
      dplyr::left_join(hit.types, by=c("calc_type" = "calc_type")) |>
      dplyr::left_join(dplyr::select(plot.head, plotkey, survey_year, linekey),
                by = c("plotkey" = "plotkey", "survey_year" = "survey_year"))
  # indicators
  } else {
    all.lines <- plot.head |>
      dplyr::group_by(plotkey, survey_year, linekey) |>
      dplyr::summarize(survey_date_min = min(survey_date),
                .groups = "drop")
    all.indicators <- dplyr::select(indicator.list, filter.tbl, name) |>
      dplyr::left_join(hit.types, by = c("filter.tbl" = "calc_type")) |>
      dplyr::rename(calc_type = filter.tbl, indicator = name)
    plot.indicators.all <-  tidyr::crossing(all.lines, all.indicators)
  }
  plot.indicators.filled <- plot.indicators.all |>
    dplyr::left_join(dplyr::select(plot.indicators, -plotkey),
              by = c("linekey" = "linekey", "survey_year" = "survey_year",
                     "indicator" = "indicator", "hit_type" = "hit_type")) |>
    dplyr::mutate(cvr_pct = dplyr::coalesce(cvr_pct, 0))
  plot.indicators.sum <- plot.indicators.filled |>
    dplyr::group_by(plotkey, survey_year, survey_date_min, calc_type, indicator,
             hit_type) |>
    dplyr::summarize(cvr_n = dplyr::n(),
              cvr_pct_mean = mean(cvr_pct),
              cvr_pct_sd = sd(cvr_pct),
              height_n = as.integer(sum(!is.na(height_cm))),
              height_cm_mean = mean(height_cm, na.rm = TRUE),
              height_cm_sd = sd(height_cm, na.rm = TRUE),
              dead_n = as.integer(sum(!is.na(dead))),
              dead_pct_mean = mean(dead, na.rm = TRUE),
              dead_pct_sd = sd(dead, na.rm = TRUE),
              .groups = "drop")
  return(plot.indicators.sum)
}

#' This function takes an indicator list, and runs \code{calc.indicators()} on
#' every indicator in the list, then binds the rows together into a final table.
#'
#' @param imported A list of tibbles produced from \code{import.data()}
#' @param indicator.list A tibble produced from \code{load.indicators()}
#' @param num.cores The number of cpu cores to use to do the indicator
#'   calculation. Optimal number is \code{nrow(indicator.list}).
#' @param use.mc logical. A flag to tell the function whether or not to use
#'   multicore processing.
#'
#' @return A tibble containing rows for every indicator processed for each
#'   unique plotkey and survey year.
#' @importFrom foreach %do% %dopar%
do.indicator.calc <- function(imported, indicator.list = NULL, num.cores = 1,
                                use.mc = FALSE){
  tictoc::tic(msg = "Calculating indicators", quiet = TRUE)
  # for indicators
  if (!is.null(indicator.list)){
    # do indicator calculations. PSOCK type parallel is way too inefficient due
    # to the overhead of copying the base data to different sockets. FORKing
    # shows performance improvement but is unavailable in windows. Thus the
    # multicore functionality of the following code is useful only to POSIX type
    # systems.
    if (use.mc) {
      cat(paste("Calculating indicators on", num.cores, "cores...\n"))
      cl <- parallel::makeCluster(num.cores, type = "FORK")
      doParallel::registerDoParallel(cl)
    } else {
      cat(paste("Calculating indicators...\n"))
    }
    # by using suppressWarnings(), we can just write one %dopar% which will
    # default to a foreach::`%do%` if no parallel cluster has been registered
    indicators <-  suppressWarnings(
      foreach::foreach(i=1:nrow(indicator.list), .combine = dplyr::bind_rows
              #, .packages = c("dplyr", "tidyr", "stringr")
              #, .export = c("pintercept.long", "pintercept.wide")
    ) %dopar% {
      cat(paste0("\t", indicator.list$name[i], "...\n"))
      calc.indicators(name = indicator.list$name[i],
                     filter.exp = indicator.list$filter.exp[i],
                     calc.type = indicator.list$filter.tbl[i],
                     hits = eval(parse(
                       text = paste0("imported$pintercept.",
                                     indicator.list$filter.tbl[i]))),
                     ptcount = imported$point.count)
    })
    if (use.mc) {
      parallel::stopCluster(cl)
    }
  } else {
    cat(paste("Calculating species codes...\n"))
    # for species
    indicators <- calc.indicators(calc.type = "long",
                                  hits = imported$pintercept.long,
                                  ptcount = imported$point.count)
  }
  tictoc::toc(log = TRUE, quiet = TRUE)

  tictoc::tic(msg = "Averaging values and sorting", quiet = TRUE)
  cat("Averaging indicators for plot...\n")
  plot.indicators <- calc.plot(imported = imported, indicators = indicators,
                               indicator.list = indicator.list)
  tictoc::toc(log = TRUE, quiet = TRUE)
  return(plot.indicators)
}

#' Takes input from a user provided indicator list and exports the filtered data
#' (either in long or wide form) into individual delimited files in order to
#' test that the indicator filter string is working as expected.
#'
#' @param imported A list of tibbles produced from \code{import.data()}
#' @param indicator.list A tibble produced from \code{load.indicators()}
#' @param test.dir A string directory file path in which to save
#'   the individual indicator output files.
#' @param sep A character. The character to use whe writing the delimited
#'   outputs.
do.indicator.test <- function(imported, indicator.list, test.dir, sep = ","){
  # get filtered raw lpi data instead
  tictoc::tic(msg = "Exporting filtered raw data", quiet = TRUE)
  if (!dir.exists(test.dir)){
    cat(paste0("creating directory ", test.dir, "\n"))
    dir.create(test.dir)
  }
  foreach::foreach(i=1:nrow(indicator.list)
  ) %do% {
    raw.out <- test.indicators(name = indicator.list$name[i],
                               filter.exp = indicator.list$filter.exp[i],
                               calc.type = indicator.list$filter.tbl[i],
                               hits = eval(parse(
                                 text = paste0("imported$pintercept.",
                                               indicator.list$filter.tbl[i]))))
    cat(paste0("Writing delimited output to ",
               file.path(test.dir, indicator.list$name[i]), ".csv\n"))
    utils::write.table(raw.out,
                file = paste0(file.path(test.dir, indicator.list$name[i]),
                                       ".csv"),
                row.names = FALSE, na = "", col.names = TRUE, sep = sep)
  }
  tictoc::toc(log = TRUE, quiet = TRUE)
}

#' The main processing function for the module
#'
#' @param dbname A string. The database name to connect to in the postgres
#'   instance.
#' @param host A string. The IP address or DNS name which hosts the database.
#' @param port An integer. The port which the postgres service monitors for
#'   connections.
#' @param user A string. The database user used to connect to the database.
#' @param password A string. The password used to connect to the database.
#' @param indicator.path A string. The file path to a tab delimited file which
#'   contains indicator definitions. See README for file requirements.
#' @param test A string. The file path to a directory in which to output a
#'   filtered version of the raw data for each indicator, for testing purposes.
#' @param out.file A string. The file path for the output file. Must have a .rds
#'   or .csv extension.
#' @param sep A character which is used to delimit the output file
#'   (\code{out.file}) in the case that it is to be in the CSV format.
#' @param enable_parallel A Boolean flag telling the system to run parallel
#'   processing for those calculations and system on which it is supported. This
#'   can be useful generally for smaller data sets with many indicators.
#' @return A list containing process time elapsed information.
#' @export
calc.lpi <- function(
    dbname, host, port, user, password,
    indicator.path = NULL, test = NULL, out.file = NULL,
    sep = ",", enable_parallel = FALSE){

  indicator.list <- load.indicators(indicator.path)
  if (!is.null(indicator.list)){
    num.cores <- min(nrow(indicator.list), parallel::detectCores()-1)
  } else {
    num.cores <- 1
  }
  if (enable_parallel == TRUE){
    use.mc <- switch(Sys.info()[['sysname']],
                     Windows= {FALSE},
                     Linux  = {TRUE},
                     Darwin = {TRUE})
  } else {
    use.mc <- FALSE
  }
  imported <- import.data(dbname = dbname, host = host, port = port,
                          user = user, password = password)

  if (!is.null(test) & !is.null(indicator.list)){
    do.indicator.test(imported = imported, indicator.list = indicator.list,
      test.dir = test, sep = sep)
  # Indicator calculation and processing
  } else {
    out.table <- do.indicator.calc( imported = imported,
                                    indicator.list = indicator.list,
                                    num.cores = num.cores, use.mc = use.mc)
    tictoc::tic(msg = "Writing output")
    if (tools::file_ext(out.file) == "csv") {
      cat(paste("Writing delimited output to", out.file, "\n"))
      utils::write.table(out.table, file = out.file, row.names = FALSE,
                  na = "", col.names = TRUE, sep = sep)

    } else if (tools::file_ext(out.file) == "rds") {
      cat(paste("Writing RDS output to", out.file, "\n"))
      saveRDS(out.table, file = out.file)
    } else {
      cat(paste("File output extension not recognized. ",
                "Skipping writing of results...\n"))
    }
  }
  tictoc::toc(log = TRUE, quiet = TRUE)
  log.txt <- tictoc::tic.log(format = TRUE)
  log.lst <- tictoc::tic.log(format = FALSE)
  tictoc::tic.clearlog()
  cat("\n")
  return(list(log.txt = log.txt, log.lst = log.lst))
}


# Run if module is called from Rscript.
if (sys.nframe() == 0) {
  option_list = list (
    optparse::make_option(opt_str = c("-p", "--port"), default = 5432,
                          type = "integer",
                help = paste0("The Postgres connection port")),
    optparse::make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    optparse::make_option(opt_str = c("-w", "--password"),
                help = paste0("The password for the user")),
    optparse::make_option(opt_str = c("-o", "--outfile"),
                help = paste0("the output path for the calculated indicators ",
                              "(.csv, .rds)")),
    optparse::make_option(opt_str = c("-i", "--indicators"),
                help = paste0("A file path to a tab delimited list of ",
                              "indicators and their respective dplyr ",
                              "dplyr::filter() strings (See indicators/* for ",
                              "examples). These indicators will be exported ",
                              "instead of species cover values")),
    optparse::make_option(opt_str = c("-t", "--test"),
                help = paste0("A folder path for test output. ",
                              "Used in conjunction with --indicator, switches ",
                              "indicator output to the raw filtered data used ",
                              "to calculate the indicators. This can be used ",
                              "to test different dplyr filter strings.")),
    optparse::make_option(opt_str = c("-s", "--sep"), default = ",",
                help = paste0("Separator to use for delimited output. In the ",
                              r"{case of escaped characters (e.g. \t) you must}",
                              " pass the literal character recongnized your ",
                              r"{shell (e.g. $'\t' for bash).}")),
    optparse::make_option(opt_str = c("-e", "--enable_parallel"),
                          action = 'store_true',
                default = FALSE,
                help = paste0("Enable parallel processing. This is currently ",
                              "not recommended for use in large datasets as
                              the parallel performance increases are ",
                              "outweighed by the memory and communitication ",
                              "overhead of executing in parallel, even on ",
                              "fork compatible POSIX systems."))
    )
  opt_parser = optparse::OptionParser(usage = paste0("usage: %prog [options] ",
                                           "dbname user"),
               option_list=option_list, prog = NULL,
               description = paste0("\nThis script will export either cover ",
               "values, or a set of custom defined indicators for each plot ",
               "from the line-point intercept method.\ndbname is the name of ",
               "the database to connect to and user is the database user.")
  )
  args = commandArgs(trailingOnly = TRUE)
  opt = optparse::parse_args(opt_parser, positional_arguments = 2, args = args)

  # option checks
  if (is.null(opt$options$password)){
    opt$options$password = getPass::getPass()
  }
  if (is.null(opt$options$test) & is.null(opt$options$outfile)){
    stop("One option, either --test or --outfile is necessary. Exiting...")
  }
  if(!is.null(opt$options$outfile)){
    if(!(tools::file_ext(opt$options$outfile) %in% c("csv", "rds"))){
      stop("Output file must have extension .csv or .rds")
    }
    if (!dir.exists(dirname(opt$options$outfile))){
      stop(paste0("Directory ", dirname(opt$options$outfile), " does not exist."))
    }
  }
  time <- calc.lpi(
    dbname = opt$args[1], host = opt$options$host,
    port = opt$options$port,
    user = opt$args[2], password = opt$options$password,
    indicator.path = opt$options$indicators, test = opt$options$test,
    out.file = opt$options$outfile,
    sep = opt$options$sep,
    enable_parallel = opt$options$enable_parallel)

  timings <- unlist(lapply(time$log.lst, function(x) x$toc - x$tic))
  writeLines(unlist(time$log.txt))
  cat(paste0("Total script running time: ", sum(timings),
             " sec elapsed\n"))

}
