#!/usr/bin/env Rscript

# libraries = c("dbplyr", "doParallel", "dplyr", "getPass", "glue",
#               "lubridate", "optparse", "parallel", "readr",
#               "RPostgres", "stringi", "stringr", "tictoc", "tidyr",
#               "tools", "yaml", "nanoparquet")
#
# for (lib in libraries){
#   suppressMessages(library(lib, character.only = TRUE))
# }

#' load special binary operators
`%do%` <- foreach::`%do%`
`%dopar%` <- foreach::`%dopar%`
.data <- rlang::.data

#' This function connects to a Postgres instance of the LLEIA database and reads
#' in relevant tables for Point intercept calculations.
#'
#' @param con A DBI connection object to a database with LLEIA schema.
#' @return A list of tibbles imported from the connection source.
import_data <- function(con = con) {
  tictoc::tic.clearlog()
  DBI::dbExecute(con, "SET client_min_messages TO WARNING;")
  tictoc::tic(msg = "Loading data from data sources", quiet = TRUE)

  cat("Importing table 'plant'...\n")
  plant <- tibble::as_tibble(dplyr::tbl(con, "plant")) |>
    dplyr::mutate(
      duration_first = gsub(",.*$", "", .data$duration),
      growth_habit_first = gsub(",.*$", "", .data$growth_habit)
    )

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
  genus_family_codes <- dplyr::union(
    dplyr::filter(plant, .data$code_type == "Genus") |>
      dplyr::select("accepted_symbol", "code_type"),

    dplyr::mutate(plant,
                  test = stringr::str_to_upper(substring(family, 1, 6))) |>
      dplyr::select("test") |>
      dplyr::rename(accepted_symbol = "test") |>
      dplyr::group_by(.data$accepted_symbol) |>
      dplyr::summarize(.groups = "drop")  |>
      dplyr::filter(!is.na(.data$accepted_symbol)) |>
      dplyr::mutate(code_type = "Family")
  )

  # add species level info to our long table
  cat("Processing generic species codes in table 'pintercept'...\n")
  pintercept_long <- pintercept |>
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
      dplyr::rename(genus_family_codes, tail_type = "code_type"),
      by = c("tail_id" = "accepted_symbol")
    ) |>
    dplyr::mutate(hit_clean = dplyr::case_when(!is.na(tail_id) &
                                                 !is.na(tail_type)
                                               ~ tail_id, TRUE ~ hit)) |>
    dplyr::mutate(
      growth_habit_alt = dplyr::case_when(
        !is.na(tail_type) ~ tail_gh, !is.na(head_gh) ~ head_gh,
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
        growth_habit_alt %in% c("PF", "PG", "2FP", "2GP", "SH", "TR", "2SHRUB",
                                "2SUBS", "2TREE", "2VW") |
          hit %in% c("PPFF", "PPGG", "PPSH", "PPTR",
                     "PPSU")  ~ "Perennial",
        growth_habit_alt %in% c("2BRY", "2MOSS") ~
          NA_character_,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::mutate(
      growth_habit_first =
        replace(.data$growth_habit_first, is.na(.data$growth_habit_first),
                .data$gh_alt[is.na(.data$growth_habit_first)])
    ) |>
    dplyr::mutate(
      duration_first =
        replace(.data$duration_first, is.na(.data$duration_first),
                .data$duration_alt[is.na(.data$duration_first)])
    ) |>
    dplyr::select(-tidyselect::starts_with("tail"),
                  -tidyselect::starts_with("head"),
                  -tidyselect::contains("alt"))

  cat("Transposing long format 'pintercept' to wide format...\n")
  pintercept_hit_wide <- pintercept |>
    dplyr::select(tidyselect::all_of(c("reckey", "mark", "hit_order", "hit_sub",
                                       "hit"))) |>
    tidyr::pivot_wider(
      names_from = c("hit_order", "hit_sub"),
      names_glue = "hit{hit_order}_{hit_sub}",
      values_from = "hit", names_sort = TRUE
    ) |>
    dplyr::rename_all(dplyr::recode,
                      "hit1_w" = "hit_woody",
                      "hit2_h" = "hit_herb",
                      "hit3_h" = "hit_herb_lwr") |>
    dplyr::select("reckey", "mark",
                  tidyselect::any_of(c("hit1_t", "hit2_l", "hit3_l", "hit4_l",
                                       "hit5_l", "hit6_l", "hit7_l", "hit8_l",
                                       "hit9_s", "hit_woody", "hit_herb",
                                       "hit_herb_lwr")))

  pintercept_dead_wide <- pintercept |>
    dplyr::select(tidyselect::all_of(c("reckey", "mark", "hit_order", "hit_sub",
                                       "dead"))) |>
    tidyr::pivot_wider(
      names_from = c("hit_order", "hit_sub"),
      names_glue = "dead{hit_order}_{hit_sub}",
      values_from = "dead", names_sort = TRUE
    ) |>
    dplyr::rename_all(dplyr::recode,
                      "dead1_w" = "dead_woody",
                      "dead2_h" = "dead_herb",
                      "dead3_h" = "dead_herb_lwr") |>
    dplyr::select("reckey", "mark",
                  tidyselect::any_of(c("dead1_t", "dead2_l", "dead3_l",
                                       "dead4_l", "dead5_l", "dead6_l",
                                       "dead7_l", "dead8_l", "dead9_s",
                                       "dead_woody", "dead_herb",
                                       "dead_herb_lwr")))

  pintercept_height_wide <- pintercept |>
    dplyr::select(
      tidyselect::all_of(
        c("reckey", "mark", "hit_order", "hit_sub", "height_cm")
      )
    ) |>
    tidyr::pivot_wider(
      names_from = c("hit_order", "hit_sub"),
      names_glue = "heightcm{hit_order}_{hit_sub}",
      values_from = "height_cm", names_sort = TRUE
    ) |>
    dplyr::rename_all(dplyr::recode,
                      "heightcm1_w" = "height_woody",
                      "heightcm2_h" = "height_herb",
                      "heightcm3_h" = "height_herb_lwr") |>
    dplyr::select(
      "reckey", "mark",
      tidyselect::any_of(
        c("heightcm1_t", "heightcm2_l", "heightcm3_l", "heightcm4_l",
          "heightcm5_l", "heightcm6_l", "heightcm7_l", "heightcm8_l",
          "heightcm9_s",  "heightcm_woody", "heightcm_herb",
          "heightcm_herb_lwr")
      )
    )

  pintercept_wide <- pintercept_hit_wide |>
    dplyr::left_join(pintercept_dead_wide,
                     by = c("reckey" = "reckey", "mark" = "mark")) |>
    dplyr::left_join(pintercept_height_wide,
                     by = c("reckey" = "reckey", "mark" = "mark"))

  # get total number of complete points per reckey in order to evaluate cover
  cat("Calculating number of vaild points per LPI record...\n")
  point_count <- dplyr::filter(pintercept_wide, !is.na(.data$hit1_t),
                               !is.na(.data$hit9_s)) |>
    dplyr::group_by(.data$reckey) |>
    dplyr::summarize(pt_count = dplyr::n(), .groups = "drop")

  tictoc::toc(log = TRUE, quiet = TRUE)

  list(pintercept.long = pintercept_long,
       pintercept.wide = pintercept_wide,
       point.count = point_count,
       site = site, point = point, transect = transect,
       pinterceptmeta = pinterceptmeta)
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
load_indicators <- function(indicators) {
  if (!is.null(indicators)) {
    ind_list <- yaml::read_yaml(indicators)
    ind_tbl <- dplyr::bind_rows(lapply(X = ind_list,
                                       FUN = function(x) tibble::as_tibble(x)))
  } else {
    ind_tbl <- NULL
  }

  ind_tbl
}

#' This function will take a point intercept data data (either wide or long)
#' and calculate cover, height, and % dead indicators based on the filter
#' strings evaluated as an expression on the raw table.
#'
#' @param name A string. The name of the indicator.
#' @param filter_exp A string. A \code{dpylr::filter()} expression
#'   (or union-ed set thereof) which is used to filter the table.
#' @param calc_type A string. The table type on which to perform the filter
#'   expression.
#' @param hits A tibble. The table to perform the filter on.
#' @param ptcount A tibble which contains two fields,"reckey" and "pt_count",
#'   the former a string field containing a unique set of record keys and the
#'   latter an integer field containing the number of complete points for that
#'   record.
#'
#' @return A tibble, with the \code{filter.exp} applied to \code{hits}
#' @examples
#' calc_indicators(
#'   name = "tree",
#'   filter_exp = paste(
#'     "dplyr::filter(hits, growth_habit_first == 'Tree',",
#'     "calc_type = 'long', hits = pintercept.long, ptcount = my_ptcount)"
#'   )
#' )
calc_indicators <- function(name = NULL, filter_exp, calc_type, hits,
                            ptcount) {
  # if there is no filter expression, function defaults to calculating
  # species level indicators
  if (is.null(filter_exp)) {
    filter_exp <- paste0(
      'dplyr::filter(hits, !(hit %in% c("N", "L", "HL", "WL", ',
      '"NL", "DS", "W", "VL", "S", "LC", "M", "D", "R", ',
      '"CY", "EL", "GR", "CB", "ST", "BY", "BR", "None", ',
      '"RF", "AL", "OM", "WA")) &  !is.na(hit) & hit != "")'
    )
    calc_type <- "long"
  }
  filtered <- eval(parse(text = filter_exp))

  if (calc_type == "long") {
    if (is.null(name)) {
      # creates a shim for when no indicator is passed so that the following
      # functions work in the shpecies_code level
      name <- as.symbol("hit")
    }

    # filter by user defined expression(s) to get marks with indicator present
    # for any cover class
    ind_count_any <- filtered |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::group_by(.data$reckey, .data$mark, .data$hit_type,
                      .data$indicator) |>
      dplyr::summarize(.groups = "drop") |>
      dplyr::group_by(.data$reckey, .data$hit_type, .data$indicator) |>
      dplyr::summarize(ind_count = dplyr::n(), .groups = "drop") |>
      # takes out growth habit hits, since we can't really use them to calc
      # cover but we want to keep them in for later join to height if necessary
      dplyr::mutate(ind_count = ifelse(.data$hit_type == "g", 0,
                                       .data$ind_count)) |>
      dplyr::group_by(.data$reckey, .data$indicator) |>
      dplyr::summarize(ind_count = as.integer(sum(.data$ind_count)),
                       .groups = "drop")

    ind_height_any <- filtered |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::filter(!is.na(.data$height_cm)) |>
      dplyr::group_by(.data$reckey, .data$indicator) |>
      dplyr::summarize(
        height_n = dplyr::n(),
        height_cm_mean = mean(.data$height_cm, na.rm = TRUE),
        height_cm_sd = sd(.data$height_cm, na.rm = TRUE),
        .groups = "drop"
      )

    ind_dead_any <- filtered |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::filter(!is.na(.data$dead) & .data$hit_type == "l") |>
      dplyr::group_by(.data$reckey, .data$mark, .data$indicator) |>
      dplyr::summarize(dead = all(.data$dead), .groups = "drop") |>
      dplyr::group_by(.data$reckey, .data$indicator) |>
      dplyr::summarize(dead_n = dplyr::n(), dead = sum(.data$dead),
                       .groups = "drop") |>
      dplyr::mutate(dead_pct = ifelse(.data$dead_n == 0, NA_real_,
                                      .data$dead / .data$dead_n))

    # join indicator count to total point count per method instance (reckey) and
    # calculate percent cover
    ind_cover_any_pre <- dplyr::left_join(ptcount, ind_count_any,
                                          by = c("reckey" = "reckey")) |>
      dplyr::mutate(n = dplyr::case_when(is.na(.data$ind_count) ~ as.integer(0),
                                         TRUE ~ .data$ind_count)) |>
      dplyr::mutate(hit_type = "any",
                    cvr_pct = as.double(.data$n / .data$pt_count))

    # if name is passed to the function as a character vector
    if (class(name) == "character") {
      ind_cover_any <- ind_cover_any_pre |>
        dplyr::mutate(
          indicator = dplyr::case_when(is.na(.data$indicator) ~ name,
                                       TRUE ~ .data$indicator)
        )
    } else {
      ind_cover_any <- ind_cover_any_pre |>
        dplyr::filter(!is.na(.data$ind_count) & !is.na(.data$indicator))
    }

    ind_any <- ind_cover_any |>
      dplyr::left_join(ind_height_any,
                       by = c("reckey" = "reckey",
                              "indicator" = "indicator")) |>
      dplyr::left_join(
        dplyr::select(ind_dead_any, "reckey", "indicator", "dead_pct"),
        by = c("reckey" = "reckey", "indicator" = "indicator")
      ) |>
      dplyr::mutate(height_n = ifelse(is.na(.data$height_n), 0, .data$height_n))

    # filter by user defined expression(s) to get marks with indicator present
    # for different cover classes (top, lower, surface/basal)
    ind_count_class <- filtered |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::group_by(.data$reckey, .data$mark, .data$hit_type, .data$hit_sub,
                      .data$indicator) |>
      dplyr::summarize(.groups = "drop") |>
      dplyr::group_by(.data$reckey, .data$hit_type, .data$hit_sub,
                      .data$indicator) |>
      dplyr::summarize(ind_count = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(ind_count = ifelse(.data$hit_type == "g", 0,
                                       .data$ind_count)) |>
      dplyr::group_by(.data$reckey, .data$hit_sub, .data$indicator) |>
      dplyr::summarize(ind_count = as.integer(sum(.data$ind_count)),
                       .groups = "drop")

    ind_height_class <- filtered |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::filter(!is.na(.data$height_cm)) |>
      dplyr::group_by(.data$reckey, .data$indicator, .data$hit_sub) |>
      dplyr::summarize(
        height_n = dplyr::n(), height_cm_mean = mean(.data$height_cm),
        height_cm_sd = sd(.data$height_cm), .groups = "drop"
      )

    ind_dead_class <- filtered |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::filter(!is.na(.data$dead) & .data$hit_type == "l") |>
      dplyr::group_by(.data$reckey, .data$mark, .data$indicator,
                      .data$hit_sub) |>
      dplyr::summarize(dead = all(.data$dead), .groups = "drop") |>
      dplyr::group_by(.data$reckey, .data$indicator, .data$hit_sub) |>
      dplyr::summarize(dead_n = dplyr::n(), dead = sum(.data$dead),
                       .groups = "drop") |>
      dplyr::mutate(dead_pct = ifelse(.data$dead_n == 0, NA_real_,
                                      .data$dead / .data$dead_n))

    # join indicator count to total point count per method instance (reckey) and
    # calculate percent cover
    ind_cover_class_pre <- dplyr::left_join(
      ptcount, ind_count_class, by = c("reckey" = "reckey")
    ) |>
      dplyr::filter(!is.na(.data$hit_sub)) |>
      dplyr::mutate(n = dplyr::case_when(is.na(.data$ind_count) ~ as.integer(0),
                                         TRUE ~ .data$ind_count)) |>
      dplyr::mutate(cvr_pct = as.double(.data$n / .data$pt_count)) |>
      dplyr::mutate(
        hit_type = dplyr::case_when(
          hit_sub == "t" ~ "top",
          hit_sub == "l" ~ "lower",
          hit_sub == "s" ~ "surface",
          hit_sub == "w" ~ "woody",
          hit_sub == "h" ~ "herbaceous"
        )
      )

    if (class(name) == "character") {
      # named indicator
      ind_cover_class <- ind_cover_class_pre |>
        dplyr::mutate(
          indicator = dplyr::case_when(is.na(.data$indicator) ~ name,
                                       TRUE ~ .data$indicator)
        )
    } else {
      # species level
      ind_cover_class <- ind_cover_class_pre |>
        dplyr::filter(!is.na(.data$ind_count) & !is.na(.data$indicator))
    }

    ind_class <- ind_cover_class |>
      dplyr::left_join(ind_height_class,
                       by = c("reckey" = "reckey",
                              "indicator" = "indicator",
                              "hit_sub" = "hit_sub")) |>
      dplyr::left_join(
        dplyr::select(ind_dead_class, "reckey", "indicator", "hit_sub",
                      "dead_pct"),
        by = c("reckey" = "reckey", "indicator" = "indicator",
               "hit_sub" = "hit_sub")
      ) |>
      dplyr::mutate(height_n = ifelse(is.na(.data$height_n), 0,
                                      .data$height_n)) |>
      dplyr::select(-"hit_sub")

    ind_cover <-  dplyr::union(ind_any, ind_class) |>
      dplyr::select(-"pt_count", -"ind_count", -"n")

  } else if (calc_type == "wide") {
    ind_count_any <- filtered |>
      dplyr::mutate(indicator = !!name) |>
      dplyr::group_by(.data$reckey, .data$mark, .data$indicator) |>
      dplyr::summarize(.groups = "drop") |>
      dplyr::group_by(.data$reckey, .data$indicator) |>
      dplyr::summarize(ind_count = dplyr::n(), .groups = "drop")

    ind_cover_any <- dplyr::left_join(ptcount, ind_count_any,
                                      by = c("reckey" = "reckey")) |>
      dplyr::mutate(n = dplyr::case_when(is.na(.data$ind_count) ~ as.integer(0),
                                         TRUE ~ .data$ind_count)) |>
      dplyr::mutate(
        hit_type = "all",
        cvr_pct = as.double(.data$n / .data$pt_count),
        indicator = dplyr::case_when(is.na(indicator) ~ name,
                                     TRUE ~ indicator),
        height_n = NA_integer_,
        height_cm_mean = NA_real_,
        height_cm_sd = NA_real_
      )

    ind_cover <- dplyr::select(ind_cover_any, "reckey", "indicator", "hit_type",
                               "cvr_pct", "height_n", "height_cm_mean",
                               "height_cm_sd")
  }

  ind_cover
}


#' This function will filter out a table based on a string evaluated as an
#' expression.
#'
#' @param name A string. The name of the indicator.
#' @param filter_exp A string. A \code{dpylr::filter()} expression (or union-ed
#'   set thereof) which is used to filter the table
#' @param calc_type A string. The table type on which to perform the filter
#'   expression.
#' @param hits A tibble. The table to perform the filter on.
#'
#' @return A tibble, with the \code{filter_exp} applied to \code{hits}
#'
#' @examples
#' test_indicators(
#'   name = "tree",
#'   filter_exp = "dplyr::filter(hits, growth_habit_first == 'Tree')",
#'   calc_type = "long",
#'   hits = pintercept.long
#' )
test_indicators <- function(name, filter_exp, calc_type, hits) {
  # filter by user defined expression(s) to get marks with indicator present for
  # any cover class
  ind_count_any <- eval(parse(text = filter_exp)) |>
    dplyr::mutate(indicator = name)

  ind_count_any
}

#' This function takes calculated indicators at the survey level (reckey) and
#' adds zeros (cover) or NA's (height) for those surveys which do not have
#' values for specific indicators and then calculates the average for each
#' unique plot/survey year.
#'
#' @param imported  A list of tibbles produced from \code{import_data()}
#' @param indicators A tibble produced from \code{calc_indicators()}. These are
#'   the calculated indicators at the survey (reckey) level.
#' @param indicator_list A tibble produced from \code{load_indicators()}
#'
#' @return A tibble containing the calculated indicators averaged at the plot
#'   level.
calc_plot <-  function(imported, indicators, indicator_list = NULL) {
  hit_types <- tibble::as_tibble(
    list(hit_type = c("any", "top", "lower", "surface", "woody", "herbaceous",
                      "all"),
         calc_type = c("long", "long", "long", "long", "long", "long", "wide"))
  )

  plot_head <- dplyr::select(imported$transect, "linekey", "lineid",
                             "plotkey") |>
    dplyr::inner_join(
      dplyr::select(imported$pinterceptmeta, "reckey", "survey_date",
                    "linekey"),
      by = c("linekey" = "linekey")
    ) |>
    dplyr::mutate(survey_year = lubridate::year(.data$survey_date))

  plot_indicators <-  plot_head |>
    dplyr::inner_join(indicators, by = c("reckey" = "reckey")) |>
    dplyr::rename(height_cm = "height_cm_mean",
                  dead = "dead_pct")

  # species
  if (is.null(indicator_list)) {
    plot_indicators_head <- plot_indicators |>
      dplyr::group_by(.data$plotkey, .data$survey_year, .data$indicator) |>
      dplyr::summarize(survey_date_min = min(.data$survey_date),
                       .groups = "drop") |>
      dplyr::mutate(calc_type = "long")

    # expands our header to all valid hit types with many-to-many join
    plot_indicators_expanded <- plot_indicators_head |>
      dplyr::left_join(hit_types, by = c("calc_type" = "calc_type"),
                       relationship = "many-to-many")

    # expands our header to all valid lines with many-to-many join
    # here distinct() is needed in case of >1 readings of line in same year
    plot_indicators_all <- plot_indicators_expanded |>
      dplyr::left_join(
        dplyr::select(plot_head, "plotkey", "survey_year", "linekey") |>
          dplyr::distinct(),
        by = c("plotkey" = "plotkey", "survey_year" = "survey_year"),
        relationship = "many-to-many"
      )

    # indicators
  } else {
    all_lines <- plot_head |>
      dplyr::group_by(.data$plotkey, .data$survey_year, .data$linekey) |>
      dplyr::summarize(survey_date_min = min(.data$survey_date),
                       .groups = "drop")
    all_indicators <- dplyr::select(indicator_list, "filter.tbl", "name") |>
      dplyr::left_join(hit_types, by = c("filter.tbl" = "calc_type"),
                       relationship = "many-to-many") |>
      dplyr::rename(calc_type = "filter.tbl", indicator = "name")
    plot_indicators_all <-  tidyr::crossing(all_lines, all_indicators)
  }

  # joins our constructed plot/line/calc_type/hit_type header with actual data
  # we do this so we can fill in the NULLs with zeros, as NULL indicates that
  # species was not present on that line for cover calcs
  plot_indicators_filled <- plot_indicators_all |>
    dplyr::left_join(
      dplyr::select(plot_indicators, -"plotkey"),
      by = c("linekey" = "linekey", "survey_year" = "survey_year",
             "indicator" = "indicator", "hit_type" = "hit_type")
    ) |>
    dplyr::mutate(cvr_pct = dplyr::coalesce(.data$cvr_pct, 0))

  plot_indicators_sum <- plot_indicators_filled |>
    dplyr::group_by(.data$plotkey, .data$survey_year, .data$survey_date_min,
                    .data$calc_type, .data$indicator, .data$hit_type) |>
    dplyr::summarize(
      cvr_n = dplyr::n(),
      cvr_pct_mean = mean(.data$cvr_pct),
      cvr_pct_sd = sd(.data$cvr_pct),
      height_n = as.integer(sum(!is.na(.data$height_cm))),
      height_cm_mean = mean(.data$height_cm, na.rm = TRUE),
      height_cm_sd = sd(.data$height_cm, na.rm = TRUE),
      dead_n = as.integer(sum(!is.na(.data$dead))),
      dead_pct_mean = mean(.data$dead, na.rm = TRUE),
      dead_pct_sd = sd(.data$dead, na.rm = TRUE),
      .groups = "drop"
    )

  plot_indicators_sum
}


#' This function takes an indicator list, and runs \code{calc_indicators()} on
#' every indicator in the list, then binds the rows together into a final table.
#'
#' @param imported A list of tibbles produced from \code{import_data()}
#' @param indicator_list A tibble produced from \code{load_indicators()}
#' @param num_cores The number of cpu cores to use to do the indicator
#'   calculation. Optimal number is \code{nrow(indicator_list}).
#' @param use_mc logical. A flag to tell the function whether or not to use
#'   multicore processing.
#'
#' @return A tibble containing rows for every indicator processed for each
#'   unique plotkey and survey year.
#' @importFrom foreach %do% %dopar%
do_indicator_calc <- function(imported, indicator_list = NULL, num_cores = 1,
                              use_mc = FALSE) {
  tictoc::tic(msg = "Calculating indicators", quiet = TRUE)
  # for indicators
  if (!is.null(indicator_list)) {
    # do indicator calculations. PSOCK type parallel is way too inefficient due
    # to the overhead of copying the base data to different sockets. FORKing
    # shows performance improvement but is unavailable in windows. Thus the
    # multicore functionality of the following code is useful only to POSIX type
    # systems.
    if (use_mc) {
      cat(paste("Calculating indicators on", num_cores, "cores...\n"))
      cl <- parallel::makeCluster(num_cores, type = "FORK")
      doParallel::registerDoParallel(cl)
    } else {
      cat(paste("Calculating indicators...\n"))
    }
    # by using suppressWarnings(), we can just write one %dopar% which will
    # default to a foreach::`%do%` if no parallel cluster has been registered
    indicators <-  suppressWarnings(
      foreach::foreach(
        i = seq_len(nrow(indicator_list)), .combine = dplyr::bind_rows
        #, .packages = c("dplyr", "tidyr", "stringr")
        #, .export = c("pintercept.long", "pintercept.wide")
      ) %dopar% {
        cat(paste0("\t", indicator_list$name[i], "...\n"))
        calc_indicators(
          name = indicator_list$name[i],
          filter_exp = indicator_list$filter.exp[i],
          calc_type = indicator_list$filter.tbl[i],
          hits = eval(parse(
            text = paste0(
              "imported$pintercept.", indicator_list$filter.tbl[i]
            )
          )),
          ptcount = imported$point.count
        )
      }
    )
    if (use_mc) {
      parallel::stopCluster(cl)
    }
  } else {
    cat(paste("Calculating species codes...\n"))
    # for species
    indicators <- calc_indicators(calc_type = "long",
                                  hits = imported$pintercept.long,
                                  ptcount = imported$point.count)
  }
  tictoc::toc(log = TRUE, quiet = TRUE)

  tictoc::tic(msg = "Averaging values and sorting", quiet = TRUE)
  cat("Averaging indicators for plot...\n")
  plot.indicators <- calc_plot(imported = imported, indicators = indicators,
                               indicator_list = indicator_list)
  tictoc::toc(log = TRUE, quiet = TRUE)
  return(plot.indicators)
}


# Used to split reckeys into partitioned data instead of indicators.
# Not used for now. Partitioning by reckey for multicore use explodes memory
# use (parallelizing all chunks and running every indicator calculation for each
# chunk).
chunk_vector <- function(x, n_chunks) {
  split(x, ((seq_along(x) - 1) %% n_chunks) + 1)
}

# Maybe faster than do_indicator_calc(), utilizing better vectorizion.
# Unfortunately, using future.apply::future_lapply() with multiple cores is
# less efficient (due to overhead) than just using lapply().
# Using this new function requires refactoring calc_indicators to accept a
# pre-filtered df caled 'filtered' instead of the full df (hits) and a 
# filter expression (filter_exp).  Leaving this function for future development,
# but for now, not a better solution that original function.
do_indicator_calc_new <- function(imported, indicator_list = NULL,
                                  num_cores = 1,
                                  use_mc = FALSE) {
  tictoc::tic(msg = "Calculating indicators", quiet = TRUE)
  # for indicators
  if (!is.null(indicator_list)) {
    if (use_mc) {
      cat(paste("Calculating indicators on", num_cores, "cores...\n"))
      # resricts data to smaller size before we send them to multiple cores
      # at the cost of creating a large new variable in system memory
      chunks <- lapply(
        X = seq_len(nrow(indicator_list)),
        FUN = function(i) {
          name <- indicator_list$name[i]
          filter_exp <- indicator_list$filter.exp[i]
          calc_type <- indicator_list$filter.tbl[i]
          hits <- eval(parse(
            text = paste0(
              "imported$pintercept.", indicator_list$filter.tbl[i]
            )
          ))
          filtered <- eval(parse(text = filter_exp))
          list(name = name, calc_type = calc_type, filtered = filtered)
        }
      )
      point.count = imported$point.count
      tictoc::toc(log = TRUE, quiet = TRUE)
    } else {
      cat(paste("Calculating indicators...\n"))
    }


    # set parallelization
    old_plan <- future::plan()
    future::plan(future::multisession, workers = nrow(indicator_list))
    on.exit(future::plan(old_plan), add = TRUE)

    # tictoc::tic()
    indicators <- future.apply::future_lapply(
      X = chunks,
      future.packages = c("dplyr", "tidyr", "stringr"),
      FUN = function(chunk_i) {
        cat(paste0("\t", chunk_i$name, "...\n"))
        calc_indicators(
          name = chunk_i$name,
          filtered = chunk_i$filtered,
          calc_type = chunk_i$calc_type,
          ptcount = point.count
        )
      }
    ) |>
      dplyr::bind_rows()
    # tictoc::toc()

  } else {
    cat(paste("Calculating species codes...\n"))
    # for species
    if (is.null(filter_exp)) {
      filtered <- imported$pintercept.long |>
        dplyr::filter(
          !(.data$hit %in% c(
            "N", "L", "HL", "WL", "NL", "DS", "W", "VL", "S", "LC", "M", "D",
            "R", "CY", "EL", "GR", "CB", "ST", "BY", "BR", "None", "RF", "AL",
            "OM", "WA"
          )) &  
          !is.na(.data$hit) & 
          .data$hit != "")
      
    indicators <- calc_indicators(calc_type = "long",
                                  filtered = filtered,
                                  ptcount = imported$point.count)
  }
  tictoc::toc(log = TRUE, quiet = TRUE)

  tictoc::tic(msg = "Averaging values and sorting", quiet = TRUE)
  cat("Averaging indicators for plot...\n")
  plot_indicators <- calc_plot(imported = imported, indicators = indicators,
                               indicator_list = indicator_list)
  tictoc::toc(log = TRUE, quiet = TRUE)
  return(plot_indicators)
}

#' Takes input from a user provided indicator list and exports the filtered data
#' (either in long or wide form) into individual delimited files in order to
#' test that the indicator filter string is working as expected.
#'
#' @param imported A list of tibbles produced from \code{import_data()}
#' @param indicator_list A tibble produced from \code{load_indicators()}
#' @param test_dir A string directory file path in which to save
#'   the individual indicator output files.
#' @param sep A character. The character to use whe writing the delimited
#'   outputs.
do_indicator_test <- function(imported, indicator_list, test_dir, sep = ",") {
  # get filtered raw lpi data instead
  tictoc::tic(msg = "Exporting filtered raw data", quiet = TRUE)
  if (!dir.exists(test_dir)) {
    cat(paste0("creating directory ", test_dir, "\n"))
    dir.create(test_dir)
  }
  foreach::foreach(i = seq_len(nrow(indicator_list))
  ) %do% {
    raw_out <- test_indicators(
      name = indicator_list$name[i],
      filter_exp = indicator_list$filter.exp[i],
      calc_type = indicator_list$filter.tbl[i],
      hits = eval(parse(
        text = paste0("imported$pintercept.", indicator_list$filter.tbl[i])
      ))
    )
    cat(paste0("Writing delimited output to ",
               file.path(test_dir, indicator_list$name[i]), ".csv\n"))
    utils::write.table(
      raw_out,
      file = paste0(file.path(test_dir, indicator_list$name[i]), ".csv"),
      row.names = FALSE, na = "", col.names = TRUE, sep = sep
    )
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
#' @param indicator_path A string. The file path to a tab delimited file which
#'   contains indicator definitions. See README for file requirements.
#' @param test A string. The file path to a directory in which to output a
#'   filtered version of the raw data for each indicator, for testing purposes.
#' @param out_file A string. The file path for the output file. Must have a
#'   .rds, .csv, .tsv, .parquet, or .pqt extension.
#' @param sep A character which is used to delimit the output file
#'   (\code{out_file}) in the case that it is to be in the CSV format.
#' @param enable_parallel A Boolean flag telling the system to run parallel
#'   processing for those calculations and system on which it is supported. This
#'   can be useful generally for smaller data sets with many indicators.
#' @return A list containing process time elapsed information.
#' @export
calc_lpi <- function(
    con, indicator_path = NULL, test = NULL, out_file = NULL, table_name = NULL,
    schema_out = NULL, sep = ",", enable_parallel = FALSE, overwrite = FALSE) {

  indicator_list <- load_indicators(indicator_path)

  if (!is.null(indicator_list)) {
    num_cores <- min(nrow(indicator_list), parallel::detectCores() - 1)
  } else {
    num_cores <- 1
  }
  if (enable_parallel == TRUE) {
    use_mc <- switch(
      Sys.info()[["sysname"]],
      Windows = {
        FALSE
      },
      Linux  = {
        TRUE
      },
      Darwin = {
        TRUE
      }
    )
  } else {
    use_mc <- FALSE
  }
  imported <- import_data(con = con)

  if (!is.null(test) && !is.null(indicator_list)) {
    do_indicator_test(imported = imported, indicator_list = indicator_list,
                      test_dir = test, sep = sep)
    # Indicator calculation and processing
  } else {
    out_table <- do_indicator_calc(imported = imported,
                                   indicator_list = indicator_list,
                                   num_cores = num_cores, use_mc = use_mc)
    tictoc::tic(msg = "Writing output")
    if (!is.null(out_file)) {
      if (tolower(tools::file_ext(out_file)) %in% c("csv", "tsv")) {
        cat(paste("Writing delimited output to", out_file, "\n"))
        utils::write.table(out_table, file = out_file, row.names = FALSE,
                           na = "", col.names = TRUE, sep = sep)
      } else if (tolower(tools::file_ext(out_file)) == "rds") {
        cat(paste("Writing RDS output to", out_file, "\n"))
        saveRDS(out_table, file = out_file)
      } else if (tolower(tools::file_ext(out_file)) %in% c("parquet", "pqt")) {
        cat(paste("Writing Parquet output to", out_file, "\n"))
        nanoparquet::write_parquet(out_table, file = out_file)
      } else {
        cat(paste("File output extension not recognized. ",
                  "Skipping writing of results...\n"))
      }
    } else if (!is.null(table_name)) {
      if (is.null(schema_out)) {
        schema_out <- "public"
      }
      cat(glue::glue("Writing output to {schema_out}.{table_name}\n"))
      DBI::dbWriteTable(
        conn = con,
        name = DBI::Id(schema = schema_out, table = table_name),
        value = out_table,
        append = FALSE,
        overwrite = overwrite
      )
    }
  }
  tictoc::toc(log = TRUE, quiet = TRUE)
  log.txt <- tictoc::tic.log(format = TRUE)
  log.lst <- tictoc::tic.log(format = FALSE)
  tictoc::tic.clearlog()
  cat("\n")

  list(log.txt = log.txt, log.lst = log.lst)
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
      help = "The Postgres connection port [default: %default]."
    ),
    optparse::make_option(
      opt_str = c("-H", "--host"), default = "localhost",
      help = paste0("The host name or ip address of the connection ",
                    "[default: %default].")
    ),
    optparse::make_option(
      opt_str = c("-w", "--password"),
      help = paste0("The password for the user [user will be ",
                    "prompted if password is required but noy given].")
    ),
    optparse::make_option(
      opt_str = c("-o", "--out_file"),
      help = paste0("the output path for the calculated indicators ",
                    "(.csv, .tsv, .rds, .parquet, .pqt)")
    ),
    optparse::make_option(
      opt_str = c("-n", "--table_name"),
      help = paste("The table name to write to in the PostgreSQL source",
                   "database if no `out_file` is provided")
    ),
    optparse::make_option(
      opt_str = c("-x", "--schema_out"),
      help = paste("The name of the schema in `dbname` or `out_dsn` with which",
                   "to write the output features if `out_file` is not",
                   "provided.")
    ),
    optparse::make_option(
      opt_str = c("-i", "--indicators"),
      help = paste0("A file path to a tab delimited list of ",
                    "indicators and their respective dplyr ",
                    "dplyr::filter() strings (See indicators/* for ",
                    "examples). These indicators will be exported ",
                    "instead of species cover values")
    ),
    optparse::make_option(
      opt_str = c("-t", "--test"),
      help = paste0("A folder path for test output. ",
                    "Used in conjunction with --indicator, switches ",
                    "indicator output to the raw filtered data used ",
                    "to calculate the indicators. This can be used ",
                    "to test different dplyr filter strings.")
    ),
    optparse::make_option(
      opt_str = c("-s", "--sep"), default = ",",
      help = paste0("Separator to use for delimited output. In the ",
                    r"{case of escaped characters (e.g. \t) you }",
                    "must pass the literal character recognized ",
                    r"{your shell (e.g. $'\t' for bash). }",
                    "[default: %default].")
    ),
    optparse::make_option(
      opt_str = c("-O", "--overwrite"), action = "store_true",
      default = FALSE,
      help = paste("Overwrite `out_file` or `table_name` with the results of",
                   "the calcuations.")
    )#,
    # optparse::make_option(
    #   opt_str = c("-e", "--enable_parallel"), action = "store_true",
    #   default = FALSE,
    #   help = paste0("Enable parallel processing. This is currently ",
    #                 "not recommended for use in large datasets as
    #                 the parallel performance increases are ",
    #                 "outweighed by the memory and communication ",
    #                 "overhead of executing in parallel, even on ",
    #                 "fork compatible POSIX systems. ",
    #                 "[default: %default]")
    # )
  )

  opt_parser <- optparse::OptionParser(
    usage = "usage: %prog [options] dbname",
    option_list = option_list,
    prog = NULL,
    description = paste0(
      "\nThis script will export either cover ",
      "values, or a set of custom defined indicators for each plot ",
      "from the line-point intercept method.\n`dbname` is the name of ",
      "the database to which to connect."
    )
  )

  opt <- optparse::parse_args(opt_parser, positional_arguments = 1, args = args)

  con <- connect_pg(
    dbname = opt$args[1],
    host = opt$options$host,
    port = opt$options$port,
    user = opt$options$user,
    password = opt$options$password
  )

  mutually_excl <- c(
    opt$options$test, opt$options$out_file, opt$options$table_name
  )

  if (length(mutually_excl) > 1) {
    stop(paste(
      "One option, either `test`, `out_file`, or `table_name` is necessary.",
      "Exiting..."
    ))
  }

  valid_ext <- c("rds", "csv", "tsv", "parquet", "pqt")
  if (!is.null(opt$options$out_file)) {
    if (!(tolower(tools::file_ext(opt$options$outfile)) %in% valid_exts)) {
      stop(
        paste0(
          "Output file extension must be one of: ",
          paste(valid_ext, collapse = ", "), "."
        )
      )
    }
    if (!dir.exists(dirname(opt$options$outfile))) {
      stop(paste0("Directory ", dirname(opt$options$outfile),
                  " does not exist."))
    }
  }

  time <- calc_lpi(
    con = con,
    indicator_path = opt$options$indicators,
    test = opt$options$test,
    out_file = opt$options$outfile,
    table_name = opt$options$table_name,
    schema_out = opt$options$schema_out,
    sep = opt$options$sep,
    overwrite = opt$options$overwrite,
    enable_parallel = FALSE
  )

  timings <- unlist(lapply(time$log.lst, function(x) x$toc - x$tic))
  writeLines(unlist(time$log.txt))
  cat(paste0("Total script running time: ", sum(timings),
             " sec elapsed\n"))

  DBI::dbDisconnect(con)
}
