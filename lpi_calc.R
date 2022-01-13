#!/usr/bin/env Rscript

libraries = c("dbplyr", "doParallel", "dplyr", "getPass", "glue", 
              "lubridate", "optparse", "parallel", "readr", 
              "RPostgres", "stringi", "stringr", "tictoc", "tidyr", 
              "tools")

for (lib in libraries){
  suppressMessages(library(lib, character.only = TRUE))
}


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
#' @export
import.data <- function(dbname, host, port, user, password){
  tic.clearlog()
  con <- dbConnect(RPostgres::Postgres(), dbname = dbname, 
                   host = host, port = port, 
                   user = user, password = password)
  res <- dbExecute(con, "SET client_min_messages TO WARNING;")
  tic(msg = "Loading data from data sources", quiet = TRUE)
  
  cat("Importing table 'plant'...\n")
  plant <- as_tibble(tbl(con, "plant")) %>%
    mutate(duration_first = gsub( ",.*$", "", duration),
           growth_habit_first = gsub( ",.*$", "", growth_habit))
  cat("Importing table 'site'...\n")
  site <- as_tibble(tbl(con, "site"))
  cat("Importing table 'point'...\n")
  point <- as_tibble(tbl(con, "point"))
  cat("Importing table 'transect'...\n")
  transect <- as_tibble(tbl(con, "transect"))
  pinterceptmeta <- as_tibble(tbl(con, "pintercept_meta"))
  cat("Importing table 'pintercept'...\n")
  pintercept <- as_tibble(tbl(con, "pintercept"))
  toc(log = TRUE, quiet = TRUE)
  
  tic(msg = "Preprocessing imported data", quiet = TRUE)
  cat("Processing code types in table 'plant'...\n")
  genus.family.codes <- union(
    filter(plant, code_type == "Genus") %>% 
      select(accepted_symbol, code_type), 
    mutate(plant, test = str_to_upper(substring(family, 1, 6))) %>% 
      select(test) %>% 
      rename(accepted_symbol = test) %>% 
      group_by(accepted_symbol) %>% 
      summarize(.groups = "drop")  %>% 
      filter(!is.na(accepted_symbol)) %>% 
      mutate(code_type = 'Family'))
  
  # add species level info to our long table
  cat("Processing generic species codes in table 'pintercept'...\n")
  pintercept.long <- pintercept %>%
    # join with plant table in order to evaluate indicator
    left_join(plant, by = c("hit" = "accepted_symbol")) %>%
    # Some genus/family codes have growth habits attached at the end of them 
    # (e.g. LUPINAF) or are unknown species codes (e.g. AF01). The following added
    # fields help tease these out for use in indicator calculations.
    extract("hit", into = c("tail_id", "tail_gh"), 
            regex = "(.+)(AF|PF|AG|PG|SH|TR)$", remove = FALSE) %>%
    extract("hit", into = c("head_gh", "head_id"), 
            regex = paste0("^(AF|PF|AG|PG|SH|TR|2FA|2FP|2GP|2SHRUB|2SUBS|2GA|",
                           "2TREE|2VW|2BRY|2MOSS)(\\d+)$"), remove = FALSE) %>%
    left_join(rename(genus.family.codes, tail_type = code_type), 
              by = c("tail_id" = "accepted_symbol")) %>%
    mutate(hit_clean = case_when(!is.na(tail_id) & !is.na(tail_type) 
                                 ~ tail_id, TRUE ~ hit)) %>%
    mutate(growth_habit_alt = case_when(!is.na(tail_type) ~ tail_gh,
                                        !is.na(head_gh) ~ head_gh,
                                        TRUE ~ NA_character_)) %>%
    mutate(gh_alt = case_when(growth_habit_alt %in% c("AF", "PF", "2FA", "2FP") |
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
                              TRUE ~ NA_character_)) %>%
    mutate(duration_alt = case_when(
      growth_habit_alt %in% c("AF", "AG", "2FA", "2GA") |
        hit %in% c("AAFF", "AAGG", "AASU")  ~ "Annual",
      growth_habit_alt %in% c("PF", "PG", "2FP", "2GP", 
                              "SH", "TR", "2SHRUB", 
                              "2SUBS", "2TREE", "2VW") |
        hit %in% c("PPFF", "PPGG", "PPSH", "PPTR", 
                   "PPSU")  ~ "Perennial",
      growth_habit_alt %in% c("2BRY", "2MOSS") ~ 
        NA_character_,
      TRUE ~ NA_character_)) %>%
    mutate(growth_habit_first = replace(growth_habit_first, 
                                        is.na(growth_habit_first), 
                                        gh_alt[is.na(growth_habit_first)])) %>%
    mutate(duration_first = replace(duration_first, 
                                    is.na(duration_first), 
                                    duration_alt[is.na(duration_first)])) %>% 
    select(-starts_with("tail"), -starts_with("head"), -contains("alt"))
  
  # arrange the data table for testing/viewing purposes
  # pintercept.long <- arrange(pintercept.long, reckey, mark, hit_order)
  
  cat("Transposing long format 'pintercept' to wide format...\n")
  pintercept.hit.wide <- pintercept %>% 
    select(reckey, mark, hit_order, hit_sub, hit) %>%
    pivot_wider(names_from = c(hit_order, hit_sub), 
                names_glue ="hit{hit_order}_{hit_sub}", 
                values_from = hit, names_sort = TRUE) %>%
    rename_all(recode, "hit1_w" = "hit_woody", "hit2_h" = "hit_herb",
               "hit3_h" = "hit_herb_lwr") %>%
    select(reckey, mark, any_of(c("hit1_t", "hit2_l", "hit3_l", "hit4_l", 
                                  "hit5_l", "hit6_l", "hit7_l", "hit8_l", 
                                  "hit9_s", "hit_woody", "hit_herb", 
                                  "hit_herb_lwr")))
  
  pintercept.dead.wide <- pintercept %>% 
    select(reckey, mark, hit_order, hit_sub, dead) %>%
    pivot_wider(names_from = c(hit_order, hit_sub), 
                names_glue ="dead{hit_order}_{hit_sub}", 
                values_from = dead, names_sort = TRUE) %>%
    rename_all(recode, "dead1_w" = "dead_woody", "dead2_h" = "dead_herb",
               "dead3_h" = "dead_herb_lwr") %>%
    select(reckey, mark, any_of(c("dead1_t", "dead2_l", "dead3_l", "dead4_l", 
                                  "dead5_l", "dead6_l", "dead7_l", "dead8_l", 
                                  "dead9_s", "dead_woody", "dead_herb", 
                                  "dead_herb_lwr")))
  
  pintercept.height.wide <- pintercept %>% 
    select(reckey, mark, hit_order, hit_sub, height_cm) %>%
    pivot_wider(names_from = c(hit_order, hit_sub), 
                names_glue ="heightcm{hit_order}_{hit_sub}", 
                values_from = height_cm, names_sort = TRUE) %>%
    rename_all(recode, "heightcm1_w" = "height_woody", 
               "heightcm2_h" = "height_herb",  
               "heightcm3_h" = "height_herb_lwr") %>%
    select(reckey, mark, any_of(c("heightcm1_t", "heightcm2_l", "heightcm3_l", 
                                  "heightcm4_l", "heightcm5_l", "heightcm6_l", 
                                  "heightcm7_l", "heightcm8_l", "heightcm9_s", 
                                  "heightcm_woody", "heightcm_herb", 
                                  "heightcm_herb_lwr")))
  
  pintercept.wide <- pintercept.hit.wide %>% 
    left_join(pintercept.dead.wide, 
              by = c("reckey" = "reckey", "mark" = "mark")) %>%
    left_join(pintercept.height.wide, 
              by = c("reckey" = "reckey", "mark" = "mark"))
  
  # get total number of complete points per reckey in order to evaluate cover
  cat("Calculating number of vaild points per LPI record...\n")
  point.count <- filter(pintercept.wide, !is.na(hit1_t), !is.na(hit9_s)) %>%
    group_by(reckey) %>%
    summarise(pt_count = n(), .groups = "drop")
  
  toc(log = TRUE, quiet = TRUE)
  dbDisconnect(con)
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
#'   filter(hits, my_field = "some parameter"). Multiple filter expressions can
#'   by union-ed with the \code{union} operator, and the filter statement can 
#'   use the (now-deprecated) \code{filter_if()}, \code{filter_at}, and 
#'   \code{filter_all} as well as the use the \code{across()} operator.
#'
#' @return A tibble containing the filter statements from \code{indicators}.
#' @export
load.indicators <- function(indicators){
  if (!is.null(indicators)){
    indicator.list <- as_tibble(read.delim(indicators, sep = "\t", 
                                           comment.char = "#",
                                           quote = "", stringsAsFactors = FALSE,
                                           blank.lines.skip = TRUE)) %>%
      mutate_if(is.character, .funs = function(x) str_trim(x))
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
#' @param filter.exp A string. A \code{dpylr::filter()} expression (or union-ed 
#'   set thereof) which is used to filter the table.
#' @param calc.type A string. The table type on which to perform the filter 
#'   expression.
#' @param hits A tibble. The table to perform the filter on.
#' @param ptcount A tibble which contains two fields,"reckey" and "pt_count",
#'   the former a string field containing a unique set of record keys and the 
#'   latter an integer field containing the number of complete points for that 
#'   record.
#'
#' @return A tibble, with the \code{filter.exp} applied to \code{hits}
#' @export
#'
#' @examples calc.indicators("tree", 'filter(hits, growth_habit_first == 
#'   "Tree", "long", pintercept.long')
calc.indicators <- function(name = NULL, filter.exp = NULL, calc.type, hits, 
                            ptcount) {
  if(calc.type == "long") {
    # if there is no filter expression, function defaults to calculating
    # species level indicators
    if (is.null(filter.exp)){
      filter.exp = paste0('filter(hits, !(hit %in% c("N", "L", "HL", "WL", ',
                          '"NL", "DS", "W", "VL", "S", "LC", "M", "D", "R", ',
                          '"CY", "EL", "GR", "CB", "ST", "BY", "BR", "None", ',
                          '"RF", "AL", "OM", "WA")) &  !is.na(hit) & hit != "")')
    }
    if (is.null(name)){
      name = as.symbol("hit")
    }
    
    # filter by user defined expression(s) to get marks with indicator present for 
    # any cover class
    ind.count.any <- eval(parse(text = filter.exp)) %>%
      mutate(indicator = !!name) %>%
      group_by(reckey, mark, hit_type, indicator) %>%
      summarise(.groups = "drop") %>% 
      group_by(reckey, hit_type, indicator) %>%
      summarize(ind_count = n(), .groups = "drop") %>%
      # takes out growth habit hits, since we can't really use them to calc cover
      # but we want to keep them in for later join to height if necessary
      mutate(ind_count = ifelse(hit_type == "g", 0, ind_count)) %>%
      group_by(reckey, indicator) %>%
      summarize(ind_count = as.integer(sum(ind_count)), .groups = "drop")
    
    ind.height.any <- eval(parse(text = filter.exp)) %>%
      mutate(indicator = !!name) %>%
      filter(!is.na(height_cm)) %>%
      group_by(reckey, indicator) %>%
      summarize(height_n = n(), 
                height_cm_mean = mean(height_cm, na.rm = T), 
                height_cm_sd = sd(height_cm, na.rm = T),
                .groups = "drop")
    
    ind.dead.any <- eval(parse(text = filter.exp)) %>%
      mutate(indicator = !!name) %>%
      filter(!is.na(dead) & hit_type == "l") %>%
      group_by(reckey, mark, indicator) %>%
      summarise(dead = all(dead), .groups = "drop") %>% 
      group_by(reckey, indicator) %>%
      summarize(dead_n = n(), 
                dead = sum(dead),
                .groups = "drop") %>%
      mutate(dead_pct = ifelse(dead_n == 0, NA_real_, dead/dead_n))
    
    # join indicator count to total point count per method instance (reckey) and 
    # calculate percent cover
    ind.cover.any.pre <- left_join(ptcount, ind.count.any,  
                                   by = c("reckey"="reckey")) %>%
      mutate(n = case_when(is.na(ind_count) ~ as.integer(0), 
                           TRUE ~ ind_count)) %>%
      mutate(hit_type = "any",
             cvr_pct = as.double(n/pt_count))
    if (class(name) == "character"){
      ind.cover.any <- ind.cover.any.pre %>% 
        mutate(indicator = case_when(is.na(indicator) ~ name,
                                     TRUE ~ indicator))
    } else {
      ind.cover.any <- ind.cover.any.pre %>% 
        filter(!is.na(ind_count) & !is.na(indicator))
    }
    ind.any <- ind.cover.any %>% 
      left_join(ind.height.any, by = c("reckey" = "reckey", 
                                       "indicator" = "indicator")) %>%
      left_join(select(ind.dead.any, reckey, indicator, dead_pct), 
                by = c("reckey" = "reckey", "indicator" = "indicator")) %>%
      mutate(height_n = ifelse(is.na(height_n), 0, height_n))
  
  # filter by user defined expression(s) to get marks with indicator present for 
  # different cover classes (top, lower, surface/basal)
    ind.count.class <- eval(parse(text = filter.exp)) %>%
      mutate(indicator = !!name) %>%
      group_by(reckey, mark, hit_type, hit_sub, indicator) %>%
      summarise(.groups = "drop") %>% 
      group_by(reckey, hit_type, hit_sub, indicator) %>%
      summarize(ind_count = n(), .groups = "drop") %>%
      mutate(ind_count = ifelse(hit_type == "g", 0, ind_count)) %>%
      group_by(reckey, hit_sub, indicator) %>%
      summarize(ind_count = as.integer(sum(ind_count)), .groups = "drop")
    
    ind.height.class <- eval(parse(text = filter.exp)) %>%
      mutate(indicator = !!name) %>%
      filter(!is.na(height_cm)) %>%
      group_by(reckey, indicator, hit_sub) %>%
      summarize(height_n = n(), height_cm_mean = mean(height_cm), 
                height_cm_sd = sd(height_cm), .groups = "drop")
    
    ind.dead.class <- eval(parse(text = filter.exp)) %>%
      mutate(indicator = !!name) %>%
      filter(!is.na(dead) & hit_type == "l") %>%
      group_by(reckey, mark, indicator, hit_sub) %>%
      summarise(dead = all(dead), .groups = "drop") %>% 
      group_by(reckey, indicator, hit_sub) %>%
      summarize(dead_n = n(), 
                dead = sum(dead),
                .groups = "drop") %>%
      mutate(dead_pct = ifelse(dead_n == 0, NA_real_, dead/dead_n))
    
    # join indicator count to total point count per method instance (reckey) and 
    # calculate percent cover
    ind.cover.class.pre <- left_join(ptcount, ind.count.class, 
                                     by = c("reckey"="reckey")) %>%
      filter(!is.na(hit_sub)) %>%
      mutate(n = case_when(is.na(ind_count) ~ as.integer(0), 
                           TRUE ~ ind_count)) %>%
      mutate(cvr_pct = as.double(n/pt_count)) %>%
      mutate(hit_type = case_when(hit_sub == "t" ~ "top",
                                  hit_sub == "l" ~ "lower",
                                  hit_sub == "s" ~ "surface",
                                  hit_sub == "w" ~ "woody",
                                  hit_sub == "h" ~ "herbaceous"))
    if (class(name) == "character"){
      ind.cover.class <- ind.cover.class.pre %>% 
        mutate(indicator = case_when(is.na(indicator) ~ name,
                                     TRUE ~ indicator))
    } else {
      ind.cover.class <- ind.cover.class.pre %>% 
        filter(!is.na(ind_count) & !is.na(indicator))
    }
    
    ind.class <- ind.cover.class %>% 
      left_join(ind.height.class, by = c("reckey" = "reckey", 
                                       "indicator" = "indicator",
                                       "hit_sub" = "hit_sub")) %>%
      left_join(select(ind.dead.class, reckey, indicator, hit_sub, dead_pct), 
                by = c("reckey" = "reckey", "indicator" = "indicator", 
                       "hit_sub" = "hit_sub")) %>%
      mutate(height_n = ifelse(is.na(height_n), 0, height_n)) %>%
      select(-hit_sub)
    
    ind.cover <-  union(ind.any, ind.class) %>% 
      select(-pt_count, -ind_count, -n)
  } else if(calc.type == "wide") {
    ind.count.any <- eval(parse(text = filter.exp)) %>%
      mutate(indicator = !!name) %>%
      group_by(reckey, mark, indicator) %>%
      summarise(.groups = "drop") %>% 
      group_by(reckey, indicator) %>%
      summarize(ind_count = n(), .groups = "drop")
    
    ind.cover.any <- left_join(ptcount, ind.count.any,  
                                   by = c("reckey"="reckey")) %>%
      mutate(n = case_when(is.na(ind_count) ~ as.integer(0), 
                           TRUE ~ ind_count)) %>%
      mutate(hit_type = "all",
             cvr_pct = as.double(n/pt_count),
             indicator = case_when(is.na(indicator) ~ name,
                                   TRUE ~ indicator),
             height_n = NA_integer_,
             height_cm_mean = NA_real_,
             height_cm_sd = NA_real_
             )

    ind.cover <- select(ind.cover.any, reckey, indicator, hit_type, 
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
#' @export
#'
#' @examples test.indicators("tree", 'filter(hits, growth_habit_first == 
#'   "Tree", "long", pintercept.long')
test.indicators <- function(name, filter.exp, calc.type, hits) {
  # filter by user defined expression(s) to get marks with indicator present for 
  # any cover class
  ind.count.any <- eval(parse(text = filter.exp)) %>%
    mutate(indicator = name)
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
#' @export 
calc.plot <-  function(imported, indicators, indicator.list = NULL){
  hit.types <- as_tibble(list(hit_type = c("any", "top", "lower", "surface", 
                                           "woody", "herbaceous", "all"),
                              calc_type = c("long", "long", "long", "long",
                                            "long", "long", "wide")))
  
  plot.head <- select(imported$transect, linekey, lineid, plotkey) %>%
    inner_join(select(imported$pinterceptmeta, reckey, survey_date, linekey), 
               by = c("linekey" = "linekey")) %>%
    mutate(survey_year = year(survey_date))
  
  plot.indicators <-  plot.head %>%
    inner_join(indicators, by = c("reckey" = "reckey")) %>% 
    rename(height_cm = height_cm_mean, 
           dead = dead_pct)
  
  # species
  if (is.null(indicator.list)){
    plot.indicators.all <- plot.indicators %>% 
      group_by(plotkey, survey_year, indicator) %>%
      summarize(.groups = "drop") %>%
      mutate(calc_type = "long") %>%
      left_join(hit.types, by=c("calc_type" = "calc_type")) %>%
      left_join(select(plot.head, plotkey, survey_year, linekey),
                by = c("plotkey" = "plotkey", "survey_year" = "survey_year"))
  # indicators
  } else {
    all.lines <- plot.head %>%
      group_by(plotkey, survey_year, linekey) %>%
      summarize(.groups = "drop")
    all.indicators <- select(indicator.list, filter.tbl, name) %>%
      left_join(hit.types, by = c("filter.tbl" = "calc_type")) %>%
      rename(calc_type = filter.tbl, indicator = name)
    plot.indicators.all <-  crossing(all.lines, all.indicators)
  }
  plot.indicators.filled <- plot.indicators.all %>%
    left_join(select(plot.indicators, -plotkey), 
              by = c("linekey" = "linekey", "survey_year" = "survey_year",
                     "indicator" = "indicator", "hit_type" = "hit_type")) %>%
    mutate(cvr_pct = coalesce(cvr_pct, 0))
  plot.indicators.sum <- plot.indicators.filled %>%
    group_by(plotkey, survey_year, calc_type, indicator, hit_type) %>%
    summarize(cvr_n = n(),
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
#' @export
do.indicator.calc <- function(imported, indicator.list = NULL, num.cores = 1, 
                                use.mc = FALSE){
  tic(msg = "Calculating indicators", quiet = TRUE)
  # for indicators
  if (!is.null(indicator.list)){
    # do indicator calculations. PSOCK type parallel is way too inefficient due
    # to the overhead of copying the base data to different sockets. FORKing  
    # shows performance improvement but is unavailable in windows. Thus the  
    # multicore functionality of the following code is useful only to POSIX type
    # systems.
    if (use.mc) {
      cat(paste("Calculating indicators on", num.cores, "cores...\n"))
      cl <- makeCluster(num.cores, type = "FORK")
      registerDoParallel(cl)
    } else {
      cat(paste("Calculating indicators...\n"))
    }
    indicators <-  suppressWarnings(  # this way we can just write 1 do/dopar
      foreach(i=1:nrow(indicator.list), .combine = bind_rows 
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
      stopCluster(cl)
    }
  } else {
    cat(paste("Calculating species codes...\n"))
    # for species 
    indicators <- calc.indicators(calc.type = "long", 
                                  hits = imported$pintercept.long,
                                  ptcount = imported$point.count)
  }
  toc(log = TRUE, quiet = TRUE)
  
  tic(msg = "Averaging values and sorting", quiet = TRUE)
  cat("Averaging indicators for plot...\n")
  plot.indicators <- calc.plot(imported = imported, indicators = indicators,
                               indicator.list = indicator.list)
  toc(log = TRUE, quiet = TRUE)
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
#'
#' @return Nothing
#' @export
do.indicator.test <- function(imported, indicator.list, test.dir, sep = ","){
  # get filtered raw lpi data instead
  tic(msg = "Exporting filtered raw data", quiet = TRUE)
  if (!dir.exists(test.dir)){
    cat(paste0("creating directory ", test.dir, "\n"))
    dir.create(test.dir)
  }
  foreach(i=1:nrow(indicator.list)
  ) %do% {
    raw.out <- test.indicators(name = indicator.list$name[i], 
                               filter.exp = indicator.list$filter.exp[i], 
                               calc.type = indicator.list$filter.tbl[i], 
                               hits = eval(parse(
                                 text = paste0("imported$pintercept.",
                                               indicator.list$filter.tbl[i]))))
    cat(paste0("Writing delimited output to ", 
               file.path(test.dir, indicator.list$name[i]), ".csv\n"))
    write.table(raw.out, 
                file = paste0(file.path(test,dir, indicator.list$name[i]), 
                                       ".csv"), 
                row.names = FALSE, na = "", col.names = TRUE, sep = sep)
  }
  toc(log = TRUE, quiet = TRUE)
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
#' @return A list containing process time elapsed information.
#' @export
main <- function(dbname, host, port, user, password, 
                 indicator.path = NULL, test = NULL, out.file = NULL, 
                 sep = ","){
  
  indicator.list <- load.indicators(indicator.path)
  if (is.null(indicator.list)){
    num.cores <- min(nrow(indicator.list), detectCores()-1)
  } else {
    num.cores <- 1
  }
  use.mc <- switch(Sys.info()[['sysname']],
                   Windows= {FALSE},
                   Linux  = {TRUE},
                   Darwin = {TRUE})
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
    tic(msg = "Writing output")
    if (file_ext(out.file) == "csv") {
      cat(paste("Writing delimited output to", out.file, "\n"))
      write.table(out.table, file = out.file, row.names = FALSE, 
                  na = "", col.names = TRUE, sep = sep)
      
    } else if (file_ext(out.file) == "rds") {
      cat(paste("Writing RDS output to", out.file, "\n"))
      saveRDS(out.table, file = out.file)
    } else {
      cat(paste("File output extension not recognized. ",
                "Skipping writing of results...\n"))
    }
  }
  toc(log = TRUE, quiet = TRUE)
  log.txt <- tic.log(format = TRUE)
  log.lst <- tic.log(format = FALSE)
  tic.clearlog()
  cat("\n")
  return(list(log.txt = log.txt, log.lst = log.lst))
}


# Run if module is called from Rscript.
if (sys.nframe() == 0) {
  option_list = list (
    make_option(opt_str = c("-p", "--port"), default = 5432, type = "integer",
                help = paste0("The Postgres connection port")),
    make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    make_option(opt_str = c("-P", "--password"),
                help = paste0("The password for the user provided.")),
    make_option(opt_str = c("-o", "--outfile"), 
                help = paste0("the output path for the calculated indicators ",
                              "(.csv, .rds")),
    make_option(opt_str = c("-i", "--indicators"), 
                help = paste0("A file path to a tab delimited list of ",
                              "indicators and their respective dplyr filter() ",
                              "strings (See indicators/* for examples). ",
                              "These indicators will be exported instead of ",
                              "species cover values")),
    make_option(opt_str = c("-t", "--test"), 
                help = paste0("A folder path for test output. ",
                              "Used in conjunction with --indicator, switches ", 
                              "indicator output to the raw filtered data used to",
                              " calculate the indicators. This can be used to ",
                              "test different dplyr filter strings.")),
    make_option(opt_str = c("-s", "--sep"), default = ",",
                help = "Separator to use for delimited output.")
    )
  opt_parser = OptionParser(usage = paste0("usage: %prog [options] ",
                                           "dbname user"), 
               option_list=option_list, prog = NULL, 
               description = paste0("\nThis script will export either cover ",
               "values, or a set of custom defined indicators for each plot ",
               "from the line-point intercept method.\ndbname is the name of ",
               "the database to connect to and user is the database user.")
  )
  args = commandArgs(trailingOnly = TRUE)
  opt = parse_args(opt_parser, positional_arguments = 2, args = args)
  
  # option checks
  if (is.null(opt$options$password)){
    opt$options$password = getPass()
  }
  if (is.null(opt$options$test) & is.null(opt$options$outfile)){
    stop("One option, either --test or --outfile is necessary. Exiting...")
  }
  if(!(file_ext(opt$options$outfile) %in% c("csv", "rds"))){
    stop("Output file must have extension .csv or .rds")
  }
  
  if (!dir.exists(dirname(opt$options$outfile))){
    stop(paste0("Directory ", dirname(opt$options$outfile), " does not exist."))
  }
  time <- main(dbname = opt$args[1], host = opt$options$host, 
               port = opt$options$port, 
               user = opt$args[2], password = opt$options$password, 
               indicator.path = opt$options$indicators, test = opt$options$test,
               out.file = opt$options$outfile,
               sep = opt$options$sep)
  
  timings <- unlist(lapply(time$log.lst, function(x) x$toc - x$tic))
  writeLines(unlist(time$log.txt))
  cat(paste0("Total script running time: ", sum(timings), 
             " sec elapsed\n"))
}
