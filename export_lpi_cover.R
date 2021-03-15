#!/usr/bin/env Rscript
suppressMessages(library(Rpostgres))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(dbplyr))
suppressMessages(library(glue))
suppressMessages(library(getPass))
suppressMessages(library(parallel))
suppressMessages(library(stringi))
suppressMessages(library(stringr))
suppressMessages(library(doParallel))
suppressMessages(library(tictoc))
suppressMessages(library(optparse))
suppressMessages(library(readr))


CalcIndicators <- function(name, filter.exp, calc.type, hits, ptcount) {
  # filter by user defined expression(s) to get marks with indicator present for 
  # any cover class
  ind.count.any <- eval(parse(text = filter.exp)) %>%
    group_by(reckey, mark) %>%
    summarise() %>% 
    ungroup() %>%
    group_by(reckey) %>%
    summarize(ind_count = n())
  
  # join indicator count to total point count per method isntance (reckey) and 
  # calculate percent cover
  ind.cover.any <- left_join(ptcount, ind.count.any,  
                             by = c("reckey"="reckey")) %>%
    mutate(n = case_when(is.na(ind_count) ~ as.integer(0), 
                         TRUE ~ ind_count)) %>%
    mutate(indicator = name, hit_type = case_when(calc.type == "long" ~ "any",
                                                  calc.type == "wide" ~ "all",
                                                  TRUE ~ NA_character_),
           cvr_pct = as.double(n/pt_count))
  
  # filter by user defined expression(s) to get marks with indicator present for 
  # different cover classes (top, lower, surface/basal)
  if(calc.type == "long") {
    ind.count.class <- eval(parse(text = filter.exp)) %>%
      group_by(reckey, mark, hit_type) %>%
      summarise() %>% 
      ungroup() %>%
      group_by(reckey, hit_type) %>%
      summarize(ind_count = n())
    
    # join indicator count to total point count per method instance (reckey) and 
    # calculate percent cover
    ind.cover.class <- left_join(ptcount, ind.count.class, 
                                 by = c("reckey"="reckey")) %>%
      filter(!is.na(hit_type)) %>%
      mutate(n = case_when(is.na(ind_count) ~ as.integer(0), 
                           TRUE ~ ind_count)) %>%
      mutate(indicator = name, cvr_pct = as.double(n/pt_count))
    
    ind.cover <-  union(ind.cover.any, ind.cover.class) %>% 
      select(-pt_count, -ind_count, -n) %>%
      spread(key = hit_type, value = cvr_pct, fill = 0) %>% 
      select(reckey, indicator, cvr_pct_any = matches("any"), 
             cvr_pct_top = matches("top"), cvr_pct_l = matches("l"),
             cvr_pct_surf = matches("surf"))
  } else {
    ind.cover <- select(ind.cover.any, reckey, indicator, cvr_pct_all = cvr_pct)
  }
  return(ind.cover)
}


CalcSpecies <- function(hits, ptcount) {
  # filter out special non-plant codes to get marks with plant codes present for 
  # any cover class
  plant.hits <- filter(hits, !(hit_code %in% c("N", "L", "HL", "WL", 
                    "NL", "DS", "W", "VL", "S", "LC", "M", "D", "R", "CY", "EL", 
                    "GR", "CB", "ST", "BY", "BR", "None", "RF", "AL", "OM", 
                    "WA")) &  !is.na(hit_code) & hit_code != "")
  plant.count.any <- plant.hits %>%
    group_by(reckey, mark, hit_code) %>%
    summarise() %>% 
    ungroup() %>%
    group_by(reckey, hit_code) %>%
    summarize(plant_count = n())
  
  # join indicator count to total point count per method isntance (reckey) and 
  # calculate percent cover
  plant.cover.any <- left_join(ptcount, plant.count.any,  
                             by = c("reckey"="reckey")) %>%
    filter(!is.na(plant_count) & !is.na(hit_code)) %>%
    mutate(hit_type = "any", cvr_pct = as.double(plant_count/pt_count))
  
  # filter out special non-plant codes to get marks with plant codes present for 
  # different cover classes (top, lower, surface/basal)
  plant.count.class <- plant.hits %>%
    group_by(reckey, mark, hit_type, hit_code) %>%
    summarise() %>% 
    ungroup() %>%
    group_by(reckey, hit_type, hit_code) %>%
    summarize(plant_count = n())
  
  # join indicator count to total point count per method instance (reckey) and 
  # calculate percent cover
  plant.cover.class <- left_join(ptcount, plant.count.class,  
                               by = c("reckey"="reckey")) %>%
    filter(!is.na(plant_count) & !is.na(hit_code) & !is.na(hit_type)) %>%
    mutate(cvr_pct = as.double(plant_count/pt_count))
  
  # union the 2 hit_type tables and transpose hit_types into columns
  plant.cover <-  union(plant.cover.any, plant.cover.class) %>% 
    select(-pt_count, -plant_count) %>%
    spread(key = hit_type, value = cvr_pct, fill = 0) %>% 
    select(reckey, hit_code, cvr_pct_any = matches("any"), 
           cvr_pct_top = matches("top"), cvr_pct_l = matches("l"),
           cvr_pct_surf = matches("surf"))
  return(plant.cover)
}


TestIndicators <- function(name, filter.exp, calc.type, hits) {
  # filter by user defined expression(s) to get marks with indicator present for 
  # any cover class
  ind.count.any <- eval(parse(text = filter.exp)) %>%
  mutate(indicator = name)
  return(ind.count.any)
}


main <- function(dbname, host, port, user, password){
  tic.clearlog()
  tic(msg = "Loading data from data sources", quiet = TRUE)
  
  con <<- dbConnect(RPostgres::Postgres(), dbname = dbname, 
                    host = host, port = port, 
                    user = user, password = password)
  res <- dbExecute(con, "SET client_min_messages TO WARNING;")
  
  # bring in pintercept
  cat("Importing table 'pintercept'...\n")
  pintercept <- as_tibble(tbl(con, "pintercept"))
  
  # bring in the plant table
  cat("Importing table 'plant'...\n")
  plant <- as_tibble(tbl(con, "plant")) %>%
    mutate(duration_first = gsub( ",.*$", "", duration),
           growth_habit_first = gsub( ",.*$", "", growth_habit))
  
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
  
  toc(log = TRUE, quiet = TRUE)
  tic(msg = "Preprocessing imported data", quiet = TRUE)
  # transpose pintercept.wide table to be useful for summarise()
  cat("Processing species codes in table 'pintercept'...\n")
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
    mutate(hit_code_clean = case_when(!is.na(tail_id) & !is.na(tail_type) 
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
  pintercept.long <- arrange(pintercept.long, reckey, mark, hit_order)
  
  # get total number of complete points per reckey in order to evaluate cover  
  point.count <- filter(pintercept.wide, !is.na(hit1_top), !is.na(hit9_surf)) %>%
    group_by(reckey) %>%
    summarise(pt_count = n())
  toc(log = TRUE, quiet = TRUE)
  
  tic(msg = "Loading header tables")
  cat("Loading header tables for joining...\n")
  site <- as_tibble(tbl(con, "site"))
  point <- as_tibble(tbl(con, "point"))
  transect <- as_tibble(tbl(con, "transect"))
  pinterceptmeta <- as_tibble(tbl(con, "pinterceptmeta"))
  
  plot.head <- select(site, c("sitekey", "siteid", "site_name", "source", 
                              "montype")) %>%
    inner_join(point, by = c("sitekey" = "sitekey")) %>%
    select(-geom)
  
  
  line.head <- inner_join(plot.head,transect, by = c("plotkey" = "plotkey")) %>%
    select(-geom) %>%
    inner_join(pinterceptmeta, by = c("linekey" = "linekey"))
  toc(log = TRUE, quiet = TRUE)
  
  # Indicator calculation and processing
  if (!is.null(opt$options$indicators)){
    indicator.list <- as_tibble(read.delim(opt$options$indicators, sep = "\t", 
                                           comment.char = "#",
                                           quote = "", stringsAsFactors = FALSE,
                                           blank.lines.skip = TRUE)) %>%
      mutate_if(is.character, .funs = function(x) str_trim(x))
    
    if (is.null(opt$options$test)){
      tic(msg = "Calculating indicators", quiet = TRUE)
      # do indicator calculations. PSOCK type parallel is way too inefficient due
      # to the overhead of copying the base data to different sockets. FORKing  
      # shows performance improvement but is unavailable in windows. Thus the  
      # multicore functionality of the following code is useful only to POSIX type
      # systems.
      num.cores <- min(nrow(indicator.list), detectCores()-1)
      use.mc <- switch(Sys.info()[['sysname']],
                       Windows= {FALSE},
                       Linux  = {TRUE},
                       Darwin = {TRUE})
      
      # create a list of arguments to pass to either mapply or mcmapply
      # needed for use of mcmapply
      # args <- list(FUN=CalcIndicators, name = indicator.list$name, 
      #              filter.exp = indicator.list$filter.exp,
      #              calc.type = indicator.list$filter.tbl,
      #              # chooses which table to send to the function based on the 
      #              # filter.tbl variable in the data frame
      #              hits = lapply(parse(text = paste0("pintercept.", 
      #                                         indicator.list$filter.tbl)), 
      #                            FUN = eval),
      #              ptcount = list(point.count),
      #              SIMPLIFY = FALSE)
      if (use.mc) {
        cat(paste("Calculating indicators on", num.cores, "cores...\n"))
        #indicators <- bind_rows(do.call(mcmapply, args))
        cl <- makeCluster(num.cores, type = "FORK")
        registerDoParallel(cl)
        indicators <-  foreach(i=1:nrow(indicator.list), .combine = bind_rows 
                               #, .packages = c("dplyr", "tidyr", "stringr") 
                               #, .export = c("pintercept.long", "pintercept.wide")
        ) %dopar% {
          CalcIndicators(name = indicator.list$name[i], 
                         filter.exp = indicator.list$filter.exp[i], 
                         calc.type = indicator.list$filter.tbl[i], 
                         hits = eval(parse(text = paste0("pintercept.", 
                                                         indicator.list$filter.tbl[i]))), 
                         ptcount = point.count)
        }
        stopCluster(cl)
      } else {
        # Not using parallel processing to calculate indicators (Windows SOCK 
        # parallel inefficiency with large datasets).
        cat("Not using parallel processing...\n")
        #indicators <- bind_rows(do.call(mapply, args))
        indicators <-  foreach(i=1:nrow(indicator.list), .combine = bind_rows
        ) %do% {
          cat(paste0("Calculating ", indicator.list$name[i], "\n"))
          CalcIndicators(name = indicator.list$name[i], 
                         filter.exp = indicator.list$filter.exp[i], 
                         calc.type = indicator.list$filter.tbl[i], 
                         hits = eval(parse(text = paste0("pintercept.", 
                                                         indicator.list$filter.tbl[i]))), 
                         ptcount = point.count)
        }
      }
      toc(log = TRUE, quiet = TRUE)
      tic(msg = "Averaging values and sorting", quiet = TRUE)
      cat("Averaging indicators for plot...\n")
      line.indicators <- inner_join(line.head, 
                                    indicators, by = c("reckey" = "reckey"))
      plot.indicators <- group_by(line.indicators, plotkey, survey, indicator) %>%
        summarize(n = n(), 
                  cvr_pct_any_mean = mean(cvr_pct_any, na.rm=TRUE), 
                  cvr_pct_any_sd = sd(cvr_pct_any, na.rm=TRUE),
                  cvr_pct_top_mean = mean(cvr_pct_top, na.rm=TRUE), 
                  cvr_pct_top_sd = sd(cvr_pct_top, na.rm=TRUE),
                  cvr_pct_l_mean = mean(cvr_pct_l, na.rm=TRUE), 
                  cvr_pct_l_sd = sd(cvr_pct_l, na.rm=TRUE),
                  cvr_pct_surf_mean = mean(cvr_pct_surf, na.rm=TRUE), 
                  cvr_pct_surf_sd = sd(cvr_pct_surf, na.rm=TRUE), 
                  cvr_pct_all_mean = mean(cvr_pct_all, na.rm=TRUE), 
                  cvr_pct_all_sd = sd(cvr_pct_all, na.rm=TRUE)) %>%
        ungroup() %>%
        arrange(plotkey, survey, indicator)
      out.table <- plot.indicators
      toc(log = TRUE, quiet = TRUE)
    } else {
      # get filtered raw lpi data instead
      tic(msg = "Exporting filtered raw data", quiet = TRUE)
      if (!dir.exists(opt$options$test)){
        cat(paste0("creating directory ", opt$options$test, "\n"))
        dir.create(opt$options$test)
      }
      ifelse(!dir.exists(opt$options$test), dir.create(opt$options$test), FALSE)
      foreach(i=1:nrow(indicator.list)
      ) %do% {
        raw.out <- TestIndicators(name = indicator.list$name[i], 
                                  filter.exp = indicator.list$filter.exp[i], 
                                  calc.type = indicator.list$filter.tbl[i], 
                                  hits = eval(parse(text = paste0("pintercept.", 
                                                                  indicator.list$filter.tbl[i]))))
        cat(paste0("Writing delimitted output to ", file.path(opt$options$test, 
                                                              indicator.list$name[i]), ".csv\n"))
        write.table(raw.out, file = paste0(file.path(opt$options$test, 
                                                     indicator.list$name[i]), 
                                           ".csv"), 
                    row.names=FALSE, na="", col.names=TRUE, sep="|")
      }
      toc(log = TRUE, quiet = TRUE)
    }
  } else {
    # calculate species level indicators
    tic(msg = "Calculating species cover", quiet = TRUE)
    cat("Calculating cover for species...\n")
    species <- CalcSpecies(hits = pintercept.long, ptcount = point.count)
    
    # create plot level species list in order create 0 values for missing plants
    line.species <- inner_join(line.head, species, 
                               by = c("reckey" = "reckey")) %>%
      group_by(plotkey, survey, hit_code) %>% summarize() %>% ungroup() %>%
      inner_join(select(transect, plotkey, linekey), 
                 by = c("plotkey" = "plotkey")) %>%
      inner_join(select(pinterceptmeta, linekey, reckey), 
                 by = c("linekey" = "linekey")) %>%
      left_join(species, by = c("reckey" = "reckey", "hit_code" = "hit_code")) %>%
      replace_na(list(cvr_pct_any = 0, cvr_pct_top = 0, cvr_pct_l = 0, 
                      cvr_pct_surf = 0))
    toc(log = TRUE, quiet = TRUE)
    
    tic(msg = "Averaging values and sorting", quiet = TRUE)
    cat("Averaging plant species for plot...\n")
    plot.species <- group_by(line.species, plotkey, survey, hit_code) %>%
      summarize(n = n(),
                cvr_pct_any_mean = mean(cvr_pct_any, na.rm=TRUE), 
                cvr_pct_any_sd = sd(cvr_pct_any, na.rm=TRUE),
                cvr_pct_top_mean = mean(cvr_pct_top, na.rm=TRUE), 
                cvr_pct_top_sd = sd(cvr_pct_top, na.rm=TRUE),
                cvr_pct_l_mean = mean(cvr_pct_l, na.rm=TRUE), 
                cvr_pct_l_sd = sd(cvr_pct_l, na.rm=TRUE),
                cvr_pct_surf_mean = mean(cvr_pct_surf, na.rm=TRUE), 
                cvr_pct_surf_sd = sd(cvr_pct_surf, na.rm=TRUE)) %>%
      ungroup() %>%
      left_join(plant, by = c("hit_code" = "accepted_symbol")) %>%
      select(plotkey, survey, hit_code, code_type, scientific_name, common_name, 
             starts_with("cvr")) %>%
      arrange(plotkey, survey, hit_code)
    out.table <- plot.species
    toc(log = TRUE, quiet = TRUE)
  }
  if (is.null(opt$options$test)){
    tic(msg = "Writing output")
    dbDisconnect(con)
    if (!is.null(opt$options$csvout)) {
      cat(paste("Writing delimitted output to", opt$options$csvout, "\n"))
      write.table(out.table, file = opt$options$csvout, row.names=FALSE, 
                  na="", col.names=TRUE, sep="|")
      
    }
    if (!is.null(opt$options$rout)) {
      cat(paste("Writing RDS output to", opt$options$rout, "\n"))
      saveRDS(out.table, file = opt$options$rout)
    }
  }
  toc(log = TRUE, quiet = TRUE)
  log.txt <- tic.log(format = TRUE)
  log.lst <- tic.log(format = FALSE)
  tic.clearlog()
  cat("\n")
  writeLines(unlist(log.txt))
  timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
  cat(paste0("Total script running time: ", sum(timings), " sec elapsed\n"))
}


# run only if called from a script.
if (sys.nframe() == 0) {
  args = commandArgs(trailingOnly = TRUE)
  option_list = list (
    make_option(opt_str = c("-p", "--port"), default = 5432, type = "integer",
                help = paste0("The Postgres connection port")),
    make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    make_option(opt_str = c("-w", "--password"),
                help = paste0("The password for the user provided.")),
    make_option(opt_str = c("-c", "--csvout"), 
                help = paste0("the output path for the calculated indicators ",
                              "(pipe delimited, .csv file)")),
    make_option(opt_str = c("-r", "--rout"), 
                help = paste0("the output path for the calculated indicators ",
                            "(R native, .RDS file)")),
    make_option(opt_str = c("-i", "--indicators"), 
                help = paste0("A file path to a tab delimited list of ",
                              "indicators and their respective dplyr filter() ",
                              "strings (See ind_cvr_example.txt). ",
                              "These indicators will be exported instead of ",
                              "species cover values")),
    make_option(opt_str = c("-t", "--test"), 
                help = paste0("A folder path for test output. ",
                              "Used in conjunction with --indicator, switches ", 
                              "indicator output to the raw filtered data used to",
                              " calculate the indicators. This can be used to ",
                              "test different dplyr filter strings."))
    )
  opt_parser = OptionParser(usage = paste0("usage: %prog [options] ",
                                           "dbname user"), 
               option_list=option_list, prog = NULL, 
               description = paste0("\nThis script will export either cover ",
               "values, or a set of custom defined indicators for each plot ",
               "from the line-point intercept method.\ndbname is the name of ",
               "the database to connect to and user is the database user.")
  )
  opt = parse_args(opt_parser, positional_arguments = 2, args = args)
  
  if (is.null(opt$options$password)){
    opt$options$password = getPass()
  }
  if (is.null(opt$options$test)){
    if (is.null(opt$options$rout) & is.null(opt$options$csvout)){
      stop("One option, either csvout or rout is necessary. Exiting...")
    } 
  }
  main(dbname = opt$args[1], host = opt$options$host, port = opt$options$port, 
       user = opt$args[2], password = opt$options$password)
}



