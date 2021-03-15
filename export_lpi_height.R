#!/usr/bin/env Rscript
suppressMessages(library(odbc))
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


CalcHeightIndicators <- function(name, filter.exp, calc.type, hits) {
  # filter by user defined expression(s) to get marks with indicator present
  ind.hits <- eval(parse(text = filter.exp)) %>% 
    filter(!is.na(height_cm))
  ind.height <- group_by(ind.hits, reckey) %>%
    summarise(height_mean_cm = mean(height_cm), 
              height_sd_cm = sd(height_cm), 
              n = n()) %>% 
    ungroup() %>% 
    mutate(indicator = name) %>%
    select(reckey, indicator, height_mean_cm, height_sd_cm, n)
  return(ind.height)
}


CalcHeightSpecies <- function(hits) {
  # filter out special non-plant codes to get marks with plant codes present for 
  # any cover class
  plant.hits <- filter(hits, !is.na(hit_code) & height_cm != 0)
  plant.height <- plant.hits %>%
    group_by(reckey, hit_code) %>%
    summarise(height_mean_cm = mean(height_cm), 
              height_sd_cm = sd(height_cm), n = n()) %>% 
    ungroup()
  return(plant.height)
}


TestHeightIndicators <- function(name, filter.exp, calc.type, hits) {
  # filter by user defined expression(s) to get marks with indicator present for 
  # any cover class
  ind.count.any <- eval(parse(text = filter.exp)) %>% 
    filter(!is.na(height_cm)) %>%
    mutate(indicator = name)
  return(ind.count.any)
}


ApplyFilterExp <- function(filter.exp, hits) {
  hits.filtered <- eval(parse(text = filter.exp)) %>% 
    filter(!is.na(height_cm))
  return(hits.filtered)
}


option_list = list (
  make_option(opt_str = c("-c", "--csvout"), 
              help = paste0("the output path for the calculated indicators ",
                            "(pipe delmitted, .csv file)")),
  make_option(opt_str = c("-r", "--rout"), 
              help = paste0("the output path for the calculated indicators ",
                          "(R native, .RDS file)")),
  make_option(opt_str = c("-i", "--indicators"), 
              help = paste0("A file path to a tab delimitted list of ",
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
                                         "connection_string"), 
             option_list=option_list, prog = NULL, 
             description = paste0("\nThis script will export either species ",
             "height values, or heights for a set of custom defined indicators",
             " for each plot ",
             "from the line-point intercept method.\nThe 'connection_string' ",
             "argument is an odbc connection string for the database to ",
             "connect to.\nPostgres example: Driver={PostgreSQL UNICODE};",
             "Server=IP address;Port=5432;Database=myDataBase;Uid=myUsername;",
             "Pwd=myPassword;\n",
             "This argument can also be a file path containing the connection ",
             "string. getPass() can also be used in the con string to ",
             "provide the password interactively.\n")
)
opt = parse_args(opt_parser, positional_arguments = 1)
if (file.exists(opt$args[1])){
  con.string <- read_file(opt$args[1])
} else {
  con.string <- opt$args[1]
}
test.con.string <- str_split(con.string , "getPass()")[[1]]
if (length(test.con.string) > 1) {
  con.string <- paste0(test.con.string[1], getPass())
}

if (is.null(opt$options$test)){
  if (is.null(opt$options$rout) & is.null(opt$options$csvout)){
    stop("One option, either csvout or rout is necessary. Exiting...")
  } 
}

tic.clearlog()
tic(msg = "Loading data from data sources", quiet = TRUE)

cat("Connecting to database...\n")
con <- dbConnect(odbc(), .connection_string = con.string)

# bring in pintercept
cat("Importing table 'pastureheights'...\n")
heights.src <- as_tibble(tbl(con, "pastureheights"))

# bring in the plant table
cat("Importing table 'plant'...\n")
plant <- as_tibble(tbl(con, "plant")) %>%
  mutate(duration_first = gsub( ",.*$", "", duration),
         growth_habit_first = gsub( ",.*$", "", growth_habit))

genus.family.codes <- union(filter(plant, code_type == "Genus") %>% 
  select(accepted_symbol, code_type), 
  mutate(plant, test = str_to_upper(substring(family, 1, 6))) %>% 
    select(test) %>% 
    rename(accepted_symbol = test) %>% group_by(accepted_symbol) %>% 
    summarize() %>% ungroup() %>% filter(!is.na(accepted_symbol)) %>% 
    mutate(code_type = 'Family'))

toc(log = TRUE, quiet = TRUE)
tic(msg = "Preprocessing imported data", quiet = TRUE)
heights.long <- heights.src %>% 
  # join with plant table in order to evaluate indicator
  left_join(plant, by = c("pcode" = "accepted_symbol")) %>%
# Some genus/family codes have growth habits attached at the end of them 
# (e.g. LUPINAF) or are unknown species codes (e.g. AF01). The following added
# fields help tease these out for use in indicator calculations.
  extract("pcode", into = c("tail_id", "tail_gh"), 
        regex = "(.+)(AF|PF|AG|PG|SH|TR)$", remove = FALSE) %>%
  extract("pcode", into = c("head_gh", "head_id"), 
          regex = paste0("^(AF|PF|AG|PG|SH|TR|2FA|2FP|2GP|2SHRUB|2SUBS|2GA|",
                         "2TREE|2VW|2BRY|2MOSS)(\\d+)$"), remove = FALSE) %>%
  left_join(rename(genus.family.codes, tail_type = code_type), 
            by = c("tail_id" = "accepted_symbol")) %>%
  mutate(pcode_clean = case_when(!is.na(tail_id) & !is.na(tail_type) 
                                    ~ tail_id, TRUE ~ pcode)) %>%
  mutate(growth_habit_alt = case_when(!is.na(tail_type) ~ tail_gh,
                                      !is.na(head_gh) ~ head_gh,
                                      TRUE ~ NA_character_)) %>%
  mutate(gh_alt = case_when(growth_habit_alt %in% c("AF", "PF", "2FA", "2FP") |
                              pcode %in% c("AAFF", "PPFF", "FORB") 
                            ~ "Forb/herb",
                            growth_habit_alt %in% c("AG", "PG", "2GP", "2GA") |
                              pcode %in% c("AAGG", "PPGG", "GRASS")  
                            ~ "Graminoid",
                            growth_habit_alt %in% c("TR", "2TREE") |
                              pcode %in% c("PPTR", "TREE") ~ "Tree",
                            growth_habit_alt %in% c("SH", "2SHRUB") |
                              pcode %in% c("PPSH", "SHRUB") ~ "Shrub",
                            growth_habit_alt %in% c("2SUBS") |
                              pcode %in% c("SUBSHRUB") ~ "Subshrub",
                            growth_habit_alt %in% c("2VW") ~ "Vine",
                            growth_habit_alt %in% c("2BRY", "2MOSS") |
                              pcode %in% c("MOSS", "LICHEN") ~ "Nonvascular",
                            TRUE ~ NA_character_)) %>%
  mutate(duration_alt = case_when(
                            growth_habit_alt %in% c("AF", "AG", "2FA", "2GA") |
                            pcode %in% c("AAFF", "AAGG", "AASU") ~ "Annual",
                            growth_habit_alt %in% c("PF", "PG", "2FP", "2GP", 
                                                    "SH", "TR", "2SHRUB", 
                                                    "2SUBS", "2TREE", "2VW") |
                            pcode %in% c("PPFF", "PPGG", "PPSH", "PPTR", 
                                            "PPSU") ~ "Perennial",
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
heights.long <- arrange(heights.long, reckey, mark, hit_type, hit_sub, hit_order)
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
    
    if (use.mc) {
      cat(paste("Calculating indicators on", num.cores, "cores...\n"))
      #indicators <- bind_rows(do.call(mcmapply, args))
      cl <- makeCluster(num.cores, type = "FORK")
      registerDoParallel(cl)
      indicators <-  foreach(i=1:nrow(indicator.list), .combine = bind_rows 
      ) %dopar% {
        CalcHeightIndicators(name = indicator.list$name[i], 
                       filter.exp = indicator.list$filter.exp[i], 
                       calc.type = indicator.list$filter.tbl[i], 
                       hits = eval(parse(text = paste0("heights.", 
                                                indicator.list$filter.tbl[i]))))
      }
      stopCluster(cl)
    } else {
      # Not using parallel processing to calculate indicators (Windows SOCK 
      # parallel inefficiency with large datasets).
      cat("Not using parallel processing...\n")
      indicators <-  foreach(i=1:nrow(indicator.list), .combine = bind_rows
      ) %do% {
        cat(paste0("Calculating ", indicator.list$name[i], "\n"))
        CalcHeightIndicators(name = indicator.list$name[i], 
                       filter.exp = indicator.list$filter.exp[i], 
                       calc.type = indicator.list$filter.tbl[i], 
                       hits = eval(parse(text = paste0("heights.", 
                                                indicator.list$filter.tbl[i]))))
      }
    }
    toc(log = TRUE, quiet = TRUE)
    tic(msg = "Averaging values and sorting", quiet = TRUE)
    cat("Averaging indicators for plot...\n")
    line.indicators <- inner_join(line.head, indicators, 
                                  by = c("reckey" = "reckey"))
    plot.indicators <- group_by(line.indicators, plotkey, survey, indicator) %>%
      summarize(n = n(),
                height_mean = mean(height_mean_cm, na.rm=TRUE), 
                height_sd = sd(height_mean_cm, na.rm=TRUE)) %>%
      ungroup() %>%
      rename(height_mean_cm = height_mean, height_sd_cm = height_sd) %>%
      select(plotkey, survey, indicator, 
             starts_with("height")) %>%
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
    foreach(i=1:nrow(indicator.list)
    ) %do% {
      raw.out <- TestHeightIndicators(name = indicator.list$name[i], 
                                filter.exp = indicator.list$filter.exp[i], 
                                calc.type = indicator.list$filter.tbl[i], 
                                hits = eval(parse(text = paste0("heights.", 
                                       indicator.list$filter.tbl[i]))))
      cat(paste("Writing delimitted output to", file.path(opt$options$test, 
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
  tic(msg = "Calculating species heights", quiet = TRUE)
  cat("Calculating heights for species...\n")
  species <- CalcHeightSpecies(hits = heights.long)
  toc(log = TRUE, quiet = TRUE)
  
  # join records to plots
  tic(msg = "Averaging values and sorting", quiet = TRUE)
  cat("Averaging plant species for plot...\n")
  line.species <- inner_join(line.head, species, by = c("reckey" = "reckey"))
  plot.species <- group_by(line.species, plotkey, survey, hit_code) %>%
    summarize(n = n(),
              height_mean = mean(height_mean_cm, na.rm=TRUE), 
              height_sd = sd(height_mean_cm, na.rm=TRUE)) %>%
    ungroup() %>%
    rename(height_mean_cm = height_mean, height_sd_cm = height_sd) %>%
    left_join(plant, by = c("hit_code" = "accepted_symbol")) %>%
    select(plotkey, survey, hit_code, code_type, scientific_name, common_name, 
           starts_with("height")) %>%
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
