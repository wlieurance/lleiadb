#!/usr/bin/env Rscript
suppressMessages(library(optparse))
suppressMessages(library(RPostgres))
suppressMessages(library(rpostgis))
suppressMessages(library(doParallel))
suppressMessages(library(spdplyr))
suppressMessages(library(rgdal))
suppressMessages(library(getPass))
suppressMessages(library(stringr))
suppressMessages(library(readr))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(sf))


dominant_plot_eco <- function(esfsg.s, transect.s, point.s){
  # purpose of this function is to take the line and record based implementation
  # of ecological site and provide a dominant ecological site
  eco.plot <- esfsg %>% 
    inner_join(transect, by = c("linekey" = "linekey")) %>%
    inner_join(as_tibble(point), by = c("plotkey" = "plotkey")) %>%
    mutate(esd_length = end_mark - start_mark)
  eco.sum <- eco.plot %>% group_by(plotkey, ecoid_std) %>% 
    summarize(esdl_eco = sum(esd_length)) %>% ungroup()
  plot.sum <- eco.plot %>% group_by(plotkey) %>% 
    summarize(esdl_plot = sum(esd_length)) %>% ungroup()
  eco.ranked <- eco.sum %>% left_join(plot.sum, 
                                     by = c("plotkey" = "plotkey")) %>%
    mutate(eco_pct = case_when(esdl_plot == 0 ~ 1,
                               TRUE ~ esdl_eco/esdl_plot)) %>%
    arrange(plotkey, desc(eco_pct)) %>%
    group_by(plotkey) %>% mutate(row_n = row_number(plotkey)) %>% ungroup()
  eco.dom <- filter(eco.ranked, row_n == 1) %>% 
    select(plotkey, ecoid_std, eco_pct)
  return(eco.dom)
    
}

option_list = list (
  make_option(opt_str = c("-c", "--cover"), 
              help = paste0("the input path(s) to cover calculation RDS files ",
                            "(comma separated)")),
  make_option(opt_str = c("-t", "--height"), 
              help = paste0("the input path(s) to height calculation RDS files ",
                            "(comma separated)")),
  make_option(opt_str = c("-n", "--name"), 
              help = "the name of the layer in the output geopackage."),
  make_option(opt_str = c("-s", "--srid"), 
              help = "the epsg/srid of the exported feature.",
              default = 4269)
)
opt_parser = OptionParser(usage = paste0("usage: %prog [options] ",
                                         "indicator_choice_file out_path.gpkg ",
                                         "connection_string"), 
                          option_list=option_list, prog = NULL, 
                          description = 
  paste0("\nThis script will read in multiple .rds files produced from the ",
         "export functions and will also read in a tab delimited","
         indicator_choice_file which tells the script which indicators and ",
         "which layer calculations (any, top, lower, surface, all) to use to","
         produce a spatial feature.\n",
         "The 'connection_string' argument is an odbc connection string ",
         "for the database to connect to.\nPostgres example: ",
         "Driver={PostgreSQL UNICODE};Server=IP address;Port=5432;",
         "Database=myDataBase;Uid=myUsername;Pwd=myPassword;\n",
         "This argument can also be a file path containing the ",
         "connection string. getPass() can also be used in the con ",
         "string to provide the password interactively.\n")
)
opt = parse_args(opt_parser, positional_arguments = 3)

# change for dif filename
ind.path <- opt$args[1]
out.file <- opt$args[2]
cover.rds <- opt$options$cover
height.rds <- opt$options$height
layer.name <- opt$options$name
srid <-  opt$options$srid
if (is.null(layer.name)){
  layer.name <- "main"
}

if (!is.null(cover.rds)) {
  cover.rds <- str_split(cover.rds, ",")[[1]] 
}
if (!is.null(height.rds)) {
  height.rds <- str_split(height.rds, ",")[[1]] 
}
if (file.exists(opt$args[3])){
  con.string <- read_file(opt$args[3])
} else {
  con.string <- opt$args[3]
}
# test constring to see if it has getpass() in it and prompt/replace if so
test.con.string <- str_split(con.string , "getPass()")[[1]]
if (length(test.con.string) > 1) {
  con.string <- paste0(test.con.string[1], getPass())
}
# need to parse out con.string to feed through rpostgres specific DBI driver
# in order to have rpostgis functionality, which lets us read geometry from the
# database
con.parts <-  str_split(con.string, ";")[[1]]
con.matrix <- str_split_fixed(con.parts, "=", n = 2)
con.vec <- con.matrix[,2]
names(con.vec) <- con.matrix[,1]

con.post <- dbConnect(RPostgres::Postgres(), dbname = con.vec[["Database"]], 
                  host = con.vec[["Server"]], 
                  port = as.integer(con.vec[["Port"]]), 
                  password = con.vec[["Pwd"]], user = con.vec[["Uid"]])

cvr.indicators <- foreach(i = cover.rds, .combine = bind_rows) %do% {
  readRDS(i)
}
hgt.indicators <- foreach(i = height.rds, .combine = bind_rows) %do% {
  readRDS(i)
}
combined.indicators <- list(cover = cvr.indicators, height = hgt.indicators)
indicator.list <- as_tibble(read.delim(ind.path, sep = "\t", 
                                       comment.char = "#",
                                       quote = "", stringsAsFactors = FALSE,
                                       blank.lines.skip = TRUE, 
                                       na.strings = "")) %>%
  mutate_if(is.character, .funs = function(x) str_trim(x))

# extracts the relevant indicator and hit type from combined indicators
final.indicators <- foreach(i = 1:nrow(indicator.list), 
                            .combine = bind_rows) %do% {
  type <- ifelse(is.na(indicator.list[i,]$layer), "height", 
                 indicator.list[i,]$layer) 
  sub.indicator <- combined.indicators[[indicator.list[i,]$ind.type]] %>%
    filter(indicator == indicator.list[i,]$ind.name) %>%
    select(plotkey, survey, indicator, contains(paste0(type, "_mean"))) %>%
    rename(mean = contains(paste0(type, "_mean"))) %>%
    mutate(indicator = paste0(indicator, "_", indicator.list[i,]$ind.type))
  sub.indicator
}

wide.indicators <- spread(final.indicators, indicator, mean)

site <- as_tibble(tbl(con.post, "site"))
point <- st_read(con.post, 
                 query = "SELECT * FROM point WHERE geom IS NOT NULL") %>%
  st_zm(point, drop = TRUE, what = "ZM")
transect <- as_tibble(tbl(con.post, "transect"))
esfsg <- as_tibble(tbl(con.post, "esfsg"))
eco.plot <- dominant_plot_eco(esfsg, transect, point)

out.table <- select(point, sitekey, plotkey, survey, plotid, establish_date, 
                    latitude, longitude, elevation, elev_units, geo_datum) %>%
  inner_join(select(site, sitekey, siteid, site_name, montype, source),
                   by = c("sitekey" = "sitekey")) %>%
  left_join(eco.plot, by = c("plotkey" = "plotkey")) %>%
  left_join(wide.indicators, by = c("plotkey"="plotkey", "survey"="survey"))

out.transform <- st_transform(out.table, srid)
st_write(out.transform, out.file, driver = "GPKG", layer = layer.name)
dbDisconnect(con.post)



