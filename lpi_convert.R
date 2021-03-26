#!/usr/bin/env Rscript

libraries = c("optparse", "RPostgres", "stringr", "dplyr", "foreach", "sf", 
              "getPass", "tools", "readr", "tidyr")

for (lib in libraries){
  if(lib %in% rownames(installed.packages()) == FALSE) {
    install.packages(lib)
  }
  suppressMessages(library(lib, character.only = TRUE))
}


#' takes a filter string and applies the filter to the raw input data, in the 
#' case of multiple filters, uses dplyr::bind_rows() to combine.
#' 
#' @param in.tbl A tibble. Produced by the lpi_calc module and imported.
#' @param val A string. Determines which field is pivoted longer. 
#'   One of c("cover", "height", "dead")
#' @param filter.str A string in the format of "{indicator1}, {hit_type1}; 
#'   {indicator2}, {hit_type2}; etc." which is used to build filters for in.tbl
#' @return A tibble which has been pivoted to a wide format via 
#'   tidyr::pivot_longer() where each row has a unique plotkey and survey_year
#' @export
filter.table <- function(in.tbl, val, filter.str){
  filter.matrix <- (filter.str %>% 
    str_match_all("([^,;\\s]+)[\\s,]*([^,;\\s]+)*[\\s;$]*"))[[1]]
  
  if (nrow(filter.matrix) > 0){
    filt.tbl <- foreach (i = 1:nrow(filter.matrix), 
                         .combine = bind_rows) %do% {
      if (is.na(filter.matrix[i, 3])){
        filt.tbl <- in.tbl %>% filter(indicator == filter.matrix[i, 2])
      } else {
        filt.tbl <- in.tbl %>% filter(indicator == filter.matrix[i, 2] &
                                          hit_type == filter.matrix[i, 3])
      }
      filt.tbl                
    }
  } else {
    print(filter.str)
    print(filter.matrix)
    stop(paste0("Invalid filter string for ", val))
  }
  
  if (val == "cover"){
    values.from = c("cvr_pct_mean", "cvr_pct_sd")
    values.fill = 0
  } else if (val == "height"){
    values.from = c("height_n", "height_cm_mean", "height_cm_sd")
    values.fill = NA_real_
  } else if (val == "dead"){
    values.from = c("dead_n", "dead_pct_mean", "dead_pct_sd")
    values.fill = NA_real_
  } else {
    stop(paste0("val: ", val, " is not one of (cover, height, dead)."))
  }
  
  filt.wide <- filt.tbl %>% 
    pivot_wider(id_cols = c("plotkey", "survey_year"),
                names_from = c(indicator, hit_type),
                values_from = all_of(values.from),
                names_glue = "{indicator}_{hit_type}_{.value}",
                values_fill = values.fill, names_sort = TRUE)
  fnames <- sort(names(filt.wide)[!names(filt.wide) %in% c("plotkey", 
                                                          "survey_year")])
  filt.wide <- filt.wide %>% select(plotkey, survey_year, all_of(fnames))
  return(filt.wide)
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
#' @return Nothing.
#' @export
write.spatial <- function(tbl.out, out.path, dbname, user, password, host, 
                          port, layer.name, srid){
  con <- dbConnect(RPostgres::Postgres(), dbname = dbname, 
                   host = host, port = port, 
                   user = user, password = password)
  res <- dbExecute(con, "SET client_min_messages TO WARNING;")
  site <- as_tibble(dbGetQuery(con, "SELECT * FROM public.site;"))
  point <- st_read(con, 
                   query = "SELECT * FROM public.point WHERE geom IS NOT NULL")
    # st_zm(point, drop = TRUE, what = "ZM")
  
  final.tbl <- site %>% 
    select(sitekey, siteid, site_name, source, source_type) %>%
    inner_join(select(point, sitekey, plotkey, plotid), 
               by = c("sitekey" = "sitekey")) %>%
    inner_join(tbl.out, by = c("plotkey" = "plotkey"))
  if (srid != 4326){
    out <- st_transform(final.tbl, srid)
  } else {
    out <- final.tbl
  }
  st_write(out, out.path, layer = layer.name)
  
}

#' The main processing function for the module. 
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
#' @param cover.str A string in the format of "{indicator1}, {hit_type1}; 
#'   {indicator2}, {hit_type2}; etc." which is used to build filters for in.tbl
#'   and determines which cover fields the wide output table will have.
#' @param height.str A string. Similar to \code{cover.str}, but for height fields.
#' @param dead.str A string. Similar to \code{cover.str}, but for dead fields.
#' @returns Nothing.
#' @export
main <- function(dbname, user, password, in.path, out.path, host = "localhost", 
                 port = 5432, layer.name = "point", srid = 4326, sep = ",",
                 cover.str = NULL, height.str = NULL, dead.str = NULL){
  if (is.null(cover.str) & is.null(height.str) & is.null(dead.str)){
    stop("There must be at least 1 indicator given in cover, height, or dead.")
  }
  if (!file.exists(in.path)){
    stop(paste0("Input file ", in.path, " does not exist."))
  }
  if (!(file_ext(in.path) %in% c("rds", "csv"))){
    stop(paste0("Input file extension must have .csv or.rds extension."))
  }
  cat(paste0("Reading in data from ", in.path, "\n"))
  if(file_ext(in.path) == "rds"){
    in.tbl <- readRDS(in.path)
  } else {
    in.tbl <- as_tibble(read_delim(in.path, delim = sep))
  }
  
  tbl.head <- in.tbl %>% group_by(plotkey, survey_year) %>%
    summarize(line_n = max(cvr_n), .groups = "drop")
  
  if (!is.null(cover.str)){
    filt.tbl.cvr <- filter.table(in.tbl = in.tbl, val = "cover", 
                                 filter.str = cover.str)
    tbl.head <- tbl.head %>% 
      left_join(filt.tbl.cvr, by = c("plotkey" = "plotkey", 
                                     "survey_year" = "survey_year"))
  }
  
  if (!is.null(height.str)){
    filt.tbl.hgt <- filter.table(in.tbl = in.tbl, val = "height", 
                                 filter.str = height.str)
    tbl.head <- tbl.head %>% 
      left_join(filt.tbl.hgt, by = c("plotkey" = "plotkey", 
                                     "survey_year" = "survey_year"))
  }
  
  if (!is.null(dead.str)){
    filt.tbl.dead <- filter.table(in.tbl = in.tbl, val = "dead", 
                                 filter.str = dead.str)
    tbl.head <- tbl.head %>% 
      left_join(filt.tbl.dead, by = c("plotkey" = "plotkey", 
                                     "survey_year" = "survey_year"))
  }
  write.spatial(tbl.out = tbl.head, out.path = out.path, dbname = dbname, 
                user = user, password = password, host = host, 
                port = port, layer.name = layer.name, srid = srid)
  
}

# Run if module is called from Rscript.
if (sys.nframe() == 0) {
  args = commandArgs(trailingOnly = TRUE)
  option_list = list (
    make_option(opt_str = c("-p", "--port"), default = 5432, type = "integer",
                help = paste0("The Postgres connection port")),
    make_option(opt_str = c("-H", "--host"), default = "localhost",
                help = paste0("The host name or ip address of the connection")),
    make_option(opt_str = c("-P", "--password")),
    make_option(opt_str = c("-n", "--name"), default = "point",
                help = "The name of the layer in the output geopackage."),
    make_option(opt_str = c("-S", "--srid"), 
                help = "The epsg/srid of the exported feature.",
                default = 4326),
    make_option(opt_str = c("-s", "--sep"), 
                help = "The delimiter, in case of a csv file.",
                default = ","),
    make_option(opt_str = c("-c", "--cover"),
                help = paste0("string which restricts output fields to ",
                              "specific cover indicators using the following ",
                              "format: \"indicator_name1, hit_type1; ",
                              "indicator_name2, hit_type2; etc.\", where ",
                              "hit type is either blank or one of (any, all, ",
                              "top, lower, surface, herbaceous, woody, all)")),
    make_option(opt_str = c("t", "--height"),
                help = paste0("string which restricts output fields to ",
                              "specific height indicators.")),
    make_option(opt_str = c("d", "--dead"),
                help = paste0("string which restricts output fields to ",
                              "specific dead indicators."))
  )
  opt_parser = OptionParser(usage = paste0("usage: %prog [options] ",
                                           "dbname user in_file out_path"), 
                            option_list=option_list, prog = NULL, 
                            description = 
    paste0("\nThis script will read in a csv/rds file produced from the ",
           "lpi_calc module, restrict the indicators according to option ", 
           "input, convert the result to wide format, and then output to ",
           "a spatial output based on the file extension (dsn ",
           "needs to be something sf::st_write() can guess).")
  )
  opt = parse_args(opt_parser, positional_arguments = 4, args = args)
  
  # option checks
  if (is.null(opt$options$password)){
    opt$options$password = getPass()
  }
  main(dbname = opt$args[1], host = opt$options$host, port = opt$options$port, 
       user = opt$args[2], password = opt$options$password, 
       layer.name = opt$options$name, srid = opt$options$srid, 
       cover.str = opt$options$cover, height.str = opt$options$height,
       dead.str = opt$options$dead, in.path = opt$args[3], 
       out.path = opt$args[4], sep = opt$options$sep)
  cat("Script finished.\n")
}



