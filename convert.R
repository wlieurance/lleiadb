library(stringi)
library(tidyr)
library(lutz)
library(foreach)
library(sf)
library(lubridate)


### LMF conversion functions ###
# site conversion
site.lmf <- function(point.s, src.name){
  site <- point.s %>% rename_with(tolower) %>% 
    select(state, county, psu, own) %>%
    mutate(sitekey = paste0(sprintf("%02d", state), sprintf("%02d", county),
                            psu),
           own = ifelse(own == "9", "BLM", own)) %>% 
    rename(siteid = psu) %>% 
    select(sitekey, siteid, own) %>%
    group_by(sitekey, siteid, own) %>% summarize(.groups = "drop") %>% 
    group_by(sitekey, siteid) %>% 
    summarize(ownership = paste0(own, collapse = ", "), .groups = "drop") %>% 
    mutate(source = src.name, source_type = "lmf") %>%
    mutate_if(is.character, 
              list(~na_if(str_trim(stri_enc_toutf8(., validate=TRUE)), "")))
  return(site)
}

# point conversion
point.lmf <- function(point.s, pointcoordinates.s, gps.s) {
  pc <- pointcoordinates.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  gps <-  gps.s %>%rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  point <- point.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point)) %>%
    left_join(select(pc, plotkey, field_latitude, 
                     field_longitude), 
              by = c("plotkey" = "plotkey")) %>%
    left_join(select(gps, plotkey, nogps, capdate, elevation), 
              by = c("plotkey" = "plotkey")) %>%
    mutate(sitekey = paste0(state, county, psu), 
           state = sprintf("%02d", state), county = sprintf("%03d", county),
           own = ifelse(own == "9", "BLM", own), landform_major = NA_character_, 
           landform_minor = NA_character_, 
           aspect = case_when(slope_aspect == "N" ~ as.integer(0),
                              slope_aspect == "NE" ~ as.integer(45),
                              slope_aspect == "E" ~ as.integer(90),
                              slope_aspect == "SE" ~ as.integer(135),
                              slope_aspect == "S" ~ as.integer(180),
                              slope_aspect == "SW" ~ as.integer(225),
                              slope_aspect == "W" ~ as.integer(270),
                              slope_aspect == "NW" ~ as.integer(315),
                              TRUE ~ NA_integer_),
           elevation = elevation / 3.28084, geo_datum = "NAD83", 
           elev_units = "m", field_longitude = -field_longitude,
           plotid = as.character(point),
           establish_date = as.POSIXct(capdate, format="%Y/%m/%d %H:%M:%S", 
                                       tz = "UTC")) %>%
    rename(latitude = field_latitude, longitude = field_longitude) %>% 
    # in order to crate R spatial feature, add this code
    # st_as_sf(crs = 4269, coords = c("longitude", "latitude", "elevation"), 
    #          remove = FALSE) %>%
    mutate(longitude = ifelse(longitude == 0, NA, longitude),
           latitude = ifelse(latitude == 0, NA, latitude),
           elevation = ifelse(elevation == 0, NA, elevation)) %>%
    select(plotkey, sitekey, plotid, survey, establish_date, state, 
           county, mlra, own, landform_major, landform_minor, 
           vertical_slope_shape, horizontal_slope_shape, slope_percent, 
           slope_length, aspect, ssaid, musym, component_name, component_id, 
           soil_confidence_rating, apparent_trend, grazing_use, hayed, latitude, 
           longitude, elevation, nogps, geo_datum, elev_units)
  return(point)
}


# disturbance conversion
disturb.lmf <- function(disturbance.s, gps.s){
  conv.vars <- c("cultivation", "mowing", "hay_removal", "heavy_machinery", 
                 "seedbed_preparation", "livestock_tanks", 
                 "livestock_heavy_use", "livestock_grazing", "insects", 
                 "small_rodents", "non_rodent_animals", "wildlife_grazing", 
                 "mining_equipment_operations", "recreation_foot_traffic", 
                 "recreation_vehicles_bikes", "livestock_walkways", 
                 "roads_dirt", "roads_gravel", "roads_paved", "drainage", 
                 "underground_utilities", "overhead_transmission_lines", 
                 "construction", "water_ponding", "soil_deposition_water", 
                 "soil_deposition_wind", "water", "wind", "transported_fill", 
                 "wildfire", "prescribed_fire", "fire_fighting_operations", 
                 "brush_management_chemical", "brush_management_mechanical", 
                 "brush_management_biological")
  disturb <- disturbance.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  gps <-  gps.s %>%rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  disturbance <- select(disturb, -state, -county, -psu, -point) %>%
    left_join(select(gps, plotkey, capdate), 
              by = c("plotkey" = "plotkey")) %>%
    mutate(survey_date = as.POSIXct(capdate, format="%Y/%m/%d %H:%M:%S", 
                                    tz = "UTC")) %>%
    mutate_at(conv.vars, .funs = list(function(x) (x == 'Y'))) %>%
    select(-capdate)
  return(disturbance)
}

# transect conversion
transect.lmf <- function(pintercept.s, pastureheights.s, gps.s) {
  pi <- pintercept.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  ph <- pastureheights.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  gps <-  gps.s %>%rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  transect <- select(pi, survey, state, county, psu, point, 
                     transect, plotkey) %>%
    union(select(ph, survey, state, county, psu, point, 
                 transect, plotkey)) %>%
    group_by(survey, state, county, psu, point, transect, plotkey) %>%
    summarise(.groups = "drop") %>%
    mutate(
      lineid = transect,
      linekey = paste0(plotkey, str_to_upper(str_sub(transect, -2))),
      azimuth = case_when(
        transect == "nwse" ~ 135, 
        transect == "nesw" ~ 225,
        TRUE ~ NA_real_),
      azimuth_type = "magnetic",
      # elev_units = "ft",
      transect_units = "ft",
      transect_length = 150,
    ) %>%
    left_join(gps, by = c("plotkey" = "plotkey")) %>%
    mutate(establish_date = as.POSIXct(capdate, 
                                       format="%Y/%m/%d %H:%M:%S", 
                                       tz = "UTC")) %>%
    select(linekey, plotkey, establish_date, lineid, azimuth, 
           azimuth_type, transect_length, transect_units) %>%
    # converts all blank strings to NAs
    mutate_if(is.character, 
              list(~na_if(str_trim(stri_enc_toutf8(., validate=TRUE)), "")))
  return(transect)
}

# ecosite / esfsg functions. These are a little tricky because we are taking 
# what is essentially a line method stored via plotkey and transect name in lmf
# and combining what is a plot method with esd states stored via reckey for
# repeated visits in the dima. We need to convert to a line based method with a
# reckey in case of repeated measures on the same line for both methods.
esfsg.lmf <- function(esfsg.s, transect.f, point.f){
  eco <- esfsg.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  esfsg <- select(eco, -survey, -state, -county, -psu, -point) %>%
    mutate(ecoid_std = paste0(esfsg_mlra, esfsg_site, esfsg_state)) %>%
    left_join(transect.f, by = c("plotkey" = "plotkey")) %>%
    rename(survey_date = establish_date, notes = esfsg_name) %>%
    filter(coverage == "all" | lineid == coverage) %>%
    mutate(end_mark = case_when(end_mark == 0 & 
                                  !is.na(transect_length) ~ 
                                  as.double(transect_length),
                                TRUE ~ as.double(end_mark)),
           start_mark = as.double(start_mark),
           linekey = ifelse(is.na(linekey), paste0(plotkey, "ES"), linekey))
  
  # Necessary to check/add dummy lines since we are converting what is stored as 
  # a plot method to a line method (i.e plotkey -> linekey). Should in theory be
  # an empty tibble for lmf databases.
  new.lines <- select(esfsg, plotkey, linekey) %>%
    anti_join(transect.f, by = c("linekey" = "linekey")) %>%
    left_join(select(point.f, plotkey, establish_date), 
              by = c("plotkey" = "plotkey")) %>% 
    mutate(lineid = "esfsg") %>%
    select(linekey, plotkey, establish_date, lineid)
  transect.new <- dplyr::bind_rows(transect.f, new.lines)
  
  esfsg.new <- mutate(esfsg, reckey = paste0(linekey, "9")) %>% 
    select(linekey, reckey, seqnum, survey_date, ecoid_std, start_mark, 
           end_mark, notes) %>%
    mutate_if(is.character, 
              list(~na_if(str_trim(stri_enc_toutf8(., validate=TRUE)), "")))
  return(list(esfsg = esfsg.new, transect = transect.new))
}

# point intercept metadata conversion
pimeta.lmf <- function(pintercept.s, pastureheights.s, gps.s) {
  pi <- pintercept.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  ph <- pastureheights.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  gps <-  gps.s %>%rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  pinterceptmeta <- select(pi, survey, state, county, 
                           psu, point, transect, plotkey) %>%
    union(select(ph, survey, state, county, psu, point, 
                 transect, plotkey)) %>%
    group_by(survey, state, county, psu, point, transect, plotkey) %>%
    summarise(.groups = "drop") %>%
    mutate(
      linekey = paste0(plotkey, str_to_upper(str_sub(transect, -2))),
      reckey = paste0(plotkey, str_to_upper(str_sub(transect, -2)), '1')
    ) %>%
    left_join(gps, by = c("plotkey" = "plotkey")) %>%
    mutate(survey_date = as.POSIXct(capdate, 
                                    format="%Y/%m/%d %H:%M:%S", 
                                    tz = "UTC")) %>%
    mutate(survey = as.integer(strftime(survey_date, "%Y"))) %>%
    select(linekey, reckey, survey_date) %>%
    mutate_if(is.character, 
              list(~na_if(str_trim(stri_enc_toutf8(., validate=TRUE)), "")))
  return(pinterceptmeta)
}

# point intercept conversion
pi.lmf <- function(pintercept.s) {
  pi <- pintercept.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  pintercept <- select(pi, transect, mark, hit1, hit2, hit3, hit4, 
                       hit5, hit6, basal, nonsoil, 
                       sage_shape = sagebrush_shape, 
                       sage_spp = sagebrush_spp, plotkey) %>%
    # Half of all hits @ 75ft are identical, which make sense since the 
    # transects intersect here and the pin will be dropped at almost exactly the 
    # same point. In order produce accurate cover values where a point is not 
    # sampled twice, we can remove one of the 75 foot rows from one of the 
    # transects. Not done here but code left for reference.
    # filter(!(transect == 'nwse' & mark == 75)) %>%
    mutate(
      reckey = paste0(plotkey, str_to_upper(str_sub(transect, -2)), '1'),
      hit1_top = hit1, hit2_l = hit2, hit3_l = hit3,
      hit4_l = hit4, hit5_l = hit5, hit6_l = hit6,
      hit9_surf = case_when(basal == "None" & nonsoil == "" ~ "S", 
                            basal == "None" & nonsoil != "" ~ nonsoil,
                            nonsoil == "W" ~ nonsoil, 
                            TRUE ~ basal),
      sagebrush_shape = case_when(sage_shape == 0 ~ NA_character_,
                                  sage_shape == 1 ~ "C",
                                  sage_shape == 2 ~ "S",
                                  sage_shape == 3 ~ "M",
                                  TRUE ~ NA_character_),
      sagebrush_spp = sage_spp
    ) %>%
    mutate_if(is.character, 
              list(~na_if(str_trim(stri_enc_toutf8(., validate=TRUE)), ""))) %>%
    select(reckey, mark, hit1_top, hit2_l, hit3_l, hit4_l, hit5_l, hit6_l, 
           hit9_surf, sagebrush_spp, sagebrush_shape)
  return(pintercept)
}

# pasture heights conversion
ph.lmf <- function(pastureheights.s) {
  ph <- pastureheights.s %>% rename_with(tolower) %>%
    mutate(plotkey = paste0(survey, state, county, psu, point))
  pastureheights.wide <- select(ph, transect, mark = distance, 
                                hplant_src = hplant, hheight_src = height, 
                                wplant_src = wplant, wheight_src = wheight, 
                                plotkey)  %>%
    # gets rid of dup @ 75 ft mark
    # filter(!(transect == 'nwse' & mark == 75)) %>%
    separate(hheight_src, c("hheight_num", "hheight_units"), extra = "drop", 
             fill = "right", sep = "\\s+") %>%
    separate(wheight_src, c("wheight_num", "wheight_units"), extra = "drop", 
             fill = "right", sep = "\\s+") %>%
    mutate(
      reckey = paste0(plotkey, str_to_upper(str_sub(transect, -2)), '1'),
      hplant = hplant_src,
      wplant = wplant_src,
      hheight = as.double(str_extract(hheight_num, "[\\d\\.]+")),
      hunits = case_when(hheight_num == "0" ~ "in",
                         hheight_units == "" | 
                           is.na(hheight_units) ~ NA_character_, 
                         TRUE ~ hheight_units),
      wheight = as.double(str_extract(wheight_num, "[\\d\\.]+")),
      wunits = case_when(wheight_num == "0" ~ "in",
                         wheight_units == "" | 
                           is.na(wheight_units) ~ NA_character_, 
                         TRUE ~ wheight_units)
    ) %>%
    mutate_if(is.character, 
              list(~na_if(str_trim(stri_enc_toutf8(., validate=TRUE)), ""))) %>%
    select(reckey, mark, hplant, wplant, hunits, wunits, hheight, wheight)
  
  pastureheights <- select(pastureheights.wide, reckey, mark, hplant, hunits, 
                           hheight) %>%
    mutate(height_cm = case_when(hunits == "ft" ~ hheight * 30.48,
                                 hunits == "in" ~ hheight * 2.54,
                                 TRUE ~ hheight),
           hit_type = "growth habit",
           hit_order = 2,
           hit_sub = "herbaceous") %>%
    rename(pcode = hplant) %>% 
    select(reckey, mark, hit_type, hit_sub, hit_order, pcode, height_cm) %>%
    union(
      select(pastureheights.wide, reckey, mark, wplant, wunits, 
             wheight) %>%
        mutate(height_cm = case_when(wunits == "ft" ~ wheight * 30.48,
                                     wunits == "in" ~ wheight * 2.54,
                                     TRUE ~ wheight),
               hit_type = "growth habit",
               hit_order = 1,
               hit_sub = "woody") %>%
        rename(pcode = wplant) %>%
        select(reckey, mark, hit_type, hit_sub, hit_order, pcode, height_cm)
    ) %>%
    mutate(pcode = str_trim(pcode))
  return(pastureheights)
}

### DIMA conversion functions ###

recalc_xy <- function(in.df, p.key, out_srid = 4269, force_west = TRUE) {
  # takes a DIMA input table with gpscoordsys, datum, zone, easting and 
  # northing, computes epsg/srid and reprojects into NAD83 lat/long
  
  # make epsg data frame for srid lookups
  # EPSG <- make_EPSG()  # not needed for current srid construction
  vars <- c(key = p.key, X ="X", Y = "Y")
  
  in.df.t <- in.df %>%
    filter(!(easting == 0 & northing == 0) & 
             !(is.na(easting) | is.na(northing))) %>%
    mutate(zone_int = as.integer(str_extract(zone, "\\d{2}"))) %>%
    mutate(valid_latlong = between(easting, -180, 180) & 
             between(northing, -90, 90)) %>%
    # suppressWarnings() is necessary because even though sprintf() should never
    # encounter a NA due to case_when ordering, case_when evaluates each case
    # for every row anyway and returns warnings for those rows with NAs
    mutate(srid = suppressWarnings(case_when(
      str_detect(datum, "WGS\\s*84") & valid_latlong == TRUE ~ as.integer(4326),
      str_detect(datum, "NAD\\s*83") & valid_latlong == TRUE ~ as.integer(4269),
      is.na(zone_int) ~ NA_integer_,
      str_detect(datum, "WGS\\s*84") ~ 
        as.integer(paste0("326", sprintf("%02d", zone_int))),
      str_detect(datum, "NAD\\s*83") ~ 
        as.integer(paste0("269", sprintf("%02d", zone_int))),
      TRUE ~ NA_integer_))) %>%
    mutate(easting = ifelse(valid_latlong == TRUE & easting > 0 & 
                              force_west == TRUE, -1*easting, easting))
  
  srid <- select(in.df.t, srid) %>% group_by(srid) %>% summarize(.groups = "drop") %>% 
    filter(!is.na(srid))
  if (nrow(srid) > 0) {
    coords <- foreach (i = 1:nrow(srid), .combine = bind_rows) %do% {
      s <- srid[i,][[1]]
      input.df <- filter(in.df.t, srid == s)
      spatial.df <- st_as_sf(input.df, crs = s, coords = c("easting", "northing"), 
                             remove = FALSE)
      transform.df <- st_transform(spatial.df, out_srid)
      coords.matrix <- as_tibble(st_coordinates(transform.df))
      out <- bind_cols(input.df, coords.matrix) %>% select(!!vars)
    }
  } else {
    coords <- tibble(key = character(), X = double(), Y = double())
  }
  return(coords)
}

# site conversion #
site.dima <- function(tblsites.s, src.name){
  
  site <- tblsites.s %>% rename_with(tolower) %>%
    select(sitekey, siteid, sitename, notes, ownership, 
                 contactname) %>%
    rename(site_name = sitename, contact_name = contactname) %>%
    mutate(source = src.name, source_type = "dima") %>%
    select(sitekey, siteid, site_name, notes, source, source_type, ownership, 
           contact_name) %>%
    mutate_if(is.character, 
              list(~na_if(str_trim(stri_enc_toutf8(., validate=TRUE)), "")))
  return(site)
}


point.dima <- function(tblplots.s, state, county, data.type) {
  point <- tblplots.s %>% rename_with(tolower) %>%
    filter(!(plotkey %in% c("888888888", "999999999"))) %>%
    left_join(state, by = c("state" = "stusps")) %>% 
    left_join(county, by = c("statefp" = "statefp", "county" = "name")) %>%
    mutate(vertical_slope_shape = 
             case_when(substr(esd_slopeshape,1,1) == "C" ~ "concave",
                       substr(esd_slopeshape,1,1) == "V" ~ "convex",
                       substr(esd_slopeshape,1,1) == "L" ~ "linear",
                       TRUE ~ NA_character_),
           horizontal_slope_shape = 
             case_when(substr(esd_slopeshape,2,2) == "C" ~ "concave",
                       substr(esd_slopeshape,2,2) == "V" ~ "convex",
                       substr(esd_slopeshape,2,2) == "L" ~ "linear",
                       TRUE ~ NA_character_),
           musym = ifelse(nchar(soil) > 6, NA_character_, soil),
           aspect = case_when(aspect == "-1" ~ NA_integer_,
                              TRUE ~ suppressWarnings(as.integer(
                                round(as.numeric(aspect)), 0))),
           nogps = "Successful", 
           elev_units = case_when(elevationtype == 1 ~ "m",
                                  elevationtype == 2 ~ "ft",
                                  TRUE ~ NA_character_),
           landscapetype = str_replace(landscapetype, "\\*+", "")
    )
  # must do a real clean on coords because DIMAs tend to be messy
  coords <- recalc_xy(in.df = point, p.key = "plotkey")
  point2 <- left_join(point, coords, by = c("plotkey" = "key")) %>% 
    mutate(longitude = ifelse(!is.na(X), X, longitude),
           latitude = ifelse(!is.na(Y), Y, latitude),
           geo_datum = "NAD83") %>%
    rename(state_abbr = state, county_name = county) %>%
    # using rename_all in case column doesn't exist
    rename_all(recode, statefp = "state", countyfp = "county", 
               slope = "slope_percent", mapunitcomponent = "component_name", 
               landscapetype = "landform_major", 
               landscapetypesecondary = "landform_minor") %>%
    mutate(valid_latlong = between(easting, -180, 180) & 
             between(northing, -90, 90)) %>%
    mutate(valid_latlong = ifelse(is.na(valid_latlong), FALSE, 
                                  valid_latlong)) %>%
    mutate(X = ifelse(valid_latlong == TRUE, X, 0),
           Y = ifelse(valid_latlong == TRUE, Y, 0)) %>%
    mutate(tz = case_when(
      valid_latlong == FALSE ~ NA_character_,
      valid_latlong == TRUE ~
        lutz::tz_lookup_coords(lat = Y, lon = X, method = "accurate"),
      TRUE ~ NA_character_)) %>%
    mutate(tz = ifelse(is.na(tz), "UTC", tz)) %>%
    mutate(establish_date = as_datetime(establishdate), tz = tz) %>%
    # mutate(establish_date = force_tzs(establishdate, tz, 
    #                                   tzone_out = "UTC")) %>%
    mutate(survey = as.integer(lubridate::year(establish_date))) %>%
    select(any_of(c("plotkey", "sitekey", "plotid", "survey", "establish_date", 
                  "state", "county", "landform_major", "landform_minor", 
                  "vertical_slope_shape", "horizontal_slope_shape", 
                  "slope_percent", "aspect", "musym", "component_name", 
                  "latitude", "longitude", "elevation", "nogps", "geo_datum", 
                  "elev_units", "tz"))) %>%
    mutate_if(is.character, 
              list(~na_if(str_trim(stri_enc_toutf8(., validate=TRUE)), "")))
  return(point2)
}


convert <- function(tbls, desc) {
  out.tables <-  list()
  schemas <- names(tbls)
  for (schema in schemas){
    cat(paste0("For ", schema, " schema...\n\n"))
    if (schema == "lmf") {
      cat("processing site table...\n")
      out.tables[[schema]][["site"]] <- 
        site.lmf(point.s = tbls[[schema]][["POINT"]], src.name = desc)
      cat("processing point table...\n")
      out.tables[[schema]][["point"]] <- 
        point.lmf(point.s = tbls[[schema]][["POINT"]], 
                  pointcoordinates.s = tbls[[schema]][["POINTCOORDINATES"]],
                  gps.s = tbls[[schema]][["GPS"]])
      cat("processing disturbance table...\n")
      out.tables[[schema]][["disturbance"]] <- 
        disturb.lmf(disturbance.s = tbls[[schema]][["DISTURBANCE"]], 
                    gps.s = tbls[[schema]][["GPS"]])
      cat("processing transect table...\n")
      out.tables[[schema]][["transect"]] <- 
        transect.lmf(pintercept.s = tbls[[schema]][["PINTERCEPT"]], 
                     pastureheights.s = tbls[[schema]][["PASTUREHEIGHTS"]], 
                     gps.s = tbls[[schema]][["GPS"]])
      cat("processing esfsg table...\n")
      esfsg.list <- esfsg.lmf(esfsg.s = tbls[[schema]][["ESFSG"]], 
                              transect.f = out.tables[[schema]][["transect"]], 
                              point.f = out.tables[[schema]][["point"]])
      out.tables[[schema]][["esfsg"]] <- esfsg.list$esfsg
      out.tables[[schema]][["transect"]] <- esfsg.list$transect
      cat("processing pinterceptmeta table...\n")
      out.tables[[schema]][["pinterceptmeta"]] <- 
        pimeta.lmf(pintercept.s = tbls[[schema]][["PINTERCEPT"]], 
                   pastureheights.s = tbls[[schema]][["PASTUREHEIGHTS"]], 
                   gps.s = tbls[[schema]][["GPS"]])
      cat("processing pintercept table...\n")
      out.tables[[schema]][["pintercept"]] <- 
        pi.lmf(pintercept.s = tbls[[schema]][["PINTERCEPT"]])
      cat("processing pastureheights table...\n")
      out.tables[[schema]][["pastureheights"]] <- 
        ph.lmf(pastureheights.s = tbls[[schema]][["PASTUREHEIGHTS"]])
    }
    else if (schema == "dima"){
      cat("processing site table...\n")
      out.tables[[schema]][["site"]] <- 
        site.dima(tblsites.s = tbls[[schema]][["tblSites"]], src.name = desc) 
      cat("processing point table...\n")
      state <- as_tibble(tbl(con, "state"))
      county <- as_tibble(tbl(con, "county"))
      out.tables[[schema]][["point"]] <- 
        point.dima(tblplots.s = tbls[[schema]][["tblPlots"]], state = state, 
                   county = county)
      cat("processing disturbance table...\n")
      disturbance <- disturb.dima(tblplots.src, point)
      cat("processing transect table...\n")
      transect <- transect.dima(tbllines.src, tblplots.src, tbllpiheader.src, point)
      cat("processing esfsg table...\n")
      esfsg.list <- esfsg.dima(tblplots.src, tblplothistory.src, transect, point)
      esfsg <- esfsg.list$esfsg
      transect <- esfsg.list$transect
      cat("processing pinterceptmeta table...\n")
      pinterceptmeta <- pimeta.dima(tbllpiheader.src, point, transect)
      cat("processing pintercept table...\n")
      pintercept <- pi.dima(tbllpidetail.src, plant.src)
      cat("processing pastureheights table...\n")
      pastureheights <- ph.dima(tbllpidetail.src, tbllpiheader.src)
      if (import.eco == TRUE){
        cat("processing ecosite table...\n")
        ecosite <- eco.dima(tblecolsites.src)
      }
    } else {
      cat(paste0("Schema ", schema, " not recognized. Skipping...\n"))
    }
  }
}