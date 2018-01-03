library(tidyverse)
library(lubridate)
library(rgdal)

# pull TEOM data
query1 <- paste0("SELECT i.deployment, t.datetime, t.pm10_stp AS pm10, ", 
                 "t.pm25, t.teom_bp, t.teom_at, t.teom_rh, ",
                 "flags.field_is_invalid(t.deployment_id, 145, t.datetime) ",
                 "AS invalid_pm10, ",
                 "flags.field_is_invalid(t.deployment_id, 146, t.datetime) ",
                 "AS invalid_pm25, ",
                 "flags.field_is_invalid(t.deployment_id, 320, t.datetime) ",
                 "AS invalid_bp, ",
                 "flags.field_is_invalid(t.deployment_id, 150, t.datetime) ",
                 "AS invalid_at, ",
                 "flags.field_is_invalid(t.deployment_id, 151, t.datetime) ",
                 "AS invalid_rh ",
                 "FROM teom.pm_1hour t JOIN info.deployments i ",
                 "ON t.deployment_id = i.deployment_id ", 
                 "WHERE (DATE_TRUNC('minute', datetime) - ", 
                 "'1 second'::interval)::date ",
                 "BETWEEN '", start_date, "'::date ",
                 "AND '", end_date, "'::date;")
pm_df <- query_db("saltonsea", query1)
#attributes(pm_df$datetime)$tzone <- "America/Los_Angeles"

# pull met data
query1 <- paste0("SELECT i.deployment, m.datetime, ",
                 "COALESCE(m.ws_10m, m.ws_sonic_2d) AS ws, ", 
                 "COALESCE(m.wd_10m, m.wd_sonic) AS wd, m.solar_rad, ", 
                 "m.at_2m, m.rh_2m, m.delta_temp_2m, m.delta_temp_10m, ", 
                 "flags.field_is_invalid(m.deployment_id, 114, m.datetime) ",
                 "AS invalid_ws, ",
                 "flags.field_is_invalid(m.deployment_id, 298, m.datetime) ",
                 "AS invalid_wd, ",
                 "flags.field_is_invalid(m.deployment_id, 337, m.datetime) ",
                 "AS invalid_rad, ",
                 "flags.field_is_invalid(m.deployment_id, 119, m.datetime) ",
                 "AS invalid_at, ",
                 "flags.field_is_invalid(m.deployment_id, 120, m.datetime) ",
                 "AS invalid_rh, ",
                 "flags.field_is_invalid(m.deployment_id, 115, m.datetime) ",
                 "AS invalid_d2, ",
                 "flags.field_is_invalid(m.deployment_id, 116, m.datetime) ",
                 "AS invalid_d10 ",
                 "FROM met.met_1hour m JOIN info.deployments i ",
                 "ON m.deployment_id = i.deployment_id ", 
                 "WHERE (DATE_TRUNC('minute', datetime) - ", 
                 "'1 second'::interval)::date ",
                 "BETWEEN '", start_date, "'::date ",
                 "AND '", end_date, "'::date ",
                 "AND i.deployment IN ('Bombay Beach', 'Sonny Bono', ", 
                 "'Torres Martinez', 'Naval Test Base', ",
                 "'Salton City', 'Salton Sea Park')")
met_df <- query_db("saltonsea", query1)
#attributes(met_df$datetime)$tzone <- "America/Los_Angeles"

# pull and summarize flag data
query1 <- paste0("SELECT i.deployment, f.flagged_period, ff.flag, ", 
                 "f.flag_note, f.invalidate, f.reviewed ", 
                 "FROM flags.flagged_data f JOIN flags.flags ff ",
                 "ON f.flag_id = ff.flag_id ", 
                 "JOIN info.deployments i ",
                 "ON f.deployment_id = i.deployment_id ") 
flag_pull <- query_db("saltonsea", query1)
flag_pull$start.date <- substr(flag_pull$flagged_period, 3, 12)
flag_pull$start.hour <- substr(flag_pull$flagged_period, 14, 21)
flag_pull$end.date <- substr(flag_pull$flagged_period, 25, 34)
flag_pull$end.hour <- substr(flag_pull$flagged_period, 36, 43)
flag_df <- flag_pull %>% filter(between(as.Date(start.date), 
                                        as.Date(start_date), 
                                        as.Date(end_date))) %>%
select(deployment, start.date, start.hour, end.date, end.hour, flag, 
       flag_note, reviewed, invalidate) %>% filter(invalidate)
flag_df$flag <- 
    sapply(flag_df$flag, 
           function(x) if_else(x %in% c("Site Visit", "TEOM Op Mode Error", 
                                        "TEOM Status Error", 
                                        "Excessive Negative PM10 Concentration", 
                                        "Missing TEOM Data"), x, 
                               "Other Invalidation"))

# build full list of hours in period to calculate data capture ratio
hour_seq <- seq(as.POSIXct(paste0(start_date, " 01:00:00")),
                as.POSIXct(paste0(end_date %m+% days(1), " 00:00:00")), 
                by="hour")
data_hours <- length(hour_seq)

# pull deployment locations
query1 <- paste0("SELECT deployment, lat_dd, lon_dd ", 
                 "FROM info.deployments")
loc_pull <- query_db("saltonsea", query1)
loc_df <- loc_pull %>% filter(deployment %in% unique(pm_df$deployment))
longlatcoord <- SpatialPoints(cbind(loc_df$lon_dd, loc_df$lat_dd), 
                              proj4string=CRS("+proj=longlat"))
utmcoord <- spTransform(longlatcoord, CRS("+proj=utm +zone=11N"))
loc_df$x <- utmcoord$coords.x1
loc_df$y <- utmcoord$coords.x2

# pull photo data
query1 <- paste0("SELECT i.datetime, d.deployment, i.image_deployment_id, f.s3_url ", 
                 "FROM images.images i ", 
                 "JOIN images.image_files f ON i.image_file_id=f.image_file_id ", 
                 "JOIN images.image_deployments id ", 
                 "ON f.image_deployment_id = id.image_deployment_id ", 
                 "JOIN info.deployments d ON id.deployment_id=d.deployment_id ",
                 "WHERE d.deployment != '1004' ", 
                 "AND i.image_deployment_id != '3' ",
                 "AND (datetime - '1 second'::interval)::date ",
                 "BETWEEN '", start_date, "'::date ",
                 "AND '", end_date, "'::date;")
image_df <- query_db("saltonsea", query1)

# pull 5 minute wind data
query1 <- paste0("SELECT i.deployment, m.datetime, m.ws_2m AS ws ", 
                 "FROM met.met_5min m JOIN info.deployments i ",
                 "ON m.deployment_id = i.deployment_id ", 
                 "WHERE (datetime - '1 second'::interval)::date ",
                 "BETWEEN '", start_date, "'::date ",
                 "AND '", end_date, "'::date ",
                 "AND i.deployment IN ('1001', '1004', '1102')")
met_5min_pull <- query_db("saltonsea", query1)
zns <- data.frame(deployment=c("1001", "1102", "1004"), 
                  zone=c("N", "W", "E"))
met_5min_df <- met_5min_pull[complete.cases(met_5min_pull), ] %>%
    left_join(zns, by="deployment")
#attributes(met_5min_df$datetime)$tzone <- "America/Los_Angeles"
                 
