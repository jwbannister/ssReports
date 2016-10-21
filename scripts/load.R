load_all()
library(tidyverse)
library(lubridate)
library(rgdal)

# pull TEOM data
query1 <- paste0("SELECT i.deployment, t.datetime, t.pm10, t.pm25 ", 
                 "FROM teom.pm_1hour t JOIN info.deployments i ",
                 "ON t.deployment_id = i.deployment_id ", 
                 "WHERE datetime BETWEEN '", start_date, " 01:00:00' ",
                 "AND '", end_date %m+% days(1), " 00:00:00'")
pm_pull <- query_salton(query1)
pm_df <- pm_pull[complete.cases(pm_pull), ]
pm_df$datetime <- as.POSIXct(as.character(pm_df$datetime), tz="UTC")

# pull met data
query1 <- paste0("SELECT i.deployment, m.datetime, m.ws_10m, m.wd_sonic, ", 
                 "m.delta_temp_2m, m.delta_temp_10m, m.net_rad, m.rh_2m, ",
                 "m.at_2m ", 
                 "FROM met.met_1hour m JOIN info.deployments i ",
                 "ON m.deployment_id = i.deployment_id ", 
                 "WHERE datetime BETWEEN '", start_date, " 01:00:00' ",
                 "AND '", end_date %m+% days(1), " 00:00:00' ",
                 "AND i.deployment IN ('Bombay Beach', 'Sonny Bono', ", 
                 "'Torres-Martinez', 'Naval Test Base', ",
                 "'Salton City', 'Salton Sea Park')")
met_pull <- query_salton(query1)
met_pull$delta_temp_diff <- 
    met_pull$delta_temp_10m - met_pull$delta_temp_2m
met_pull <- select(met_pull, -delta_temp_10m, -delta_temp_2m)
met_df <- met_pull[complete.cases(met_pull), ]

# pull and summarize flag data
query1 <- paste0("SELECT i.deployment, f.flagged_period, ff.flag, ", 
                 "f.flag_note, f.invalidate, f.reviewed ", 
                 "FROM flags.flagged_data f JOIN flags.flags ff ",
                 "ON f.flag_id = ff.flag_id ", 
                 "JOIN info.deployments i ",
                 "ON f.deployment_id = i.deployment_id ") 
flag_pull <- query_salton(query1)
flag_pull$start.date <- substr(flag_pull$flagged_period, 3, 12)
flag_pull$start.hour <- substr(flag_pull$flagged_period, 14, 21)
flag_pull$end.date <- substr(flag_pull$flagged_period, 25, 34)
flag_pull$end.hour <- substr(flag_pull$flagged_period, 36, 43)
flag_df <- flag_pull %>% filter(between(as.Date(start.date), 
                                        as.Date(start_date), 
                                        as.Date(end_date))) %>%
select(deployment, start.date, start.hour, end.date, end.hour, flag, 
       flag_note, reviewed, invalidate)

# build full list of hours in period to calculate data capture ratio
hour_seq <- seq(as.POSIXct(paste0(start_date, " 01:00:00"), tz="UTC"),
                as.POSIXct(paste0(end_date %m+% days(1), " 00:00:00"), tz="UTC"), 
                by="hour")
data_hours <- length(hour_seq)

# pull deployment locations
query1 <- paste0("SELECT deployment, lat_dd, lon_dd ", 
                 "FROM info.deployments")
loc_pull <- query_salton(query1)
loc_df <- loc_pull %>% filter(deployment %in% unique(pm_df$deployment))
longlatcoord <- SpatialPoints(cbind(loc_df$lon_dd, loc_df$lat_dd), 
                              proj4string=CRS("+proj=longlat"))
utmcoord <- spTransform(longlatcoord, CRS("+proj=utm +zone=11N"))
loc_df$x <- utmcoord$coords.x1
loc_df$y <- utmcoord$coords.x2

# pull photo data
query1 <- paste0("SELECT i.datetime, f.s3_url, f.fpath ", 
                 "FROM images.images i JOIN images.image_files f ", 
                 "ON i.image_file_id=f.image_file_id ", 
                 "WHERE f.image_deployment_id = 2 ", 
                 "AND i.datetime BETWEEN '", start_date, " 00:00:00' ",
                 "AND '", end_date %m+% days(1), " 00:00:00' ")
image_df <- query_salton(query1)
                 
save(pm_df, met_df, flag_df, loc_df, image_df, 
     file="~/code/ssReports/data/load.RData")
