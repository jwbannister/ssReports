library(ggplot2)
library(tidyverse)
library(lubridate)
library(readxl)
load_all("~/code/Roses")
load_all()

# pull TEOM data
query1 <- paste0("SELECT i.deployment, t.datetime, t.pm10, ", 
                 "flags.is_tf_invalid(t.deployment_id, 144, 
                 t.datetime - '1 hour'::interval, t.datetime) ", 
                 "AS invalid_pm10 ",
                 "FROM teom.pm_1hour t JOIN info.deployments i ",
                 "ON t.deployment_id = i.deployment_id ", 
                 "WHERE (datetime - '1 second'::interval)::date='2017-03-30'::date;")
pm_df <- query_salton(query1)
attributes(pm_df$datetime)$tzone <- "America/Los_Angeles"
pm_df <- filter(pm_df, !invalid_pm10) %>% arrange(deployment, datetime) %>%
    select(-invalid_pm10)

# pull met data
query2 <- paste0("SELECT i.deployment, m.datetime, ",
                 "COALESCE(m.ws_10m, m.ws_sonic_2d) AS ws, ", 
                 "COALESCE(m.wd_10m, m.wd_sonic) AS wd, ", 
                 "flags.is_tf_invalid(m.deployment_id, 114, 
                 m.datetime - '1 hour'::interval, m.datetime) ", 
                 "AS invalid_ws, ",
                 "flags.is_tf_invalid(m.deployment_id, 298, 
                 m.datetime - '1 hour'::interval, m.datetime) ", 
                 "AS invalid_wd ",
                 "FROM met.met_1hour m JOIN info.deployments i ",
                 "ON m.deployment_id = i.deployment_id ", 
                 "WHERE (datetime - '1 second'::interval)::date='2017-03-30'::date;")
met_df <- query_salton(query2)
attributes(met_df$datetime)$tzone <- "America/Los_Angeles"
met_df <- filter(met_df, !invalid_ws & !invalid_wd) %>% 
    arrange(deployment, datetime) %>%
    select(-invalid_ws, -invalid_wd) %>%
    filter(!is.na(ws) & !is.na(wd))
met_other <- read.csv("./data/March30_CIMIS.csv")
met_other$date <- as.Date(met_other$date, "%m/%d/%Y")
met_other$datetime <- as.character(as.POSIXct(paste0(met_other$date, " ", 
                                         met_other$hour, ":00:00"), 
                                  tz="PDT"))
met_other <- met_other %>% select(-date, -hour) %>% 
    filter(!is.na(ws) & !is.na(wd))
met_df <- rbind(met_df, met_other)
met_df$ws <- as.numeric(met_df$ws)
met_df$wd <- as.numeric(met_df$wd)
met_df <- filter(met_df, !is.na(ws) & !is.na(wd)) %>%
    filter(!(deployment %in% c('La Quinta II', 'Westmorland North')))
met_final <- read.csv("./data/Other_IID_Stations.csv")
met_final$datetime <- as.POSIXct(met_final$datetime, format="%m/%d/%Y %H:%M", 
                                 tz='PDT')
met_df <- rbind(met_df, met_final)

teom_data <- pm_df %>% left_join(met_df, by=c("deployment", "datetime"))
teom_other <- read.csv("./data/March30_PM.csv")
teom_other$date <- as.Date(teom_other$date, "%m/%d/%Y")
teom_other$datetime <- as.character(as.POSIXct(paste0(teom_other$date, " ", 
                                         teom_other$hour, ":00:00"), 
                                  tz="PDT") %m+% hours(1))
teom_other <- teom_other %>% select(-date, -hour) %>% filter(!is.na(pm10))
teom_data <- rbind(teom_data, teom_other)
teom_data$pm10 <- as.numeric(teom_data$pm10)
teom_data <- filter(teom_data, !is.na(pm10))

# pull deployment locations
query1 <- paste0("SELECT deployment, lat_dd, lon_dd ", 
                 "FROM info.deployments")
loc_pull <- query_salton(query1)
loc_df <- loc_pull %>% filter(deployment %in% unique(met_df$deployment))
longlatcoord <- sp::SpatialPoints(cbind(loc_df$lon_dd, loc_df$lat_dd), 
                              proj4string=sp::CRS("+proj=longlat"))
utmcoord <- sp::spTransform(longlatcoord, sp::CRS("+proj=utm +zone=11N"))
loc_df$x <- utmcoord$coords.x1
loc_df$y <- utmcoord$coords.x2
loc_other <- read.csv("./data/locations.csv")
xycoord <- sp::SpatialPoints(cbind(loc_other$x, loc_other$y), 
                              proj4string=sp::CRS("+proj=utm +zone=11N"))
llcoord <- sp::spTransform(xycoord, sp::CRS("+proj=longlat"))
loc_other$lon_dd <- llcoord$coords.x1
loc_other$lat_dd <- llcoord$coords.x2
loc_df <- rbind(loc_df, loc_other)

background <- plot_extended_background()
for (i in 0:23){
    fl <- letters[i + 1]
    dt1 <- filter(teom_data, hour(datetime %m-% seconds(1))<=i)
    p1 <- pm_plot(loc_df, dt1, background) 
    png(file=paste0("~/Desktop/pm_roses/pm", fl, ".png"), height=6, width=6, 
        units="in", res=300)
    print(p1)
    dev.off()
}
met_df$ws <- sapply(met_df$ws, function(x) x + rnorm(1, mean=0, sd=0.01))
background <- plot_wind_background()
for (i in 0:23){
    fl <- letters[i + 1]
    dt2 <- filter(met_df, hour(datetime %m-% seconds(1))<=i)
    p2 <- wind_plot(loc_df, dt2, background) 
    png(file=paste0("~/Desktop/wind_roses/wind", fl, ".png"), height=6, width=6, 
        units="in", res=300)
    print(p2)
    dev.off()
}


