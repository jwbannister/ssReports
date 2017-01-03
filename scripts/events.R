load_all()
load_all("~/code/Roses")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RS3)
library(RColorBrewer)
library(gridExtra)
library(grid)

pm10_cutoff <- 150
pm25_cutoff <- 35

# categorize sites into zones
zones <- data.frame(deployment=c("Torres-Martinez", "Salton Sea Park", 
                                 "Bombay Beach", "Sonny Bono", 
                                 "Naval Test Base", "Salton City"), 
                    zone=c("N", "N", "E", "E", "W", "W"), 
                    camera=c(rep("Torres-Martinez", 2), 
                             rep("1003", 2), 
                             rep("Salton City", 2)))

# detect dust events
met_df$date <- as.Date(substr(met_df$datetime %m-% minutes(30), 1, 10)) 
pm_df$date <- as.Date(substr(pm_df$datetime %m-% minutes(30), 1, 10))
pm_summary <- pm_df %>% select(-teom_bp, -teom_at, -teom_rh) %>% 
    group_by(deployment, date) %>%
    summarize(daily.pm10.avg = round(sum(pm10)/24, 1), 
              daily.pm25.avg = round(sum(pm25)/24, 1))
events <- pm_summary %>% ungroup() %>%
    filter(daily.pm10.avg > pm10_cutoff | 
           daily.pm25.avg > pm25_cutoff) %>%
arrange(date)
event_days <- unique(events$date)

# get background for use in dust rose plot
basemap <- ggmap::get_map(location=c(lon=-115.8434, lat=33.3286), color="color", 
                          source="google", maptype="satellite", zoom=10)
background <- ggmap::ggmap(basemap, extent="device")

event_list <- vector(mode="list", length=length(event_days))
names(event_list) <- event_days
for (i in names(event_list)){ 
    pm_tmp <- pm_df %>% filter(date==i) %>%
        select(deployment, datetime, pm10, pm25)
    met_tmp <- met_df %>% filter(date==i) %>%
        select(deployment, datetime, ws, wd)
    event_df <- left_join(pm_tmp, met_tmp, by=c("deployment", "datetime")) %>%
        left_join(zones, by="deployment")
    # build timeseries plot
    timeseries <- event_df %>%
        arrange(deployment, datetime) %>% filter(hour(datetime)!=0) %>%
        gather(pm, conc, pm10:pm25) %>%
        filter(pm=="pm10") %>%
        ggplot(aes(x=hour(datetime), y=conc)) +
        geom_path(aes(color=deployment)) +
        ylab(bquote('PM10 Concentration ('*mu~'g/'*m^3~')')) + xlab("Hour")
    event_list[[i]]$time_img <- paste0(tempfile(), ".png")
    png(filename=event_list[[i]]$time_img, width=8, height=2, units="in", 
        res=300)
    print(timeseries)
    dev.off()
    # build event photos
    event_list[[i]]$photos <- vector(mode="list", length=3)
    names(event_list[[i]]$photos) <- c("N", "E", "W")
    daylight_pm <- filter(event_df, between(hour(datetime), 7, 16))
    daylight_wind <- filter(met_5min_df, between(hour(datetime), 6, 15)) %>%
        filter(date(datetime)==i)
    tmp_zone <- select(zones, -deployment)[!duplicated(select(zones, -deployment)), ]
    zone_names <- c("N"="North", "E"="East", "W"="West")
    for (j in c("N", "E", "W")){
        tmp_pm <- filter(daylight_pm, zone==j)
        worst_hour <- hour(tmp_pm[tmp_pm$pm10==max(tmp_pm$pm10), ]$datetime) - 1
        tmp_wind <- filter(daylight_wind, zone==j, hour(datetime)==worst_hour)
        target.datetime <- tmp_wind[tmp_wind$ws==max(tmp_wind$ws), ]$datetime
        image_tmp <- image_df %>% 
            left_join(tmp_zone, by=c("deployment"="camera")) %>%
            filter(date(datetime)==date(target.datetime) & zone==j) %>%
            filter(hour(datetime)==worst_hour) %>%
            mutate(delta = abs(difftime(datetime, target.datetime)))
        pic.datetime <- filter(image_tmp, delta==min(delta))$datetime
        image.key <- substring(filter(image_tmp, delta==min(delta))$s3_url, 49)
        if (length(image.key)!=0){
            image.file <- tempfile()
            S3_bucket_access(image.key, image.file)
            img <- jpeg::readJPEG(image.file)
            image.grob <- grid::rasterGrob(img, interpolate=T)
        } else{
            p1 <- ggplot(data.frame(x=1:10, y=1:10), aes(x=x, y=y)) +
                  geom_text(aes(x=3, y=5, label="No Image Available")) + 
                  theme(panel.background=element_blank(), 
                        axis.title=element_blank(), 
                        axis.text=element_blank(), 
                        axis.ticks=element_blank())
            image.grob <- ggplotGrob(p1)
        }
        event_list[[i]]$photos[[j]] <- image.grob
    }
    event_list[[i]]$photo_img <- paste0(tempfile(), ".png")
    png(filename=event_list[[i]]$photo_img, width=8, height=1.9, units="in", 
        res=300)
        grid.arrange(grobs=list(event_list[[i]]$photos$W, 
                               event_list[[i]]$photos$E, 
                               event_list[[i]]$photos$N), ncol=3)
    dev.off()
    # build dustrose map
    event_list[[i]]$map <- event_plot(loc_df, 
                                      event_df[complete.cases(event_df), ], 
                                      background)
    event_list[[i]]$map_img <- paste0(tempfile(), ".png")
    png(filename=event_list[[i]]$map_img, width=8, height=3, units="in", 
        res=300)
    print(event_list[[i]]$map)
    dev.off()
}
names(events) <- c("Deployment", "Date", "24-hour PM10 Avg.", 
                   "24-hour PM2.5 Avg.")
