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
background <- plot_salton_background()
# set coordinates for determining daylight hours
salton_sea <- matrix(c(-115.8434, 33.3286), nrow=1) 

event_list <- vector(mode="list", length=length(event_days))
names(event_list) <- event_days
for (i in names(event_list)){ 
    pm_tmp <- pm_df %>% filter(date==i) %>%
        select(deployment, datetime, pm10, pm25)
    met_tmp <- met_df %>% filter(date==i) %>%
        select(deployment, datetime, ws, wd)
    event_df <- left_join(pm_tmp, met_tmp, by=c("deployment", "datetime")) %>%
        left_join(zones, by="deployment")
    a <- select(event_df, deployment, datetime, value=pm10) %>% 
        mutate(factor="PM10 (ug/m^3)")
    b <- select(event_df, deployment, datetime, value=ws) %>% 
        mutate(factor="Wind Speed (m/s)")
    plot_df <- rbind(a, b)
    # build timeseries plot
    timeseries <- plot_df %>%
        arrange(deployment, datetime) %>% 
        ggplot(aes(x=datetime, y=value)) +
        geom_path(aes(color=deployment)) +
        facet_grid(factor ~ ., scales="free_y") +
        scale_color_brewer(palette="Set1") +
        ylab("") + xlab("") +
        theme(legend.title=element_blank(), 
              panel.grid.minor=element_blank(), 
              strip.text=element_text(size=4))
    event_list[[i]]$time_img <- paste0(tempfile(), ".png")
    png(filename=event_list[[i]]$time_img, width=8, height=2.5, units="in", 
        res=300)
    print(timeseries)
    dev.off()
    # build event photos
    event_list[[i]]$photos <- vector(mode="list", length=3)
    names(event_list[[i]]$photos) <- c("N", "E", "W")
    dt <- as.POSIXct(i, tz="America/Los_Angeles")
    sunrise <- maptools::sunriset(salton_sea, dt, direction="sunrise", 
                                  POSIXct.out=T)$time
    sunset <- maptools::sunriset(salton_sea, dt, direction="sunset", 
                                 POSIXct.out=T)$time
    daylight_pm <- filter(event_df, between(datetime, sunrise, sunset))
    daylight_wind <- filter(met_5min_df, between(datetime, sunrise, sunset)) %>%
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
            prelim.grob <- grid::rasterGrob(img, interpolate=T)
            p1 <- ggplot(data.frame(x=1:10, y=1:10), aes(x=x, y=y)) +
                  theme(panel.background=element_blank(), 
                        axis.title=element_blank(), 
                        axis.text=element_blank(), 
                        axis.ticks=element_blank(),
                        legend.position="none", 
                        plot.title = element_text(hjust=0.5)) +
                  annotation_custom(prelim.grob, xmin=-Inf, xmax=Inf, 
                                    ymin=-Inf, ymax=Inf) +
                  ggtitle(zone_names[[j]])
            image.grob <- ggplotGrob(p1)
        } else{
            p1 <- ggplot(data.frame(x=1:10, y=1:10), aes(x=x, y=y)) +
                  geom_blank() +
                  geom_text(aes(x=5, y=5, label="No Image Available", 
                                hjust="center")) + 
                  ggtitle(zone_names[[j]]) + 
                  theme(panel.background=element_blank(), 
                        axis.title=element_blank(), 
                        axis.text=element_blank(), 
                        axis.ticks=element_blank(), 
                        legend.position="none", 
                        plot.title = element_text(hjust=0.5)) 
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
names(events) <- c("Deployment", "Date", 
                   "24-hour PM<sub>10</sub> Avg. (ug/m<sup>3</sup>)", 
                   "24-hour PM<sub>2.5</sub> Avg. (ug/m<sup>3</sup>)")
