library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(ggmap)
set_AWS_token()
google_key = Sys.getenv("SALTON_MAPS_KEY")

pm10_cutoff <- 150
pm25_cutoff <- 35

# categorize sites into zones
zones <- data.frame(deployment=c("Torres Martinez", "Salton Sea Park", 
                                 "Bombay Beach", "Sonny Bono", "1003",  
                                 "Naval Test Base", "Salton City"), 
                    zone=c("N", "N", "E", "E", "E", "W", "W"))
cam_pref <- list("N"=c(12, 13), "E"=c(10, 9, 11, 8, 4), "W"=c(2, 7))

# detect dust events
met_clean$date <- as.Date(met_clean$datetime %m-% minutes(1), 
                         tz='America/Los_Angeles')
pm_clean$date <- as.Date(pm_clean$datetime %m-% minutes(1), 
                         tz='America/Los_Angeles')
pm_summary <- pm_clean %>% select(-teom_bp, -teom_at, -teom_rh) %>% 
    filter(!is.na(date)) %>%
    group_by(deployment, date) %>%
    summarize(daily.pm10.avg = round(sum(pm10, na.rm=T)/sum(!is.na(pm10)), 1), 
              daily.pm25.avg = round(sum(pm25, na.rm=T)/sum(!is.na(pm25)), 1),
              valid.hours.pm10 = sum(!is.na(pm10)), 
              valid.hours.pm25 = sum(!is.na(pm25)))
pm_summary$valid.pm10 <- sapply(pm_summary$valid.hours.pm10, 
                                function(x) if_else(x>=18, T, F))
pm_summary$valid.pm25 <- sapply(pm_summary$valid.hours.pm25, 
                                function(x) if_else(x>=18, T, F))
for (i in 1:nrow(pm_summary)){
    if (!pm_summary$valid.pm10[i] | is.nan(pm_summary$daily.pm10.avg[i])){
        pm_summary$daily.pm10.avg[i] <- NA
    } 
    if (!pm_summary$valid.pm25[i] | is.nan(pm_summary$daily.pm25.avg[i])){
        pm_summary$daily.pm25.avg[i] <- NA
    }
}

events <- pm_summary %>% ungroup() %>%
    filter((daily.pm10.avg > pm10_cutoff & valid.pm10) | 
           (daily.pm25.avg > pm25_cutoff & valid.pm25)) %>%
    arrange(date)

# get background for use in dust rose plot
ext <- sp::SpatialPointsDataFrame(coords=data.frame(x=c(-116.25, -115.4), 
                                             y=c(33.015, 33.66)), 
                            data=data.frame(id=1:2), 
                            proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
background <- photo_background(ext@coords[1], ext@coords[2], 
                               ext@coords[3], ext@coords[4], 
                               zone="11N", utm_bounds=F, 
                               key=google_key)
# set coordinates for determining daylight hours
salton_sea <- matrix(c(-115.8434, 33.3286), nrow=1) 

event_list <- vector(mode="list", length=length(unique(events$date)))
names(event_list) <- unique(events$date)
for (i in names(event_list)){ 
    pm_tmp <- pm_clean %>% filter(date==i) %>%
        select(deployment, datetime, pm10, pm25)
    met_tmp <- met_clean %>% filter(date==i) %>%
        select(deployment, datetime, ws, wd)
    event_df <- left_join(pm_tmp, met_tmp, by=c("deployment", "datetime")) %>%
        left_join(zones, by="deployment")
    a <- select(event_df, deployment, datetime, value=pm10) %>% 
        mutate(factor="PM10 (ug/m^3)")
    b <- select(event_df, deployment, datetime, value=ws) %>% 
        mutate(factor="Wind Speed (m/s)")
    plot_df <- rbind(a, b)
    dummy <- met_clean %>% 
        select(deployment, datetime, value=ws) %>% 
        mutate(factor="Wind Speed (m/s)")
    dummy$datetime <- rep(met_tmp$datetime[1], nrow(dummy))
    # build timeseries plot
    timeseries <- plot_df %>%
        arrange(deployment, datetime) %>% 
        ggplot(aes(x=datetime, y=value)) +
        geom_path(aes(color=deployment)) +
        facet_grid(factor ~ ., scales="free_y") +
        geom_blank(data=dummy) + 
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
        tmp_pm <- filter(daylight_pm, zone==j & !is.na(pm10))
        if (nrow(tmp_pm)==0){
            image.key <- c()
        } else{
            worst_hour <- hour(tmp_pm[tmp_pm$pm10==max(tmp_pm$pm10), ]$datetime) - 1
            tmp_wind <- filter(daylight_wind, zone==j & hour(datetime)==worst_hour)
            if (nrow(tmp_wind)==0){
                tmp_wind <- filter(daylight_wind, hour(datetime)==worst_hour)
            }
            target.datetime <- tmp_wind[tmp_wind$ws==max(tmp_wind$ws), ]$datetime[1]
            target.datetime <- format(target.datetime, tz="Etc/GMT+8")
            image_tmp <- image_df %>% 
                left_join(zones, by="deployment") %>%
                filter(date(datetime)==date(target.datetime) & zone==j) %>%
                filter(hour(datetime)==worst_hour) %>%
                mutate(delta = abs(difftime(datetime, target.datetime)))
            possible_images <- image_tmp %>% group_by(image_deployment_id) %>%
                filter(delta==min(delta)) %>%
                mutate(sort_field = factor(image_deployment_id, 
                                           levels=cam_pref[[j]]), ordered=T) %>%
                arrange(sort_field)
            pic.deployment <- possible_images$deployment[1]
            image.key <- substring(possible_images$s3_url[1], 49)
        }
        if (length(image.key)!=0 & !is.na(image.key)){
            image.file <- tempfile()
            S3_save_object(bucket="saltonimages", key=image.key, file=image.file)
            img <- jpeg::readJPEG(image.file)
            prelim.grob <- grid::rasterGrob(img, interpolate=T)
            p1 <- ggplot(data.frame(x=1:10, y=1:10), aes(x=x, y=y)) +
                  theme(panel.background=element_blank(), 
                        axis.title=element_blank(), 
                        axis.text=element_blank(), 
                        axis.ticks=element_blank(),
                        legend.position="none", 
                        plot.subtitle = element_text(hjust=0.5),
                        plot.title = element_text(hjust=0.5)) +
                  annotation_custom(prelim.grob, xmin=-Inf, xmax=Inf, 
                                    ymin=-Inf, ymax=Inf) +
                  ggtitle(pic.deployment,
                          subtitle=paste0(possible_images$dir[1], " ", 
                                          substring(possible_images$datetime[1], 12)))
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
    #    plot_data <- event_df %>% select(deployment, datetime, pm10, wd) %>%
    #        filter(!is.na(pm10) &  !is.na(wd))
    plot_data <- event_df %>% select(deployment, datetime, pm10, wd) %>%
        filter(!is.na(pm10))
    wd_missing <- plot_data[is.na(plot_data$wd), 1:2]
    if (nrow(wd_missing)>0){
        wind_fill_query <- paste0("SELECT i.deployment, m.datetime, ",
                                  "m.wdv_2d as wd, ",
                                  "flags.is_invalid(m.deployment_id, 136, ", 
                                  "m.datetime - '1 hour'::interval, m.datetime) ", 
                                  "AS invalid_wd ",
                                  "FROM met.met_1hour m JOIN info.deployments i ",
                                  "ON m.deployment_id=i.deployment_id ",
                                  "WHERE i.deployment IN ('",
                                  paste(unique(wd_missing$deployment), 
                                        collapse="', '"), "') ", 
                                  "AND (m.datetime - '1 second'::interval)::date=", 
                                  "'", i, "'::date;")
        wd_fill <- query_db("saltonsea", wind_fill_query) %>%
            filter(!invalid_wd)
        plot_data <- plot_data %>% 
            left_join(wd_fill, by=c("deployment", "datetime")) %>%
            mutate(wd=coalesce(wd.x, wd.y)) %>% select(-wd.x, -wd.y)
    }
    # filter out Naval Test Base for August 2016 - bad wind data
    if (year(start_date)==2016 & month(start_date)<11){
        plot_data <- filter(plot_data, deployment!='Naval Test Base')
    }
    event_list[[i]]$map <- event_plot(loc_df, plot_data, background)
    event_list[[i]]$map_img <- paste0(tempfile(), ".png")
    ht <- ifelse(nrow(filter(events, date==i))>3, 2.5, 3)
    png(filename=event_list[[i]]$map_img, width=8, height=ht, units="in", 
        res=300)
    print(event_list[[i]]$map)
    dev.off()
}
names(events) <- c("Deployment", "Date", 
               "24-hour PM<sub>10</sub> Avg. (ug/m<sup>3</sup>)", 
               "24-hour PM<sub>2.5</sub> Avg. (ug/m<sup>3</sup>)", 
               "valid.hours.pm10", "valid.hours.pm25", "valid.pm10", 
               "valid.pm25")
