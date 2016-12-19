load_all()
load_all("~/code/Roses")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RS3)
library(RColorBrewer)

pm10_cutoff <- 150
pm25_cutoff <- 35

# detect dust events
met_df$date <- as.Date(substr(met_df$datetime %m-% minutes(30), 1, 10)) 
pm_df$date <- as.Date(substr(pm_df$datetime %m-% minutes(30), 1, 10))
pm_summary <- pm_df %>% group_by(deployment, date) %>%
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
    event_df <- left_join(pm_tmp, met_tmp, by=c("deployment", "datetime"))
    worst_loc <- filter(event_df, pm10==max(pm10))$deployment
    worst_df <- event_df %>% filter(deployment==worst_loc)
    timeseries <- event_df %>%
        arrange(deployment, datetime) %>% filter(hour(datetime)!=0) %>%
        gather(pm, conc, pm10:pm25) %>%
        filter(pm=="pm10") %>%
        ggplot(aes(x=hour(datetime), y=conc)) +
        geom_path(aes(color=deployment)) +
        ylab(bquote('PM10 Concentration ('*mu~'g/'*m^3~')')) + xlab("Hour")
    fl <- tempfile()
    png(filename=fl, width=8, height=3, units="in", res=300)
    print(timeseries)
    dev.off()
    time_plot <- png::readPNG(fl)
    event_list[[i]]$time_grob <- grid::rasterGrob(time_plot, interpolate=TRUE)

    daylight <- filter(worst_df, between(hour(datetime), 7, 16))
    target.datetime <- daylight[daylight$pm10==max(daylight$pm10), ]$datetime
    image_tmp <- image_df %>% filter(date(datetime)==date(target.datetime)) %>%
        mutate(delta = abs(difftime(datetime, target.datetime)))
    pic.datetime <- filter(image_tmp, delta==min(delta))$datetime
    image.key <- substring(filter(image_tmp, delta==min(delta))$s3_url, 49)
    if (length(image.key)==0) image.key <- NA
    if (!is.na(image.key)){
    image.file <- tempfile()
    S3_bucket_access(image.key, image.file)
    img <- jpeg::readJPEG(image.file)
    image.grob <- grid::rasterGrob(img, interpolate=T)
    event_list[[i]]$image.plot <- ggplot(data.frame(x=1:10, y=1:10), 
                                         aes(x=x, y=y)) +
                                  geom_blank() +
                                  annotation_custom(image.grob, 
                                                    xmin=-Inf, xmax=Inf, 
                                                    ymin=-Inf, ymax=Inf) +
                                  ggtitle(paste0("Salton City Camera 1\n", 
                                                 pic.datetime)) +
                                  theme(panel.background=element_blank(), 
                                        axis.title=element_blank(), 
                                        axis.text=element_blank(), 
                                        axis.ticks=element_blank())
    } else{
        event_list[[i]]$image.plot <- ggplot(data.frame(x=1:10, y=1:10), 
                                             aes(x=x, y=y)) +
                                  geom_text(aes(x=3, y=5, 
                                                label="No Image Available")) +
                                  theme(panel.background=element_blank(), 
                                        axis.title=element_blank(), 
                                        axis.text=element_blank(), 
                                        axis.ticks=element_blank())
    }
    combo.plot <- gridExtra::arrangeGrob(event_list[[i]]$timeseries, 
                                         event_list[[i]]$image.plot, ncol=2)
    combo.file <- paste0(tempfile(), ".jpg")
    ggsave(combo.file, combo.plot, height=3, width=6.75, units="in")
    event_list[[i]]$combo.grob <- 
        grid::rasterGrob(jpeg::readJPEG(combo.file), interpolate=T)
    event_list[[i]]$map <- event_plot(loc_df, 
                                      event_df[complete.cases(event_df), ], 
                                      background)
}

names(events) <- c("Deployment", "Date", "24-hour PM10 Avg.", 
                   "24-hour PM2.5 Avg.")
