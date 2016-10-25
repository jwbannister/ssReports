load_all()
load_all("~/code/Roses")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RS3)
library(RColorBrewer)

pm10_cutoff <- 150
pm25_cutoff <- 50

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

aws_access <- read.table("~/config/credentials/AWS_cred.txt")[2, 1]
aws_secret <- read.table("~/config/credentials/AWS_cred.txt")[4, 1]
S3_connect(aws_access, aws_secret, hostname="s3-us-west-2.amazonaws.com")

event_list <- vector(mode="list", length=length(event_days))
names(event_list) <- event_days
for (i in names(event_list)){ 
    pm_tmp <- pm_df %>% filter(date==i) %>%
        select(deployment, datetime, pm10, pm25)
    met_tmp <- met_df %>% filter(date==i) %>%
        select(deployment, datetime, ws_10m, wd_sonic)
    event_df <- left_join(met_tmp, pm_tmp, by=c("deployment", "datetime"))
    event_df <- event_df[complete.cases(event_df), ]
    worst_loc <- filter(event_df, pm10==max(pm10))$deployment
    event_list[[i]]$timeseries <- event_df %>% filter(deployment==worst_loc) %>%
        arrange(datetime) %>%
        gather(pm, conc, pm10:pm25) %>%
        filter(pm=="pm10") %>%
        ggplot(aes(x=datetime, y=conc)) +
        geom_path() 
    met_day <- filter(met_tmp, between(hour(datetime), 7, 16))
    max_hour <- hour(met_day[met_day$ws_10m==max(met_day$ws_10m), ]$datetime)
    event_list[[i]]$image.key <- 
        substring(filter(image_df, hour(datetime)==max_hour)$s3_url[1], 49)
    event_list[[i]]$map <- event_plot(loc_df, event_df)
}

