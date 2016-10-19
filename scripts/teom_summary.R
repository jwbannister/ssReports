library(lubridate)
library(tidyverse)

# pull and summarize TEOM data
query1 <- paste0("SELECT i.deployment, t.datetime, t.pm10, t.pm25 ", 
                 "FROM teom.pm_1hour t JOIN info.deployments i ",
                 "ON t.deployment_id = i.deployment_id ", 
                 "WHERE datetime BETWEEN '", start_date, " 01:00:00' ",
                 "AND '", end_date %m+% days(1), " 00:00:00'")
pm_pull <- query_salton(query1)
pm_df <- pm_pull[complete.cases(pm_pull), ]
pm_df$datetime <- as.POSIXct(as.character(pm_df$datetime), tz="UTC")
hour_seq <- seq(as.POSIXct(paste0(start_date, " 01:00:00"), tz="UTC"),
                as.POSIXct(paste0(end_date %m+% days(1), " 00:00:00"), tz="UTC"), 
                by="hour")
data_hours <- length(hour_seq)
row_names <- c("Data Capture", "Monthly Mean", "Max Hour", "Min Hour")
# summarize PM10 data
pm10_month <- pm_df %>% group_by(deployment) %>%
    summarize(data.capture=round(length(pm10)/data_hours, 2),
              monthly.mean=round(mean(pm10), 1), 
              max.hour=round(max(pm10), 1), 
              min.hour=round(min(pm10), 1)) 
pm10_tbl <- as.data.frame(t(pm10_month), col.names=pm10_month$deployment, 
                          optional=T)[-1, ]
colnames(pm10_tbl) <- pm10_month$deployment
rownames(pm10_tbl) <- row_names
# summarize PM25 data
pm25_month <- pm_df %>% group_by(deployment) %>%
    summarize(data.capture=round(length(pm25)/data_hours, 2),
              monthly.mean=round(mean(pm25), 1), 
              max.hour=round(max(pm25), 1), 
              min.hour=round(min(pm25), 1)) 
pm25_tbl <- as.data.frame(t(pm25_month), col.names=pm25_month$deployment, 
                          optional=T)[-1, ]
colnames(pm25_tbl) <- pm25_month$deployment
rownames(pm25_tbl) <- row_names

# detect dust events
pm_df$date <- as.Date(pm_df$datetime %m-% hours(1), tz="UTC", 
                         format="%m-%d%-%y")
pm_summary <- pm_df %>% group_by(deployment, date) %>%
    summarize(daily.pm10.avg = sum(pm10)/24, 
              daily.pm25.avg = sum(pm25)/24)
events <- pm_summary %>% filter(daily.pm10.avg > 150 | daily.pm25.avg > 50)
