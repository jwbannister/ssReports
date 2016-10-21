library(lubridate)
library(tidyverse)

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

