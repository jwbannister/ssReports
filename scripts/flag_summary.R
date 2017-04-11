load_all()
library(tidyverse)
library(lubridate)

flag_df$start.datetime <- as.POSIXct(paste0(flag_df$start.date, " ",
                                            flag_df$start.hour)) 
flag_df$end.datetime <- as.POSIXct(paste0(flag_df$end.date, " ",
                                            flag_df$end.hour)) 
flag_df$interval <- difftime(flag_df$end.datetime,
                             flag_df$start.datetime, units="hours")
                                     
invalid_summary <- flag_df %>% 
    filter(invalidate, deployment %in% unique(pm_clean$deployment)) %>%
    group_by(deployment, flag) %>%
    summarize(hours = as.integer(sum(interval)))
invalid_spread <- invalid_summary %>%
    spread(flag, hours, fill=0)

names(invalid_spread)[1] <- "Deployment"


