load_all()
library(tidyverse)
library(lubridate)

flag_df$start.datetime <- as.POSIXct(paste0(flag_df$start.date, " ",
                                            flag_df$start.hour)) 
flag_df$end.datetime <- as.POSIXct(paste0(flag_df$end.date, " ",
                                            flag_df$end.hour)) 
flag_df$end.datetime <- 
    sapply(flag_df$end.datetime, 
           function(x) ifelse(as.Date(x %m-% seconds(1))>end_date, 
                              as.POSIXct(paste(end_date, "23:59:59")), x))
flag_df$end.datetime <- as.POSIXct(flag_df$end.datetime, origin='1970-01-01')

flag_df$interval <- difftime(flag_df$end.datetime,
                             flag_df$start.datetime, units="hours")
                                     
invalid_summary <- flag_df %>% 
    filter(invalidate, deployment %in% unique(pm_clean$deployment)) %>%
    group_by(deployment, flag) %>%
    summarize(hours = as.integer(sum(interval)))
invalid_spread <- invalid_summary %>%
    spread(flag, hours, fill=0)
col_order <- c(names(invalid_spread)[names(invalid_spread)!="Other Invalidation"], 
               "Other Invalidation")
col_num <- match(col_order, names(invalid_spread))
invalid_spread <- invalid_spread %>% select(col_num)
names(invalid_spread)[names(invalid_spread)=="Excessive Negative PM10 Concentration"] <- 
    "Excessive Negative\nPM10 Concentration"
names(invalid_spread)[1] <- "Deployment"



