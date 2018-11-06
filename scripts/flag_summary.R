load_all()
library(tidyverse)
library(lubridate)

flag_df$start.datetime <- as.POSIXct(paste0(flag_df$start.date, " ",
                                            flag_df$start.hour), 
                                     tz='America/Los_Angeles') 
flag_df$end.datetime <- as.POSIXct(paste0(flag_df$end.date, " ",
                                            flag_df$end.hour), 
                                   tz='America/Los_Angeles') 

flag_df$overshoot <- sapply(flag_df$end.datetime, function (x)
            difftime(as.POSIXct(paste0(end_date, " 00:00:00"), 
                                tz='America/Los_Angeles') %m+% days(1), x, 
                                units="secs"))

for (i in 1:nrow(flag_df)){
    if (flag_df$overshoot[i] < 0){
        flag_df$end.datetime[i] <- flag_df$end.datetime[i] + (flag_df$overshoot[i] - 1)
    }
}

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



