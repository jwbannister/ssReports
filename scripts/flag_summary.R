library(lubridate)
library(tidyverse)

# pull and summarize flag data
query1 <- paste0("SELECT i.deployment, f.flagged_period, ff.flag, ", 
                 "f.flag_note, f.invalidate, f.reviewed ", 
                 "FROM flags.flagged_data f JOIN flags.flags ff ",
                 "ON f.flag_id = ff.flag_id ", 
                 "JOIN info.deployments i ",
                 "ON f.deployment_id = i.deployment_id ") 
flag_pull <- query_salton(query1)
flag_pull$start.date <- substr(flag_pull$flagged_period, 3, 12)
flag_pull$start.hour <- substr(flag_pull$flagged_period, 14, 21)
flag_pull$end.date <- substr(flag_pull$flagged_period, 25, 34)
flag_pull$end.hour <- substr(flag_pull$flagged_period, 36, 43)
flag_df <- flag_pull %>% filter(between(as.Date(start.date), 
                                        as.Date(start_date), 
                                        as.Date(end_date))) %>%
select(deployment, start.date, start.hour, end.date, end.hour, flag, 
       flag_note, reviewed, invalidate)
