library(lubridate)
library(tidyverse)

query1 <- paste0("SELECT i.deployment, m.datetime, m.ws_10m, m.wd_sonic, ", 
                 "m.delta_temp_2m, m.delta_temp_10m, m.net_rad, m.rh_2m, ",
                 "m.at_2m ", 
                 "FROM met.met_1hour m JOIN info.deployments i ",
                 "ON m.deployment_id = i.deployment_id ", 
                 "WHERE datetime BETWEEN '", start_date, " 01:00:00' ",
                 "AND '", end_date %m+% days(1), " 00:00:00' ",
                 "AND i.deployment IN ('Bombay Beach', 'Sonny Bono', ", 
                 "'Torres-Martinez', 'Naval Test Base', ",
                 "'Salton City', 'Salton Sea Park')")
met_pull <- query_salton(query1)
met_pull$delta_temp_diff <- 
    met_pull$delta_temp_10m - met_pull$delta_temp_2m
met_pull <- select(met_pull, -delta_temp_10m, -delta_temp_2m)
met_pull <- met_pull[complete.cases(met_pull), ]
areas <- unique(met_pull$deployment)

met_summary <- vector(mode="list", length=length(areas))
names(met_summary) <- areas
row_names <- c("Data Capture", "Monthly Mean", "Max Hour", "Min Hour")
for (i in areas){
temp <- met_pull %>% filter(deployment==i) %>%
    select(-deployment) %>%
    gather(measure, value, -datetime) %>%
    group_by(measure) %>%
    summarize(data.capture=round(length(value)/data_hours, 2),
              monthly.mean=round(mean(value), 1), 
              max.hour=round(max(value), 1), 
              min.hour=round(min(value), 1)) 
met_summary[[i]] <- as.data.frame(t(temp), col.names=temp$measure, 
                                  optional=T)[-1, ]
colnames(met_summary[[i]]) <- temp$measure
rownames(met_summary[[i]]) <- row_names
}


