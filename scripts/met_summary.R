load_all()
library(lubridate)
library(tidyverse)

areas <- unique(met_df$deployment)

met_summary <- vector(mode="list", length=length(areas))
names(met_summary) <- areas
for (i in areas){
temp <- met_df %>% filter(deployment==i) %>%
    select(-deployment, -wd) %>%
    gather(measure, value, -datetime) %>%
    group_by(measure) %>%
    summarize(data.capture=round(length(value)/data_hours, 2),
              monthly.mean=round(mean(value), 1), 
              max.hour=round(max(value), 1), 
              min.hour=round(min(value), 1)) 
met_summary[[i]] <- as.data.frame(t(temp), optional=T)[-1, ]
colnames(met_summary[[i]]) <- temp$measure
met_summary[[i]] <- met_summary[[i]] %>%
    select(ws, at_2m, rh_2m, delta_temp_2m, delta_temp_10m)
}


