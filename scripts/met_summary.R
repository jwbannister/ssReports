load_all()
library(lubridate)
library(tidyverse)

areas <- unique(met_df$deployment)

met_clean <- met_df
for (j in 1:nrow(met_df)){
    for (k in 3:9){
        met_clean[j, k] <- ifelse(met_clean[j, k+7], NA, met_clean[j, k])
    }
}
met_clean <- met_clean[ , 1:9]

met_summary <- vector(mode="list", length=length(areas))
names(met_summary) <- areas
for (i in areas){
temp <- met_clean %>% filter(deployment==i) %>%
    select(-deployment) %>%
    gather(measure, value, -datetime) %>%
    group_by(measure) %>%
    summarize(data.capture=round(sum(!is.na(value))/data_hours, 2),
              monthly.mean=round(mean(value, na.rm=T), 1), 
              max.hour=ifelse(sum(!is.na(value))>0, 
                              round(max(value, na.rm=T), 1), NA), 
              min.hour=ifelse(sum(!is.na(value))>0, 
                              round(min(value, na.rm=T), 1), NA))
temp[is.na(temp)] <- "-"
met_summary[[i]] <- as.data.frame(t(temp), optional=T)[-1, ]
colnames(met_summary[[i]]) <- temp$measure
met_summary[[i]] <- met_summary[[i]] %>%
    select(ws, wd, at_2m, rh_2m, delta_temp_2m, delta_temp_10m, net_rad)
met_summary[[i]][1, ] <- paste0(as.numeric(met_summary[[i]][1 , ])*100, "%")
met_summary[[i]]$wd[2:4] <- "-"
}


