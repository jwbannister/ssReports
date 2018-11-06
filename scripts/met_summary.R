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
        summarize(data.capture=sum(!is.na(value))/data_hours,
                  monthly.mean=mean(value, na.rm=T), 
                  max.hour=ifelse(sum(!is.na(value))>0, 
                                  max(value, na.rm=T), NA), 
                  min.hour=ifelse(sum(!is.na(value))>0, 
                                  min(value, na.rm=T), NA))
    temp[ , 2] <- sapply(temp[ , 2], 
                         function (x) paste0(format(round(x, 2)*100, nsmall=0), "%"))
    for (j in seq(1,7)){
        round_num <- 1
        temp[j, 3:5] <- sapply(temp[j, 3:5], 
                       function (x) format(round(as.numeric(x), round_num), nsmall=1))
    }
    temp[temp=='NaN' | temp =='NA'] <- "-"
    met_summary[[i]] <- as.data.frame(t(temp), optional=T,
                                      stringsAsFactors=F)[-1, ]
    colnames(met_summary[[i]]) <- temp$measure
    met_summary[[i]] <- met_summary[[i]] %>%
        select(ws, wd, at_2m, rh_2m, delta_temp_2m, delta_temp_10m, solar_rad)
    met_summary[[i]]$wd[2:4] <- "-"
}
met_summary <- met_summary[sort(names(met_summary))]
