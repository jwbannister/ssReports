library(lubridate)
library(tidyverse)

pm_clean <- pm_df
for (j in 1:nrow(pm_df)){
    for (k in 3:7){
        pm_clean[j, k] <- ifelse(pm_clean[j, k+5], NA, pm_clean[j, k])
    }
}
pm_clean <- pm_clean[ , 1:7]

teom_summary <- vector(mode="list", length=length(areas))
names(teom_summary) <- areas
for (i in areas){
temp <- pm_clean %>% filter(deployment==i) %>%
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
    for (j in seq(1,5)){
        if (j==4){
            round_num <- 3
        }else{
            round_num <- 1
        }
        temp[j, 3:5] <- sapply(temp[j, 3:5], 
                       function (x) format(round(as.numeric(x), round_num), nsmall=1))
    }
temp[temp=='NaN' | temp =='NA'] <- "-"
teom_summary[[i]] <- as.data.frame(t(temp), optional=T)[-1, ]
colnames(teom_summary[[i]]) <- temp$measure
}
teom_summary <- teom_summary[sort(names(teom_summary))]
