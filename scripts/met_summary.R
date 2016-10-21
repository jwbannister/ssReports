library(lubridate)
library(tidyverse)

areas <- unique(met_df$deployment)

met_summary <- vector(mode="list", length=length(areas))
names(met_summary) <- areas
row_names <- c("Data Capture", "Monthly Mean", "Max Hour", "Min Hour")
for (i in areas){
temp <- met_df %>% filter(deployment==i) %>%
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


