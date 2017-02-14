library(lubridate)
library(tidyverse)

pm_clean <- pm_df
for (j in 1:nrow(pm_df)){
    for (k in 3:7){
        pm_clean[j, k] <- ifelse(pm_clean[j, k+5], NA, pm_clean[j, k])
    }
}
pm_clean <- pm_clean[ , 1:7]

teom_summary <- vector(mode="list", length=5)
names(teom_summary) <- c("PM<sub>10</sub> (ug/m<sup>3</sup>)", 
                         "PM<sub>2.5</sub> (ug/m<sup>3</sup>)", 
                         "Pressure (atm)", "Temperature (<sup>o</sup>C)", 
                         "Relative Humidity (%)")
row_names <- c("Data Capture", "Monthly Mean", "Max Hour", "Min Hour")
for(i in 1:5){
    col_ind <- names(pm_clean)[i+2]
    round_ind <- ifelse(i==3, 3, 1)
    month_df <- pm_clean %>% select_("deployment", col_ind) %>% group_by_("deployment") %>%
        summarize_(data.capture=paste0("round(sum(!is.na(", col_ind, "))/", data_hours, 
                                       "*100, ", 1, ")"),
                   monthly.mean=paste0("round(mean(", col_ind, ", na.rm=T), ", 
                                       round_ind, ")"), 
                   max.hour=paste0("round(max(", col_ind, ", na.rm=T), ", 
                                   round_ind, ")"),
                   min.hour=paste0("round(min(", col_ind, ", na.rm=T), ", 
                                   round_ind, ")" ))
    month_df[is.na(month_df)] <- "-"
    month_df$data.capture <- paste0(month_df$data.capture, "%")
    month_tbl <- as.data.frame(t(month_df), col.names=pm10_month$deployment, 
                               optional=T)[-1, ]
    colnames(month_tbl) <- month_df$deployment
    rownames(month_tbl) <- row_names
    teom_summary[[i]] <- month_tbl
}

