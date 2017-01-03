library(lubridate)
library(tidyverse)

teom_summary <- vector(mode="list", length=5)
names(teom_summary) <- c("PM10 (micrograms/m<sup>3</sup>)", 
                         "PM2.5 (micrograms/m<sup>3</sup>)", 
                         "Pressure (atm)", "Temperature (<sup>o</sup>C)", 
                         "Relative Humidity (%)")
row_names <- c("Data Capture", "Monthly Mean", "Max Hour", "Min Hour")
for(i in 1:5){
    col_ind <- names(pm_df)[i+2]
    round_ind <- ifelse(i==3, 3, 1)
    month_df <- pm_df %>% select_("deployment", col_ind) %>% group_by_("deployment") %>%
        summarize_(data.capture=paste0("round(length(", col_ind, ")/", data_hours, 
                                       "*100, ", round_ind, ")"),
                   monthly.mean=paste0("round(mean(", col_ind, "), ", 
                                       round_ind, ")"), 
                   max.hour=paste0("round(max(", col_ind, "), ", 
                                   round_ind, ")"),
                   min.hour=paste0("round(min(", col_ind, "), ", 
                                   round_ind, ")" ))
    month_df$data.capture <- paste0(month_df$data.capture, "%")
    month_tbl <- as.data.frame(t(month_df), col.names=pm10_month$deployment, 
                               optional=T)[-1, ]
    colnames(month_tbl) <- month_df$deployment
    rownames(month_tbl) <- row_names
    teom_summary[[i]] <- month_tbl
}

