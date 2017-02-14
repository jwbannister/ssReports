#!/usr/bin/env Rscript
library(lubridate)
args <- commandArgs(trailingOnly=TRUE)
if (is.na(mdy(args[1]))){
    print("Invalid date string")
    quit()
}
start_date <- mdy(args[1]) # date to start reporting period
end_date <- start_date %m+% months(1) %m-% days(1)
report_date <- format(Sys.Date(), "%m-%d-%Y")
file_name <- paste0("SS_report_", month(start_date, label=TRUE), 
                    year(start_date))

# render HTML file from markdown document
rmarkdown::render(paste0("scripts/SS_report.Rmd"), 
                  output_file=paste0("~/code/ssReports/output/", 
                                     file_name, ".html"))
# convert HTML to PDF 
convert_command <- paste0("wkhtmltopdf ", "~/code/ssReports/output/", 
                          file_name, ".html ", 
                          "~/dropbox/salton/'Salton Sea Field Operations'/", 
                          "0_Level2/'Monthly Reports'/", file_name, ".pdf")
system(convert_command)

# save workspace if needed for debugging
img_fl <- paste0("/tmp/SS_report_image.RData")
save.image(file=img_fl)
print(img_fl)
