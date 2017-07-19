#!/usr/bin/env Rscript
library(lubridate)
args <- commandArgs(trailingOnly=TRUE)
if (is.na(mdy(args[1]))){
    print("Invalid date string")
    quit()
}
start_date <- mdy(args[1]) # date to start reporting period
end_date <- start_date %m+% months(4) %m-% days(1)
report_date <- format(Sys.Date(), "%m-%d-%Y")
file_name <- paste0("Extended_SS_report_", month(start_date, label=TRUE), 
                    year(start_date), "-", month(end_date, label=TRUE), 
                    year(end_date), ".pdf")
fl1 <- tempfile(fileext='.html')
fl2 <- tempfile(fileext='.pdf')

# render HTML file from markdown document
rmarkdown::render("scripts/multimonth_report.Rmd", output_file=fl1)
# convert HTML to PDF 
convert_command <- paste0("xvfb-run wkhtmltopdf --page-size letter ", 
                          "--javascript-delay 2000 ", fl1, " ", fl2) 
system(convert_command)
system(paste0("gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 ",
              "-dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH ",
              "-dDetectDuplicateImages -dCompressFonts=true -r150 ",
              "-sOutputFile=", 
              path.expand(paste0("~/dropbox/salton/'Salton Sea Field Operations'/", 
                                 "0_Level2/'Monthly Reports'/Drafts/")), file_name, 
              " ", fl2))

# save workspace if needed for debugging
img_fl <- paste0("/tmp/SS_report_image.RData")
save.image(file=img_fl)
print(img_fl)


