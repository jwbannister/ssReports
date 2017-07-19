#!/usr/bin/env Rscript
library(lubridate)
cl_args <- commandArgs(trailingOnly=FALSE)
if (is.na(mdy(cl_args[6]))){
    print("Invalid date string")
    quit()
}
# set working directory to script directory
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", cl_args[grep(file.arg.name, cl_args)])
if (length(script.name)>0) setwd(dirname(script.name))

start_date <- mdy(cl_args[6]) # date to start reporting period
end_date <- start_date %m+% months(1) %m-% days(1)
report_date <- format(Sys.Date(), "%m-%d-%Y")

file_name <- paste0("SS_report_", month(start_date, label=TRUE), 
                    year(start_date), ".pdf")
fl1 <- tempfile(fileext='.html')
fl2 <- tempfile(fileext='.pdf')

# render HTML file from markdown document
rmarkdown::render("SS_report.Rmd", output_file=fl1)
# convert HTML to PDF 
convert_command <- paste0("xvfb-run wkhtmltopdf --page-size letter ", 
                          "--javascript-delay 2000 ", fl1, " ", fl2) 
system(convert_command)
system(paste0("gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 ",
              "-dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH ",
              "-dDetectDuplicateImages -dCompressFonts=true -r150 ",
              "-sOutputFile=", tempdir(), "/", file_name, " ", fl2))
system(paste0(path.expand(getwd()), "/gdrive upload ", 
              "-p 0B8qHESXOhs-DMzlOcUZHS1ptdXc ", tempdir(), "/", file_name))

# save workspace if needed for debugging
img_fl <- paste0("/tmp/SS_report_image.RData")
save.image(file=img_fl)
print(img_fl)

