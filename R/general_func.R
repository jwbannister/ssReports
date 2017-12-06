
report_header <- function(start_date, end_date, report_date){
    logo <- path.expand("~/code/ssReports/data/logo.png")
    cat("<img style=\"float: right;\" src=\"", logo, "\"> \n", sep="")
    cat(" \n# ", as.character(month(start_date, label=T, abbr=F)), " ", 
        year(start_date), " Monitoring Summary \n")
    cat(" \n## IID Air Quality Program\n")
    cat(" \n##### Summary Period: ", format(start_date, "%m-%d-%Y"), " through ", 
        format(end_date, "%m-%d-%Y"), " \n")
    cat(" \n##### Report Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}

tag_value <- function(val, css_tag, threshold){
    tagged_txt <- ifelse(val>threshold, 
                         paste0("<", css_tag, ">", val, "</", css_tag, ">"), 
                         as.character(val))
    tagged_txt
}

    
report_header_multi <- function(start_date, end_date, report_date){
    logo <- path.expand("~/code/ssReports/data/logo.png")
    cat("<img style=\"float: right;\" src=\"", logo, "\"> \n", sep="")
    cat(" \n# ", as.character(month(start_date, label=T, abbr=F)), " ", 
        year(start_date), " - ", as.character(month(end_date, label=T, abbr=F)), 
        " ", year(end_date), " Monitoring Summary \n")
    cat(" \n## IID Air Quality Program\n")
    cat(" \n##### Summary Period: ", format(start_date, "%m-%d-%Y"), " through ", 
        format(end_date, "%m-%d-%Y"), " \n")
    cat(" \n##### Report Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}
    
