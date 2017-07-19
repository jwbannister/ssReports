S3_bucket_access <- function(key, file){
    aws_access <- read.table("~/system/credentials/AWS_cred.txt")[2, 1]
    aws_secret <- read.table("~/system/credentials/AWS_cred.txt")[4, 1]
    RS3::S3_connect(aws_access, aws_secret, hostname="s3-us-west-2.amazonaws.com")
    RS3::S3_get_object("saltonimages", key, file)
}

report_header <- function(start_date, end_date, report_date){
    cat("<img style=\"float: right;\" src=\"/home/john/code/ssReports/data/logo.png\"> \n")
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
    cat("<img style=\"float: right;\" src=\"/home/john/code/ssReports/data/logo.png\"> \n")
    cat(" \n# ", as.character(month(start_date, label=T, abbr=F)), "-", 
        as.character(month(end_date, label=T, abbr=F)), " ", 
        year(start_date), " Monitoring Summary \n")
    cat(" \n## IID Air Quality Program\n")
    cat(" \n##### Summary Period: ", format(start_date, "%m-%d-%Y"), " through ", 
        format(end_date, "%m-%d-%Y"), " \n")
    cat(" \n##### Report Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}
    
