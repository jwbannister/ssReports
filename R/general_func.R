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
    cat(" \n## IID Air Quality \n")
    cat(" \nSummary Period: ", format(start_date, "%m-%d-%Y"), " through ", 
        format(end_date, "%m-%d-%Y"), " \n")
    cat(" \nReport Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}
