
query_salton <- function(query){
  usr <- readLines("~/config/credentials/airsci_db_cred.txt")[3]
  psswrd <- readLines("~/config/credentials/airsci_db_cred.txt")[4]
  hst <- "airdbss1.cwxikzzkese5.us-west-2.rds.amazonaws.com"
  con <- RPostgreSQL::dbConnect("PostgreSQL", host=hst, port=5432,
                                dbname="saltonsea", user=usr, password=psswrd)
  dat <- RPostgreSQL::dbGetQuery(con, query)
  RPostgreSQL::dbDisconnect(con)
  dat
}
