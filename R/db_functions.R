
# query of AWS saltonsea database
query_salton <- function(query){
  a <- Sys.getenv(c("PSQL_HOST_SS", "PSQL_PASSWORD_SS", "PSQL_USER", "PSQL_PORT"))
    print(a)
  con <- RPostgreSQL::dbConnect("PostgreSQL", host=a[1], port=a[4],
                                dbname="saltonsea", user=a[3], password=a[2])
  dat <- RPostgreSQL::dbGetQuery(con, query)
  RPostgreSQL::dbDisconnect(con)
  dat
}
