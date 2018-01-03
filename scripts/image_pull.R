
query1 <- paste0("SELECT i.datetime, d.deployment, i.image_deployment_id, f.s3_url ", 
                 "FROM images.images i ", 
                 "JOIN images.image_files f ON i.image_file_id=f.image_file_id ", 
                 "JOIN images.image_deployments id ", 
                 "ON f.image_deployment_id = id.image_deployment_id ", 
                 "JOIN info.deployments d ON id.deployment_id=d.deployment_id ",
                 "WHERE (i.datetime - '1 second'::interval)::date='2017-03-30'::date;")
image_df <- query_db("saltonsea", query1)
