
query1 <- "SELECT pm.datetime, pm.pm10, pm.pm25, dp.deployment 
  FROM teom.pm_1hour pm 
  INNER JOIN info.deployments dp 
  ON (pm.deployment_id = dp.deployment_id)
  WHERE datetime::date BETWEEN '2016-03-01'::date AND '2016-04-01'::date
  ORDER BY (dp.deployment, pm.datetime)"

# test query for example, no invalid or null filtering
query1 <- function(start_date, end_date){
    query <- paste0("SELECT dp.deployment, MIN(pm.pm10) AS min_pm10, ", 
                        "MAX(pm.pm10) AS max_pm10, MEAN(pm.pm10) AS mean_pm10, ", 
                        "MIN(pm.pm25) AS min_pm25, MAX(pm.pm25) AS max_pm25, ",
                        "MEAN(pm.pm25) AS mean_pm25
                    "FROM generate_series('" 2016-03-01'::date, '2016-04-01'::date, ",
                        "'1 hour'::interval) ",
                    AS s(datetime) 
  LEFT OUTER JOIN teom.pm_1hour pm 
  ON (s.datetime = pm.datetime)
  INNER JOIN info.deployments dp 
  ON (pm.deployment_id = dp.deployment_id)
  WHERE s.datetime::date BETWEEN '2016-03-01'::date AND '2016-04-01'::date
  GROUP BY dp.deployment

# invalid and null filtering in progress
SELECT DISTINCT s.datetime, dp.deployment, pm.pm10, flg.invalidate
  FROM generate_series('2016-03-01'::date, '2016-04-01'::date, '1 hour'::interval) 
  AS s(datetime) 
  LEFT OUTER JOIN teom.pm_1hour pm 
  ON (s.datetime = pm.datetime)
  INNER JOIN info.deployments dp 
  ON (pm.deployment_id = dp.deployment_id) 
  LEFT OUTER JOIN flags.flagged_data flg
  ON (pm.deployment_id = flg.deployment_id 
      AND s.datetime::timestamp <@ flg.flagged_period)
  WHERE s.datetime::date BETWEEN '2016-03-01'::date AND '2016-04-01'::date
  ORDER BY (dp.deployment, s.datetime)

