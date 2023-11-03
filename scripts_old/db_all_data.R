library(DBI)
library(here)

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"), read_only = FALSE)

if("all_data" %in% dbListTables(con)){
  dbRemoveTable(con, "all_data")
}


dbExecute(con,
          "
           CREATE TABLE all_data as
            SELECT date, CONCAT_WS('', station_id) AS station_id, name, value, 'us' AS region
            FROM data
            JOIN (
              SELECT station_id, timeseries_id
              FROM (
                UNPIVOT stations
                ON timeseries_id_no2, timeseries_id_o3
                INTO
                  NAME station_id
                  VALUE timeseries_id
              )
            ) USING (timeseries_id)
            WHERE (flag_value <= 7)
           "
)

dbExecute(con,
          "INSERT INTO all_data
              SELECT date, station_id, variable AS name, value, 'eea' as region
              FROM eeaData
              WHERE (flag_value  == 1)
           ")

dbDisconnect(con, shutdown = T)
