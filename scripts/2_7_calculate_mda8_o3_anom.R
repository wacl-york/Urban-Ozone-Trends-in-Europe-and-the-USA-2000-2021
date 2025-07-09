library(DBI)
library(here)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

if(dbExistsTable(con, "mda8_o3_anom")){
  dbRemoveTable(con, "mda8_o3_anom")
}

dbExecute(
  con,
  '
  CREATE TABLE mda8_o3_anom AS
    SELECT q01.*, mda8 - season AS mda8_anom
    FROM (
      SELECT seasonality.*, date, "value", timezone, mda8, local_date, x, "day"
      FROM seasonality
      LEFT JOIN (
        SELECT mda8_o3.*, EXTRACT(MONTH FROM date) AS m
        FROM mda8_o3
      ) RHS
        ON (
          seasonality."name" = RHS."name" AND
          seasonality.station_id = RHS.station_id AND
          seasonality.m = RHS.m
        )
    ) q01
  '
)

dbDisconnect(con, shutdown = T)
