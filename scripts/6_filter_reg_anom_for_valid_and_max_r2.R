library(DBI)
library(dplyr)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

if(dbExistsTable(con, "reg_anom_filt")){
  dbRemoveTable(con, "reg_anom_filt")
}

dbExecute(
  con,
  "
  CREATE TABLE reg_anom_filt AS
  FROM reg_anom
  LIMIT 0
  "
)

dbExecute(
  con,
  "
  WITH results AS (
   SELECT station_id, name, reg, scenario_idx, r2
      FROM reg_anom_r2
      ANTI JOIN (
        SELECT DISTINCT station_id, name, scenario_idx, startYear, endYear , endYear - startYear AS endDiff
        FROM qr_regressions
        WHERE endDiff <= 2
      )
        USING (station_id, name, scenario_idx)
      WHERE (reg = 'pqr_2')
  )

  INSERT INTO reg_anom_filt
    SELECT t3.x, t3.date, t1.station_id, t1.name, t2.scenario_idx, t3.piece, t3.anom, t3.reg, t3.value
    FROM (
      SELECT station_id, name, MAX(r2) as r2_max
      FROM results
      GROUP BY station_id, name
    ) t1
    INNER JOIN results as t2
      ON t1.station_id = t2.station_id AND t1.name = t2.name AND t1.r2_max = t2.r2

    INNER JOIN reg_anom AS t3
      ON t1.station_id = t3.station_id AND t1.name = t3.name AND t2.scenario_idx = t3.scenario_idx
  "
)

dbDisconnect(con, shutdown = T)

