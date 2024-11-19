library(DBI)
library(dplyr)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

if(dbExistsTable(con, "valid_scen")){
  dbRemoveTable(con, "valid_scen")
}

if(dbExistsTable(con, "reg_anom_filt")){
  dbRemoveTable(con, "reg_anom_filt")
}

if(dbExistsTable(con, "pqr_r2_max")){
  dbRemoveTable(con, "pqr_r2_max")
}

valid_scen_header = c(
  "scenario_idx" = "numeric",
  "reg" = "character",
  "name" = "character",
  "station_id" = "character",
  "r2" = "numeric"
)

dbCreateTable(con, "valid_scen", valid_scen_header)

dbExecute(
  con,
  "
  INSERT INTO valid_scen
  SELECT scenario_idx, reg, name, station_id, r2
  FROM reg_anom_r2
  ANTI JOIN (
    SELECT DISTINCT station_id, name, scenario_idx, startYear, endYear , endYear - startYear AS endDiff
    FROM qr_regressions
    WHERE endDiff <= 2
  )
  USING (station_id, name, scenario_idx)
  WHERE (reg = 'pqr_1') OR (reg = 'pqr_2')
"
)

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
  INSERT INTO reg_anom_filt
  SELECT t3.x, t3.date, t1.station_id, t1.name, t2.scenario_idx, t3.piece, t3.anom, t3.reg, t3.value
    FROM (
      SELECT station_id, name, reg, MAX(r2) as r2_max
      FROM valid_scen
      GROUP BY station_id, name, reg
    ) t1

    INNER JOIN valid_scen as t2
      ON t1.station_id = t2.station_id AND t1.name = t2.name AND t1.r2_max = t2.r2 AND t1.reg = t2.reg

    INNER JOIN reg_anom AS t3
      ON t1.station_id = t3.station_id AND t1.name = t3.name AND t2.scenario_idx = t3.scenario_idx AND t1.reg = t3.reg
  "
)

dbExecute(
  con,
  "
  CREATE TABLE pqr_r2_max AS
  WITH valid_scen AS (
   SELECT station_id, name, reg, scenario_idx, r2
      FROM reg_anom_r2
      ANTI JOIN (
        SELECT DISTINCT station_id, name, scenario_idx, startYear, endYear , endYear - startYear AS endDiff
        FROM qr_regressions
        WHERE endDiff <= 2
      )
        USING (station_id, name, scenario_idx)
      WHERE (reg = 'pqr_1') OR (reg = 'pqr_2')
  )

  SELECT t1.station_id, t1.name, t2.scenario_idx, t2.reg, t2.r2
    FROM (
      SELECT station_id, name, reg, MAX(r2) as r2_max
      FROM valid_scen
      GROUP BY station_id, name, reg
    ) t1
    INNER JOIN valid_scen as t2
      ON t1.station_id = t2.station_id AND t1.name = t2.name AND t1.r2_max = t2.r2 AND t1.reg = t2.reg


  "
)


dbDisconnect(con, shutdown = T)
