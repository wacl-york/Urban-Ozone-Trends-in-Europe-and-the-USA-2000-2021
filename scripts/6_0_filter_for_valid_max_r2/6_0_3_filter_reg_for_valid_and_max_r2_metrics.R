library(DBI)
library(dplyr)
library(here)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

if(dbExistsTable(con, "reg_filt_metrics")){
  dbRemoveTable(con, "reg_filt_metrics")
}

if(dbExistsTable(con, "pqr_r2_max_metrics")){
  dbRemoveTable(con, "pqr_r2_max_metrics")
}


dbExecute(
  con,
  "
  CREATE TABLE reg_filt_metrics (
    x DOUBLE,
    date TEXT,
    station_id TEXT,
    metric TEXT,
    scenario_idx INTEGER,
    piece INTEGER,
    metric_value DOUBLE,
    reg TEXT,
    value DOUBLE
  )
  "
)

dbExecute(
  con,
  "
  INSERT INTO reg_filt_metrics
  SELECT t3.x, t3.date, t1.station_id, t1.metric, t2.scenario_idx, t3.piece, t3.metric_value, t3.reg, t3.value
    FROM (
      SELECT station_id, metric, reg, MAX(r2) as r2_max
      FROM reg_r2_metrics
      GROUP BY station_id, metric, reg
    ) t1

    INNER JOIN reg_r2_metrics as t2
      ON t1.station_id = t2.station_id AND t1.metric = t2.metric AND t1.r2_max = t2.r2 AND t1.reg = t2.reg

    INNER JOIN reg_metrics AS t3
      ON t1.station_id = t3.station_id AND t1.metric = t3.metric AND t2.scenario_idx = t3.scenario_idx AND t1.reg = t3.reg
  "
)

dbExecute(
  con,
  "
  CREATE TABLE pqr_r2_max_metrics AS
  WITH filtered_r2 AS (
    SELECT r.station_id, r.metric, r.reg, r.scenario_idx, r.r2
    FROM reg_r2_metrics r
    LEFT JOIN (
      SELECT station_id, metric, scenario_idx
      FROM qr_regressions
      WHERE endYear - startYear <= 2
    ) q
    ON r.station_id = q.station_id AND r.metric = q.metric AND r.scenario_idx = q.scenario_idx
    WHERE (r.reg = 'pqr_1' OR r.reg = 'pqr_2')
      AND q.station_id IS NULL  -- ← exclude matches
  )

  SELECT t1.station_id, t1.metric, t2.scenario_idx, t2.reg, t2.r2
  FROM (
    SELECT station_id, metric, reg, MAX(r2) AS r2_max
    FROM filtered_r2
    GROUP BY station_id, metric, reg
  ) t1

  INNER JOIN filtered_r2 AS t2
    ON t1.station_id = t2.station_id
    AND t1.metric = t2.metric
    AND t1.r2_max = t2.r2
    AND t1.reg = t2.reg
  "
)



dbDisconnect(con, shutdown = T)
