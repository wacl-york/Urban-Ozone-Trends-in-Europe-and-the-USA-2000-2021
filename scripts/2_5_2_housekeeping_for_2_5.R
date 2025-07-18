library(DBI)
library(dplyr)
library(purrr)
library(mirai)
library(stringr)

source(here::here('functions','utils.R'))

con = connect_to_db(FALSE)

dir_list = list(out = file.path(data_path(), "series"))
dir_list$checks = file.path(dir_list$out, "checks")
dir_list$hour = file.path(dir_list$out, "dat_hour")
dir_list$mda8 = file.path(dir_list$out, "dat_mda8")
dir_list$metrics = file.path(dir_list$out, "dat_metrics")
dir_list$coverage_annual = file.path(dir_list$out, "dat_coverage_annual")
dir_list$coverage_warm = file.path(dir_list$out, "dat_coverage_warm")
dir_list$logs = file.path(dir_list$out, "logs")

# series_checks -----------------------------------------------------------



if(dbExistsTable("series_checks")){
  dbRemoveTable(con, "series_checks")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE series_checks AS
    SELECT *
    FROM read_csv('{{dir_list$checks}}/*.csv',
      columns = {
        'notOnRemoveList': 'BOOLEAN',
        'passPreCoverageCheck': 'BOOLEAN',
        'passPostCoverageCheck': 'BOOLEAN',
        'name': 'VARCHAR',
        'station_id': 'VARCHAR',
        'qa_originalRows': 'DECIMAL',
        'qa_threshold': 'DECIMAL',
        'qa_persistence': 'DECIMAL',
        'qa_variablity': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)


# dat_hour ----------------------------------------------------------------

if(dbExistsTable("dat_hour")){
  dbRemoveTable(con, "dat_hour")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE dat_hour AS
    SELECT *
    FROM read_csv('{{dir_list$hour}}/*.csv',
      columns = {
      'x': 'DECIMAL',
      'date': 'DATE',
      'local_date': 'DATE',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR',
      'timezone': 'VARCHAR',
      'value': 'DECIMAL',
      'anom': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)


# dat_mda8 ----------------------------------------------------------------

if(dbExistsTable("dat_mda8")){
  dbRemoveTable(con, "dat_mda8")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE dat_mda8 AS
    SELECT *
    FROM read_csv('{{dir_list$mda8}}/*.csv',
      columns = {
      'x': 'DECIMAL',
      'date': 'DATE',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR',
      'timezone': 'VARCHAR',
      'mda8': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)


# dat_metrics -------------------------------------------------------------

if(dbExistsTable("dat_metrics")){
  dbRemoveTable(con, "dat_metrics")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE dat_metrics AS
    SELECT *
    FROM read_csv('{{dir_list$metrics}}/*.csv',
      columns = {
      'date': 'DATE',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR',
      'timezone': 'VARCHAR',
      'value': 'DECIMAL',
      'metric': 'VARCHAR'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)

# coverage_warm  -------------------------------------------------------------

if(dbExistsTable("coverage_warm")){
  dbRemoveTable(con, "coverage_warm")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE coverage_warm AS
    SELECT *
    FROM read_csv('{{dir_list$coverage_warm}}/*.csv',
      columns = {
      'date': 'DATE',
      'n': 'DECIMAL',
      'perc': 'DECIMAL',
      'coverage_check': 'BOOLEAN',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)

# coverage_annual ---------------------------------------------------------

if(dbExistsTable("coverage_annual")){
  dbRemoveTable(con, "coverage_annual")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE coverage_annual AS
    SELECT *
    FROM read_csv('{{dir_list$coverage_annual}}/*.csv',
      columns = {
      'date': 'DATE',
      'n': 'DECIMAL',
      'perc': 'DECIMAL',
      'coverage_check': 'BOOLEAN',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)

# logs --------------------------------------------------------------------

logFiles = list.files(dir_list$logs, pattern = ".err", full.names = T)

daemons(8)
log = map_df(logFiles, in_parallel(\(x) read_log(x), read_log = read_log))
daemons(0)

dbWriteTable(con, "log_2_5", log, overwrite = T)


# valid_series ------------------------------------------------------------

step_reached = function(data, messageContents){

  nDetect = data |>
    pull(message) |>
    str_detect(messageContents) |>
    sum()

  nDetect > 0

}

steps_reached = log |>
  nest_by(name, station_id) |>
  mutate(
    dat_hour = step_reached(data, "make hour_dat"),
    coverage_annual = step_reached(data, "make coverage_annual"),
    coverage_warm = step_reached(data, "make coverage_warm"),
    dat_mda8 = step_reached(data, "make mda8 dat"),
    metric_4MDA8 = step_reached(data, "4MDA8"),
    metric_NDGT70 = step_reached(data, "NDGT70"),
    metric_SOMO35 = step_reached(data, "SOMO35"),
    metric_3MMDA1 = step_reached(data, "3MMDA1"),
    metric_AVGMDA8 = step_reached(data, "AVGMDA8")
  ) |>
  select(-data)

valid_series = tbl(con, "dat_hour") |>
  select(name, station_id) |>
  distinct() |>
  collect() |>
  left_join(steps_reached, by = c("name", "station_id"))

dbWriteTable(con, "valid_series", valid_series, overwrite = T)

# d/c ---------------------------------------------------------------------

dbDisconnect(con, shutdown = T)
