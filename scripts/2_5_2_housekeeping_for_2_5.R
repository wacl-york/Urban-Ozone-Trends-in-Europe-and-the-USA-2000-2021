library(DBI)
library(dplyr)
library(purrr)
library(mirai)
library(tidyr)
library(stringr)


source(here::here('functions','utils.R'))
source(here::here('functions','regression.R'))

con = connect_to_db(FALSE)

dir_list = list(out = file.path(data_path(), "series"))
dir_list$checks = file.path(dir_list$out, "checks")
dir_list$hour = file.path(dir_list$out, "dat_hour")
dir_list$daily_all = file.path(dir_list$out, "dat_daily_all")
dir_list$daily_day = file.path(dir_list$out, "dat_daily_day")
dir_list$daily_night = file.path(dir_list$out, "dat_daily_night")
dir_list$mda8 = file.path(dir_list$out, "dat_mda8")
dir_list$metrics = file.path(dir_list$out, "dat_metrics")
dir_list$coverage_annual = file.path(dir_list$out, "dat_coverage_annual")
dir_list$coverage_warm = file.path(dir_list$out, "dat_coverage_warm")
dir_list$logs = file.path(dir_list$out, "logs")

# series_checks -----------------------------------------------------------

if(dbExistsTable(con,"series_checks")){
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

if(dbExistsTable(con,"dat_hour")){
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
      'date': 'TIMESTAMP',
      'local_date': 'TIMESTAMP',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR',
      'timezone': 'VARCHAR',
      'value': 'DECIMAL',
      'anom': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)


# dat_daily_all -----------------------------------------------------------------

if(dbExistsTable(con,"dat_daily_all")){
  dbRemoveTable(con, "dat_daily_all")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE dat_daily_all AS
    SELECT *
    FROM read_csv('{{dir_list$daily_all}}/*.csv',
      columns = {
      'date': 'TIMESTAMP',
      'x': 'DECIMAL',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR',
      'timezone': 'VARCHAR',
      'value': 'DECIMAL',
      'anom': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)

# dat_daily_day -----------------------------------------------------------------

if(dbExistsTable(con,"dat_daily_day")){
  dbRemoveTable(con, "dat_daily_day")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE dat_daily_day AS
    SELECT *
    FROM read_csv('{{dir_list$daily_day}}/*.csv',
      columns = {
      'date': 'TIMESTAMP',
      'x': 'DECIMAL',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR',
      'timezone': 'VARCHAR',
      'value': 'DECIMAL',
      'anom': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)

# dat_daily_day -----------------------------------------------------------------

if(dbExistsTable(con,"dat_daily_night")){
  dbRemoveTable(con, "dat_daily_night")
}

dbExecute(
  con,
  glue::glue("
  CREATE TABLE dat_daily_night AS
    SELECT *
    FROM read_csv('{{dir_list$daily_night}}/*.csv',
      columns = {
      'date': 'TIMESTAMP',
      'x': 'DECIMAL',
      'station_id': 'VARCHAR',
      'name': 'VARCHAR',
      'timezone': 'VARCHAR',
      'value': 'DECIMAL',
      'anom': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)

# dat_mda8 ----------------------------------------------------------------

if(dbExistsTable(con,"dat_mda8")){
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
      'mda8': 'DECIMAL',
      'mda8_anom': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)


# dat_metrics -------------------------------------------------------------

if(dbExistsTable(con,"dat_metrics")){
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
      'metric': 'VARCHAR',
      'x': 'DECIMAL'
      },
      nullstr = 'NA')", .open = "{{", .close = "}}", sep = "")
)

# coverage_warm  -------------------------------------------------------------

if(dbExistsTable(con,"coverage_warm")){
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

if(dbExistsTable(con,"coverage_annual")){
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


# regression_type ---------------------------------------------------------

regression_type = valid_series |>
  select(name, station_id) |>
  mutate(
    daily_all = T,
    daily_day = T,
    daily_night = T,
    daily_all_warm = T,
    daily_all_cold = T,
    daily_day_warm = T,
    daily_day_cold = T,
    daily_night_warm = T,
    daily_night_cold = T,
    mda8_all = name == "o3",
    mda8_warm = name == "o3",
    mda8_cold = name == "o3",
    metric = name == "o3"
  ) |>
  pivot_longer(-c(name, station_id), names_to = "type") |>
  filter(value)

dbWriteTable(con, "regression_type", regression_type, overwrite = T)


# Regression Scenarios ----------------------------------------------------

regression_scenarios = tbl(con, "dat_daily_all") |>
  mutate(date = date_trunc("year", date)) |>
  left_join(tbl(con, "coverage_annual"), c("date", "station_id", "name")) |>
  filter(coverage_check) |>
  group_by(station_id, name) |>
  summarise(minDate = min(date, na.rm = T),
            maxDate = max(date, na.rm = T)) |>
  collect() |>
  rowwise() |>
  mutate(scenarios = determine_scenarios(minDate, maxDate) |>
           list()) |>
  ungroup() |>
  tidyr::unnest(scenarios)

dbWriteTable(con, "regression_scenarios", regression_scenarios, overwrite = T)

# d/c ---------------------------------------------------------------------

dbDisconnect(con, shutdown = T)
