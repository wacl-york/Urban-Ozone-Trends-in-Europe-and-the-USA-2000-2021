library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(mirai)
library(lubridate)

# make function so we can use on.exit


make_piecewise = function(array_id, user){

  # Connect to database
  con = dbConnect(duckdb::duckdb(),
                  dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

  on.exit(dbDisconnect(con, shutdown = T))

  metric_station = tbl(con, "mda8_metric_scenarios") |>
    select(metric, station_id) |>
    distinct() |>
    collect() |>
    arrange(metric, station_id)

  # name_station = tbl(con, "name_station") |>
  #   collect()

  mtric = metric_station$metric[array_id]
  stn = metric_station$station_id[array_id]

  regression_scenarios = tbl(con, "mda8_metric_scenarios") |>
    filter(cp2 > cp1 | is.na(cp2) | is.na(cp1) & is.na(cp2)) |>
    select(cp1, cp2, metric, station_id, scenario_idx) |>
    distinct() |>
    collect() |>
    filter(metric == mtric,
           station_id == stn)

  outDir = file.path(readLines(here("data_config.txt"),n = 1),'data','piecewise_metrics',stn,mtric)

  if(!dir.exists(outDir)){
    dir.create(outDir, recursive = T)
  }

  dat = tbl(con, "mda8_metrics") |>
    filter(station_id == !!stn,
           metric == !!mtric) |>
    mutate(
      year = as.integer(year),
      month = 1L,
      day = 1L,
      date = make_date(year, month, day)) |>
    select(-day, -month) |>
    mutate(
      y = year(date)
    ) |>
    collect()

  dbDisconnect(con, shutdown = T)

  # this is a bit of a hack to fix the issue that the startYear and endYear lost there date information along the way
  maxEndYear = dat |>
    filter(date == max(date, na.rm = TRUE)) |>
    collect()

  maxEndYear = maxEndYear$date+months(1)

  daemons(8)
  datOut = map(regression_scenarios$scenario_idx,
               in_parallel(
                 \(regression_scenario) {

                   con = DBI::dbConnect(duckdb::duckdb(),
                                        dbdir = here::here(readLines(here::here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

                   on.exit(DBI::dbDisconnect(con, shutdown = T))

                   thisScenario = dplyr::tbl(con, "qr_regressions_metrics") |>
                     dplyr::filter(
                       station_id == stn,
                       metric == mtric,
                       scenario_idx == regression_scenario,
                       type == "fit"
                     ) |>
                     tidyr::pivot_wider(names_from = "stat") |>
                     dplyr::mutate(startYear = make_date(startYear,1L,1L), # must be ints
                                   endYear = make_date(endYear, 1L, 1L),
                                   endYear = ifelse(endYear == max(endYear), maxEndYear, endYear)
                     ) |>
                     dplyr::collect()

                   # set bounds such that startYear <= y < endYear
                   # regressions might overlap so at least this is consistent
                   pieceDat = dplyr::left_join(
                     thisScenario,
                     dat,
                     dplyr::join_by(
                       between(y$date,
                               x$startYear,
                               x$endYear,
                               bounds = "[)"),
                       "metric",
                       "station_id"),
                     copy = T) |>
                     dplyr::collect()

                   if(nrow(pieceDat) == 0){
                     return(NULL)
                   }else{
                     pieceDat = pieceDat |>
                       dplyr::mutate(piecewise = ((slope*x) + intercept),
                                     station_id = as.character(station_id)) |>
                       dplyr::arrange(x) |>
                       dplyr::select(x, tau, piecewise, scenario_idx, station_id, metric)

                     return(pieceDat)

                   }
                 }, dat = dat, maxEndYear = maxEndYear, stn = stn, mtric = mtric)
  ) |>
    bind_rows()

  daemons(0)

  fileOut = file.path(outDir,  paste0(paste(stn, mtric, sep = "_"), ".csv"))
  write.csv(datOut, fileOut, row.names = F)

}

user = system("echo $USER", intern = T)

args = commandArgs(trailingOnly = TRUE)
array_id = as.numeric(args[1])+1
make_piecewise(array_id, user)
