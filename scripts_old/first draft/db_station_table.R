library(DBI)
library(here)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)


stations = list(no2 = readRDS(here("data","stations_no2","stationList_US.RDS")),
                o3 = readRDS(here("data","stations","stationList_US.RDS"))) |>
  map(~.x |>
        mutate(country = pluck(station, "country"),
               type_of_area = pluck(station, "type_of_area"),
               type = pluck(station, "type"),
               station_id = pluck(station, "id"),
               station_type = interaction(type_of_area, type)) |>
        filter(data_start_date <= ymd_hm("2000-01-01 00:00"),
               data_end_date >= ymd_hm("2021-12-31 00:00"),
               sampling_frequency == "hourly",
               str_detect(station_type, "urban")) |>
        rename(timeseries_id = id))


stationsAll = full_join(stations$no2, stations$o3, suffix = c("_no2","_o3"), by = "station_id") |>
  select(station_id, contains("timeseries"))

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"),
                read_only = FALSE)

dbWriteTable(con, "stations",stationsAll, overwrite = TRUE)

dbDisconnect(con, shutdown = T)
