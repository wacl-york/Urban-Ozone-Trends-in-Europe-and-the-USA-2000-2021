library(DBI)
library(here)
library(dplyr)
library(purrr)
library(lubridate)

source(here::here('functions','utils.R'))

inputDir = here(readLines("data_config.txt",n = 1),"data","toar","stations")

stationFiles = list.files(inputDir)

con = connect_to_db(FALSE)

for(i in 1:length(stationFiles)){
  thisStation = readRDS(here(inputDir,stationFiles[i])) |>
    mutate(station_id = pluck(station, "id"),
           station_type = interaction(pluck(station, "type"),
                                      pluck(station,"type_of_area")) |>
             as.character(),
           country = pluck(station, "country"),
           lng = round(pluck(station, "coordinates", "lng"),5),
           lat = round(pluck(station, "coordinates", "lat" ),5),
           timezone = pluck(station, "timezone"),
           variable_name = pluck(variable, "name"),
           variable_unit = pluck(variable, "units"),
           data_start_date = ymd_hms(data_start_date),
           data_end_date = ymd_hms(data_end_date)) |>
    select(timeseries_id = id,
           station_id,
           country,
           data_start_date,
           data_end_date,
           station_type,
           variable_name,
           label,
           sampling_frequency,
           sampling_height,
           aggregation,
           lng,
           lat,
           timezone,
           variable_unit)

  if(i == 1){
    dbWriteTable(con, "toarMeta", thisStation, overwrite = TRUE)
  }else{
    dbAppendTable(con, "toarMeta", thisStation)
  }


}

dbDisconnect(con, shutdown = TRUE)
