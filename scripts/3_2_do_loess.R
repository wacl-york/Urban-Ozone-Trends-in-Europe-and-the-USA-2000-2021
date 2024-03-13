library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

dat = tbl(con, "monthly_anom")

name_station = tbl(con, "name_station") |>
  collect()

tables = dbListTables(con)

if("loess" %in% tables){

  complete_name_stations = tbl(con, "loess") |>
    select(name, station_id) |>
    distinct()
  collect()

  toDo = anti_join(name_station, complete_name_stations, by = c("name", "station_id"))

}else{

  toDo = name_station

}

for(i in 1:nrow(toDo)){

  id = name_station$station_id[i]
  nm = name_station$name[i]

  tempDat = dat |>
    filter(station_id == id,
           name == nm) |>
    arrange(date) |>
    collect()

  ts = tibble(date = seq(min(tempDat$date), max(tempDat$date), "month"))

  tempDat = ts |>
    left_join(tempDat, by = "date") |>
    group_by(date, name, station_id, station_type) |>
    summarise_all(mean, na.rm = T) |>
    ungroup() |>
    mutate(x = row_number())

  mod = loess(data = tempDat, formula = anom~x, span = 0.5)
  modVal = predict(mod)

  loessDat = tibble(loess = modVal) |>
    mutate(x = row_number(),
           station_id = id,
           name = nm)

  if("loess" %in% tables){

    dbAppendTable(con, "loess", loessDat)

  }else{

    dbWriteTable(con, "loess", loessDat)
    tables = dbListTables(con)

  }

}

dbDisconnect(con, shutdown = T)
