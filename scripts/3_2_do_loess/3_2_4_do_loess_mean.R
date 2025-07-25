library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

dat = tbl(con, "anom_mean")

name_station = tbl(con, "name_station") |>
  collect() |>
  filter(name == "o3")

tables = dbListTables(con)

if("loess_mean" %in% tables){

  complete_name_stations = tbl(con, "loess_mean") |>
    select(name, station_id) |>
    distinct() |>
    collect()

  toDo = anti_join(name_station, complete_name_stations, by = c("name", "station_id"))

}else{

  toDo = name_station

}

cli::cli_progress_bar(total = nrow(toDo))

for(i in 1:nrow(toDo)){

  id = toDo$station_id[i]
  nm = toDo$name[i]

  tempDat = dat |>
    filter(station_id == id,
           name == nm) |>
    arrange(date) |>
    collect()

  mod = loess(data = tempDat, formula = anom~x, span = 0.5)
  modVal = predict(mod)

  loessDat = tibble(loess = modVal) |>
    mutate(x = row_number(),
           station_id = id,
           name = nm)

  if("loess_mean" %in% tables){

    dbAppendTable(con, "loess_mean", loessDat)

  }else{

    dbWriteTable(con, "loess_mean", loessDat)
    tables = dbListTables(con)

  }

  cli::cli_progress_update()
}

dbDisconnect(con, shutdown = T)
