library(zoo)
library(DBI)
library(here)
library(dplyr)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

calc_mda8 = function(x){

  y = x[!is.na(x)]

  if(length(y) < 6){ # An MDA8 is valid if 6 of 8 hours are present in period
    return(NA)
  }

  mean(x, na.rm = T)

}

con = connect_to_db(FALSE)

stations = tbl(con, "name_station") |>
  filter(name == "o3") |>
  pull(station_id)

if(dbExistsTable(con, "mda8_o3")){
  dbRemoveTable(con, "mda8_o3")
}

cli::cli_progress_bar(total = length(stations))

tzMeta = tbl(con, "combinedMeta") |>
  select(station_id, timezone) |>
  collect()

for(i in 1:length(stations)){

  station_data = tbl(con, "all_data") |>
    filter(name == "o3",
           station_id == local(stations[i])) |>
    select(date, station_id, value, name) |>
    collect() |>
    arrange(date)

  ts = tibble(
    date = seq(
      min(station_data$date),
      max(station_data$date),
      by = "1 hour"),
    station_id = stations[i],
    name = "o3"
  )

  mda8_data = station_data |>
    full_join(ts, c("date","station_id","name")) |>
    left_join(
      tzMeta,
      by = "station_id"
    ) |>
    mutate(
      mda8 = rollapply(
        data = value,
        width = 8,
        FUN = calc_mda8,
        fill = NA,
        align = "left")) |>
    rowwise() |>
    mutate(
      local_date = with_tz(date, timezone)
    ) |>
    ungroup() |>
    mutate(
      mda8 = ifelse(hour(local_date) %in% 0:6, NA, mda8),
      day = date(local_date)
      ) |>
    group_by(day) |>
    filter(mda8 == max(mda8, na.rm = T)) |>
    filter(row_number() == 1) # Sometimes there are multiple MDA8s that are the same, just take the first one (we only need one per day)


  if(!dbExistsTable(con, "mda8_o3")){
    dbWriteTable(con, "mda8_o3", mda8_data)
  }else{
    dbAppendTable(con, "mda8_o3", mda8_data)
  }

  cli::cli_progress_update()
}

dbDisconnect(con, shutdown = T)
