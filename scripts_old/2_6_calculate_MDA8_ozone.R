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

# Function to account for days where we have no mda8 data
# Avoids the "no non-missing arguments to max" warnings, just incase something else is happening
max_or_empty = function(x){

  y = x[!is.na(x)]

  if(length(y) == 0){
    NA
  }else{
    max(y, na.rm = T)
  }

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
  collect() |>
  distinct()

tsHour = tibble(date = seq(min(ymd_hms("2000-01-01 00:00:00")), max(ymd_hms("2023-12-31 00:00:00")), "hour")) |>
  mutate(name = "o3")

tsDay = tibble(day = seq(min(ymd("2000-01-01")), max(ymd("2023-12-31")), "day")) |>
  mutate(x = row_number())

for(i in 1:length(stations)){

  station_data = tbl(con, "all_data") |>
    filter(name == "o3",
           station_id == local(stations[i])) |>
    select(date, station_id, value, name) |>
    collect() |>
    arrange(date)

  tsHour$station_id = stations[i]

  mda8_data = station_data |>
    full_join(tsHour, c("date","station_id","name")) |>
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
    filter(mda8 == max_or_empty(mda8)) |>
    filter(row_number() == 1) |> # Sometimes there are multiple MDA8s that are the same, just take the first one (we only need one per day)
    full_join(tsDay, by = "day")

  if(!dbExistsTable(con, "mda8_o3")){
    dbWriteTable(con, "mda8_o3", mda8_data)
  }else{
    dbAppendTable(con, "mda8_o3", mda8_data)
  }

  cli::cli_progress_update()
}

dbDisconnect(con, shutdown = T)
