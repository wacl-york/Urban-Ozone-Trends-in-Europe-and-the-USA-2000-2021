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

all_o3_data_collect = tbl(con, "all_data") |>
  filter(name == "o3") |>
  select(date, station_id, value, name) |>
  left_join(
    tbl(con, "combinedMeta") |>
      select(station_id, timezone),
    by = "station_id"
  ) |>
  group_by(station_id) |>
  collect()

MDA8_list = list()

stations = unique(all_o3_data_collect$station_id)

cli::cli_progress_bar(total = length(stations))

for(i in 1:length(stations)){
  MDA8_list[[i]] = all_o3_data_collect |>
    filter(station_id == stations[i]) |>
    mutate(
      mda8 = rollapply(
        data = value,
        width = 8,
        FUN = calc_mda8,
        fill = NA,
        align = "left"),
      local_date = with_tz(date, timezone)
    ) |>
    ungroup() |>
    mutate(mda8 = ifelse(hour(local_date) %in% 0:6, NA, mda8))

  cli::cli_progress_update()
}

MDA8 = bind_rows(MDA8_list)

dbWriteTable(con, "mda8_o3", MDA8, overwrite = T)

dbDisconnect(con, shutdown = T)
