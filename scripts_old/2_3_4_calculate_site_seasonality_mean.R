library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2021-12-31 00:00"), "month"))

datMonth = tbl(con,"all_data") |>
  mutate(date = floor_date(date,"month")) |>
  group_by(date, name, station_id, station_type) |>
  summarise(value = median(value, na.rm = T)) |>
  ungroup() |>
  mutate(m = month(date)) |>
  ungroup() |>
  left_join(tbl(con, "coverage"), by = c("name", "station_id"))


datClim = tbl(con,"all_data") |>
  mutate(m = month(date)) |>
  group_by(m, name, station_id, station_type) |>
  summarise(season = mean(value, na.rm = T)) |>
  ungroup() |>
  select(-station_type) |>
  select(name, station_id, m, season) |>
  collect()


dbWriteTable(con, "seasonality_mean", datClim, overwrite = T)

dbDisconnect(con, shutdown = T)
