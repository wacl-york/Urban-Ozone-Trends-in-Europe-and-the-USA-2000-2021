library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

ts = tibble(date = seq(min(ymd_hms("2000-01-01 00:00:00")), max(ymd_hms("2023-12-31 00:00:00")), "day")) %>%
  mutate(x = row_number())

dat = tbl(con,"all_data") |>
  mutate(m = month(date)) |>
  left_join(tbl(con, "seasonality"), by = c("name", "station_id","m")) |>
  left_join(tbl(con, "coverage"), by = c("name", "station_id")) |>
  filter(coverage_check) |>
  select(-m, -coverage_check, -n, -perc) |>
  mutate(date = floor_date(date, "day")) |>
  group_by(date, station_id, station_type, name) |>
  summarise_all(median, na.rm = T) |>
  mutate(anom = value-season) |>

  pivot_wider(values_from = c("anom","value", "season"), names_sep = "_") |>
  mutate(anom_ox = anom_o3+anom_no2) |>
  pivot_longer(-c(date, station_id, station_type, lat, lng), names_to = c("type", "name"), names_sep = "_") |>
  pivot_wider(names_from = "type") |>

  filter(!is.na(anom)) |>
  collect() |>
  left_join(ts, by = "date")

dbWriteTable(con, "anom", dat, overwrite = T)

dbDisconnect(con, shutdown = T)
