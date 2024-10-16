library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2021-12-31 00:00"), "month"))

datMonth = tbl(con,"all_data") |>
  mutate(date = floor_date(date,"month")) |>
  group_by(date, name, station_id, station_type) |>
  summarise(value = median(value, na.rm = T)) |>
  ungroup() |>
  mutate(m = month(date)) |>
  ungroup() |>
  left_join(tbl(con, "coverage"), by = c("name", "station_id"))


# seasonModel = value~sin(2*pi*m/12)+cos(2*pi*m/12)+ sin(2*pi*m/6)+cos(2*pi*m/6)
#
# dat = datMonth |>
#   filter(coverage_check) |>
#   arrange(station_id, date) |>
#   collect() |>
#   nest_by(name, station_id, station_type) |>
#   rowwise() |>
#   mutate(seasonality = tibble(m = 1:12,
#                               season = predict(lm(seasonModel,data = data),
#                                                newdata = data.frame(m = 1:12))) |>
#            list()) |>
#   select(name, station_id, seasonality) |>
#   unnest(seasonality)


datClim = tbl(con,"all_data") |>
  mutate(m = month(date)) |>
  group_by(m, name, station_id, station_type) |>
  summarise(season = median(value, na.rm = T)) |>
  ungroup() |>
  select(-station_type) |>
  select(name, station_id, m, season) |>
  collect()


dbWriteTable(con, "seasonality", datClim, overwrite = T)

dbDisconnect(con, shutdown = T)
