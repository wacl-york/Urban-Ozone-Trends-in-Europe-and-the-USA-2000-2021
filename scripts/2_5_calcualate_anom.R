library(DBI)
library(here)
library(dplyr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

ts = tibble(date = seq(min(ymd_hms("2000-01-01 00:00:00")), max(ymd_hms("2021-12-31 00:00:00")), "month")) %>%
  mutate(x = row_number())

dat = tbl(con,"all_data") |>
  mutate(m = month(date)) |>
  left_join(tbl(con, "seasonality"), by = c("name", "station_id","m")) |>
  left_join(tbl(con, "coverage"), by = c("name", "station_id")) |>
  filter(coverage_check) |>
  select(-m, -coverage_check) |>
  mutate(date = floor_date(date, "day")) |>
  group_by(date, station_id, station_type, name) |>
  summarise_all(median, na.rm = T) |>
  mutate(anom = value-season) |>
  collect() |>
  left_join(ts, by = "date")

dbWriteTable(con, "monthly_anom", dat, overwrite = T)

dbDisconnect(con, shutdown = T)
