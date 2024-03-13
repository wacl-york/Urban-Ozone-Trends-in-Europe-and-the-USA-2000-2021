library(DBI)
library(here)
library(dplyr)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

dat = tbl(con,"all_data") |>
  mutate(m = month(date)) |>
  left_join(tbl(con, "seasonality"), by = c("name", "station_id","m")) |>
  left_join(tbl(con, "coverage"), by = c("name", "station_id")) |>
  filter(coverage_check) |>
  select(-m, -coverage_check) |>
  mutate(date = floor_date(date, "month")) |>
  group_by(date, station_id, station_type, name) |>
  summarise_all(median, na.rm = T) |>
  mutate(anom = value-season) |>
  collect()

dbWriteTable(con, "monthly_anom", dat)

dbDisconnect(con, shutdown = T)
