library(DBI)
library(here)
library(dplyr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines("data_config.txt",n = 1),"data","db.duckdb"), read_only = FALSE)


ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2022-01-01 00:00"), "hour"))
x = nrow(ts)

coverage = tbl(con, "all_data") |>
  group_by(station_id, name) |>
  filter(!is.na(value)) |>
  count() |>
  mutate(perc = (n/x)*100) |>
  collect() |>
  mutate(coverage_check = perc >= 90)

dbWriteTable(con, "coverage", coverage, overwrite = T)

dbDisconnect(con, shutdown = T)

