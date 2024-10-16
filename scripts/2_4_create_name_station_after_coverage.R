library(DBI)
library(here)
library(dplyr)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

name_station = tbl(con,"all_data") |>
  select(name, station_id) |>
  distinct() |>
  left_join(tbl(con,"coverage"), by = c("name","station_id")) |>
  filter(coverage_check) |>
  select(name, station_id) |>
  collect()

dbWriteTable(con, "name_station", name_station, overwrite = T)

dbDisconnect(con, shutdown = T)
