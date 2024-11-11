library(DBI)
library(here)
library(dplyr)

con = connect_to_db(FALSE)

name_station = tbl(con,"anom") |>
  select(name, station_id) |>
  distinct() |>
  # left_join(tbl(con,"coverage"), by = c("name","station_id")) |>
  # filter(coverage_check) |>
  select(name, station_id) |>
  collect()

dbWriteTable(con, "name_station", name_station, overwrite = T)

dbDisconnect(con, shutdown = T)
