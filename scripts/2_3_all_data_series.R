library(DBI)
library(dplyr)

source(here::here('functions','utils.R'))

con = connect_to_db()

all_data_series = tbl(con, "all_data") |>
  select(name, station_id) |>
  distinct()
  collect()

dbWriteTable(con, "all_data_series", all_data_series, overwrite = T)

dbDisconnect(con, shutdown = T)
