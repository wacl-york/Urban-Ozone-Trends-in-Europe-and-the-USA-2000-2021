library(DBI)
library(dplyr)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

min_aic = inner_join(
  tbl(con, "aic"),
  tbl(con, "valid_scen"),
  by = c("scenario_idx", "reg", "name", "station_id")
  ) |>
  group_by(name, station_id, reg) |>
  filter(aic == min(aic, na.rm = T)) |>
  ungroup() |>
  collect()

dbWriteTable(con, "min_aic", min_aic, overwrite = T)

dbDisconnect(con, shutdown = T)
