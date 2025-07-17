library(DBI)
library(dplyr)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

min_aic_mda8 = inner_join(
  tbl(con, "aic_mda8"),
  tbl(con, "valid_scen"),
  by = c("scenario_idx", "reg", "name", "station_id")
  ) |>
  group_by(name, station_id, reg) |>
  filter(aic == min(aic, na.rm = T)) |>
  ungroup() |>
  collect()

dbWriteTable(con, "min_aic_mda8", min_aic_mda8, overwrite = T)

dbDisconnect(con, shutdown = T)
