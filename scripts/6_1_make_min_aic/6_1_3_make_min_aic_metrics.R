library(DBI)
library(dplyr)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

min_aic_metric = inner_join(
  tbl(con, "aic_metrics"),
  tbl(con, "reg_r2_metrics"),
  by = c("scenario_idx", "reg", "metric", "station_id")
  ) |>
  group_by(metric, station_id, reg) |>
  filter(aic == min(aic, na.rm = T)) |>
  ungroup() |>
  collect()

dbWriteTable(con, "min_aic_metric", min_aic_metric, overwrite = T)

dbDisconnect(con, shutdown = T)
