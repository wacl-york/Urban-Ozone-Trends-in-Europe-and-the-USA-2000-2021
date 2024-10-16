library(DBI)
library(here)
library(dplyr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# still keep the coverage check over 2000-2022 as the toar database doesn't have the extra data,
# then any more data we get from the eea is a bonus
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

