library(DBI)
library(here)
library(dplyr)
library(lubridate)

source(here::here('functions','utils.R'))

con = connect_to_db(FALSE)

# still keep the coverage check over 2000-2022 as the toar database doesn't have the extra data,
# then any more data we get from the eea is a bonus
startDate = ymd_hm("2000-01-01 00:00")
endDate = ymd_hm("2022-01-01 00:00")

ts = tibble(date = seq(startDate, endDate, "hour"))
x = nrow(ts)

coverage_total = tbl(con, "all_data") |>
  group_by(station_id, name) |>
  filter(!is.na(value),
         between(date, startDate, endDate)) |>
  count() |>
  mutate(perc = (n/x)*100) |>
  collect() |>
  mutate(total_coverage_check = perc >= 80)

dbWriteTable(con, "coverage_total", coverage_total, overwrite = T)

dbDisconnect(con, shutdown = T)
