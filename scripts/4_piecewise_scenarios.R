library(DBI)
library(here)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)

create_scenario = function(cp1, dateRange){

  diffs = dateRange-cp1

  dateRange2 = dateRange[abs(diffs) > 365*5]

  tibble(cp1 = cp1, cp2 = dateRange2)

}

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"),
                read_only = FALSE)

name_station = tbl(con, "name_station") |>
  collect()

dbDisconnect(con, shutdown = T)

dateRange = seq.Date(ymd("2002-01-01"), ymd("2020-01-01"), "12 month")

scenarios = map_df(dateRange, ~create_scenario(.x, dateRange)) |>
  bind_rows(tibble(cp1 = dateRange, cp2 = NA)) |>
  mutate(data = list(name_station),
         scenario_idx = row_number()) |>
  unnest(data)

