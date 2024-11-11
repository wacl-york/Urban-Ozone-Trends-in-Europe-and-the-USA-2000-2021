library(DBI)
library(here)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

create_scenario = function(cp1, dateRange){

  diffs = dateRange-cp1

  dateRange2 = dateRange[abs(diffs) > 365*5]

  tibble(cp1 = cp1, cp2 = dateRange2)

}

con = connect_to_db(FALSE)

name_station = tbl(con, "name_station") |>
  collect()


dateRange = seq.Date(ymd("2002-01-01"), ymd("2022-01-01"), "12 month")

scenarios = map_df(dateRange, ~create_scenario(.x, dateRange)) |> # 2 change points
  bind_rows(tibble(cp1 = dateRange, cp2 = NA)) |> # 1 change point
  bind_rows(tibble(cp1 = NA, cp2 = NA)) |> # 0 change points
  mutate(data = list(name_station),
         scenario_idx = row_number()) |>
  unnest(data)

dbWriteTable(con, "regression_scenarios", scenarios, overwrite = T)

dbDisconnect(con, shutdown = T)
