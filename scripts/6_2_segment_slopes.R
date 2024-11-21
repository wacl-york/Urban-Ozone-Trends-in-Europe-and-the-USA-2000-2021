library(DBI)
library(dplyr)
library(tidyr)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

slopes = inner_join(
  tbl(con, "qr_regressions"),
  tbl(con, "min_aic"),
  by = c("scenario_idx", "name", "station_id")
) |>
  filter(
    stat == "slope"
  ) |>
  collect()

segments = slopes |>
  select(station_id, name, reg, tau) |>
  distinct() |>
  mutate(segments = tribble(
    ~seg, ~segStart, ~segEnd,
    1,  2000, 2006,
    2,  2007, 2013,
    3,  2014, 2019,
    4,  2020, 2023,
    11, 2000, 2004,
    12, 2005, 2009,
    13, 2010, 2014,
    14, 2015, 2021
  ) |>
    purrr::pmap_df(~tibble(year = ..2:..3, seg = ..1)) |>
    list()) |>
  unnest(segments)


slope_segs = left_join(segments, slopes,
                       by = c("year" = "startYear",
                              "name",
                              "station_id",
                              "reg",
                              "tau",
                              "type"),
                       relationship = "many-to-many") |> # relationship is many-to-many becuase of multiple segment definitions for a single year.
  tidyr::fill(everything()) |>
  select(seg, station_id, name, reg, tau, type, value) |>
  group_by(seg, station_id, name, reg, tau, type) |>
  summarise(value = list(unique(value))) |>
  rowwise() |>
  pivot_wider(names_from = "type") |>
  mutate(data = bind_cols(fit, pv, se) |>
           list()) |>
  select(-fit, -pv, -se) |>
  unnest(data)

dbWriteTable(con, "slope_segs", slope_segs, overwrite = TRUE)

dbDisconnect(con, shutdown = T)


# left_join(segments, slopes,
#           by = c("year" = "startYear",
#                  "name",
#                  "station_id",
#                  "reg",
#                  "tau",
#                  "type"),
#           relationship = "many-to-many") |>
#   filter(station_id == "ch0011a", name == "ox", reg == "pqr_2", tau == 0.05, type == "fit", year == 2000)
