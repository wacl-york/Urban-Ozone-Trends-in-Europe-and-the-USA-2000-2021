library(DBI)
library(dplyr)
library(tidyr)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

min_aic = tbl(con, "min_aic_metric") |>
  group_by(metric, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup()                                               # the differences between the locations of the change points are not substantial
# so just take the lower of the two sceanrio_idx arbitrarily

slopes = inner_join(
  tbl(con, "qr_regressions_metrics"),
  min_aic,
  by = c("scenario_idx", "metric", "station_id")
) |>
  filter(
    stat == "slope"
  ) |>
  collect() |>
  pivot_wider(names_from = "type") |>
  select(-stat)

segments = slopes |>
  select(station_id, metric, reg, tau) |>
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
                              "metric",
                              "station_id",
                              "reg",
                              "tau"),
                       relationship = "many-to-many") |>
  fill(everything()) |>
  select(seg, station_id, metric, reg, tau, fit, se, pv) |>
  distinct() |>
  group_by(seg, station_id, metric, reg, tau) |>
  summarise(data = tibble(fit = unique(fit),
                          se = unique(se),
                          pv = unique(pv)) |>
              list()) |>
  unnest(data)

dbWriteTable(con, "slope_segs_metrics", slope_segs, overwrite = TRUE)

dbDisconnect(con, shutdown = T)

