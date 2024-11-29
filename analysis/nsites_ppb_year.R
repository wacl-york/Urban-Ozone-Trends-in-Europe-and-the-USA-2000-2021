library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(rnaturalearth)
library(rnaturalearthhires)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup()

slopes = inner_join(
  tbl(con, "qr_regressions"),
  min_aic,
  by = c("scenario_idx", "name", "station_id")
) |>
  filter(
    stat == "slope"
  ) |>
  collect() |>
  pivot_wider(names_from = "type") |>
  select(-stat)

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

slopes_year = left_join(segments, slopes,
                        by = c("year" = "startYear",
                               "name",
                               "station_id",
                               "reg",
                               "tau"),
                        relationship = "many-to-many") |>
  fill(everything()) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, country, latitude, longitude) |>
              distinct() |>
              collect(),
            by = "station_id")

slopes_year = slopes_year |>
  mutate(continent = "North America")

slopes_year$continent[slopes_year$country != "United States of America"] = "Europe"

o3_ppb_year = slopes_year |>
  filter(name == "o3") |>
  select(station_id, tau, year, continent, o3_per_year = fit, pv) |>
  mutate(o3_per_year = o3_per_year*365) |>
  distinct()

# test = o3_ppb_year |>
#   filter(station_id %in% duplicate_station_id)

blacklist_sites = tbl(con, "remove_sites") |>
  filter(spc == "o3") |>
  collect()
blacklist_sites = unique(blacklist_sites$station_id)

o3_ppb_year_wider = o3_ppb_year |>
  group_by(station_id, tau, continent) |>
 # filter(pv < 0.05) |>
  select(-pv) |>
  filter(!(station_id %in% duplicate_station_id)) |>
  filter(!(station_id %in% blacklist_sites)) |>
  pivot_wider(names_from = year, values_from = o3_per_year)

o3_count = o3_ppb_year_wider |>
  ungroup() |>
  select(-station_id) |>
  mutate(tau = as.character(tau)) |>
  group_by(tau, continent) |>
  summarise(
    across(where(is.numeric), ~ sum(. > 3, na.rm = T))
  )

