library(gt)
library(DBI)
library(dplyr)
library(stringr)

source(here::here('functions','utils.R'))

con = connect_to_db()

sites = tbl(con, "piecewise_stats_freeTau_mda8_anom_all") |>
  filter(name == "o3",
         type == "fit",
         stat == "slope") |>
  select(station_id) |>
  left_join(
    combinedMetaRegion(con) |>
      select(station_id, region, station_type, latitude, longitude) |>
      distinct(),
    "station_id"
  ) |>
  collect() |>
  distinct() |>
  mutate(station_type = str_remove(station_type, "unknown."),
         latitude = round(latitude, 3),
         longitude = round(longitude, 3)
         ) |>
  select(Region = region, `Station ID` = station_id, `Station Type` = station_type, Latitude = latitude, Longitude = longitude)


tableLines = sites |>
  gt() |>
  tab_options(latex.use_longtable = TRUE) |>
  tab_caption("List of Sites Used") |>
  as_latex() |>
  as.character() |>
  stringr::str_split("\\n") |>
  unlist()

c(tableLines[1:4],
  "\\label{si_tab:site_list} \\\\",
  tableLines[5:length(tableLines)]
  ) |>
  writeLines("figures/si_figures/tS01_site_list.txt")


removeLines = tbl(con, "remove_sites") |>
  filter(name == "o3") |>
  left_join(
    combinedMetaRegion(con) |>
      select(station_id, region, station_type, latitude, longitude) |>
      distinct(),
    "station_id"
  ) |>
  collect() |>
  mutate(station_type = str_remove(station_type, "unknown."),
         latitude = round(latitude, 3),
         longitude = round(longitude, 3)
  ) |>
  select(Region = region, `Station ID` = station_id, `Station Type` = station_type, Latitude = latitude, Longitude = longitude) |>
  gt() |>
  tab_options(latex.use_longtable = TRUE) |>
  tab_caption("Sites not used based on inspection") |>
  as_latex() |>
  as.character() |>
  stringr::str_split("\\n") |>
  unlist()

c(removeLines[1:4],
  "\\label{si_tab:remove_list} \\\\",
  removeLines[5:length(removeLines)]
) |>
  writeLines("figures/si_figures/tS02_remove_list.txt")
