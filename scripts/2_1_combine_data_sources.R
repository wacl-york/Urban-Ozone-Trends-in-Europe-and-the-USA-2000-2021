library(DBI)
library(here)
library(dplyr)
library(toarR)

source(here::here('functions','utils.R'))

con = connect_to_db(FALSE)
# nb:/ this needs ~ 50 Gb of memory
# TOAR --------------------------------------------------------------------

dbWriteTable(con, "toarFlags", list_controlled_vocabulary("Data Flag"), overwrite = TRUE)

toarFlags = tbl(con, "toarFlags") |>
  select(-value) |>
  mutate(flag = as.numeric(flag))

toarMeta = tbl(con, "toarMeta") |>
  select(timeseries_id,
         station_id,
         station_type,
         name = variable_name,
         lat,
         lng)

toarData = tbl(con, "toarData") |>
  left_join(toarFlags, by = c("flags" = "name")) |>
  filter(flag <= 7) |>
  left_join(toarMeta, by = "timeseries_id") |>
  select(date,
         station_id,
         station_type,
         name,
         value,
         lat,
         lng) |>
  mutate(station_id = as.character(station_id)) |>
  collect()

dbWriteTable(con, "all_data", toarData, overwrite = TRUE)

# EEA ---------------------------------------------------------------------

eeaMeta = tbl(con, "eeaMeta") |>
  mutate(station_type = paste(site_area, site_type, sep = ".")) |>
  select(station_id = site,
         lat = latitude,
         lng = longitude,
         station_type)

eeaData = tbl(con, "eeaData") |>
  filter(flag_value == 1) |>
  left_join(eeaMeta, by = "station_id") |>
  select(date,
         station_id,
         station_type,
         name = variable,
         value,
         lat,
         lng) |>
  group_by(date, # ~ 20 of the eeaData sites have greater than 100% data due to overlapping processes.
           station_id, # These data appear to be identical, so we just average here to get a single value per hour
           station_type,
           name,
           lat,
           lng) |>
  summarise_all(mean, na.rm = T) |>
  mutate(value = value / 1.96) |> # ugm-3 -> ppb
  collect()

dbAppendTable(con, "all_data", eeaData)

# Calculate Ox ------------------------------------------------------------

oxStations = tbl(con, "all_data") |>
  select(name, station_id) |>
  distinct() |>
  collect() |>
  nest_by(station_id) |>
  filter(nrow(data) > 1) |>
  pull(station_id)

ox = tbl(con, "all_data") |>
  filter(station_id %in% oxStations) |>
  pivot_wider() |>
  mutate(ox = no2+o3) |>
  pivot_longer(c(no2, o3, ox)) |>
  filter(name == "ox") |>
  collect()

dbAppendTable(con, "all_data", ox)

# -------------------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)
