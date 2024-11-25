library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(scico)
library(ggplot2)
library(here)
library(lubridate)
library(zoo)
library(plotly)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

all_o3_data_collect = tbl(con, "all_data") |>
  filter(name == "o3") |>
  select(date, station_id, value) |>
  group_by(station_id) |>
  collect()

MDA8 = all_o3_data_collect |>
  group_by(station_id) |>
  mutate(rolling_mean = rollmean(value,
                                 k = 8,
                                 fill = NA,
                                 align = "center"))
MDA8 = MDA8 |>
  mutate(name = "o3") |>
  select(date, station_id, name, value, MDA8 = rolling_mean)

#saveRDS(MDA8, "Z:/TOAR_II_BSN/TOAR_paper/scripts/MDA8_ozone.RDS")

dbDisconnect(con, shutdown = T)

