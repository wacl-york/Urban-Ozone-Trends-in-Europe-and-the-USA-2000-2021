library(DBI)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"))


ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2022-01-01 00:00"), "month"))

datMonth = tbl(con,"all_data") |>
  mutate(date = floor_date(date,"month")) |>
  group_by(date, name, station_id, region) |>
  summarise(value = median(value, na.rm = T)) |>
  ungroup() |>
  mutate(m = month(date)) |>
  ungroup()

climate = tbl(con,"all_data") |>
  mutate(m = month(date)) |>
  select(-date) |>
  group_by(m, name, station_id, region) |>
  summarise(clim = median(value, na.rm = T)) |>
  ungroup()

dat = left_join(datMonth, climate, by = c("m", "name", "station_id", "region")) |>
  arrange(station_id, date) |>
  collect() |>
  nest_by(name, station_id, region) |>
  mutate(data = ts |>
           left_join(data, by = "date") |>
           list()) |>
  unnest(data)

dat |>
  ggplot()+
  geom_line(aes(date, value-clim, colour = station_id))+
  geom_smooth(aes(date, value-clim))+
  guides(colour = "none")+
  facet_grid(name~region, scales = "free_y")+
  AQVisR::AQvis_plotTheme()


plotly::ggplotly()


