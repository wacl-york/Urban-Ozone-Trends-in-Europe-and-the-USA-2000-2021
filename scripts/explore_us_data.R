library(DBI)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"))


ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2022-01-01 00:00"), "month"))

datMonth = tbl(con,"data") |>
  filter(flag_value <= 7) |>
  select(-flag_value) |>
  mutate(date = floor_date(date,"month")) |>
  group_by(date, name, timeseries_id) |>
  summarise(value = median(value, na.rm = T)) |>
  ungroup() |>
  mutate(m = month(date))


climateUS = tbl(con,"data") |>
  filter(flag_value <= 7) |>
  mutate(
    m = month(date)) |>
  select(-date,-flag_value) |>
  group_by(m, name, timeseries_id) |>
  summarise(clim = median(value, na.rm = T))

stations = tbl(con, "stations") |>
  filter(!is.na(timeseries_id_o3),!is.na(timeseries_id_no2)) |>
  pivot_longer(-station_id,values_to = "timeseries_id") |>
  select(-name)

dat = left_join(
  datMonth,
  climateUS,
  by = c("m","timeseries_id","name")
  ) |>
  select(-m) |>
  left_join(stations, by = "timeseries_id") |>
  collect() |>
  ungroup() |>
  nest_by(name, timeseries_id,station_id) |>
  mutate(data = left_join(ts, data, by = "date") |>
           list()) |>
  unnest(data) |>
  arrange(date) |>
  ungroup()

dat |>
  filter(!is.na(station_id)) |>
  ggplot()+
  geom_line(aes(date,
                value,
                group = timeseries_id,
                colour = as.factor(station_id)),
            alpha = 0.5)+
  guides(colour = "none")+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  AQVisR::AQvis_plotTheme()


dat |>
  filter(!is.na(station_id)) |>
  ggplot()+
  geom_line(aes(date,
                value,
                group = timeseries_id,
                colour = name),
            alpha = 0.5)+
  guides(colour = "none")+
  trelliscopejs::facet_trelliscope(~station_id, scales = "free_y")+
  AQVisR::AQvis_plotTheme()


dat |>
  filter(!is.na(station_id)) |>
  select(station_id) |>
  distinct() |> nrow()
