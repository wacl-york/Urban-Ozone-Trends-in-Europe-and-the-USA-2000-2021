library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

con = connect_to_db()

ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2021-12-31 00:00"), "month"))

datMonth = tbl(con,"all_data") |>
  #filter(station_id == "14442") |>
  mutate(date = floor_date(date,"month")) |>
  group_by(date, name, station_id, station_type) |>
  summarise(value = median(value, na.rm = T)) |>
  ungroup() |>
  mutate(m = month(date)) |>
  ungroup() |>
  left_join(tbl(con, "coverage"), by = c("name", "station_id")) |>
  filter(coverage_check)


seasonModel = value~sin(2*pi*m/12)+cos(2*pi*m/12)+ sin(2*pi*m/6)+cos(2*pi*m/6)

datSin = datMonth |>
  arrange(station_id, date) |>
  collect() |>
  nest_by(name, station_id, station_type) |>
  rowwise() |>
  mutate(seasonality = tibble(m = 1:12,
                              season = predict(lm(seasonModel,data = data),
                                               newdata = data.frame(m = 1:12))) |>
           list()) |>
  select(name, station_id, seasonality) |>
  unnest(seasonality)

datClim = tbl(con,"all_data") |>
  mutate(m = month(date)) |>
  group_by(m, name, station_id, station_type) |>
  summarise(clim = median(value, na.rm = T)) |>
  ungroup() |>
  select(-station_type)

datComb = collect(datMonth) |>
  left_join(collect(datSin), by = c("name", "station_id", "m")) |>
  left_join(collect(datClim), by = c("name", "station_id", "m")) |>
  rename(meas = value) |>
  pivot_longer(c(meas, clim, season), names_to = "meth")

library(ggplot2)

datComb |>
  ggplot()+
  geom_line(aes(date, value, colour = meth))+
  trelliscopejs::facet_trelliscope(station_id~name, scales = "free_y", path = "trellis/ts/")+
  theme_minimal()


datComb |>
  mutate(date = month(date)) |>
  select(date, name, station_id, station_type, meth, value) |>
  group_by(date, name, station_id, station_type, meth) |>
  summarise(med = median(value, na.rm = T),
            std = sd(value, na.rm = T)) |>
  ggplot()+
  geom_ribbon(aes(date, ymin = med-std, ymax = med+std, group = meth), alpha = 0.5)+
  geom_line(aes(date, med, colour = meth))+
  trelliscopejs::facet_trelliscope(station_id~name, scales = "free_y", path = "trellis/dirunal/")+
  theme_minimal()

dbDisconnect(con, shutdown = T)
