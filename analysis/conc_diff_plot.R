library(sf)
library(DBI)
library(dplyr)
library(scico)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthhires)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

mda8 = tbl(con, "mda8_o3") |>
  mutate(day = date_trunc("day" ,date)) |>
  group_by(day) |>
  filter(MDA8 == max(MDA8, na.rm = T)) |>
  select(-name, -date, -value) |>
  ungroup()

dat = tbl(con, "slope_segs") |>
  select(station_id, spc = name) |>
  distinct() |>
  anti_join(
    tbl(con, "remove_sites"),
    by = c("station_id", "spc")) |>
  left_join(
    tbl(con, "all_data") |>
      rename(spc = name),
    by = c("station_id", "spc")) |>
  pivot_wider(names_from = "spc") |>
  mutate(day = date_trunc("day" ,date)) |>
  left_join(mda8,
            by = c("day", "station_id")) |>
  pivot_longer(c(no2, o3, ox, MDA8), names_to = "spc") |>
  left_join(
    tbl(con, "combinedMeta") |>
      select(station_id, country, latitude, longitude),
    by = "station_id") |>
  mutate(y = year(date)) |>
  filter(y %in% c(2000, 2019)) |>
  mutate(value = ifelse(country == "United States of America",value, value/1.96)) |>
  group_by(
    y,
    station_id,
    spc,
    station_type,
    lat,
    lng,
    country) |>
  summarise(q50 = quantile(value, 0.5, na.rm = T),
            q90 = quantile(value, 0.90, na.rm = T)) |>
  ungroup() |>
  pivot_longer(c(q50, q90), names_to = "quantile") |>
  pivot_wider(names_from = y) |>
  mutate(country = ifelse(country == "United States of America", country, "Europe"),
         diff = `2019`-`2000`) |>
  collect()

mycrs = 4087 #8857

world = rnaturalearth::ne_countries(scale = "large", returnclass = "sf") |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)


datSf = dat |>
  filter(!is.na(diff)) |>
  st_as_sf(coords = c("lng", "lat"),
           crs = "WGS84") |>
  st_transform(mycrs)

gus = datSf |>
  filter(spc != "MDA8") |>
  ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(aes(colour = diff), size = 2)+
  scale_colour_scico(palette = "vik")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_grid(spc~quantile)+
  theme(panel.background = element_rect(fill = "white"))


geu = datSf |>
  filter(spc != "MDA8") |>
  ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(aes(colour = diff), size = 2)+
  scale_colour_scico(palette = "vik")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_grid(spc~quantile)+
  theme(panel.background = element_rect(fill = "white"))

scale = 3000

png("plots/conc_diff_us.png",  res = 300, width = (scale*1.7), height = scale)
print(gus)
dev.off()

png("plots/conc_diff_eu.png",  res = 300, width = (scale*1.7), height = scale)
print(geu)
dev.off()




# absolute concs ----------------------------------------------------------

geu_abs = datSf |>
  select(-diff) |>
  pivot_longer(c(`2000`, `2019`), names_to = "y") |>
  filter(quantile == "q50",
         spc != "o3") |>
  ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(aes(colour = value), size = 2)+
  scale_colour_scico(palette = "batlow")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_grid(spc~y)+
  theme(panel.background = element_rect(fill = "white"))


gus_abs = datSf |>
  select(-diff) |>
  pivot_longer(c(`2000`, `2019`), names_to = "y") |>
  filter(quantile == "q50",
         spc != "o3") |>
  ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(aes(colour = value), size = 2)+
  scale_colour_scico(palette = "batlow")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_grid(spc~y)+
  theme(panel.background = element_rect(fill = "white"))

png("plots/conc_abs_us.png",  res = 300, width = (scale*1.7), height = scale)
print(gus_abs)
dev.off()

png("plots/conc_abs_eu.png",  res = 300, width = (scale*1.7), height = scale)
print(geu_abs)
dev.off()

