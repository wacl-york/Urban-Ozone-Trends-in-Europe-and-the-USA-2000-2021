library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggplot2)
library(stringr)
library(rnaturalearth)
library(rnaturalearthhires)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

mda8_o3 = tbl(con, "mda8_o3") |>
  collect()

mda8_o3_testing = mda8_o3 |>
  mutate(date = date(date)) |>
  select(-value) |>
  group_by(date, station_id, name) |>
  summarise_all(max, na.rm = T)

mycrs = 4087 #8857

world = rnaturalearth::ne_countries(scale = "small", returnclass = "sf") |>
  st_transform(mycrs)

combinedMeta = tbl(con, "combinedMeta") |>
  select(station_id, latitude, longitude, country) |>
  collect() |>
  distinct()

mda8_4MDA8 = mda8_o3_testing |>
  left_join(combinedMeta, by = "station_id") |>
  mutate(year = year(date)) |>
  filter(month(date) %in% c(4,5,6,7,8,9)) |>
  group_by(year, station_id, latitude, longitude, country) |>
  arrange(desc(MDA8)) |>
  slice(4)

mda8_4MDA8 = mda8_4MDA8 |>
  mutate(MDA8 = ifelse(country != "United States of America", MDA8/1.96, MDA8))

states = rnaturalearth::ne_states(returnclass = "sf") |>
  filter(admin %in% c("United States of America","Puerto Rico")) |>
  select(name)

usMeta = tbl(con, "combinedMeta") |>
  filter(country == "United States of America") |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_join(states, join = st_nearest_feature) |>
  select(station_id, state = name) |>
  st_drop_geometry() |>
  distinct()

mda8_4MDA8_original = mda8_4MDA8

mda8_4MDA8 = mda8_4MDA8 |>
  left_join(usMeta, by = "station_id")

mda8_4MDA8_sf = mda8_4MDA8 |>
  st_as_sf(coords = c(lon = "longitude", lat = "latitude"), crs = 4326)

# Define limits for map plotting US
limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

# Define limits for map plotting EU
limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

mda8_4MDA8_sf = mda8_4MDA8_sf |>
  mutate(bin_4mda8 = case_when(MDA8 < 30.5 ~ "0_to_30",
                               MDA8 >= 30.5 & MDA8 < 40.5 ~ "31_to_40",
                               MDA8 >= 40.5 & MDA8 < 50.5 ~ "41_to_50",
                               MDA8 >= 50.5 & MDA8 < 60.5 ~ "51_to_60",
                               MDA8 >= 60.5 & MDA8 < 70.5 ~ "61_to_70",
                               MDA8 >= 70.5 & MDA8 < 75.5 ~ "71_to_75",
                               MDA8 >= 75.5 & MDA8 < 85.5 ~ "76_to_85",
                               MDA8 >= 85.5 & MDA8 < 99.5 ~ "86_to_99",
                               MDA8 >= 99.5 ~ "100+"))

png("~/TOAR/TOAR_paper/plots/mda8_4MDA8_USA_2000_07_14_21_map.png",width = 1080*3.5, height = 1080*2.5, res = 300)
ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = mda8_4MDA8_sf |>
            arrange(MDA8) |>
            filter(year %in% c(2000, 2007, 2014, 2021)),
          aes(colour = factor(bin_4mda8, levels = c("0_to_30",
                                                    "31_to_40",
                                                    "41_to_50",
                                                    "51_to_60",
                                                    "61_to_70",
                                                    "71_to_75",
                                                    "76_to_85",
                                                    "86_to_99",
                                                    "100+"))))+
  scale_colour_viridis_d(name = "") +
  facet_wrap(~year) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])
 # labs(colour = "")
  # scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  # scale_x_continuous(limits = st_coordinates(limEU)[,1])
dev.off()


png("~/TOAR/TOAR_paper/plots/mda8_4MDA8_EU_2000_07_14_21_map.png",width = 1080*3.5, height = 1080*2.5, res = 300)
ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = mda8_4MDA8_sf |>
            arrange(MDA8) |>
            filter(year %in% c(2000, 2007, 2014, 2021)),
          aes(colour = factor(bin_4mda8, levels = c("0_to_30",
                                                    "31_to_40",
                                                    "41_to_50",
                                                    "51_to_60",
                                                    "61_to_70",
                                                    "71_to_75",
                                                    "76_to_85",
                                                    "86_to_99",
                                                    "100+"))))+
  scale_colour_viridis_d(name = "") +
  facet_wrap(~year) +
  # scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  # scale_x_continuous(limits = st_coordinates(limUS)[,1])
  # labs(colour = "")
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])
dev.off()

datScrape = mda8_4MDA8_sf |>
  filter(MDA8 >= 85.5,
         country != "United States of America",
        # state == "California",
         year == 2000)
