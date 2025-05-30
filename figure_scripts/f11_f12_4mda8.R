library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggplot2)
library(stringr)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthhires)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

combinedMeta = tbl(con, "combinedMeta") |>
  select(station_id, latitude, longitude, country) |>
  distinct()

mda8_4MDA8_sf = tbl(con, "name_station") |>
  filter(name == "o3") |>
  left_join(tbl(con, "mda8_o3"), by = c("station_id", "name")) |>
  anti_join(tbl(con, "remove_sites") |>
              rename("name" = spc), by = "station_id", "name") |>
  mutate(date = date_trunc("day", date)) |>
  select(-value) |>
  group_by(date, station_id, name) |>
  summarise_all(max, na.rm = T) |>
  left_join(combinedMeta, by = "station_id") |>
  mutate(year = year(date)) |>
  filter(month(date) %in% c(4,5,6,7,8,9)) |>
  group_by(year, station_id, latitude, longitude, country) |>
  arrange(desc(MDA8)) |>
  collect() |>
  slice(4) |>
  mutate(MDA8 = ifelse(country != "United States of America", MDA8/1.96, MDA8)) |>
  st_as_sf(coords = c(lon = "longitude", lat = "latitude"), crs = 4326) |>
  mutate(bin_4mda8 = case_when(MDA8 < 30.5 ~ "0_to_30",
                               MDA8 >= 30.5 & MDA8 < 40.5 ~ "31_to_40",
                               MDA8 >= 40.5 & MDA8 < 50.5 ~ "41_to_50",
                               MDA8 >= 50.5 & MDA8 < 60.5 ~ "51_to_60",
                               MDA8 >= 60.5 & MDA8 < 70.5 ~ "61_to_70",
                               MDA8 >= 70.5 & MDA8 < 75.5 ~ "71_to_75",
                               MDA8 >= 75.5 & MDA8 < 85.5 ~ "76_to_85",
                               MDA8 >= 85.5 & MDA8 < 99.5 ~ "86_to_99",
                               MDA8 >= 99.5 ~ "100+") |>
           factor(levels = c("0_to_30",
                             "31_to_40",
                             "41_to_50",
                             "51_to_60",
                             "61_to_70",
                             "71_to_75",
                             "76_to_85",
                             "86_to_99",
                             "100+"))) |>
  filter(year %in% c(2000, 2007, 2014, 2021)) |>
  arrange(bin_4mda8)

dbDisconnect(con, shutdown = T)

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)

# Define limits for map plotting US
limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

# Define limits for map plotting EU
limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

g1 = ggplot() +
  geom_sf(data = world)+
  geom_sf(data = mda8_4MDA8_sf,
          aes(colour = bin_4mda8))+
  scale_colour_viridis_d(name = "4MDA8")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_wrap(~year)+
  theme_minimal()+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom",
        legend.byrow = T)



pdf("figures/f11_4mda8_us.pdf", width = 11.7, height = 8.3)
print(g1)
dev.off()


g2 = ggplot() +
  geom_sf(data = world)+
  geom_sf(data = mda8_4MDA8_sf,
          aes(colour = bin_4mda8))+
  scale_colour_viridis_d(name = "4MDA8")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_wrap(~year)+
  theme_minimal()+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom",
        legend.byrow = T)

pdf("figures/f12_4mda8_eu.pdf", width = 11.7, height = 8.3)
print(g2)
dev.off()
