library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(rnaturalearth)
library(rnaturalearthhires)
library(patchwork)
library(ggpubr)
library(duckdb)
library(DBI)

source(here::here('functions','connect_to_db.R'))

qr_piece = readRDS("Z:/TOAR_II_BSN/TOAR_paper/data/qr_piece_dataframe.RDS")

qr_piece$country[qr_piece$country != "United States of America"] = "Europe"
qr_piece$A_to_B = abs(qr_piece$A_to_B)
qr_piece$B_to_C = abs(qr_piece$B_to_C)

con = connect_to_db()

blacklist = tbl(con, "remove_sites") |>
  collect() |>
  rename(name = "spc")

qr_piece_long <- qr_piece |>
  pivot_longer(cols = c("A_to_B", "B_to_C"), names_to = "transformation", values_to = "slope_mag") |>
  unnest_wider(startYears, names_sep = "_") |>
  mutate(flip_dir = ifelse(transformation == "A_to_B", A_to_B_dir_flip, B_to_C_dir_flip),
         year = ifelse(transformation == "A_to_B", startYears_2, startYears_3)) |>
  select(-c(startYears_1, startYears_2, startYears_3, A_to_B_year, B_to_C_year, A_to_B_dir_flip, B_to_C_dir_flip)) |>
  anti_join(blacklist, by = c("station_id", "name"))


col = c(
  "#E4ADD6",
  "#B5549C",
  "#65014B",
  "black",
  "#0C4C00",
  "#5F903D",
  "#C0D9A1"
)

mycrs = 4087 #8857

world = rnaturalearth::ne_countries(scale = "small", returnclass = "sf") |>
  st_transform(mycrs)

# Create sf object
qr_piece_sf = qr_piece |>
  st_as_sf(coords = c(lon = "longitude", lat = "latitude"), crs = 4326)

# Define limits for map plotting US
limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

# Define limits for map plotting EU
limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

qr_piece_sf$startYear_A_to_B <- sapply(qr_piece_sf$startYears, function(x) x[[2]])
qr_piece_sf$startYear_B_to_C <- sapply(qr_piece_sf$startYears, function(x) x[[3]])

US_O3_CP1 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            arrange(desc(A_to_B)) |>
            filter(country == "United States of America",
                   name == "o3",
                   tau == 0.5) |>
            filter(!(is.na(A_to_B_dir_flip))) |>
            group_by(A_to_B_dir_flip)
          # |>
          #   top_frac(0.75, A_to_B)
          ,
          aes(colour = startYear_A_to_B, size = A_to_B), alpha = 0.5) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~A_to_B_dir_flip) +
  scale_colour_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("First O"[3]*" changepoint"))


EU_O3_CP1 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            arrange(desc(A_to_B)) |>
            filter(country == "Europe",
                   name == "o3",
                   tau == 0.5) |>
            filter(!(is.na(A_to_B_dir_flip))) |>
            group_by(A_to_B_dir_flip)
          # |>
          #   top_frac(0.75, A_to_B)
          ,
          aes(colour = startYear_A_to_B, size = A_to_B), alpha = 0.5) +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~A_to_B_dir_flip) +
  scale_colour_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("First O"[3]*" changepoint"))


US_O3_CP2 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            arrange(desc(B_to_C)) |>
            filter(country == "United States of America",
                   name == "o3",
                   tau == 0.5) |>
            filter(!(is.na(B_to_C_dir_flip))) |>
            group_by(B_to_C_dir_flip)
          # |>
          #   top_frac(0.75, B_to_C)
          ,
          aes(colour = startYear_B_to_C, size = B_to_C), alpha = 0.5) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~B_to_C_dir_flip) +
  scale_colour_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("Second O"[3]*" changepoint"))

EU_O3_CP2 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            arrange(desc(B_to_C)) |>
            filter(country == "Europe",
                   name == "o3",
                   tau == 0.5) |>
            filter(!(is.na(B_to_C_dir_flip))) |>
            group_by(B_to_C_dir_flip)
          # |>
          #   top_frac(0.75, B_to_C)
          ,
          aes(colour = startYear_B_to_C, size = B_to_C), alpha = 0.5) +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~B_to_C_dir_flip) +
  scale_colour_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("Second O"[3]*" changepoint"))

US_NO2_CP1 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            arrange(desc(A_to_B)) |>
            filter(country == "United States of America",
                   name == "no2",
                   tau == 0.5) |>
            filter(!(is.na(A_to_B_dir_flip))) |>
            group_by(A_to_B_dir_flip)
          # |>
          #   top_frac(0.75, A_to_B)
          ,
          aes(colour = startYear_A_to_B, size = A_to_B), alpha = 0.5) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~A_to_B_dir_flip) +
  scale_colour_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("First NO"[2]*" changepoint"))


EU_NO2_CP1 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            arrange(desc(A_to_B)) |>
            filter(country == "Europe",
                   name == "no2",
                   tau == 0.5) |>
            filter(!(is.na(A_to_B_dir_flip))) |>
            group_by(A_to_B_dir_flip)
          # |>
          #   top_frac(0.75, A_to_B)
          ,
          aes(colour = startYear_A_to_B, size = A_to_B), alpha = 0.5) +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~A_to_B_dir_flip) +
  scale_colour_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("First NO"[2]*" changepoint"))


US_NO2_CP2 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            arrange(desc(B_to_C)) |>
            filter(country == "United States of America",
                   name == "no2",
                   tau == 0.5) |>
            filter(!(is.na(B_to_C_dir_flip))) |>
            group_by(B_to_C_dir_flip)
          # |>
          #   top_frac(0.75, B_to_C)
          ,
          aes(colour = startYear_B_to_C, size = B_to_C), alpha = 0.5) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~B_to_C_dir_flip) +
  scale_colour_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("Second NO"[2]*" changepoint"))

EU_NO2_CP2 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            arrange(desc(B_to_C)) |>
            filter(country == "Europe",
                   name == "no2",
                   tau == 0.5) |>
            filter(!(is.na(B_to_C_dir_flip))) |>
            group_by(B_to_C_dir_flip)
          # |>
          #   top_frac(0.75, B_to_C)
          ,
          aes(colour = startYear_B_to_C, size = B_to_C), alpha = 0.5) +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~B_to_C_dir_flip) +
  scale_colour_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("Second NO"[2]*" changepoint"))

ggarrange(EU_O3_CP1, EU_O3_CP2, nrow = 2)
ggarrange(US_O3_CP1, US_O3_CP2, nrow = 2)

ggarrange(EU_NO2_CP1, EU_NO2_CP2, nrow = 2)
ggarrange(US_NO2_CP1, US_NO2_CP2, nrow = 2)

# Create sf object
qr_piece_long_sf = qr_piece_long |>
  st_as_sf(coords = c(lon = "longitude", lat = "latitude"), crs = 4326)

qr_piece_rm_nas = qr_piece_long |>
  na.omit()

o3_slope_min = min(qr_piece_rm_nas$slope_mag[qr_piece_rm_nas$name == "o3"])
o3_slope_max = max(qr_piece_rm_nas$slope_mag[qr_piece_rm_nas$name == "o3"])

no2_slope_min = min(qr_piece_rm_nas$slope_mag[qr_piece_rm_nas$name == "no2"])
no2_slope_max = max(qr_piece_rm_nas$slope_mag[qr_piece_rm_nas$name == "no2"])

limit_year_min = 2000
limit_year_max = 2023

states = rnaturalearth::ne_states(returnclass = "sf") |>
  filter(admin %in% c("United States of America","Puerto Rico")) |>
  select(name)

usMeta = tbl(con, "combinedMeta") |>
  filter(country == "United States of America") |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_join(states, join = st_nearest_feature) |>
  select(station_id, state = name) |>
  st_drop_geometry()

dbDisconnect(con)

png("~/TOAR/TOAR_paper/plots/US_o3_tau_0.5_changepoint_year_mag_map.png",width = 1080*3.5, height = 1080*2.5, res = 300)
ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_long_sf |>
            arrange(desc(slope_mag)) |>
            filter(country == "United States of America",
                   name == "o3",
                   tau == 0.5) |>
            filter(!(is.na(flip_dir))),
          aes(colour = year, size = slope_mag), alpha = 0.5) +
  scale_size("slope_mag", limits = c(o3_slope_min, o3_slope_max)) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~transformation~flip_dir) +
  scale_colour_viridis_c(limits = c(2000, 2023)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("O"[3]))
dev.off()

png("~/TOAR/TOAR_paper/plots/US_no2_tau_0.5_changepoint_year_mag_map.png",width = 1080*3.5, height = 1080*2.5, res = 300)
ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_long_sf |>
            arrange(desc(slope_mag)) |>
            filter(country == "United States of America",
                   name == "no2",
                   tau == 0.5) |>
            filter(!(is.na(flip_dir))),
          aes(colour = year, size = slope_mag), alpha = 0.5) +
  scale_size("slope_mag", limits = c(no2_slope_min, no2_slope_max)) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~transformation~flip_dir) +
  scale_colour_viridis_c(limits = c(2000, 2023)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("NO"[2]))
dev.off()

png("~/TOAR/TOAR_paper/plots/EU_o3_tau_0.5_changepoint_year_mag_map.png",width = 1080*3.5, height = 1080*2.5, res = 300)
ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_long_sf |>
            arrange(desc(slope_mag)) |>
            filter(country == "Europe",
                   name == "o3",
                   tau == 0.5) |>
            filter(!(is.na(flip_dir))),
          aes(colour = year, size = slope_mag), alpha = 0.5) +
  scale_size("slope_mag", limits = c(o3_slope_min, o3_slope_max)) +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
 # scale_size("slope_mag", range = c(o3_slope_min, o3_slope_max))+
  facet_wrap(~transformation~flip_dir) +
  scale_colour_viridis_c(limits = c(2000, 2023)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("O"[3]))
dev.off()

png("~/TOAR/TOAR_paper/plots/EU_no2_tau_0.5_changepoint_year_mag_map.png",width = 1080*3.5, height = 1080*2.5, res = 300)
ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_long_sf |>
            arrange(desc(slope_mag)) |>
            filter(country == "Europe",
                   name == "no2",
                   tau == 0.5) |>
            filter(!(is.na(flip_dir))),
          aes(colour = year, size = slope_mag), alpha = 0.5) +
  scale_size("slope_mag", limits = c(no2_slope_min, no2_slope_max)) +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~transformation~flip_dir) +
  scale_colour_viridis_c(limits = c(2000, 2023)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(expression("NO"[2]))
dev.off()

datScrape = qr_piece_long_sf |>
  st_drop_geometry() |>
  filter(transformation == "B_to_C",
         country != "United States of America",
         tau == 0.5,
         name == "no2") |>
  left_join(usMeta, by = "station_id") |>
filter(
  flip_dir == "positive_to_negative") |>
  #flip_dir == "negative_to_positive") |>
  #flip_dir %in% c("positive_to_negative", "negative_to_positive")) |>
  distinct()


ggplot(data = datScrape, aes(x = year))+
  geom_histogram(stat = "count", binwidth = 1)
