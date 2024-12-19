library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggh4x)
library(ggtext)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

qr_piece = tbl(con, "trend_flip") |>
  mutate(A_to_B = abs(A_to_B),
         B_to_C = abs(B_to_C)) |>
  collect()

qr_piece_long <- qr_piece |>
  pivot_longer(cols = c("A_to_B", "B_to_C"), names_to = "transformation", values_to = "slope_mag") |>
  unnest_wider(startYears, names_sep = "_") |>
  mutate(flip_dir = ifelse(transformation == "A_to_B", A_to_B_dir_flip, B_to_C_dir_flip),
         year = ifelse(transformation == "A_to_B", startYears_2, startYears_3)) |>
  select(-c(startYears_1, startYears_2, startYears_3, A_to_B_dir_flip, B_to_C_dir_flip))

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

world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
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

# Create sf object
qr_piece_long_sf = qr_piece_long |>
  st_as_sf(coords = c(lon = "longitude", lat = "latitude"), crs = 4326)


dbDisconnect(con)

plotDat = qr_piece_long_sf |>
  mutate(country = ifelse(country == "United States of America", country , "Europe")) |>
  rename(spc = name) |>
  mutate(spc = case_when(spc == "o3" ~ "O<sub>3</sub>",
                         spc == "no2" ~ "NO<sub>2</sub>",
                         TRUE ~ NA) |>
           factor(levels = c("O<sub>3</sub>","NO<sub>2</sub>")),
         transformation = ifelse(transformation == "A_to_B", "First Change Point", "Second Change Point"),
         flip_dir = ifelse(flip_dir == "negative_to_positive", "Decreasing to Increasing", "Increasing to Decreasing"),
         slope_mag = ifelse(slope_mag > 2.5, 2.5, slope_mag)
         ) |>
  arrange(desc(slope_mag)) |>
  filter(tau == 0.5,
         !is.na(flip_dir),
         spc != "ox")

g1 = ggplot() +
  geom_sf(data = world)+
  geom_sf(data = plotDat,
          aes(colour = year, size = slope_mag),
          alpha = 0.5)+
  scale_size("Slope Magnitude / ppb yr<sup>-1</sup>")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_nested(flip_dir+transformation~spc)+
  scale_colour_viridis_c(limits = c(2000, 2023))+
  theme_minimal()+
  theme(panel.grid = element_line(colour = "white"),
        panel.background = element_rect(),
        strip.text = element_markdown(),
        legend.title = element_markdown(),
        legend.position = "bottom",
        legend.text = element_text(angle = 45, hjust = 0.7))


pdf(here::here('figures','f9_eu_mag_map.pdf'), width = 8.3, height = 11.7)
print(g1)
dev.off()


g2 = ggplot() +
  geom_sf(data = world)+
  geom_sf(data = plotDat,
          aes(colour = year, size = slope_mag),
          alpha = 0.5)+
  scale_size("Slope Magnitude / ppb yr<sup>-1</sup>")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_nested(flip_dir+transformation~spc)+
  scale_colour_viridis_c(limits = c(2000, 2023))+
  theme_minimal()+
  theme(panel.grid = element_line(colour = "white"),
        panel.background = element_rect(),
        strip.text = element_markdown(),
        legend.title = element_markdown(),
        legend.position = "bottom",
        legend.text = element_text(angle = 45, hjust = 0.7))


pdf(here::here('figures','f10_us_mag_map.pdf'), width = 11.7, height = 8.3)
print(g2)
dev.off()
