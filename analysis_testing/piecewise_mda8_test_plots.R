library(ggplot2)
library(sf)
library(rnaturalearth)
#library(rnaturalearthhires)
library(ggh4x)
library(ggtext)
library(scico)
library(DBI)
library(dplyr)
library(stringr)

source(here::here('functions','utils.R'))

con = connect_to_db()

### World stuff ###

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

################################################################################

# Collect sexy meta data #

toar_meta = tbl(con, "toarMeta") |>
  mutate(station_id = as.character(station_id)) |>
  select(station_id, country, lng, lat) |>
  filter(country == "United States of America") |>
  collect()

eea_meta = tbl(con, "eeaMeta") |>
  select(station_id = site, country, lng = longitude, lat = latitude) |>
  collect()

combined_meta = bind_rows(toar_meta, eea_meta) |>
  distinct()

###########################

# Collect data #

piecewise_stats  = tbl(con, "piecewise_stats_mda8_anom_all") |>
  filter(stat == "slope",
         name == "o3",
         type == "pv") |>
  select(-stat, -name, -type, -aic) |>
  rename(pv = value)

piecewise_dat = tbl(con, "piecewise_data_mda8_anom_all") |>
  filter(name == "o3", type == "fit") |>
  group_by(interaction(startYear, endYear)) |>
  mutate(piece = cur_group_id()) |>
  select(tau, startYear, endYear, aic, scenario_idx, station_id, slope, piece) |>
  distinct() |>
  left_join(piecewise_stats, by =  c("station_id", "scenario_idx", "tau", "startYear", "endYear")) |>
  collect() |>
  left_join(combined_meta, by = "station_id")

dat_0.5_USA = piecewise_dat |>
  filter(tau == 0.5) |>
  filter(country == "United States of America")

dat_0.5_USA_sf = dat_0.5_USA |>
  st_as_sf(coords = c(lon = "lng", lat = "lat"), crs = 4326) |>
  #filter(startYear == 2000) |>
  select(station_id, pv, geometry, slope, startYear) |>
  distinct() |>
  na.omit()

ggplot() +
  geom_sf(data = world)+
  geom_sf(data = dat_0.5_USA_sf,
          aes(colour = slope,
              fill = slope,
              size = pv),
          alpha = 0.5)+
  scale_size("pv")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
 # scale_fill_scico(palette = "berlin")
 # facet_nested(flip_dir+transformation~spc)+
  #scale_colour_viridis_c(limits = c(2000, 2023))+
  theme_minimal() +
  facet_wrap(~startYear)

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = dat_0.5_USA_sf,
          shape = 21,
          mapping = aes(fill = slope, size = pv),
          alpha = 0.8) +
  scale_size(name = "pv") +
  scale_fill_scico(name = "slope", palette = "berlin") +
  scale_y_continuous(limits = st_coordinates(limUS)[,2]) +
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  theme_minimal() +
  facet_wrap(~startYear)


  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_rect(fill = "white"),
        strip.text = element_markdown(),
        legend.title = element_markdown(),
        legend.position = "bottom",
        legend.text = element_text(angle = 45, hjust = 0.7))


















p_colours = c(
  rgb(0, 0, 0.6),
  rgb(0.1176, 0.3922, 1),
  rgb(0.4706, 0.7373, 1),
  rgb(0.6431, 0.7569, 0.4431),
  rgb(1, 0.7294, 0.4),
  rgb(1, 0.3922, 0),
  rgb(0.6471, 0, 0.1294)
)











plotDat = lineDat |>
  filter(spc != "ox",
         tau == 0.5,
         seg %in% 11:14) |>
  st_transform(mycrs) |>
  mutate(segLab = paste(segStart, segEnd, sep = " - "),
         spc = ifelse(spc == "no2",  "NO<sub>2</sub>", "O<sub>3</sub>") |>
           factor(levels = c( "O<sub>3</sub>","NO<sub>2</sub>")))
