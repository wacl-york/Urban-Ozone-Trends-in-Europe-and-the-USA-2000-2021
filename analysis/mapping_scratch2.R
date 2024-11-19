library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(scico)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()


min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup() # the differences between the locations of the change points are not substantial
            # so just take the lower of the two sceanrio_idx arbitratily

slope_segs = tbl(con, "slope_segs") |>
  left_join(min_aic, by = c("station_id", "name", "reg")) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude),
            by = "station_id") |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(8857) |>
  rename(spc = name)

world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(8857)

lim = tibble(lng = c(-121,45), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(8857)

qDat = slope_segs |>
  st_drop_geometry() |>
  group_by(spc, tau) |>
  summarise(qLow = quantile(value, probs = 0.02),
            qHigh = quantile(value, probs = 0.98))

slope_segs |>
  filter(
    tau == 0.5,
    spc == "o3",
    seg %in% 11:14) |>
  left_join(qDat, c("spc", "tau")) |>
  filter(between(value, qLow, qHigh)) |>
  ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(aes(colour = value*365, group = station_id))+
  scale_color_scico(palette = "vik")+
  scale_y_continuous(limits = st_coordinates(lim)[,2])+
  scale_x_continuous(limits = st_coordinates(lim)[,1])+
  facet_wrap(~seg)

# slope_segs |>
#   filter(
#     tau == 0.5,
#     spc == "o3",
#     seg %in% 1:4) |>
#   left_join(qDat, c("spc", "tau")) |>
#   filter(between(value, qLow, qHigh)) |>
#   ggplot()+
#   geom_sf(data = world)+
#   geom_sf(aes(colour = value*365))+
#   scale_color_scico(palette = "vik")+
#   scale_y_continuous(limits = st_coordinates(lim)[,2])+
#   scale_x_continuous(limits = st_coordinates(lim)[,1])+
#   facet_wrap(~seg)
dbDisconnect(con, shutdown = T)
