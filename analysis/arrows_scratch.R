library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(scico)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)

calc_arrow_end = function(df,
                          rangeMax,
                          slope,
                          length,
                          colX = "longitude",
                          colY = "latitude",
                          outNameX = "longitude_end",
                          outNameY = "latitude_end") {

  deg2rad <- function(deg) {(deg * pi) / (180)}

  theta = (90/rangeMax)*df[[slope]]

  df[[outNameX]] = length * cos(deg2rad(theta)) + df[[colX]]
  df[[outNameY]] = length * sin(deg2rad(theta)) + df[[colY]]

  df

}

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup()                                               # the differences between the locations of the change points are not substantial
                                                          # so just take the lower of the two sceanrio_idx arbitrarily

slope_segs = inner_join(
  min_aic,
  tbl(con, "slope_segs"),
  by = c("station_id", "name", "reg")) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude) |>
              distinct(),
            by = "station_id") |>
  collect()

lineGroups = slope_segs |>
  mutate(fit = fit*365) |>
  mutate(fit = case_when(fit > 5 ~ 5,
                         fit < -5 ~ -5,
                         TRUE~fit),
         pvStr = case_when(
           pv <= 0.05 & fit < 0 ~ "<0.05_-ve",
           between(pv, 0.05, 0.1) & fit < 0 ~ "0.05-0.1_-ve",
           between(pv, 0.1, 0.33) & fit < 0 ~ "0.1-0.33_-ve",
           pv >= 0.33 ~ ">0.33",
           between(pv, 0.1, 0.33) & fit > 0 ~ "0.1-0.33_+ve",
           between(pv, 0.05, 0.1) & fit > 0 ~ "0.05-0.1_+ve",
           pv <= 0.05 & fit > 0 ~ "<0.05_+ve",
           TRUE ~ NA
           )
         ) |>
  calc_arrow_end(rangeMax = 5,
                 slope = "fit",
                 length = 3) |>
  rename(latitude_start = latitude,
         longitude_start = longitude) |>
  mutate(rn = row_number()) |>
  pivot_longer(c(latitude_start,latitude_end, longitude_start, longitude_end),
               names_to = c("coordType","suffix"),
               names_sep = "_",
               values_to = "coord"
  ) |>
  pivot_wider(names_from = "coordType",
              values_from = "coord") |>
  select(-suffix)

lines = lineGroups |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  group_by(rn) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

lineDat = left_join(lineGroups, lines, "rn") |>
  select(-latitude, -longitude, -rn) |>
  st_as_sf()


dbDisconnect(con, shutdown = T)

# -------------------------------------------------------------------------

mycrs = 4087 #8857

world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)

lim = tibble(lng = c(-121,45), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

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
  rename(spc = name) |>
  filter(spc == c("ox"),
         tau %in% c(0.5),
         seg %in% 11:14) |>
  st_transform(mycrs) |>
  mutate(pvStr = factor(pvStr,
                        levels = c("<0.05_-ve",
                                   "0.05-0.1_-ve",
                                   "0.1-0.33_-ve",
                                   ">0.33",
                                   "0.1-0.33_+ve",
                                   "0.05-0.1_+ve",
                                   "<0.05_+ve"
                                   )
                        ))

ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = plotDat,
          mapping = aes(colour = pvStr),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.3, "cm"))) +
  scale_colour_manual(values = p_colours)+
  scale_y_continuous(limits = st_coordinates(lim)[,2])+
  scale_x_continuous(limits = st_coordinates(lim)[,1])+
  facet_wrap(~seg)
