library(sf)
library(gt)
library(DBI)
library(dplyr)
library(tidyr)
library(scico)
library(ggh4x)
library(ggtext)
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

min_aic = tbl(con, "min_aic_mda8") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup()                                               # the differences between the locations of the change points are not substantial
# so just take the lower of the two sceanrio_idx arbitrarily

slopes = inner_join(
  tbl(con, "qr_regressions_mda8"),
  min_aic,
  by = c("scenario_idx", "name", "station_id")
) |>
  filter(
    stat == "slope"
  ) |>
  rename(spc = name) |>
  anti_join(tbl(con, "remove_sites"),
            by = c("spc", "station_id")
  ) |>
  pivot_wider(names_from = "type") |>
  select(-stat) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude, country) |>
              distinct(),
            by = "station_id") |>
  collect() |>
  mutate(fit = fit*365) |>
  mutate(fit = ifelse(country == "United States of America", fit, fit/1.96),
         country = ifelse(country == "United States of America", country, "Europe"))  # ugm3 -> ppb

segments = slopes |>
  select(station_id, spc, reg, tau) |>
  distinct() |>
  mutate(segments = tribble(
    ~seg, ~segStart, ~segEnd,
    1,  2000, 2006,
    2,  2007, 2013,
    3,  2014, 2019,
    4,  2020, 2023,
    11, 2000, 2004,
    12, 2005, 2009,
    13, 2010, 2014,
    14, 2015, 2021
  ) |>
    purrr::pmap_df(~tibble(year = ..2:..3, seg = ..1)) |>
    list()) |>
  unnest(segments)

slopes_year = left_join(segments, slopes,
                        by = c("year" = "startYear",
                               "spc",
                               "station_id",
                               "reg",
                               "tau"),
                        relationship = "many-to-many") |>
  fill(everything())


dbDisconnect(con, shutdown = T)


pv_opt = c("p <= 0.05 (dec)",
           "0.05 < p <= 0.1 (dec)",
           "0.1  < p <= 0.33 (dec)",
           "p > 0.33",
           "0.1  < p <= 0.33 (inc)",
           "0.05 < p <= 0.1 (inc)",
           "p <= 0.05 (inc)"
)

fitLim = 5

lineGroups = slopes_year |>
  mutate(fit = case_when(fit > fitLim ~ fitLim,
                         fit < -fitLim ~ -fitLim,
                         TRUE~fit),
         pvStr = case_when(
           pv <= 0.05 & fit < 0 ~ pv_opt[1],
           pv > 0.05 & pv <= 0.1 & fit < 0 ~ pv_opt[2],
           pv > 0.1 & pv <= 0.33 & fit < 0 ~ pv_opt[3],
           pv >= 0.33 ~ pv_opt[4],
           pv > 0.1 & pv <= 0.33 & fit > 0 ~ pv_opt[5],
           pv > 0.05 & pv <= 0.1 & fit > 0 ~ pv_opt[6],
           pv <= 0.05 & fit > 0 ~ pv_opt[7],
           TRUE ~ NA
         ) |>
           factor(levels = pv_opt)
  ) |>
  calc_arrow_end(rangeMax = fitLim,
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


# -------------------------------------------------------------------------

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
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
  filter(
    year %in% c(2000,2015, 2019),
  ) |>
  st_transform(mycrs) |>
  filter(tau %in% c(0.1, 0.5, 0.9))

ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = plotDat,
          mapping = aes(colour = pvStr),
          linewidth = 0.25,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_manual(values = p_colours, name = "")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_grid(tau~year)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T)


ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = plotDat,
          mapping = aes(colour = pvStr),
          linewidth = 0.25,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_manual(values = p_colours, name = "")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_grid(tau~year)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T)










