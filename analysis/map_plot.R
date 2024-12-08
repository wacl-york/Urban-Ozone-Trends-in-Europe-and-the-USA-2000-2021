library(sf)
library(gt)
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
              select(station_id, latitude, longitude, country) |>
              distinct(),
            by = "station_id") |>
  rename(spc = name) |>
  anti_join(tbl(con, "remove_sites"),
            by = c("spc", "station_id")
  ) |>
  collect()

dbDisconnect(con, shutdown = T)

pv_opt = c("p <= 0.05 (dec)",
           "0.05 < p <= 0.1 (dec)",
           "0.1  < p <= 0.33 (dec)",
           "p > 0.33",
           "0.1  < p <= 0.33 (inc)",
           "0.05 < p <= 0.1 (inc)",
           "p <= 0.05 (inc)"
)

lineGroups = slope_segs |>
  mutate(fit = fit*365) |>
  mutate(fit = ifelse(country == "United States of America", fit, fit/1.96)) |> # ugm3 -> ppb
  mutate(fit = case_when(fit > 2.5 ~ 2.5,
                         fit < -2.5 ~ -2.5,
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
  calc_arrow_end(rangeMax = 2.5,
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

segs = tribble(
  ~seg, ~segStart, ~segEnd,
  1,  2000, 2006,
  2,  2007, 2013,
  3,  2014, 2019,
  4,  2020, 2023,
  11, 2000, 2004,
  12, 2005, 2009,
  13, 2010, 2014,
  14, 2015, 2021
)

lineDat = left_join(lineGroups, lines, "rn") |>
  select(-latitude, -longitude, -rn) |>
  st_as_sf() |>
  left_join(segs, by = "seg")



# -------------------------------------------------------------------------

mycrs = 4087 #8857

world = rnaturalearth::ne_countries(scale = "large", returnclass = "sf") |>
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

if(!dir.exists(here::here('plots','arrow_maps'))){
  dir.create(here::here('plots','arrow_maps'))
}


plotOpts = expand.grid(tau = unique(lineDat$tau),
                       spc = c("o3","no2","ox"),
                       segMin = c(1, 11),
                       segMax = c(4, 14)) |>
  filter(between(segMax-segMin, 0, 5)) |>
  tibble()

cli::cli_progress_bar(total = nrow(plotOpts))

for(i in 1:nrow(plotOpts)){

  cli::cli_progress_update()

  if(!dir.exists(here::here('plots','arrow_maps', plotOpts$spc[i],plotOpts$segMin[i]))){
    dir.create(here::here('plots','arrow_maps', plotOpts$spc[i],plotOpts$segMin[i]), recursive = T)
  }

  plotDat = lineDat |>
    filter(spc == plotOpts$spc[i],
           tau == plotOpts$tau[i],
           seg %in% plotOpts$segMin[i]:plotOpts$segMax[i]) |>
    st_transform(mycrs) |>
    mutate(segLab = paste(segStart, segEnd, sep = " - "))

  fileNameUS = paste0(
    paste(
      "US_map_spc",
      plotOpts$spc[i],
      "tau",
      plotOpts$tau[i],
      "seg",
      plotOpts$segMin[i],
      plotOpts$segMax[i],
      sep = "_"
    ),
    ".png"
  )

  fileNameEU = paste0(
    paste(
      "EU_map_spc",
      plotOpts$spc[i],
      "tau",
      plotOpts$tau[i],
      "seg",
      plotOpts$segMin[i],
      plotOpts$segMax[i],
      sep = "_"
    ),
    ".png"
  )

  gus = ggplot() +
    geom_sf(data = world, fill = "white")+
    geom_sf(data = plotDat,
            mapping = aes(colour = pvStr),
            linewidth = 0.75,
            arrow = arrow(angle = 30,
                          ends = "last",
                          type = "open",
                          length = unit(0.3, "cm"))) +
    scale_colour_manual(values = p_colours, name = "")+
    scale_y_continuous(limits = st_coordinates(limUS)[,2])+
    scale_x_continuous(limits = st_coordinates(limUS)[,1])+
    facet_wrap(~segLab)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          legend.position = "bottom",
          legend.byrow = T)


  geu = ggplot() +
    geom_sf(data = world, fill = "white")+
    geom_sf(data = plotDat,
            mapping = aes(colour = pvStr),
            linewidth = 0.75,
            arrow = arrow(angle = 30,
                          ends = "last",
                          type = "open",
                          length = unit(0.3, "cm"))) +
    scale_colour_manual(values = p_colours, name = "")+
    scale_y_continuous(limits = st_coordinates(limEU)[,2])+
    scale_x_continuous(limits = st_coordinates(limEU)[,1])+
    facet_wrap(~segLab)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          legend.position = "bottom",
          legend.byrow = T)

  scale = 3000

  png(here::here('plots','arrow_maps',plotOpts$spc[i],plotOpts$segMin[i],fileNameUS), res = 300, width = (scale*1.7), height = scale)
  print(gus)
  dev.off()

  png(here::here('plots','arrow_maps',plotOpts$spc[i],plotOpts$segMin[i],fileNameEU), res = 300, width = (scale*1.7), height = scale)
  print(geu)
  dev.off()

}





# Tables ------------------------------------------------------------------

tableDat = lineDat |>
  st_drop_geometry() |>
  mutate(country = ifelse(country == "United States of America", country, "Europe")) |>
  select(spc, seg, tau, fit, pv, Country = country) |>
  group_by(spc, seg, tau, country) |>
  summarise(
    Increasing = sum(fit < 0 & pv <= 0.33),
    Decreasing = sum(fit > 0 & pv <= 0.33),
    `No Trend` = sum(pv > 0.33)
  ) |>
  ungroup() |>
  filter(seg %in% 11:14) |>
  left_join(segs, "seg") |>
  mutate(segLab = paste(segStart, segEnd, sep = " - ")) |>
  select(-seg, -segStart, -segEnd) |>
  pivot_wider(values_from = c(Increasing, Decreasing, `No Trend`),
              names_from = segLab,
              names_sep = "_"
  )

#
#
# tableDat |>
#   filter(
#     country == "Europe",
#     spc == "o3"
#   ) |>
#   filter(seg %in% 11:14) |>
#   select(-seg) |>
#   pivot_wider(values_from = -c(spc, seg, tau, country),
#               names_from = "segLab",
#               names_sep = "_")
#
# knitr::kable()





make_table = function(x){

  x |>
    gt() |>
    tab_spanner(
      label = "Increasing",
      columns = contains("Increasing")
    ) |>
    tab_spanner(
      label = "Decreasing",
      columns = contains("Decreasing")
    ) |>
    tab_spanner(
      label = "No Trend",
      columns = contains("No Trend")
    ) |>
    tab_row_group(
      label = "O3",
      rows = spc == "o3"
    ) |>
    tab_row_group(
      label = "NO2",
      rows = spc == "no2"
    ) |>
    tab_row_group(
      label = "Ox",
      rows = spc == "ox"
    ) |>
    cols_label_with(
      columns = contains("-"),
      fn = \(x) x |>
        stringr::str_remove("Increasing_") |>
        stringr::str_remove("Decreasing_") |>
        stringr::str_remove("No Trend_")) |>
    cols_label(
      spc = "Species",
      tau = ":tau:"
    )
}

tableDat |>
  filter(country == "Europe") |>
  select(-country) |>
  make_table() |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("o3", "") |>
  stringr::str_replace_all("no2", "") |>
  stringr::str_replace_all("ox", "") |>
  stringr::str_replace(":tau:", "$\\\\tau$") |>
  writeLines(here::here('tables','europe_segs_11_14.txt'))


tableDat |>
  filter(country == "United States of America") |>
  select(-country) |>
  make_table() |>
  as_latex() |>
  as.character() |>
  stringr::str_replace_all("o3", "") |>
  stringr::str_replace_all("no2", "") |>
  stringr::str_replace_all("ox", "") |>
  stringr::str_replace(":tau:", "$\\\\tau$") |>
  writeLines(here::here('tables','usa_segs_11_14.txt'))

