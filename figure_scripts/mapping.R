library(sf)
library(DBI)
library(ggh4x)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))


con = connect_to_db()

tables = dbListTables(con)[str_detect(dbListTables(con), "piecewise_stats_freeTau")]

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
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

pv_opt = c("p <= 0.05 (dec)",
           "0.05 < p <= 0.1 (dec)",
           "0.1  < p <= 0.33 (dec)",
           "p > 0.33",
           "0.1  < p <= 0.33 (inc)",
           "0.05 < p <= 0.1 (inc)",
           "p <= 0.05 (inc)"
)

plotList = list()

for(i in 1:length(tables)){

  tableName = tables[i]
  if(str_detect(tableName, "metric")){
    groupVars = c("station_id", "name", "tau", "metric")
    facetFormula = metric~factor(year)
  }else{
    groupVars = c("station_id", "name", "tau")
    facetFormula = tau~factor(year)
  }

  slopes = tbl(con, tableName) |>
    filter(stat == "slope") |>
    left_join(
      tbl(con, "combinedMeta") |>
        select(station_id, latitude, longitude, country) |>
        distinct(),
      by = "station_id") |>
    collect() |>
    arrange(pick(all_of(groupVars))) |>
    pivot_wider(names_from = "type") |>
    select(-stat) |>
    nest_by(pick(all_of(groupVars))) |>
    mutate(data = data |>
             expand_slopes() |>
             list()) |>
    unnest(data) |>
    mutate(fit = fit*365) |>
    ungroup() |>
    filter(between(fit, quantile(fit, 0.01), quantile(fit, 0.99))) |>
    mutate(
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
        factor(levels = pv_opt),
    )

  if(str_detect(tableName, "metric")){

    range_maxs = slopes |>
      mutate(fit = abs(fit)) |>
      group_by(metric) |>
      summarise(rm = quantile(fit, 0.99))

    lineGroups = slopes |>
      nest_by(metric) |>
      left_join(range_maxs, "metric") |>
      mutate(data = data |>
               calc_arrow_end(rangeMax = rm,
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
               select(-suffix) |>
               list()) |>
      unnest(data)

  }else{
    lineGroups = slopes |>
    calc_arrow_end(rangeMax = 3.5,
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
  }

  lines = lineGroups |>
    st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
    group_by(rn) |>
    summarise(do_union = FALSE) |>
    st_cast("LINESTRING")

  lineDat = left_join(lineGroups, lines, "rn") |>
    select(-latitude, -longitude, -rn) |>
    st_as_sf()

  plotDat = lineDat |>
    filter(name == "o3",
           year %in% c(2002, 2006, 2010, 2014, 2018, 2022)) |>
    st_transform(mycrs) |>
    mutate(name = ifelse(name == "no2",  "NO<sub>2</sub>", "O<sub>3</sub>") |>
             factor(levels = c( "O<sub>3</sub>","NO<sub>2</sub>")))

  plotList$geu = ggplot() +
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
    facet_grid(facetFormula)+
    # facet_nested_wrap(~name + factor(year), nrow = 2)+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          strip.text = element_markdown(),
          legend.position = "bottom",
          legend.byrow = T)+
    ggtitle(tableName)


  plotList$gus = ggplot() +
    geom_sf(data = world, fill = "white")+
    geom_sf(data = plotDat,
            mapping = aes(colour = pvStr),
            linewidth = 0.75,
            arrow = arrow(angle = 30,
                          ends = "last",
                          type = "open",
                          length = unit(0.1, "cm"))) +
    scale_colour_manual(values = p_colours, name = "")+
    scale_y_continuous(limits = st_coordinates(limUS)[,2])+
    scale_x_continuous(limits = st_coordinates(limUS)[,1])+
    facet_grid(facetFormula)+
    # facet_nested_wrap(~name + factor(year), nrow = 2)+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          strip.text = element_markdown(),
          legend.position = "bottom",
          legend.byrow = T)+
    ggtitle(tableName)

  dirOut = data_path("figures", "o3_map")

  if(!dir.exists(dirOut)){
    dir.create(dirOut, recursive = T)
  }

  fileOut = here::here(dirOut, paste0("o3_map_", str_remove(tableName, "piecewise_stats_"), ".pdf"))

  pdf(fileOut,width = 24, height = 16)
  print(plotList)
  dev.off()

}

dbDisconnect(con, shutdown = T)
