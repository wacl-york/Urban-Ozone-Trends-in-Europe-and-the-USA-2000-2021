library(sf)
library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggplot2)

source(here::here('functions','utils.R'))
source(here::here('functions','dtw_helpers.R'))

con = connect_to_db()

# dbListTables(con)

mycrs = 4087

world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(35,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)


dat = tbl(con, "clusterTimeSeries_meancvi") |>
  left_join(
    combinedMetaRegion(con) |>
      select(station_id, latitude, longitude) |>
      distinct(),
    "station_id"
  ) |>
  filter(
    type %in% c("mda8", "mda8_warm", "mda8_cold" ),
    tau %in% c(0.05, 0.5, 0.95)
    ) |>
  collect() |>
  reindex_clusters() |>
  ungroup() |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "WGS84") |>
  st_transform(mycrs) |>
  mutate(cluster = ifelse(cluster == 99,0, cluster)) |>
  arrange(cluster) |>
  mutate(
    cluster = as.character(cluster),
    cluster = ifelse(cluster == 0, "No Cluster", cluster),
    cluster = factor(cluster,
                     levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "No Cluster")),
    type = case_when(
      type == "mda8" ~ "MDA8O<sub>3</sub>",
      type == "mda8_cold" ~ "MDA8O<sub>3</sub> Cold Season",
      type == "mda8_warm" ~ "MDA8O<sub>3</sub> Warm Season"
    ) |>
      factor(levels = c("MDA8O<sub>3</sub>", "MDA8O<sub>3</sub> Cold Season", "MDA8O<sub>3</sub> Warm Season")),
    tau = paste0("&tau; = ", tau)
    )


g_us = ggplot()+
  geom_sf(data = world)+
  geom_sf(data = filter(dat, region == "United States of America"),
          aes(colour = cluster),size = 0.5)+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  scale_colour_manual(
    name = "",
    values = c(
      "1" = "#E41A1C",
      "2" = "#377EB8",
      "3" = "#4DAF4A",
      "4" = "#984EA3",
      "5" = "#FF7F00",
      "6" = "#FFFF33",
      "7" = "#A65628",
      "8" = "#F781BF",
      "9" = "#8DD3C7",
      "10" = "#B3DE69",
      "No Cluster" = "#999999"
    ))+
  facet_grid(tau ~ type)+theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom")

g_eu = ggplot()+
  geom_sf(data = world)+
  geom_sf(data = filter(dat, region == "Europe"),
          aes(colour = cluster),size = 0.5)+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  scale_colour_manual(
    name = "",
    values = c(
      "1" = "#E41A1C",
      "2" = "#377EB8",
      "3" = "#4DAF4A",
      "4" = "#984EA3",
      "5" = "#FF7F00",
      "6" = "#FFFF33",
      "7" = "#A65628",
      "8" = "#F781BF",
      "9" = "#8DD3C7",
      "10" = "#B3DE69",
      "No Cluster" = "#999999"
    ))+
  facet_grid(tau ~ type)+theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom")

dirOut = here::here('figures','paper_figures')

if(!dir.exists(dirOut)){
  dir.create(dirOut)
}

grDevices::cairo_pdf(here(dirOut, "f07_eu_clusters.pdf"), width = 9.9, height = 7.25)
print(g_eu)
dev.off()

grDevices::cairo_pdf(here(dirOut, "f08_us_clusters.pdf"), width = 9.9, height = 5)
print(g_us)
dev.off()

dbDisconnect(con, shutdown = T)
