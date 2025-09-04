library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)
library(lubridate)
library(sf)
library(gt)
library(scico)
library(ggh4x)
library(rnaturalearth)

source(here::here('functions','utils.R'))

con = connect_to_db()

tables = dbListTables(con)[str_detect(dbListTables(con), "metric")]

################################################################################

combined_meta = tbl(con, "combinedMeta") |>
  collect() |>
  select(station_id, latitude, longitude, country) |>
  distinct()

################################################################################

metrics = tbl(con, "dat_metrics") |>
  collect() |>
  mutate(year = year(date)) |>
  filter(year %in% c(2000:2019)) |>
  mutate(decade = case_when(year %in% c(2000:2009) ~ "2000 - 2009",
                            year %in% c(2010:2019) ~ "2010 - 2019",
                            .default = NA)) |>
  select(decade, station_id, timezone, value, metric) |>
  group_by(decade, station_id, timezone, metric) |>
  summarise_all(c("mean", "sd"), na.rm = T) |>
  left_join(combined_meta, by = "station_id")

metrics_sf = metrics |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326)

################################################################################

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

metrics_4MDA8 = metrics_sf |>
  filter(metric == "4MDA8") |>
  arrange(mean)

metrics_3MMDA1 = metrics_sf |>
  filter(metric == "3MMDA1") |>
  arrange(mean)

metrics_6MMDA1 = metrics_sf |>
  filter(metric == "6MMDA1") |>
  arrange(mean)

metrics_AVGMDA8 = metrics_sf |>
  filter(metric == "AVGMDA8") |>
  arrange(mean)

metrics_NDGT70 = metrics_sf |>
  filter(metric == "NDGT70") |>
  arrange(mean)

metrics_SOMO35 = metrics_sf |>
  filter(metric == "NDGT70") |>
  arrange(mean)

################################################################################

plot_4MDA8_US = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_4MDA8 |> filter(country == "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("4MDA8")

plot_3MMDA1_US = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_3MMDA1 |> filter(country == "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("3MMDA1")

plot_6MMDA1_US = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_6MMDA1 |> filter(country == "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("6MMDA1")

plot_AVGMDA8_US = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_AVGMDA8 |> filter(country == "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("AVGMDA8")

plot_NDGT70_US = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_NDGT70 |> filter(country == "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("NDGT70")

plot_SOMO35_US = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_SOMO35 |> filter(country == "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("SOMO35")

################################################################################

plot_4MDA8_EU = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_4MDA8 |> filter(country != "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("4MDA8")

plot_3MMDA1_EU = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_3MMDA1 |> filter(country != "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("3MMDA1")

plot_6MMDA1_EU = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_6MMDA1 |> filter(country != "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("6MMDA1")

plot_AVGMDA8_EU = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_AVGMDA8 |> filter(country != "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("AVGMDA8")

plot_NDGT70_EU = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_NDGT70 |> filter(country != "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("NDGT70")

plot_SOMO35_EU = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = (metrics_SOMO35 |> filter(country != "United States of America")),
          mapping = aes(colour = mean),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_viridis_b()+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T) +
  facet_wrap(~decade) +
  ggtitle("SOMO35")

################################################################################

dbDisconnect(con, shutdown = T)

dirOut = here::here("figures","metrics_maps")

fileOut = here::here(dirOut, paste0("metrics_map_2_decade.pdf"))

pdf(fileOut,width = 12, height = 8)
print(plot_3MMDA1_EU)
print(plot_3MMDA1_US)
print(plot_4MDA8_EU)
print(plot_4MDA8_US)
print(plot_6MMDA1_EU)
print(plot_6MMDA1_US)
print(plot_AVGMDA8_EU)
print(plot_AVGMDA8_US)
print(plot_NDGT70_EU)
print(plot_NDGT70_US)
print(plot_SOMO35_EU)
print(plot_SOMO35_US)
dev.off()
