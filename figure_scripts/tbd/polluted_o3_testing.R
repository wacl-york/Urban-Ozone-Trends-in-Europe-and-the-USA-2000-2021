library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)
library(purrr)
library(ggpubr)
library(lubridate)
library(sf)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))
source(here::here('functions','dtw_helpers.R'))

p_colours = c(
  rgb(0, 0, 0.6),
  rgb(0.1176, 0.3922, 1),
  rgb(0.4706, 0.7373, 1),
  rgb(0.6431, 0.7569, 0.4431),
  rgb(1, 0.7294, 0.4),
  rgb(1, 0.3922, 0),
  rgb(0.6471, 0, 0.1294)
)

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)


# -------------------------------------------------------------------------

con = connect_to_db()

coverage = tbl(con, "coverage_annual") |>
  collect() |>
  mutate(year = year(date)) |>
  filter(name == "o3") |>
  select(year, station_id, perc, coverage_check) |>
  distinct()

# Collect mean mda8 data from database
mda8_mean_data = tbl(con, "dat_mda8") |>
  mutate(year = year(date)) |>
  select(station_id, year, mda8) |>
  group_by(station_id, year) |>
  collect() |>
  left_join(coverage, by = c("station_id", "year")) |>
  filter(coverage_check == TRUE) |>
  summarise_all("mean", na.rm = T) |>
  select(-coverage_check)


# Collect annual metric data from database
metrics_data = tbl(con, "dat_metrics") |>
  mutate(year = year(date)) |>
  select(station_id, year, metric, value) |>
  collect() |>
  pivot_wider(names_from = metric, values_from = value)

# Collect cluster data from database
cluster_data = tbl(con, "clusterTimeSeries_meancvi") |>
  collect() |>
  reindex_clusters() |>
  filter(type %in% c("mda8", "mda8_warm", "mda8_cold"),
         tau %in% c(0.05, 0.5, 0.95)) |>
  select(type, region, station_id, tau, cluster) |>
  collect() |>
  mutate(type = str_remove(pattern = "mda8_", type))

# Collect meta data
combined_meta = tbl(con, "combinedMeta") |>
  select(station_id, timezone, country, latitude, longitude) |>
  distinct() |>
  collect()

tables = c("arrow_data_freeTau_mda8_anom_all", "arrow_data_freeTau_mda8_anom_warm", "arrow_data_freeTau_mda8_anom_cold")

process_table = function(tbl_name){
  tbl(con, tbl_name) |>
    left_join(
      tbl(con, "combinedMeta") |>
        select(station_id, latitude, longitude, country, timezone) |>
        distinct(),
      by = c("station_id", "country")) |>
    collect() |>
    mutate(uncertainty = case_when(
      pv <= 0.01 ~ "very_high_certainty",
      0.05 >= pv & pv > 0.01 ~ "high_certainty",
      0.10 >= pv & pv > 0.05 ~ "medium_certainty",
      0.33 >= pv & pv > 0.10 ~ "low_certainty",
      pv > 0.33 ~ "very_low_certainty",
      .default = NA),
      map_region = ifelse(country == "United States of America", "US", "EU"),
      direction = ifelse(fit > 0, "increasing", "decreasing"),
      slopeBin = case_when(fit > 0 & fit < 0.5 ~ "small_increase",
                           fit >= 0.5 & fit < 1.5 ~ "moderate_increase",
                           fit >= 1.5 & fit < 2.5 ~ "large_increase",
                           fit >= 2.5 ~ "very_large_increase",
                           fit < 0 & fit > -0.5 ~ "small_decrease",
                           fit <= -0.5 & fit > -1.5 ~ "moderate_decrease",
                           fit <= -1.5 & fit > -2.5 ~ "large_decrease",
                           fit <= -2.5 ~ "very_large_decrease",
                           fit == 0 ~ "flat",
                           .default = NA)) |>
    select(-c(name, map_region)) |>
    mutate(type = str_remove(pattern = "arrow_data_freeTau_mda8_anom_", tbl_name),
           type = ifelse(type == "all", "mda8", type))
}

mda8_all <- map_dfr(tables, process_table)

combined_cluster_data = mda8_all |>
  left_join(mda8_mean_data, by = c("station_id", "year")) |>
  left_join(metrics_data, by = c("station_id", "year")) |>
  left_join(cluster_data, by = c("type", "station_id", "tau"))

dbDisconnect(con)

########################################################################################################

NDGT70 = metrics_data |>
  select(station_id, year, NDGT70) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  group_by(year, map_region) |>
  add_count(name = "total_sites_n") |>
  ungroup() |>
  mutate(metric_exceedance = ifelse(NDGT70 >= 25, TRUE, FALSE)) |>
  group_by(year, map_region, total_sites_n, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  ungroup() |>
  mutate(perc = (total_exceedances/total_sites_n)*100) |>
  filter(metric_exceedance == TRUE) |>
  select(year, map_region, total_sites_n, total_exceedances, perc) |>
  distinct()

ggplot(NDGT70)+
  geom_point(aes(x = year, y = total_exceedances, colour = map_region))+
  geom_line(aes(x = year, y = total_exceedances, colour = map_region))

### Look at raw exceedances ###

NDGT70_sites = metrics_data |>
  select(station_id, year, NDGT70) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  mutate(metric_exceedance = ifelse(NDGT70 >= 25, TRUE, FALSE)) |>
  group_by(year, map_region, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  filter(metric_exceedance == TRUE)

eu_ndgt70 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = NDGT70_sites,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("NDGT70")

us_ndgt70 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = NDGT70_sites,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("NDGT70")

########################################################################################################

metric_4MDA8 = metrics_data |>
  select(station_id, year, `4MDA8`) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  group_by(year, map_region) |>
  add_count(name = "total_sites_n") |>
  ungroup() |>
  mutate(metric_exceedance = ifelse(`4MDA8` >= 85, TRUE, FALSE)) |>
  group_by(year, map_region, total_sites_n, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  ungroup() |>
  mutate(perc = (total_exceedances/total_sites_n)*100) |>
  filter(metric_exceedance == TRUE) |>
  select(year, map_region, total_sites_n, total_exceedances, perc) |>
  distinct()

ggplot(metric_4MDA8)+
  geom_point(aes(x = year, y = total_exceedances, colour = map_region))+
  geom_line(aes(x = year, y = total_exceedances, colour = map_region))

### Look at raw exceedances ###

metric_4MDA8_sites = metrics_data |>
  select(station_id, year, `4MDA8`) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  mutate(metric_exceedance = ifelse(`4MDA8` >= 85, TRUE, FALSE)) |>
  group_by(year, map_region, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  filter(metric_exceedance == TRUE)

eu_4mda8 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_4MDA8_sites,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("4MDA8")

us_4mda8 = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_4MDA8_sites,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("4MDA8")

ggarrange(eu_4mda8, eu_ndgt70, nrow = 1)

heatwaves_4MDA8 = metric_4MDA8 |>
  filter(year %in% c(2003, 2006, 2015, 2018))

heatwaves_NDGT70 = NDGT70 |>
  filter(year %in% c(2003, 2006, 2015, 2018))

ggarrange(us_4mda8, us_ndgt70, nrow = 1)

########################################################################

metric_SOMO35_limit = metrics_data |>
  select(station_id, year, SOMO35) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  group_by(year, map_region) |>
  add_count(name = "total_sites_n") |>
  ungroup() |>
  mutate(metric_exceedance = ifelse(SOMO35 >= 7000, TRUE, FALSE)) |>
  group_by(year, map_region, total_sites_n, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  ungroup() |>
  mutate(perc = (total_exceedances/total_sites_n)*100) |>
  filter(metric_exceedance == TRUE) |>
  select(year, map_region, total_sites_n, total_exceedances, perc) |>
  distinct()

metric_SOMO35_sites_limit = metrics_data |>
  select(station_id, year, SOMO35) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  mutate(metric_exceedance = ifelse(SOMO35 >= 7000, TRUE, FALSE)) |>
  group_by(year, map_region, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  filter(metric_exceedance == TRUE)

eu_somo35_limit = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_SOMO35_sites_limit,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("SOMO35")

us_somo35_limit = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_SOMO35_sites_limit,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("SOMO35")

########################################################################

metric_AVGMDA8_limit = metrics_data |>
  select(station_id, year, AVGMDA8) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  group_by(year, map_region) |>
  add_count(name = "total_sites_n") |>
  ungroup() |>
  mutate(metric_exceedance = ifelse(AVGMDA8 >= 50, TRUE, FALSE)) |>
  group_by(year, map_region, total_sites_n, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  ungroup() |>
  mutate(perc = (total_exceedances/total_sites_n)*100) |>
  filter(metric_exceedance == TRUE) |>
  select(year, map_region, total_sites_n, total_exceedances, perc) |>
  distinct()

metric_AVGMDA8_sites_limit = metrics_data |>
  select(station_id, year, AVGMDA8) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  mutate(metric_exceedance = ifelse(AVGMDA8 >= 50, TRUE, FALSE)) |>
  group_by(year, map_region, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  filter(metric_exceedance == TRUE)

eu_AVGMDA8_limit = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_AVGMDA8_sites_limit,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("AVGMDA8")

us_AVGMDA8_limit = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_AVGMDA8_sites_limit,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("AVGMDA8")

########################################################################

metric_3MMDA1_limit = metrics_data |>
  select(station_id, year, `3MMDA1`) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  group_by(year, map_region) |>
  add_count(name = "total_sites_n") |>
  ungroup() |>
  mutate(metric_exceedance = ifelse(`3MMDA1` >= 85, TRUE, FALSE)) |>
  group_by(year, map_region, total_sites_n, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  ungroup() |>
  mutate(perc = (total_exceedances/total_sites_n)*100) |>
  filter(metric_exceedance == TRUE) |>
  select(year, map_region, total_sites_n, total_exceedances, perc) |>
  distinct()

metric_3MMDA1_sites_limit = metrics_data |>
  select(station_id, year, `3MMDA1`) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  mutate(metric_exceedance = ifelse(`3MMDA1` >= 85, TRUE, FALSE)) |>
  group_by(year, map_region, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  filter(metric_exceedance == TRUE)

eu_3MMDA1_limit = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_3MMDA1_sites_limit,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("3MMDA1")

us_3MMDA1_limit = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_3MMDA1_sites_limit,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("3MMDA1")

########################################################################

metric_6MMDA1_limit = metrics_data |>
  select(station_id, year, `6MMDA1`) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  group_by(year, map_region) |>
  add_count(name = "total_sites_n") |>
  ungroup() |>
  mutate(metric_exceedance = ifelse(`6MMDA1` >= 85, TRUE, FALSE)) |>
  group_by(year, map_region, total_sites_n, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  ungroup() |>
  mutate(perc = (total_exceedances/total_sites_n)*100) |>
  filter(metric_exceedance == TRUE) |>
  select(year, map_region, total_sites_n, total_exceedances, perc) |>
  distinct()

metric_6MMDA1_sites_limit = metrics_data |>
  select(station_id, year, `6MMDA1`) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(map_region = ifelse(country == "United States of America", country, "Europe")) |>
  mutate(metric_exceedance = ifelse(`6MMDA1` >= 85, TRUE, FALSE)) |>
  group_by(year, map_region, metric_exceedance) |>
  add_count(name = "total_exceedances") |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  filter(metric_exceedance == TRUE)

eu_6MMDA1_limit = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_6MMDA1_sites_limit,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("6MMDA1")

us_6MMDA1_limit = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metric_6MMDA1_sites_limit,
          mapping = aes(colour = metric_exceedance)) +
  scale_colour_manual(values = "red", name = "")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~year) +
  theme(legend.position = "none") +
  ggtitle("6MMDA1")

ggarrange(eu_somo35, eu_AVGMDA8, row = 1)
ggarrange(us_somo35, us_AVGMDA8, row = 1)

##################################################################################

con = connect_to_db()

polluted_metrics_data = metrics_data |>
  pivot_longer(cols = c(`4MDA8`, NDGT70, SOMO35, AVGMDA8, `3MMDA1`, `6MMDA1`), names_to = "metric", values_to = "value") |>
  mutate(limit = case_when(metric == "4MDA8" ~ 85,
                           metric == "NDGT70" ~ 25,
                           metric == "SOMO35" ~ 7000,
                           metric == "3MMDA1" ~ 80,
                           metric == "6MMDA1" ~ 80,
                           metric == "AVGMDA8" ~ 60,
                           .default = NA)) |>
  filter(value >= limit) |>
  left_join(combined_meta, b = "station_id") |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  mutate(category = case_when(metric %in% c("4MDA8", "NDGT70") ~ "Extreme",
                              metric %in% c("SOMO35", "AVGMDA8", "3MMDA1", "6MMDA1") ~ "Exposure",
                              .default = NA))

exceedances_data = polluted_metrics_data |>
  select(station_id, year, country, geometry, category) |>
  distinct() |>
 #filter(!(country != "United States of America" & category == "Extreme" & year %in% c(2003, 2006, 2015, 2018)))
  filter(year %in% c(2017:2021))

ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = exceedances_data,
          mapping = aes(colour = category)) +
  #scale_colour_viridis_d()+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~category+year)
#theme(legend.position = "none") +

ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = exceedances_data,
          mapping = aes(colour = category)) +
  #scale_colour_viridis_d()+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  facet_wrap(~category+year)


arrow_metric_dat = tbl(con, "arrow_data_freeTau_metrics") |>
  collect() |>
  select(metric, station_id, year, fit, pvStr) |>
  st_drop_geometry() |>
  distinct()

exceedances_data_combined = exceedances_data |>
  rename(exceedance_year = year) |>
  left_join(arrow_metric_dat, by = c("station_id")) |>
  distinct() |>
  filter(category == "Extreme" & metric %in% c("4MDA8", "NDGT70") |
         category == "Exposure" & metric %in% c("SOMO35", "AVGMDA8", "3MMDA1", "6MMDA1"))


test = combined_cluster_data |>
  st_drop_geometry() |>
  filter(tau == 0.95,
         #year %in% c(2000, 2004, 2008, 2012, 2016, 2020),
         region == "Europe",
         type == "mda8")

ggplot(test)+
  geom_point(aes(x = AVGMDA8, y = fit, colour = pvStr)) +
  scale_colour_manual(values = p_colours) +
  facet_wrap(~year, scales = "free") +
  theme_pubr()+
  theme(legend.position = "bottom") +
  ggtitle(paste0(type,", ",region))

test_binned <- test %>%
  mutate(AVGMDA8_bin = cut(AVGMDA8, breaks = seq(0, max(AVGMDA8, na.rm = TRUE), by = 5)))  # 5-unit bins

# Plot as stacked bar chart
ggplot(test_binned) +
  geom_bar(aes(x = AVGMDA8_bin, fill = pvStr), position = "stack") +
  scale_fill_manual(values = p_colours) +
  facet_wrap(~year, scales = "free") +
  theme_pubr() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.5, size = 5, hjust = 1)) +
  ggtitle("AVGMDA8 distribution by pvStr (stacked bar), Europe") +
  xlab("AVGMDA8 bins") +
  ylab("Count")

######################

# Drop geometry only once
data <- combined_cluster_data |> st_drop_geometry()

# Get unique combinations of region, type, and tau
combinations <- unique(data |> select(region, type, tau))

# Open PDF device
pdf("analysis/4MDA8_scatter_plots_by_region_type_tau.pdf", width = 12, height = 8)

# Loop through each combination
for (i in 1:nrow(combinations)) {
  region_i <- combinations$region[i]
  type_i   <- combinations$type[i]
  tau_i    <- combinations$tau[i]

  # Filter data for the current combination
  test <- data |>
    filter(region == region_i, type == type_i, tau == tau_i)

  # Skip empty subsets
  if (nrow(test) == 0) next

  # Create plot
  p <- ggplot(test) +
    geom_point(aes(x = `4MDA8`, y = fit, colour = pvStr)) +
    scale_colour_manual(values = p_colours) +
    facet_wrap(~year, scales = "fixed") +
    theme_pubr() +
    theme(legend.position = "bottom") +
    ggtitle(paste0(type_i, ", ", region_i, ", tau = ", tau_i))

  # Print to PDF (each print adds a new page)
  print(p)
}

# Close PDF device
dev.off()

