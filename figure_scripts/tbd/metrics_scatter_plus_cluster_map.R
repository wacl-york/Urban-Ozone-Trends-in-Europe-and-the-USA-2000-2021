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

################################################################################

# Drop geometry only once
data <- combined_cluster_data |>
  st_drop_geometry() |>
  filter(cluster != 99,
         year %in% c(2004,2018))

data_sf = data |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

# Get unique combinations of region, type, and tau
combinations <- unique(data |> select(region, type, tau))

# Open PDF device
pdf("analysis/6MMDA1_scatter_plots_by_region_type_tau_combo.pdf", width = 12, height = 8)

# Loop through each combination
for (i in 1:nrow(combinations)) {
  region_i <- combinations$region[i]
  type_i   <- combinations$type[i]
  tau_i    <- combinations$tau[i]

  # Filter data for the current combination
  test <- data |>
    filter(region == region_i, type == type_i, tau == tau_i)

  test_sf <- data_sf |>
    filter(region == region_i, type == type_i, tau == tau_i)

  # Skip empty subsets
  if (nrow(test) == 0) next

  # Create plot
  p_cluster <- ggplot(test) +
    geom_point(aes(x = `6MMDA1`, y = fit, colour = as.factor(cluster))) +
    #scale_colour_manual(values = p_colours) +
    facet_wrap(~year, scales = "fixed") +
    theme_pubr() +
    theme(legend.position = "bottom") +
    ggtitle(paste0(type_i, ", ", region_i, ", tau = ", tau_i))

  p_significance <- ggplot(test) +
    geom_point(aes(x = `6MMDA1`, y = fit, colour = pvStr)) +
    scale_colour_manual(values = p_colours) +
    facet_wrap(~year, scales = "fixed") +
    theme_pubr() +
    theme(legend.position = "bottom") +
    ggtitle(paste0(type_i, ", ", region_i, ", tau = ", tau_i))

  if (region_i == "Europe") {
    x_limits <- st_coordinates(limEU)[,2]
    y_limits <- st_coordinates(limEU)[,1]
  } else if (region_i == "United States of America") {
    x_limits <- st_coordinates(limUS)[,2]
    y_limits <- st_coordinates(limUS)[,1]
  } else {
    # Optional: default to full world extent
    x_limits <- c(-180, 180)
    y_limits <- c(-90, 90)
  }

  p_map = ggplot() +
    geom_sf(data = world, fill = "white")+
    geom_sf(data = test_sf, aes(colour = as.factor(cluster)))+
    #scale_colour_manual(values = p_colours, name = "")+
    scale_y_continuous(limits = x_limits)+
    scale_x_continuous(limits = y_limits) +
    theme(legend.position = "bottom")


  # Patchwork
  library(patchwork)
  # Left column: cluster on top, significance on bottom
  left_side = p_cluster / p_significance  # stacked vertically

  # Final layout: left half (vertical stack) + right half (map)
  final_plot = left_side | p_map + plot_layout(widths = c(1.25, 0.75)) # side-by-side layout


  # Print to PDF (each print adds a new page)
  print(final_plot)
}

# Close PDF device
dev.off()

