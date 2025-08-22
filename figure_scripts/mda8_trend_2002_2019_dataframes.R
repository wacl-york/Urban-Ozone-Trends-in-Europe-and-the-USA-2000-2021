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
tables = c("piecewise_stats_freeTau_mda8_anom_all", "piecewise_stats_freeTau_mda8_anom_cold", "piecewise_stats_freeTau_mda8_anom_warm")


combined_meta = tbl(con, "combinedMeta") |>
  select(station_id, timezone, country) |>
  distinct()


#tables = dbListTables(con)[str_detect(dbListTables(con), "piecewise_stats_freeTau")]
tables = "piecewise_stats_freeTau_mda8_anom_warm"
i = 1

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

tableName = tables[i]
if(str_detect(tableName, "metric")){
  groupVars = c("station_id", "name", "tau", "metric")
  facetFormula = metric~factor(year)
}else{
  groupVars = c("station_id", "name", "tau")
  facetFormula = tau~factor(year)
}

mda8_all = tbl(con, tableName) |>
  filter(stat == "slope") |>
  left_join(
    tbl(con, "combinedMeta") |>
      select(station_id, latitude, longitude, country, timezone) |>
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
  ) |>
  mutate(uncertainty = case_when(
    pv <= 0.01 ~ "very_high_certainty",
    0.05 >= pv & pv > 0.01 ~ "high_certainty",
    0.10 >= pv & pv > 0.05 ~ "medium_certainty",
    0.33 >= pv & pv > 0.10 ~ "low_certainty",
    pv > 0.33 ~ "very_low_certainty",
    .default = NA),
    map_region = ifelse(country == "United States of America", "US", "EU"))
# |>
#  filter(year %in% c(2002, 2019))

combined_meta = tbl(con, "combinedMeta") |>
  select(station_id, timezone, country, latitude, longitude) |>
  distinct() |>
  collect()

dbDisconnect(con)

############# Count the number of arrows at different uncertainties ################

# Percentage of slopes at each uncertainty/direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. What percentage of high certainty positive trends are there in the region/tau/year group?
# (does not account for magnitude)

mda8_uncertainty_count = mda8_all |>
  filter(fit != 0) |>
  #filter(year == "first") |>
  mutate(uncertainty = ifelse(uncertainty == "very_high_certainty", "high_certainty", uncertainty),
         direction = ifelse(fit > 0, "increasing", "decreasing")) |>
  select(map_region, tau, year, direction, uncertainty) |>
  group_by(map_region,, tau, year, direction, uncertainty) |>
  count() |>
  group_by(map_region, tau, year) |>
  mutate(percentage = n / sum(n)*100) |>
  pivot_wider(names_from = tau, values_from = c(n, percentage)) |>
  # These filters are used to look at the ranges for the significance bar plots
  filter(uncertainty == "high_certainty",
         map_region == "US",
         direction == "increasing")

ggplot(mda8_uncertainty_count)+
  geom_point(aes(x = year, y = n_0.95)) +
  geom_line(aes(x = year, y = n_0.95))

# Trying to explore the MDA8 trends in 95th percentile that are showing high certainty increases#

explore_0.95_inc_high_cert = mda8_all |>
  filter(tau == 0.95,
         country == "United States of America",
         uncertainty %in% c("high_certainty", "very_high_certainty"),
         fit > 0,
         year == 2017) |>
  select(station_id, timezone, fit) |>
  left_join(combined_meta, by = c("station_id", "timezone")) |>
  unique()

sites_explore_0.95_inc_high_cert = unique(explore_0.95_inc_high_cert$station_id)

explore_all = mda8_all |>
  filter(tau == 0.95,
         country == "United States of America",
         year == 2017) |>
  select(station_id, timezone, fit) |>
  left_join(combined_meta, by = c("station_id", "timezone"))

explore_all_sf = explore_all |>
  st_as_sf(coords = c(lon = "longitude", lat = "latitude"), crs = 4326) |>
  mutate(target = ifelse(station_id %in% c(explore_0.95_inc_high_cert$station_id), "yes", "no"))


#sites_explore_0.95_inc_high_cert = unique(explore_0.95_inc_high_cert$station_id)

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

ggplot() +
  geom_sf(data = world)+
  geom_sf(data = explore_all_sf |> slice_max(fit, n= 3),
          aes(fill = target, colour = target,
              size = fit),
          alpha = 0.5)+
  scale_size("ppb / yr")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  # scale_fill_scico(palette = "berlin")
  # facet_nested(flip_dir+transformation~spc)+
  #scale_colour_viridis_c(limits = c(2000, 2023))+
  theme_minimal() +
  # facet_wrap(~switch)+
  scale_color_viridis_d() +
  scale_fill_viridis_d()

test = explore_all_sf |>
  filter(target == "yes")

######### Count the number of arrows at different slope magnitudes ##########

# mda8_all |>
#   ggplot()+
#   geom_density(aes(x = fit)) +
#   facet_wrap(~map_region+tau)

# Percentage of slopes at each direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. what percentage of positive trends are there in the region/tau/year group?
# (does not account for uncertainty, or slope magnitude)
mda8_slope_count = mda8_all |>
  filter(fit != 0) |>
  #filter(year == "first") |>
  mutate(uncertainty = ifelse(uncertainty == "very_high_certainty", "high_certainty", uncertainty),
         direction = ifelse(fit > 0, "increasing", "decreasing")) |>
  select(map_region, year, tau, direction) |>
  group_by(map_region, year, tau, direction) |>
  count() |>
  ungroup() |>
  group_by(map_region, year, tau) |>
  mutate(percentage = n / sum(n)*100) |>
  filter(tau %in% c(0.05,0.5,0.95)) |>
  filter(year == 2002) |>
  #filter(tau %in% 0.95) |>
  filter(map_region == "EU")
  #pivot_wider(names_from = tau, values_from = c(n, percentage))

# Percentage of slopes at each magnitude/direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. what percentage of high magnitude positive trends are there in the region/tau/year group?
# (does not account for uncertainty)  Percentage is "of the increasing/decreasing trends".

mda8_slope_detailed_count_per_direction = mda8_all |>
  filter(fit != 0) |>
  #filter(year == "first") |>
  mutate(uncertainty = ifelse(uncertainty == "very_high_certainty", "high_certainty", uncertainty),
         direction = ifelse(fit > 0, "increasing", "decreasing"),
         slopeBin = case_when(fit > 0 & fit < 0.5 ~ "small_increase",
                              fit >= 0.5 & fit < 1.5 ~ "moderate_increase",
                              fit >= 1.5 & fit < 2.5 ~ "large_increase",
                              fit >= 2.5 ~ "very_large_increase",
                              fit < 0 & fit > -0.5 ~ "small_decrease",
                              fit <= -0.5 & fit > -1.5 ~ "moderate_decrease",
                              fit <= -1.5 & fit > -2.5 ~ "large_decrease",
                              fit <= -2.5 ~ "very_large_decrease",
                              .default = NA)) |>
  select(map_region, year, tau, direction, slopeBin) |>
  group_by(map_region, year, tau, direction, slopeBin) |>
  count() |>
  group_by(map_region, year, tau, direction) |>
  mutate(percentage = n / sum(n)*100) |>
  pivot_wider(names_from = tau, values_from = c(n, percentage)) |>
  filter(map_region == "EU") |>
  #filter(year == 2019) |>
  filter(direction == "decreasing")

# Percentage of slopes at each magnitude/direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. what percentage of high magnitude positive trends are there in the region/tau/year group?
# (does not account for uncertainty)

mda8_slope_detailed_count = mda8_all |>
  filter(fit != 0) |>
  #filter(year == "first") |>
  mutate(uncertainty = ifelse(uncertainty == "very_high_certainty", "high_certainty", uncertainty),
         direction = ifelse(fit > 0, "increasing", "decreasing"),
         slopeBin = case_when(fit > 0 & fit < 0.5 ~ "small_increase",
                              fit >= 0.5 & fit < 1.5 ~ "moderate_increase",
                              fit >= 1.5 & fit < 2.5 ~ "large_increase",
                              fit >= 2.5 ~ "very_large_increase",
                              fit < 0 & fit > -0.5 ~ "small_decrease",
                              fit <= -0.5 & fit > -1.5 ~ "moderate_decrease",
                              fit <= -1.5 & fit > -2.5 ~ "large_decrease",
                              fit <= -2.5 ~ "very_large_decrease",
                              .default = NA)) |>
  select(map_region, year, tau, direction, slopeBin) |>
  group_by(map_region, year, tau, direction, slopeBin) |>
  count() |>
  group_by(map_region, year, tau) |>
  mutate(percentage = n / sum(n)*100) |>
  pivot_wider(names_from = tau, values_from = c(n, percentage)) |>
  filter(map_region == "EU") |>
  filter(year == 2002)

# Percentage of slopes at each magnitude/uncertainty/direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. what percentage of high certainty high magnitude positive trends are there in the region/tau/year group?

mda8_slope_uncertainty_count = mda8_all |>
  filter(fit != 0) |>
  #filter(year == "first") |>
  mutate(uncertainty = ifelse(uncertainty == "very_high_certainty", "high_certainty", uncertainty),
         direction = ifelse(fit > 0, "increasing", "decreasing"),
         slopeBin = case_when(fit > 0 & fit < 0.5 ~ "small_increase",
                              fit >= 0.5 & fit < 1.5 ~ "moderate_increase",
                              fit >= 1.5 & fit < 2.5 ~ "large_increase",
                              fit >= 2.5 ~ "very_large_increase",
                              fit < 0 & fit > -0.5 ~ "small_decrease",
                              fit <= -0.5 & fit > -1.5 ~ "moderate_decrease",
                              fit <= -1.5 & fit > -2.5 ~ "large_decrease",
                              fit <= -2.5 ~ "very_large_decrease",
                              .default = NA)) |>
  select(map_region, year, tau, direction, uncertainty, slopeBin) |>
  group_by(map_region, year, tau, direction, uncertainty, slopeBin) |>
  count() |>
  filter(tau %in% c(0.05, 0.5, 0.95)) |>
  group_by(map_region, year, tau) |>
  mutate(percentage = n / sum(n, na.rm = T)*100) |>
  pivot_wider(names_from = tau, values_from = c(n, percentage)) |>
  filter(direction == "increasing") |>
  filter(uncertainty %in% c("medium_certainty", "high_certainty")) |>
  filter(year == 2002) |>
  filter(map_region == "EU")

