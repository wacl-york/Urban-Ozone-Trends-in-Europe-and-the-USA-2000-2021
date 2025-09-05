library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)
library(purrr)
library(ggpubr)
library(lubridate)

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

process_table <- function(tbl_name){
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

# plotDat = combined_cluster_data |>
#   filter(tau == 0.95,
#          type == "mda8",
#          region == "Europe",
#          cluster %in% c(4, 6))
#
# ggplot(plotDat)+
#   geom_line(aes(x = year, y = fit, colour = station_id, linewidth = 1))+
#   geom_point(aes(x = year, y = fit, fill = pvStr),
#              shape = 21, size = 2, color = "black")+
#   scale_fill_manual(values = p_colours) +
#   facet_wrap(~cluster)


stations_of_interest = combined_cluster_data |>
  filter(tau == 0.95,
         type == "mda8",
         cluster == 7,
         country == "United States of America")

unique(stations_of_interest$station_id)
#
# testing_cluster_3 = combined_cluster_data |>
#   filter(tau == 0.95,
#          type == "mda8",
#          cluster %in% c(3,5,7),
#          country != "United States of America") |>
#   select(station_id, year, fit, pvStr, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`, AVGMDA8) |>
#   pivot_longer(cols = c(fit, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`, AVGMDA8), names_to = "metric", values_to = "value") |>
#   select(-c(station_id, pvStr)) |>
#   group_by(year, metric) |>
#   summarise_all(c("mean", "sd"), na.rm = T) |>
#   mutate(region = "Southern Europe")
#
# testing_cluster_2 = combined_cluster_data |>
#   filter(tau == 0.95,
#          type == "mda8",
#          cluster == 2,
#          country != "United States of America") |>
#   select(station_id, year, fit, pvStr, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`, AVGMDA8) |>
#   pivot_longer(cols = c(fit, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`, AVGMDA8), names_to = "metric", values_to = "value") |>
#   select(-c(station_id, pvStr)) |>
#   group_by(year, metric) |>
#   summarise_all(c("mean", "sd"), na.rm = T) |>
#   mutate(region = "Central Europe")
#
# testing_cluster_4_6 = combined_cluster_data |>
#   filter(tau == 0.95,
#          type == "mda8",
#          cluster %in% c(4,6),
#          country != "United States of America") |>
#   select(station_id, year, fit, pvStr, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`, AVGMDA8) |>
#   pivot_longer(cols = c(fit, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`, AVGMDA8), names_to = "metric", values_to = "value") |>
#   select(-c(station_id, pvStr)) |>
#   group_by(year, metric) |>
#   summarise_all(c("mean", "sd"), na.rm = T) |>
#   mutate(region = "Northern Europe")
#
# testing_cluster_1 = combined_cluster_data |>
#   filter(tau == 0.95,
#          type == "mda8",
#          cluster %in% c(1),
#          country != "United States of America") |>
#   select(station_id, year, fit, pvStr, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`, AVGMDA8) |>
#   pivot_longer(cols = c(fit, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`, AVGMDA8), names_to = "metric", values_to = "value") |>
#   select(-c(station_id, pvStr)) |>
#   group_by(year, metric) |>
#   summarise_all(c("mean", "sd"), na.rm = T) |>
#   mutate(region = "Eastern Europe")
#
# testing_clusters_combined = bind_rows(testing_cluster_2, testing_cluster_3, testing_cluster_4_6,testing_cluster_1)

# ggplot(testing_clusters_combined)+
#   geom_point(aes(x = year, y = mean, colour = region)) +
#   geom_line(aes(x = year, y = mean, colour = region)) +
#   #geom_ribbon(aes(x = year, ymin = mean-sd, ymax = mean+sd, colour = region, fill = region), alpha = 0.2) +
#   facet_wrap(~metric, scales = "free")


testing_all_clusters = combined_cluster_data |>
  filter(tau == 0.95,
         type == "mda8",
         country != "United States of America") |>
  select(station_id, year, fit, pvStr, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`,`6MMDA1`, AVGMDA8, cluster) |>
  pivot_longer(cols = c(fit, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`,`6MMDA1`, AVGMDA8), names_to = "metric", values_to = "value") |>
  select(-c(station_id, pvStr)) |>
  group_by(year, metric, cluster) |>
  summarise_all(c("mean", "sd"), na.rm = T) |>
  filter(cluster != 99)

ggplot(testing_all_clusters)+
  geom_point(aes(x = year, y = mean, colour = as.factor(cluster))) +
  geom_line(aes(x = year, y = mean, colour = as.factor(cluster))) +
 # geom_ribbon(aes(x = year, ymin = mean-sd, ymax = mean+sd, colour = as.factor(cluster), fill = as.factor(cluster)), alpha = 0.1) +
  facet_wrap(~metric, scales = "free") +
  theme_pubr() +
  theme(legend.position = "right")

testing_all_clusters = combined_cluster_data |>
  filter(tau == 0.95,
         type == "mda8",
         country == "United States of America") |>
  select(station_id, year, fit, pvStr, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`,`6MMDA1`, AVGMDA8, cluster) |>
  pivot_longer(cols = c(fit, mda8, `4MDA8`, NDGT70, SOMO35, `3MMDA1`,`6MMDA1`, AVGMDA8), names_to = "metric", values_to = "value") |>
  select(-c(station_id, pvStr)) |>
  group_by(year, metric, cluster) |>
  summarise_all(c("mean", "sd"), na.rm = T) |>
  filter(cluster != 99)
  #filter(cluster == 2) |>
 # filter(year == 2020)

ggplot(testing_all_clusters)+
  geom_point(aes(x = year, y = mean, colour = as.factor(cluster))) +
  geom_line(aes(x = year, y = mean, colour = as.factor(cluster))) +
  #geom_ribbon(aes(x = year, ymin = mean-sd, ymax = mean+sd, colour = as.factor(cluster), fill = as.factor(cluster)), alpha = 0.2) +
  facet_wrap(~metric, scales = "free") +
  theme_pubr() +
  theme(legend.position = "right")

