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

mda8_all = tbl(con, "piecewise_stats_freeTau_mda8_anom_all") |>
  filter(stat == "slope",
         type %in% c("fit","pv"),
         name == "o3") |>
  select(station_id, startYear, endYear, tau, type, value) |>
  left_join(combined_meta, by = "station_id") |>
  collect() |>
  pivot_wider(names_from = type, values_from = value) |>
  mutate(fit = fit*365) |>
  filter(between(fit, quantile(fit, 0.01), quantile(fit, 0.99))) |>
  filter(tau %in% c(0.05, 0.5, 0.95)) |>
  group_by(station_id, tau, country, timezone) |>
  mutate(first_last = case_when(
    endYear == max(endYear, na.rm = TRUE) ~ "last",
    startYear == min(startYear, na.rm = TRUE) ~ "first",
    .default = NA
  )) |>
  ungroup() |>
  filter(first_last %in% c("first", "last")) |>
  mutate(uncertainty = case_when(
                                 pv <= 0.01 ~ "very_high_certainty",
                                 0.05 >= pv & pv > 0.01 ~ "high_certainty",
                                 0.10 >= pv & pv > 0.05 ~ "medium_certainty",
                                 0.33 >= pv & pv > 0.10 ~ "low_certainty",
                                 pv > 0.33 ~ "very_low_certainty",
                                 .default = NA),
         map_region = ifelse(country == "United States of America", "US", "EU"))


############# Count the number of arrows at different uncertainties ################

# Percentage of slopes at each uncertainty/direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. What percentage of high certainty positive trends are there in the region/tau/year group?
# (does not account for magnitude)

mda8_uncertainty_count = mda8_all |>
  filter(fit != 0) |>
  #filter(first_last == "first") |>
  mutate(uncertainty = ifelse(uncertainty == "very_high_certainty", "high_certainty", uncertainty),
         direction = ifelse(fit > 0, "increasing", "decreasing")) |>
  select(map_region, first_last, tau, direction, uncertainty) |>
  group_by(map_region, first_last, tau, direction, uncertainty) |>
  count() |>
  group_by(map_region, first_last, tau) |>
  mutate(percentage = n / sum(n)*100) |>
  pivot_wider(names_from = tau, values_from = c(n, percentage))


######### Count the number of arrows at different slope magnitudes ##########

# mda8_all |>
#   ggplot()+
#   geom_density(aes(x = fit)) +
#   facet_wrap(~map_region+tau)

# Percentage of slopes at each direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. what percentage of high magnitude positive trends are there in the region/tau/year group?
# (does not account for uncertainty)
mda8_slope_count = mda8_all |>
  filter(fit != 0) |>
  #filter(first_last == "first") |>
  mutate(uncertainty = ifelse(uncertainty == "very_high_certainty", "high_certainty", uncertainty),
         direction = ifelse(fit > 0, "increasing", "decreasing")) |>
  select(map_region, first_last, tau, direction) |>
  group_by(map_region, first_last, tau, direction) |>
  count() |>
  group_by(map_region, first_last, tau) |>
  mutate(percentage = n / sum(n)*100) |>
  pivot_wider(names_from = tau, values_from = c(n, percentage))

# Percentage of slopes at each magnitude/direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. what percentage of high magnitude positive trends are there in the region/tau/year group?
# (does not account for uncertainty)

mda8_slope_detailed_count = mda8_all |>
  filter(fit != 0) |>
  #filter(first_last == "first") |>
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
  select(map_region, first_last, tau, direction, slopeBin) |>
  group_by(map_region, first_last, tau, direction, slopeBin) |>
  count() |>
  group_by(map_region, first_last, tau) |>
  mutate(percentage = n / sum(n)*100) |>
  pivot_wider(names_from = tau, values_from = c(n, percentage))

# Percentage of slopes at each magnitude/uncertainty/direction within a given region (US or EU), at a given tau, at both the
# start and end slope #
# e.g. what percentage of high certainty high magnitude positive trends are there in the region/tau/year group?

mda8_slope_uncertainty_count = mda8_all |>
  filter(fit != 0) |>
  #filter(first_last == "first") |>
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
  select(map_region, first_last, tau, direction, uncertainty, slopeBin) |>
  group_by(map_region, first_last, tau, direction, uncertainty, slopeBin) |>
  count() |>
  group_by(map_region, first_last, tau) |>
  mutate(percentage = n / sum(n)*100) |>
  pivot_wider(names_from = tau, values_from = c(n, percentage))

