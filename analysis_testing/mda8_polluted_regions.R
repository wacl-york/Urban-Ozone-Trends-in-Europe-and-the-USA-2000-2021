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
#library(rnaturalearthhires)
library(ggpubr)
library(stringr)
library(here)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))

con = connect_to_db()

######

mda8 = tbl(con, "dat_mda8") |>
  select(date, station_id, timezone, mda8) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude, country) |>
              distinct(),
            by = "station_id") |>
  collect()  |>
  mutate(year = year(date),
         month(date) %in% c(4,5,6,7,8,9)) |>
  select(year, station_id, mda8, country, timezone) |>
  group_by(year, station_id, country, timezone) |>
  summarise_all("mean", na.rm = T) |>
  mutate(region = case_when(country %in% c("austria", "belgium", "switzerland", "czechia", "germany", "denmark",
                                           "estonia", "finland", "france", "united_kingdom", "ireland", "iceland",
                                           "netherlands", "norway", "poland", "sweden", "slovakia") ~ "Northern and Central Europe",
                            country %in% c("bulgaria", "greece", "italy", "portugal", "spain", "slovenia") ~ "Southern Europe",
                            timezone %in% c("America/Chicago", "America/New_York") ~ "Eastern and Central US",
                            timezone %in% c("America/Los_Angeles", "America/Phoenix", "America/Denver", "Pacific/Honolulu") ~ "Western US and Hawaii",
                            .default = NA)) |>
  rename(value = mda8)


metrics = tbl(con, "dat_metrics") |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude, country) |>
              distinct(),
            by = "station_id") |>
  collect() |>
  mutate(US_EU = case_when(country == "United States of America" ~ "United States of America",
                           .default = "Europe")) |>
  mutate(region = case_when(country %in% c("austria", "belgium", "switzerland", "czechia", "germany", "denmark",
                                           "estonia", "finland", "france", "united_kingdom", "ireland", "iceland",
                                           "netherlands", "norway", "poland", "sweden", "slovakia") ~ "Northern and Central Europe",
                            country %in% c("bulgaria", "greece", "italy", "portugal", "spain", "slovenia") ~ "Southern Europe",
                            timezone %in% c("America/Chicago", "America/New_York") ~ "Eastern and Central US",
                            timezone %in% c("America/Los_Angeles", "America/Phoenix", "America/Denver", "Pacific/Honolulu") ~ "Western US and Hawaii",
                            .default = NA))

ggplot(mda8, aes(x = year, y = value)) +
  geom_point() +
  facet_wrap(~region) +
  gghighlight::gghighlight(value >= 50)

station_ids_mean_mda8 = mda8 |>
  filter(value >= 50) |>
  left_join(cluster_timeseries, by = "station_id") |>
  na.omit(cluster_region)

ggplot(station_ids_mean_mda8, aes(x=cluster_region)) +
  geom_histogram(stat = "count") +
  facet_wrap(~year)

combined_meta = tbl(con, "combinedMeta") |>
  select(station_id, timezone, country) |>
  distinct()

tables = "piecewise_stats_freeTau_mda8_anom_warm"
i = 1

pv_opt = c("p <= 0.05 (dec)",
           "0.05 < p <= 0.1 (dec)",
           "0.1  < p <= 0.33 (dec)",
           "p > 0.33",
           "0.1  < p <= 0.33 (inc)",
           "0.05 < p <= 0.1 (inc)",
           "p <= 0.05 (inc)"
)

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

test = mda8_all |>
  left_join(station_id_)
