library(DBI)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(sf)
library(wesanderson)

source(here::here('functions','utils.R'))

con = connect_to_db()

########### World data #############

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)


limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)


########### Describe scenario types ###########

scenarioTypes = tbl(con, "regression_scenarios") |>
  select(cp1, cp2, scenario_idx) |>
  distinct() |>
  collect() |>
  arrange(scenario_idx) |>
  mutate(scenarioType = case_when(
    (!is.na(cp1) & !is.na(cp2)) ~ "PQR_2",
    (!is.na(cp1) & is.na(cp2)) ~ "PQR_1",
    (is.na(cp1) & is.na(cp2)) ~ "QR"
  ))

# Collect meta data #

combined_meta = tbl(con, "combinedMeta") |>
  collect() |>
  select(station_id, country, lng = longitude, lat = latitude) |>
  distinct()

# toar_meta = tbl(con, "toarMeta") |>
#   mutate(station_id = as.character(station_id)) |>
#   select(station_id, country, lng, lat) |>
#   filter(country == "United States of America") |>
#   collect()
#
# eea_meta = tbl(con, "eeaMeta") |>
#   select(station_id = site, country, lng = longitude, lat = latitude) |>
#   collect()
#
# combined_meta = bind_rows(toar_meta, eea_meta) |>
#   distinct()

###########################

################################################

piece_dat = tbl(con, "piecewise_data_freeTau_mda8_anom_all") |>
  filter(name == "o3", type == "fit") |>
  collect()
#group_by(station_id, scenario_idx, tau, startYear, endYear)

###############################################

reg_dat = tbl(con, "reg_all_mda8_anom_all") |>
  filter(stat ==  "slope") |>
  collect() |>
  left_join(scenarioTypes, by = "scenario_idx") |>
  ungroup() |>
  arrange(startYear) |>
  select(tau, slope = value, startYear, endYear, scenario_idx, station_id, cp1, cp2, scenarioType) |>
  group_by(tau, station_id, scenario_idx, cp1, cp2, scenarioType) |>
  mutate(piece = row_number()) |>
  distinct() |>
  ungroup()


################################################

piece_stats  = tbl(con, "piecewise_stats_freeTau_mda8_anom_all") |>
  filter(stat == "slope",
         name == "o3",
         type == "pv") |>
  select(-stat, -name, -type, -aic) |>
  rename(pv = value) |>
  collect()

################################################

combined_dat <- piece_dat |>
  left_join(piece_stats, by = c("tau", "startYear", "endYear", "scenario_idx", "station_id")) |>
  left_join(
    reg_dat |> select(-slope),  # drop slope from reg_dat to avoid conflict
    by = c("tau", "startYear", "endYear", "scenario_idx", "station_id")
  ) |>
  mutate(slope = slope * 365)


### Summary tables ###

number_of_reg_type_US_EU = combined_dat |>
  select(tau, scenario_idx, station_id, scenarioType, timezone) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(US_EU = case_when(country == "United States of America" ~ "United States of America",
                           .default = "Europe")) |>
  #filter(station_id %in% c("1138", "1149")) |>
  select(tau, scenarioType, US_EU) |>
  group_by(tau, scenarioType, US_EU) |>
  add_count(scenarioType, name = "count") |>
  distinct()

number_of_reg_type_region = combined_dat |>
  select(tau, scenario_idx, station_id, scenarioType, timezone) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(US_EU = case_when(country == "United States of America" ~ "United States of America",
                           .default = "Europe")) |>
  mutate(region = case_when(country %in% c("austria", "belgium", "switzerland", "czechia", "germany", "denmark",
                                           "estonia", "finland", "france", "united_kingdom", "ireland", "iceland",
                                           "netherlands", "norway", "poland", "sweden", "slovakia") ~ "Northern and Central Europe",
                            country %in% c("bulgaria", "greece", "italy", "portugal", "spain", "slovenia") ~ "Southern Europe",
                            timezone %in% c("America/Chicago", "America/New_York") ~ "Eastern and Central US",
                            timezone %in% c("America/Los_Angeles", "America/Phoenix", "America/Denver", "Pacific/Honolulu") ~ "Western US and Hawaii",
                            .default = NA)) |>
  #filter(station_id %in% c("1138", "1149")) |>
  select(tau, scenarioType, US_EU, region) |>
  group_by(tau, scenarioType, US_EU, region) |>
  add_count(scenarioType, name = "count") |>
  distinct() |>
  ungroup() |>
  complete(tau, scenarioType, US_EU, region, fill = list(count = 0)) |>
  filter(US_EU == "United States of America" & region %in% c("Eastern and Central US", "Western US and Hawaii") |
           US_EU == "Europe" & region %in% c("Northern and Central Europe", "Southern Europe"))

number_of_reg_type_timezone = combined_dat |>
  select(tau, scenario_idx, station_id, scenarioType, timezone) |>
  left_join(combined_meta, by = "station_id") |>
  distinct() |>
  mutate(US_EU = case_when(country == "United States of America" ~ "United States of America",
                           .default = "Europe")) |>
  #filter(station_id %in% c("1138", "1149")) |>
  select(tau, scenarioType, US_EU, timezone) |>
  group_by(tau, scenarioType, US_EU, timezone) |>
  add_count(scenarioType, name = "count") |>
  distinct()

#############################

ggplot(number_of_reg_type_region, aes(x = factor(tau), y = count, fill = scenarioType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "Tau",
    y = "Count",
    fill = "Scenario Type",
    #title = "Count of Scenario Types by Tau"
  ) +
  scale_fill_manual(values = wes_palette("Darjeeling1")) +
  facet_wrap(~ US_EU + region, scales = "free") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom")
dev.off()

number_of_reg_type_US_EU |>
  filter(US_EU == "Europe",
         tau == 0.5
  )

###### slopes changing direction ######

slope_changes = combined_dat |>
  filter(scenarioType %in% c("PQR_1", "PQR_2")) |>
  select(station_id, tau, startYear, endYear, scenarioType, slope, pv, piece) |>
  distinct() |>
  group_by(station_id, tau, scenarioType, startYear, endYear) |>
  pivot_wider(names_from = "piece", values_from = c("slope", "pv", "startYear", "endYear"))


slope_changes_pqr1 = slope_changes |>
  filter(scenarioType == "PQR_1") |>
  select(-c(slope_3, pv_3, startYear_3, endYear_3)) |>
  #filter(pv_1 < 0.33 & pv_2 < 0.33) |>
  mutate(slope_1_direction = case_when(slope_1 < 0 ~ "negative",
                                       .default = "positive"),
         slope_2_direction = case_when(slope_2 < 0 ~ "negative",
                                       .default = "positive"),
         switch = case_when(slope_1_direction == "negative" &
                              slope_2_direction == "positive" ~ "neg_to_pos",
                            slope_1_direction == "positive" &
                              slope_2_direction == "negative" ~ "pos_to_neg",
                            .default = "no_change"),
         abs_diff = max(slope_1, slope_2)-min(slope_1, slope_2))

slope_changes_pqr1_sf = slope_changes_pqr1 |>
  left_join(combined_meta, by = "station_id") |>
  st_as_sf(coords = c(lon = "lng", lat = "lat"), crs = 4326) |>
  filter(pv_1 < 0.33 & pv_2 < 0.33) |>
  filter(tau %in% c(0.05, 0.5, 0.95))


ggplot() +
  geom_sf(data = world)+
  geom_sf(data = slope_changes_pqr1_sf,
          aes(colour = startYear_2,
              fill = startYear_2,
              size = abs_diff),
          alpha = 0.5)+
  scale_size("ppb / yr")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  # scale_fill_scico(palette = "berlin")
  # facet_nested(flip_dir+transformation~spc)+
  #scale_colour_viridis_c(limits = c(2000, 2023))+
  theme_minimal() +
  # facet_wrap(~switch)+
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(colour = "CP year", fill = "CP year") +
  facet_wrap(~tau+switch)

########################

slope_changes_pqr2_p1 = slope_changes |>
  filter(scenarioType == "PQR_2") |>
  select(-c(slope_3, pv_3, startYear_3, endYear_3)) |>
  #filter(pv_1 < 0.33 & pv_2 < 0.33) |>
  mutate(slope_1_direction = case_when(slope_1 < 0 ~ "negative",
                                       .default = "positive"),
         slope_2_direction = case_when(slope_2 < 0 ~ "negative",
                                       .default = "positive"),
         switch = case_when(slope_1_direction == "negative" &
                              slope_2_direction == "positive" ~ "neg_to_pos",
                            slope_1_direction == "positive" &
                              slope_2_direction == "negative" ~ "pos_to_neg",
                            .default = "no_change"),
         abs_diff = max(slope_1, slope_2)-min(slope_1, slope_2)) |>
  ungroup() |>
  select(station_id, tau, first_slope = slope_1, second_slope = slope_2, first_pv = pv_1, second_pv = pv_2,
         change_point_year = startYear_2, switch, abs_diff) |>
  mutate(cp_number = "first CP")


slope_changes_pqr2_p2 = slope_changes |>
  filter(scenarioType == "PQR_2") |>
  select(-c(slope_1, pv_1, startYear_1, endYear_1)) |>
  #filter(pv_1 < 0.33 & pv_2 < 0.33) |>
  mutate(slope_2_direction = case_when(slope_2 < 0 ~ "negative",
                                       .default = "positive"),
         slope_3_direction = case_when(slope_3 < 0 ~ "negative",
                                       .default = "positive"),
         switch = case_when(slope_2_direction == "negative" &
                              slope_3_direction == "positive" ~ "neg_to_pos",
                            slope_2_direction == "positive" &
                              slope_3_direction == "negative" ~ "pos_to_neg",
                            .default = "no_change"),
         abs_diff = max(slope_2, slope_3)-min(slope_2, slope_3)) |>
  ungroup() |>
  select(station_id, tau, first_slope = slope_2, second_slope = slope_3, first_pv = pv_2, second_pv = pv_3,
         change_point_year = startYear_3, switch, abs_diff) |>
  mutate(cp_number = "second CP")

combined_pqr2 = bind_rows(slope_changes_pqr2_p1, slope_changes_pqr2_p2)

combined_pqr2_sf = combined_pqr2 |>
  left_join(combined_meta, by = "station_id") |>
  st_as_sf(coords = c(lon = "lng", lat = "lat"), crs = 4326) |>
  filter(first_pv < 0.33 & second_pv < 0.33) |>
  filter(tau %in% c(0.05, 0.5, 0.95),
         cp_number == "second CP")

ggplot() +
  geom_sf(data = world)+
  geom_sf(data = combined_pqr2_sf,
          aes(colour = change_point_year,
              fill = change_point_year,
              size = abs_diff),
          alpha = 0.5)+
  scale_size("ppb / yr")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  # scale_fill_scico(palette = "berlin")
  # facet_nested(flip_dir+transformation~spc)+
  #scale_colour_viridis_c(limits = c(2000, 2023))+
  theme_minimal() +
  facet_wrap(~switch)+
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  labs(colour = "CP year", fill = "CP year") +
  facet_wrap(~switch+tau, ncol = 3)
