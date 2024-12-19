library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(rnaturalearth)
library(rnaturalearthhires)
library(patchwork)
library(ggpubr)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

blacklist = tbl(con, "remove_sites") |>
  collect()

# Find minimum AIC scenarios
min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |>
  ungroup() |>
  left_join(tbl(con, "reg_anom"), join_by(scenario_idx, reg, name, station_id))

# Filter reg data for all relevant slope information
qr_reg =  tbl(con, "qr_regressions")|>
filter(stat == "slope",
       type == "fit") |>
  select(-stat, -type)

met_data = tbl(con, "combinedMeta") |>
  select(station_id, country, latitude, longitude) |>
  distinct()


# Create data for absolute change in slope (ppb y-1)
qr_piece = min_aic |>
  left_join(qr_reg, by = c("station_id", "scenario_idx", "name")) |>
  anti_join(tbl(con, "remove_sites") |> rename("name" = spc), by = c("station_id", "name")) |>
  left_join(met_data, by = "station_id") |>
  #mutate(value = ifelse(country == "United States of America", value, value/1.96)) |>
  select(-c(npiece, reg, aic, r2)) |>
  group_by(station_id, scenario_idx, name, tau) |>
  collect() |>
  arrange(c(station_id, scenario_idx, name, tau, startYear)) |>
  mutate(n = row_number()) |>
  mutate(startYears = list(c(startYear))) |>
  select(-c(startYear, endYear)) |>
  pivot_wider(names_from = n, values_from = value) |>
  rename("A" = `1`, "B" = `2`, "C" = `3`) |>
  mutate(A_to_B = (B-A)*365, B_to_C = (C-B)*365) |>
  ungroup() |>
  mutate(A_to_B_dir_flip = case_when(
    A > 0 & B < 0 ~ "positive_to_negative",
    A < 0 & B > 0 ~ "negative_to_positive"
  ),
  B_to_C_dir_flip = case_when(
    B > 0 & C < 0 ~ "positive_to_negative",
    B < 0 & C > 0 ~ "negative_to_positive"))
  # mutate(A_to_B_year = ifelse(length(startYears[[2]]) > 0, startYears[[2]], NA),
  #        B_to_C_year = ifelse(length(startYears[[3]]) > 0, startYears[[3]], NA))

qr_piece$country[qr_piece$country != "United States of America"] = "Europe"

#saveRDS(qr_piece, "~/scratch/TOAR/qr_piece_dataframe.RDS")
