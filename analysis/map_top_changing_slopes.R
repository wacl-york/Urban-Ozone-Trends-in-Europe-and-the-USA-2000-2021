#library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
#library(rnaturalearth)
#library(rnaturalearthhires)
library(patchwork)
library(ggpubr)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

# Find minimum AIC scenarios
min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |>
  ungroup()
  #left_join(tbl(con, "reg_anom"), join_by(scenario_idx, reg, name, station_id))

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
    B < 0 & C > 0 ~ "negative_to_positive")) |>
  mutate(A_to_B_year = ifelse(length(startYears[[2]]) > 0, startYears[[2]], NA),
         B_to_C_year = ifelse(length(startYears[[3]]) > 0, startYears[[3]], NA))

qr_piece$country[qr_piece$country != "United States of America"] = "Europe"

#saveRDS(qr_piece, "~/scratch/TOAR/qr_piece_dataframe.RDS")

col = c(
  "#E4ADD6",
  "#B5549C",
  "#65014B",
  "black",
  "#0C4C00",
  "#5F903D",
  "#C0D9A1"
)

# qr_piece |>
#   filter(name %in% c("o3", "no2")) |>
#   ggplot(aes(x = B_to_C))+
#   geom_density_ridges(aes(fill = factor(tau), y = factor(tau))) +
#   facet_grid(~name~country, scales = "free_y") +
#   scale_fill_manual(values = col) +
#   theme_minimal()

# Create sf object
qr_piece_sf = qr_piece |>
  st_as_sf(coords = c(lon = "longitude", lat = "latitude"), crs = 4326)

# Define limits for map plotting US
limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

# Define limits for map plotting EU
limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            na.omit() |>
            arrange(desc(A_to_B)) |>
            filter(country == "United States of America") |>
            group_by(A_to_B_dir_flip) |>
            top_frac(0.25, A_to_B), aes(colour = A_to_B_dir_flip, size = A_to_B), alpha = 0.2) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~A_to_B_dir_flip) |>


ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = qr_piece_sf |>
            na.omit() |>
            arrange(desc(B_to_C)) |>
            filter(country == "United States of America") |>
            group_by(B_to_C_dir_flip) |>
            top_frac(0.25, B_to_C), aes(colour = B_to_C_dir_flip, size = B_to_C), alpha = 0.2) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  facet_wrap(~B_to_C_dir_flip)

x = qr_piece |>
  mutate(A_to_B_year = ifelse(length(startYears[[2]]) > 0, startYears[[2]], NA))
