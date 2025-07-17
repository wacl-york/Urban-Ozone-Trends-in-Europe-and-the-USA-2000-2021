library(DBI)
library(dplyr)
library(tidyr)
library(here)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# Find minimum AIC scenarios
min_aic = tbl(con, "min_aic_mda8") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |>
  ungroup()

# Filter reg data for all relevant slope information
qr_reg =  tbl(con, "qr_regressions_mda8")|>
  filter(stat == "slope",
         type == "fit") |>
  select(-stat, -type)

met_data = tbl(con, "combinedMeta") |>
  select(station_id, country, latitude, longitude) |>
  distinct()


# Create data for absolute change in slope (ppb y-1)
qr_piece = min_aic |>
  anti_join(tbl(con, "remove_sites") |> rename("name" = spc), by = c("station_id", "name")) |>
  left_join(met_data, by = "station_id") |>
  left_join(qr_reg, by = c("station_id", "scenario_idx", "name")) |>
  mutate(value = ifelse(country == "United States of America", value, value/1.96),
         country = ifelse(country == "United States of America", country, "Europe")
  ) |>
  select(-c(npiece, reg, aic, r2)) |>
  group_by(station_id, scenario_idx, name, tau) |>
  collect() |>
  arrange(startYear) |>
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

dbWriteTable(con, "trend_flip_mda8", qr_piece, overwrite = T)

dbDisconnect(con, shutdown = T)

