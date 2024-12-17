library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()


min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup()

slopes = inner_join(
  tbl(con, "qr_regressions"),
  min_aic,
  by = c("scenario_idx", "name", "station_id")
) |>
  filter(
    stat == "slope"
  ) |>
  rename(spc = name) |>
  anti_join(tbl(con, "remove_sites"),
            by = c("spc", "station_id")
  ) |>
  collect() |>
  pivot_wider(names_from = "type") |>
  select(-stat)

segments = slopes |>
  select(station_id, spc, reg, tau) |>
  distinct() |>
  mutate(segments = tribble(
    ~seg, ~segStart, ~segEnd,
    1,  2000, 2006,
    2,  2007, 2013,
    3,  2014, 2019,
    4,  2020, 2023,
    11, 2000, 2004,
    12, 2005, 2009,
    13, 2010, 2014,
    14, 2015, 2021
  ) |>
    purrr::pmap_df(~tibble(year = ..2:..3, seg = ..1)) |>
    list()) |>
  unnest(segments)

slopes_year = left_join(segments, slopes,
                        by = c("year" = "startYear",
                               "spc",
                               "station_id",
                               "reg",
                               "tau"),
                        relationship = "many-to-many") |>
  fill(everything()) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, country, latitude, longitude) |>
              distinct() |>
              collect(),
            by = "station_id")

inc_no2 = slopes_year |>
  filter(
    year %in% 2019:2020,
    country != "United States of America",
    seg %in% 11:14,
    #pv < 0.05,
    spc == "no2",
    tau == 0.5
  ) |>
  select(station_id, spc, tau, fit, year, pv) |>
  pivot_wider(values_from = c(fit,pv),
              names_from = year) |>
  filter(fit_2020 > 0 & fit_2019 < 0)

# trend ranges
inc_no2 |>
  select(-contains("pv")) |>
  pivot_longer(contains("fit")) |>
  mutate(value = value*365/1.96) |>
  group_by(spc, tau, name) |>
  summarise(mn = round(min(value),2),
            mx = round(max(value),2)
  )

datList = list()

for(stn in inc_no2$station_id){

  datList[[stn]] = tbl(con,"reg_anom") |>
    filter(station_id == !!stn) |>
    left_join(min_aic, c("scenario_idx", "name", "station_id", "reg")) |>
    collect() |>
    mutate(scenario_idx = ifelse(reg == "loess", 0, aic),
           piece = ifelse(reg == "loess", "1", piece)) |>
    mutate(aic = ifelse(reg %in% c("qr","loess"), 0, aic)) |>
    filter(!is.na(aic)) |>
    select(-aic, -r2, -npiece, -scenario_idx) |>
    pivot_wider(names_from = "reg") |>
    arrange(date)

}

inc_no2_pre2020 = slopes_year |>
  filter(
    year %in% 2019:2020,
    country != "United States of America",
    seg %in% 11:14,
    #pv < 0.05,
    spc == "no2",
    tau == 0.5
  ) |>
  select(station_id, spc, tau, fit, year, pv) |>
  pivot_wider(values_from = c(fit,pv),
              names_from = year) |>
  filter(fit_2020 > 0 & fit_2019 >= 0)

datListpre2020 = list()

for(stn in inc_no2_pre2020$station_id){

  datListpre2020[[stn]] = tbl(con,"reg_anom") |>
    filter(station_id == !!stn) |>
    left_join(min_aic, c("scenario_idx", "name", "station_id", "reg")) |>
    collect() |>
    mutate(scenario_idx = ifelse(reg == "loess", 0, aic),
           piece = ifelse(reg == "loess", "1", piece)) |>
    mutate(aic = ifelse(reg %in% c("qr","loess"), 0, aic)) |>
    filter(!is.na(aic)) |>
    select(-aic, -r2, -npiece, -scenario_idx) |>
    pivot_wider(names_from = "reg") |>
    arrange(date)

}

dbDisconnect(con, shutdown = T)

dat = bind_rows(datList)

datPre2020 = bind_rows(datListpre2020)

# -------------------------------------------------------------------------

dat |>
  filter(name != "ox") |>
  mutate(date = lubridate::floor_date(date, "1 month")) |>
  group_by(date, station_id, name, piece) |>
  summarise_all(median, na.rm = T) |>
  ggplot()+
  geom_line(aes(date, anom/1.96, colour = name))+
  geom_line(aes(date, pqr_2/1.96, colour = name, group = interaction(name,piece)), linewidth = 2)+
  facet_wrap(~station_id, scale = "free_y")+
  theme_minimal()

datPre2020 |>
  filter(name != "ox") |>
  mutate(date = lubridate::floor_date(date, "1 month")) |>
  group_by(date, station_id, name, piece) |>
  summarise_all(median, na.rm = T) |>
  ggplot()+
  geom_line(aes(date, anom/1.96, colour = name))+
  geom_line(aes(date, pqr_2/1.96, colour = name, group = interaction(name,piece)), linewidth = 2)+
  facet_wrap(~station_id, scale = "free_y")+
  theme_minimal()



# O3 CP? force CP to 2020? ------------------------------------------------

cp2020 = tbl(con, "aic") |>
  left_join(
    tbl(con, "regression_scenarios"),
    by = c("name", "station_id", "scenario_idx")
  ) |>
  filter(station_id %in% !!inc_no2$station_id,
         year(cp2) == 2020) |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T))


datListCp2020 = list()
for(stn in inc_no2$station_id){

  datListCp2020[[stn]] = tbl(con,"reg_anom") |>
    filter(station_id == !!stn) |>
    left_join(cp2020, c("scenario_idx", "name", "station_id", "reg")) |>
    collect() |>
    filter(reg == "pqr_2") |>
    # mutate(scenario_idx = ifelse(reg == "loess", 0, aic),
    #        piece = ifelse(reg == "loess", "1", piece)) |>
    # mutate(aic = ifelse(reg %in% c("qr","loess"), 0, aic)) |>
    filter(!is.na(aic)) |>
    pivot_wider(names_from = "reg") |>
    arrange(date)

}

datCp2020 = bind_rows(datListCp2020)

scens = datCp2020 |>
  select(station_id, name, scenario_idx) |>
  distinct() |>
  mutate(id = paste(station_id, name, scenario_idx, sep = "."))

o3_insig_2020 = tbl(con, "qr_regressions") |>
  mutate(
    id = paste(station_id, name, scenario_idx, sep = ".")) |>
  filter(
    id %in% !!scens$id,
    tau == 0.5,
    stat == "slope",
    startYear == 2020) |>
  left_join(
    tbl(con, "regression_scenarios"),
    by = c("name", "station_id", "scenario_idx")
    ) |>
 # select(-startYear, -endYear, -id) |>
  pivot_wider(names_from = type) |>
  filter(pv > 0.33,
         name == "o3") |>
  collect()


o3_no2_sites = datCp2020 |>
  select(name, station_id) |>
  distinct() |>
  pivot_wider(values_from = "name") |> # what a gross use of pivot
  filter(!is.na(ox))


plotDat = dat |>
  bind_rows(
    datCp2020 |>
      filter(name %in% c("o3", "ox")) |>
      mutate(name = case_when(name == "o3" ~ "o3_cp2020",
                              name == "ox" ~ "ox_cp2020",
                              TRUE ~ name))
  ) |>
  # filter(name != "ox") |>
  select(date, station_id, name, anom, pqr_2, piece) |>
  mutate(date = lubridate::floor_date(date, "1 month")) |>
  group_by(date, station_id, name, piece) |>
  summarise_all(median, na.rm = T) |>
  ungroup() |>
  filter(station_id %in% o3_no2_sites$station_id,
         !station_id %in% o3_insig_2020$station_id,
         station_id != "gr0031a")


plotDat |>
  filter(name %in% c("no2", "o3_cp2020", "ox_cp2020")) |>
  ggplot()+
  geom_line(aes(date, anom/1.96, colour = name))+
  geom_line(aes(date, pqr_2/1.96, colour = name, group = interaction(name,piece)), linewidth = 2)+
  facet_wrap(~station_id, scale = "free_y")+
  theme_minimal()



# -------------------------------------------------------------------------

tbl(con, "eeaMeta") |>
  filter(site %in% !!unique(plotDat$station_id)) |>
  collect()



