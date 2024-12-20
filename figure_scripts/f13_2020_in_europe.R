library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggplot2)

source(here::here('functions','connect_to_db.R'))
source(here::here('functions','pad_time_series.R'))

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

# sites where NO2 switched to increasing between 2019 and 2020

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

# get best scenario for O3 and Ox where cp2 changes in 2020 also
# at sites identified by inc_no2
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

dat = bind_rows(datList)

# sites where the 2020 slope has p > 0.33 - for removal

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

dbDisconnect(con, shutdown = T)

# sites that have both o3 and no2
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
  mutate(date = lubridate::floor_date(date, "1 month"),
         anom = anom/1.96,
         pqr_2 = pqr_2/1.96) |> # ug m-3 -> ppb
  group_by(date, station_id, name, piece) |>
  summarise_all(median, na.rm = T) |>
  ungroup() |>
  filter(station_id %in% o3_no2_sites$station_id,
         !station_id %in% o3_insig_2020$station_id,
         station_id != "gr0031a") |>
  filter(name %in% c("no2", "o3_cp2020", "ox_cp2020")) |>
  mutate(name = case_when(name == "o3_cp2020" ~ "O<sub>3</sub>",
                           name == "ox_cp2020" ~ "O<sub>x</sub>",
                           name == "no2" ~ "NO<sub>2</sub>") |>
           factor(levels = c("O<sub>3</sub>","NO<sub>2</sub>","O<sub>x</sub>"))
           ) |>
  nest_by(name, station_id) |>
  mutate(data = data |>
           pad_time_series("1 month") |>
           list()
  ) |>
  unnest(data)

g1 = plotDat |>
  ggplot()+
  geom_line(aes(date, anom, colour = name))+
  geom_line(aes(date, pqr_2, group = interaction(name,piece)), linewidth = 1.2, colour = "black")+
  geom_line(aes(date, pqr_2, colour = name, group = interaction(name,piece)), linewidth = 0.8)+
  scale_y_continuous(name = "Anomaly Mixing Ratio / ppb")+
  scale_x_datetime(name = "Date")+
  scale_colour_manual(values = c("#FF0000", "#F98400", "#00A08A"), name = "")+
  facet_wrap(~station_id, scale = "free_y")+
  theme_minimal()+
  theme(legend.text = element_markdown(),
        legend.position = "bottom")

pdf("figures/f13_2020_in_europe.pdf", width = 11.7, height = 8.3)
print(g1)
dev.off()
