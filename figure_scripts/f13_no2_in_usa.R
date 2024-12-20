library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
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
    year >= 2015,
    country == "United States of America",
    seg %in% 11:14,
    tau == 0.5,
  ) |>
  select(-scenario_idx, -endYear, -aic, -r2, -se) |>
  pivot_wider(values_from = c(fit, pv),
              names_from = spc) |>
  filter(pv_no2 < 0.33,
         fit_no2 > 0,
         !is.na(fit_no2)) # |> pull(year) |> max()
  select(-year) |>
  distinct()

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

dat = bind_rows(datList)

plotDat = dat |>
  select(date, station_id, name, anom, pqr_2, piece) |>
  mutate(date = lubridate::floor_date(date, "1 month")) |>
  group_by(date, station_id, name, piece) |>
  summarise_all(median, na.rm = T) |>
  ungroup() |>
  mutate(name = case_when(name == "o3" ~ "O<sub>3</sub>",
                          name == "ox" ~ "O<sub>x</sub>",
                          name == "no2" ~ "NO<sub>2</sub>") |>
           factor(levels = c("O<sub>3</sub>","NO<sub>2</sub>","O<sub>x</sub>"))
  )


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



pdf("figures/f14_no2_in_usa.pdf", width = 11.7, height = 8.3)
print(g1)
dev.off()
