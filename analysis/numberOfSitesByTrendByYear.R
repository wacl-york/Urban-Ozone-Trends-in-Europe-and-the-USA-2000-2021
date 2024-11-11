library(DBI)
library(zoo)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

pqr_2_r2 = tbl(con, "reg_anom_r2") |>
  group_by(station_id, name) |>
  filter(r2 == max(r2, na.rm = T),
         reg == "pqr_2") |>
  ungroup()

meta = tbl(con, "combinedMeta") |>
  select(station_id, country) |>
  distinct() |>
  collect()

dat = tbl(con, "qr_regressions") |>
  left_join(pqr_2_r2, c("name", "station_id", "scenario_idx")) |>
  filter(!is.na(r2)) |>
  collect()

datYearExpanded = dat |>
  select(name, station_id, stat, tau, type) |>
  distinct() |>
  mutate(year = tibble(year = 2000:2023) |>
           list()) |>
  unnest(year)

statByYear = left_join(datYearExpanded, dat, by = c("name", "station_id","stat", "tau", "type", "year" = "startYear")) |>
  select(-scenario_idx, -endYear, -reg, -r2) |>
  group_by(name, station_id, stat, tau, type) |>
  mutate(value = na.locf(value, na.rm = F)) |>
  ungroup() |>
  filter(!is.na(value)) |>
  distinct() |>
  left_join(meta, "station_id")

plotDat = statByYear |>
  filter(stat == "slope",
         type == "fit") |>
  mutate(dir = ifelse(value >= 0, "inc", "dec")) |>
  group_by(name, tau, type, stat, dir, year, country) |>
  count() |>
  ungroup() |>
  mutate(n = ifelse(dir == "dec", n*-1, n))


g_us = plotDat |>
  filter(country == "United States of America") |>
  ggplot()+
  geom_bar(aes(year, n, fill = dir), stat = "identity")+
  facet_grid(name~tau, scale = "free_y")+
  scale_x_continuous(name = "Year", guide = guide_axis(n.dodge = 2))+
  scale_y_continuous(name = "Number of Sites by trend direction")+
  AQVisR::AQvis_plotTheme()+
  ggtitle("United States of America")

g_eu = plotDat |>
  filter(country != "United States of America") |>
  ggplot()+
  geom_bar(aes(year, n, fill = dir), stat = "identity")+
  facet_grid(name~tau, scale = "free_y")+
  scale_x_continuous(name = "Year", guide = guide_axis(n.dodge = 2))+
  scale_y_continuous(name = "Number of Sites by trend direction")+
  AQVisR::AQvis_plotTheme()+
  ggtitle("Europe")


# threshold ---------------------------------------------------------------

thresh = statByYear |>
  group_by(name, station_id, stat, type, tau) |>
  filter(abs(value) == max(abs(value)),
         type == "fit",
         stat == "slope") |>
  group_by(tau, name) |>
  summarise(q25 = quantile(abs(value), probs = 0.25),
            q90 = quantile(abs(value), probs = 0.90)) |>
  ungroup()



plotDat2 = statByYear |>
  filter(stat == "slope",
         type == "fit") |>
  left_join(thresh, c("name", "tau")) |>
  filter(between(abs(value),q25, q90)) |>
  mutate(dir = ifelse(value >= 0, "inc", "dec")) |>
  group_by(name, tau, type, stat, dir, year, country) |>
  count() |>
  ungroup() |>
  mutate(n = ifelse(dir == "dec", n*-1, n))


plotDat2 |>
  filter(country != "United States of America") |>
  ggplot()+
  geom_bar(aes(year, n, fill = dir), stat = "identity")+
  facet_grid(name~tau, scale = "free_y")+
  scale_x_continuous(name = "Year", guide = guide_axis(n.dodge = 2))+
  scale_y_continuous(name = "Number of Sites by trend direction")+
  AQVisR::AQvis_plotTheme()+
  ggtitle("Europe")

plotDat2 |>
  filter(country == "United States of America") |>
  ggplot()+
  geom_bar(aes(year, n, fill = dir), stat = "identity")+
  facet_grid(name~tau, scale = "free_y")+
  scale_x_continuous(name = "Year", guide = guide_axis(n.dodge = 2))+
  scale_y_continuous(name = "Number of Sites by trend direction")+
  AQVisR::AQvis_plotTheme()+
  ggtitle("United States of America")





