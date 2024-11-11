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
  select(station_id, country, GPW4_2000, GPW4_2020) |>
  distinct() |>
  collect()

dat = tbl(con, "qr_regressions") |>
  left_join(pqr_2_r2, c("name", "station_id", "scenario_idx")) |>
  filter(!is.na(r2)) |>
  collect() |>
  pivot_wider(names_from = type) |>
  filter(stat == "slope") |>
  group_by(station_id, name, tau) |>
  filter(abs(fit) == max(fit)) |>
  ungroup() |>
  left_join(meta, "station_id")

dat |>
  mutate(country = ifelse(country != "United States of America" , "Europe", country)) |>
  filter(tau == 0.5,
         fit < 0.2) |>
  ggplot()+
  geom_point(aes(GPW4_2020/GPW4_2000, fit, group = station_id))+
  facet_grid(country~name, scales = "free")

plotly::ggplotly()

DBI::dbDisconnect(con, shutdown = T)
