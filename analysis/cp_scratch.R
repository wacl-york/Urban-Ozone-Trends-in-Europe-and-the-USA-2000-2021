library(scico)
library(dplyr)
library(tidyr)
library(ggplot2)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

temp = tbl(con, "qr_regressions") |>
  filter(stat == "slope",
         tau == 0.5,
         type == "fit") |>
  group_by(stat, tau, type, station_id, name, scenario_idx) |>
  mutate(id = row_number(),
         piece = case_when(id == 1 ~ "a",
                           id == 2 ~ "b",
                           id == 3 ~ "c"),
         value = value * 365
         ) |>
  select(-id, -startYear, -endYear) |>
  pivot_wider(names_from = "piece") |>
  mutate(b_a = b-a,
         c_b = c-b) |>
  inner_join(tbl(con, "pqr_r2_max"), by = join_by(station_id, name, scenario_idx)) |>
  pivot_longer(c(b_a, c_b), names_to = "diff") |>
  collect()


temp |>
  ggplot()+
  geom_histogram(aes(value, fill = diff), position = "identity", alpha = 0.5, binwidth = 0.1)+
  geom_vline(data = tibble(int = c(-0.5, 0.5)),
             aes(xintercept = int))


# When do the cps occur ---------------------------------------------------

tbl(con, "qr_regressions") |>
  filter(stat == "slope",
         tau == 0.5,
         type == "fit") |>
  inner_join(tbl(con, "pqr_r2_max"), by = join_by(station_id, name, scenario_idx)) |>
  inner_join(tbl(con, "regression_scenarios"), by = join_by(station_id, name, scenario_idx)) |>
  select(cp1, cp2, name) |>
  pivot_longer(c(cp1, cp2), names_to = "cp") |>
  mutate(y = year(value)) |>
  group_by(y, name, cp) |>
  ggplot()+
  geom_bar(aes(y, fill = cp))+
  facet_wrap(~name)

meta = tbl(con, "combinedMeta") |>
  select(station_id, region = country) |>
  mutate(region = ifelse(region != "United States of America", "Europe", region))



# cps in 2D ---------------------------------------------------------------

test = tbl(con, "reg_anom_r2") |>
  filter(reg != "loess") |>
  left_join(meta, by = join_by(station_id)) |>
  group_by(name, station_id) |>
  mutate(r2 = (r2-min(r2, na.rm = T))/(max(r2, na.rm = T)-min(r2, na.rm = T))) |>
  inner_join(tbl(con, "regression_scenarios"), by = join_by(station_id, name, scenario_idx)) |>
  mutate(across(contains("cp"), \(x) lubridate::year(x))) |>
  ungroup() |>
  collect() |>
  group_by(name, cp1, cp2, region) |>
  summarise(r2 = sum(r2)) |>
  group_by(name, region) |>
  mutate(r2 = (r2-min(r2, na.rm = T))/(max(r2, na.rm = T)-min(r2, na.rm = T)))

test |>
  ggplot()+
  geom_raster(aes(cp1, cp2, fill = r2))+
  scico::scale_fill_scico(palette = "batlow")+
  facet_grid(region~name)


# R2 diff in 2D -----------------------------------------------------------

pqr_2 = tbl(con, "reg_anom_r2") |>
  filter(reg == "pqr_2") |>
  pivot_wider(names_from = "reg", values_from = "r2")

qr = tbl(con, "reg_anom_r2") |>
  filter(reg == "qr") |>
  pivot_wider(names_from = "reg", values_from = "r2") |>
  select(-scenario_idx)

left_join(pqr_2, qr, by = join_by(station_id, name)) |>
  left_join(meta, by = join_by(station_id)) |>
  left_join(tbl(con, "regression_scenarios"), by = join_by(station_id, name, scenario_idx)) |>
  collect() |>
  mutate(across(contains("cp"), \(x) lubridate::year(x)),
         r2 = ((pqr_2-qr)/qr)*100) |>
  group_by(station_id, name, region,  cp1, cp2) |>
  summarise(r2 = sum(r2, na.rm = T)) |>
  group_by(name, region) |>
  mutate(r2 = scales::rescale(r2)) |>
  ungroup() |>
  filter(between(r2,0.1, 1)) |>
  ggplot()+
  geom_raster(aes(cp1, cp2, fill = r2))+
  scico::scale_fill_scico(palette = "batlow")+
  facet_grid(region~name)

tbl(con, "reg_anom") |>
  filter(station_id == "15009",
         reg == "qr") |>
  ggplot()+
  geom_line(aes(date, anom))+
  geom_line(aes(date, value, colour = reg))+
  facet_wrap(~name)

DBI::dbDisconnect(con, shutdown = T)
