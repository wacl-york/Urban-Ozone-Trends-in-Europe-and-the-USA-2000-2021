library(sf)
library(scico)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

meta = tbl(con, "combinedMeta") |>
  select(station_id, latitude, longitude)

reg_anom_filt = tbl(con, "pqr_r2_max") |>
  select(station_id, name, scenario_idx, reg) |>
  distinct()

dat = tbl(con, "qr_regressions") |>
  left_join(reg_anom_filt, c("station_id", "name", "scenario_idx")) |>
  filter(!is.na(reg)) |>
  # select(-reg) |>
  left_join(meta, "station_id") |>
  pivot_wider(names_from = "type") |>
  filter(stat == "slope") |>
  group_by(tau, name, station_id, reg) |>
  filter(fit, max(fit, na.rm = T)) |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(8857) |>
  rename(spc = name)

world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

qDat = dat |>
  st_drop_geometry() |>
  group_by(spc, tau) |>
  summarise(qLow = quantile(fit, probs = 0.02),
            qHigh = quantile(fit, probs = 0.98))

dat |>
  ggplot()+
  geom_density(aes(fit, fill = spc))+
  geom_vline(data = pivot_longer(qDat, contains("q")), aes(xintercept = value, colour = as.factor(tau)))+
  facet_wrap(~spc)

lim = tibble(lng = c(-121,45), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(8857)

dat |>
  filter(tau == 0.5) |>
  left_join(qDat, c("spc", "tau")) |>
#  filter(between(fit, qLow, qHigh)) |>
  mutate(fit = fit*365) |>
  ggplot()+
  geom_sf(data = world, fill = "white", colour = "black")+
  geom_sf(aes(colour = fit))+
  scale_color_scico(palette = "vik")+
  scale_y_continuous(limits = st_coordinates(lim)[,2])+
  scale_x_continuous(limits = st_coordinates(lim)[,1])+
  facet_wrap(~spc, ncol = 2, nrow = 2)


dat |>
  filter(tau == 0.5) |>
  left_join(qDat, c("spc", "tau")) |>
  filter(between(fit, qLow, qHigh)) |>
  mutate(fit = fit*365) |>
  arrange(desc(abs(fit)))

# -------------------------------------------------------------------------


DBI::dbDisconnect(con, shutdown = T)

