library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(scico)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(here)
library(lubridate)
library(plotly)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup() # the differences between the locations of the change points are not substantial
# so just take the lower of the two sceanrio_idx arbitratily

slope_segs = tbl(con, "slope_segs") |>
  inner_join(min_aic, by = c("station_id", "name", "reg")) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude),
            by = "station_id") |>
  filter(type == "fit") |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(8857) |>
  rename(spc = name)

# test = slope_segs |>
#   filter(seg == 1, station_id == "1138", spc == "o3", reg == "pqr_2", scenario_idx == 111)
#
world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(8857)

lim = tibble(lng = c(-121,45), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(8857)

qDat = slope_segs |>
  st_drop_geometry() |>
  group_by(spc, tau) |>
  summarise(qLow = quantile(value, probs = 0.02),
            qHigh = quantile(value, probs = 0.98))

slope_segs |>
  filter(
    tau == 0.5,
    spc == "no2",
    seg %in% 1:4) |>
  left_join(qDat, c("spc", "tau")) |>
  filter(!(between(value, qLow, qHigh))) |>
  ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(aes(colour = value*365, group = station_id))+
  scale_color_scico(palette = "vik")+
  scale_y_continuous(limits = st_coordinates(lim)[,2])+
  scale_x_continuous(limits = st_coordinates(lim)[,1])+
  facet_wrap(~seg)

#####

min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |>
  distinct() |>
  ungroup() |>
  select(scenario_idx) |>
  collect()

qr_reg_names_slopes = tbl(con, "reg_anom_filt") |>
select(station_id, name, scenario_idx, reg) |>
distinct() |>
left_join(
  tbl(con, "qr_regressions"),
  by = c("station_id", "name", "scenario_idx")) |>
filter(stat == "slope",
       type == "fit",
       tau  == 0.5) |>
select(station_id, spc = name, scenario_idx, reg, year = startYear, slope = value) |>
  filter(scenario_idx %in% min_aic$scenario_idx) |>
collect()

outlier_sites = slope_segs |>
  filter(
    tau == 0.5) |>
  left_join(qDat, c("spc", "tau")) |>
  filter(!(between(value, qLow, qHigh))) |>
  ungroup() |>
  select(station_id, spc) |>
  distinct()

anom_dat = tbl(con, "anom") |>
  filter(station_id %in% outlier_sites$station_id) |>
  collect()

anom_dat = anom_dat |>
  select(date, station_id, spc = name, anom) |>
  inner_join(outlier_sites, by = c("station_id", "spc")) |>
  left_join(qr_reg_names_slopes, by = c("station_id", "spc"))


pdf(here("analysis/outlier_anom.pdf"), width = 12, height = 9)
for(i in 1:nrow(outlier_sites)){

  temp_anom_dat =  anom_dat |>
    filter(station_id == outlier_sites$station_id[i],
           spc == outlier_sites$spc[i])

  change_points = temp_anom_dat |>
    select(year, slope) |>
    distinct() |>
    mutate(date = ymd_hms((paste0(year,"-01-01 00:00:00"))))

  g = temp_anom_dat |>
    ggplot()+
    geom_line(aes(date, anom))+
    geom_vline(data = change_points, aes(xintercept = date), colour = "blue", size = 2)+
    ggtitle(label = paste0(outlier_sites$station_id[i], " - ", outlier_sites$spc[i]))+
    theme_classic()

  print(g)
}

dev.off()

pdf(here("analysis/remove_sites.pdf"), width = 12, height = 9)
for(i in 1:nrow(remove_sites)){

  temp_anom_dat =  anom_dat |>
    filter(station_id == remove_sites$station_id[i],
           spc == remove_sites$spc[i])

  change_points = temp_anom_dat |>
    select(year, slope) |>
    distinct() |>
    mutate(date = ymd_hms((paste0(year,"-01-01 00:00:00"))))

  g = temp_anom_dat |>
    ggplot()+
    geom_line(aes(date, anom))+
    geom_vline(data = change_points, aes(xintercept = date), colour = "red", size = 2)+
    ggtitle(label = paste0(remove_sites$station_id[i], " - ", remove_sites$spc[i]))+
    theme_classic()

  print(g)
}

dev.off()

### Investigate outlier slopes ###

outlier_slopes = qr_reg_names_slopes |>
  inner_join(outlier_sites, by = c("station_id", "spc")) |>
  left_join(qDat |> filter(tau == 0.5), by = c("spc")) |>
  filter(!(between(slope, qLow, qHigh)))

outlier_slopes |>
  ggplot()+
  geom_point(aes(x = station_id, y = slope*365, group = station_id)) +
  facet_wrap(~spc, scales = "free_y")

############################################

# Test sites with huge concurrent changes in no2 and o3 #

dbDisconnect(con, shutdown = T)

