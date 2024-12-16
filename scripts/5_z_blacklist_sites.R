library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(scico)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(read_only = FALSE)

slope_segs = tbl(con, "slope_segs") |>
  inner_join(min_aic, by = c("station_id", "name", "reg")) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude),
            by = "station_id") |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(8857) |>
  rename(spc = name)

qDat = slope_segs |>
  st_drop_geometry() |>
  group_by(spc, tau) |>
  summarise(qLow = quantile(fit, probs = 0.02),
            qHigh = quantile(fit, probs = 0.98))

# slope_segs |>
#   filter(
#     tau == 0.5,
#     spc == "no2",
#     seg %in% 1:4) |>
#   left_join(qDat, c("spc", "tau")) |>
#   filter(!(between(value, qLow, qHigh))) |>
#   ggplot()+
#   geom_sf(data = world, fill = "white")+
#   geom_sf(aes(colour = value*365, group = station_id))+
#   scale_color_scico(palette = "vik")+
#   scale_y_continuous(limits = st_coordinates(lim)[,2])+
#   scale_x_continuous(limits = st_coordinates(lim)[,1])+
#   facet_wrap(~seg)

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
  filter(!(between(fit, qLow, qHigh))) |>
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

# pdf(here("analysis/outlier_anom.pdf"), width = 12, height = 9)
# for(i in 1:nrow(outlier_sites)){
#
#   temp_anom_dat =  anom_dat |>
#     filter(station_id == outlier_sites$station_id[i],
#            spc == outlier_sites$spc[i])
#
#   change_points = temp_anom_dat |>
#     select(year, slope) |>
#     distinct() |>
#     mutate(date = ymd_hms((paste0(year,"-01-01 00:00:00"))))
#
#   g = temp_anom_dat |>
#     ggplot()+
#     geom_line(aes(date, anom))+
#     geom_vline(data = change_points, aes(xintercept = date), colour = "blue", size = 2)+
#     ggtitle(label = paste0(outlier_sites$station_id[i], " - ", outlier_sites$spc[i]))+
#     theme_classic()
#
#   print(g)
# }
#
# dev.off()

# pdf(here("analysis/remove_sites.pdf"), width = 12, height = 9)
# for(i in 1:nrow(remove_sites)){
#
#   temp_anom_dat =  anom_dat |>
#     filter(station_id == remove_sites$station_id[i],
#            spc == remove_sites$spc[i])
#
#   change_points = temp_anom_dat |>
#     select(year, slope) |>
#     distinct() |>
#     mutate(date = ymd_hms((paste0(year,"-01-01 00:00:00"))))
#
#   g = temp_anom_dat |>
#     ggplot()+
#     geom_line(aes(date, anom))+
#     geom_vline(data = change_points, aes(xintercept = date), colour = "red", size = 2)+
#     ggtitle(label = paste0(remove_sites$station_id[i], " - ", remove_sites$spc[i]))+
#     theme_classic()
#
#   print(g)
# }
#
# dev.off()

### Investigate outlier slopes ###

remove_sites = data.frame(
  station_id = c("bg0013a", "bg0043a", "bg0040a", "bg0052a", "es1529a", "es1529a", "fr31002", "gr0030a",
                 "it0963a", "pt03072", "se0022a", "fr04058", "gr0031a", "fr33211","ie0028a"),
  spc = c("o3", "o3", "no2", "o3", "o3", "ox", "o3", "o3", "no2", "no2", "o3", "no2", "ox", "o3","ie0028a")
)

second_chance_sites = data.frame(
  station_id = c("fr31002", "se0022a", "fr04058", "fr33211"),
  spc = c("o3", "o3", "no2", "o3")
)

remove_remove_sites = remove_sites |>
  anti_join(second_chance_sites, by = c("station_id", "spc"))

ox_sites = anom_dat |>
  select(station_id, name) |>
  distinct() |>
  filter(name == "ox") |>
  filter(station_id %in% remove_remove_sites$station_id) |>
  rename(spc = name)

blacklist_sites = remove_remove_sites |>
  bind_rows(ox_sites) |>
  distinct()

dbWriteTable(con, "remove_sites", blacklist_sites, overwrite = T)

dbDisconnect(con, shutdown = T)
