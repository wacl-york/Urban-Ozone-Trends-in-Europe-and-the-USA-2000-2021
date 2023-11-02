library(sf)
library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(quantreg)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)

# -------------------------------------------------------------------------

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"), read_only = FALSE)

tsMonth = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2021-12-31 00:00"), "month")) |>
  mutate(x = row_number())

coverage = tbl(con, "coverage") |>
  collect()

datWithStat = readRDS(here("data","dataWithStat.RDS")) |>
  filter(class(fit_se)[1] == "numeric") |>
  mutate(slope_q05 = fit[2,1],
         slope_q50 = fit[2,2],
         slope_q95 = fit[2,3],
         inter_q05 = fit[1,1],
         inter_q50 = fit[1,2],
         inter_q95 = fit[1,3],
         slopeSe_q05 = fit_se[1],
         slopeSe_q50 = fit_se[2],
         slopeSe_q95 = fit_se[3],
         slopePv_q50 = fit_pv[2,1],
         slopePv_q50 = fit_pv[2,2],
         slopePv_q95 = fit_pv[2,3],
         interPv_q05 = fit_pv[1,1],
         interPv_q50 = fit_pv[1,2],
         interPv_q95 = fit_pv[1,3]
  ) |>
  filter(perc > 90) |>
  select(-seasonality, -contains("fit")) |>
  unnest(data) |>
  pivot_longer(contains("_q"), names_to = c(".value","quantile"), names_sep = "_")

datWithStat |>
  select(name, region, station_id) |>
  distinct() |>
  group_by(name, region) |>
  count()

datWithStat |>
  filter(quantile == "q50") |>
  ggplot()+
  geom_line(aes(x, anom, colour = station_id))+
  geom_abline(aes(slope = slope, intercept = inter))+
  #scale_linetype_manual(values = c(2,1,2))+
  guides(colour = "none")+
  #trelliscopejs::facet_trelliscope(name~station_id)+
  facet_grid(name~region, scales = "free_y")+
  AQVisR::AQvis_plotTheme()

datWithStat |>
  ggplot()+
  geom_line(aes(x, anom, colour = station_id))+
  geom_abline(aes(slope = slope, intercept = inter, linetype = quantile))+
  scale_linetype_manual(values = c(2,1,2))+
  guides(colour = "none")+
  #trelliscopejs::facet_trelliscope(name~station_id)+
  facet_grid(name~region, scales = "free_y")+
  AQVisR::AQvis_plotTheme()

# Make some maps ----------------------------------------------------------

eeaMeta = tbl(con, "eeaMeta") |>
  select(station_id = site,lat = latitude, lng = longitude) |>
  collect()

usMeta = map(c(here("data","stations_no2","stationList_US.RDS"), here("data","stations","stationList_US.RDS")),
                ~.x |>
                  readRDS() |>
                  mutate(lat = pluck(station,"coordinates", "lat"),
                         lng = pluck(station,"coordinates", "lng"),
                         station_id = pluck(station, "id") |>
                           as.character()) |>
                  select(station_id, lat, lng)) |>
  bind_rows() |>
  distinct()

allMeta = eeaMeta |>
  bind_rows(usMeta) |>
  distinct()

datSf = datWithStat |>
  select(name, slope, quantile, station_id) |>
  distinct() |>
  left_join(allMeta, by = "station_id") |>
  st_as_sf(coords = c("lng","lat"), crs = "WGS84") |>
  mutate(slope_dir = ifelse(slope > 0, "Increasing", "Decreasing"))

bb = st_bbox(datSf)

ggplot()+
  geom_sf(data = rnaturalearth::ne_coastline(returnclass = "sf"))+
  geom_sf(data = datSf,
          aes(fill = slope, colour = station_id), shape = 21, size = 2)+
  scale_x_continuous(limits = c(bb[1], bb[3]))+
  scale_y_continuous(limits = c(bb[2], bb[4]))+
  #scale_colour_manual(values = c(scales::muted("blue"),scales::muted("red")))+
  scale_fill_gradient2(low = scales::muted("blue"),high = scales::muted("red"))+
  facet_grid(quantile~name)+
  guides(colour = "none")+
  AQVisR::AQvis_plotTheme()

plotly::ggplotly()


# -------------------------------------------------------------------------

datWithStat |>
  select(name, slope, region, quantile, station_id) |>
  distinct() |>
  left_join(allMeta, by = "station_id") |>
  ggplot()+
  geom_point(aes(lng, slope, colour = region))+
  geom_hline(aes(yintercept = 0))+
  facet_grid(name~quantile)+
  AQVisR::AQvis_plotTheme()









# -------------------------------------------------------------------------


dbDisconnect(con, shutdown = T)
