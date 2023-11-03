library(sf)
library(purrr)
library(rnaturalearth)
library(rnaturalearthdata)

eeaMeta = tbl(con, "eeaMeta") |>
  select(station_id = site,lat = latitude, lng = longitude) |>
  collect()

usMetaRaw = readRDS(here("data","stationList.RDS"))

usMeta = usMetaRaw |>
  mutate(lat = pluck(station,"coordinates", "lat"),
         lng = pluck(station,"coordinates", "lng"),
         station_id = pluck(station, "id") |>
           as.character()) |>
  select(station_id, lat, lng)

allMeta = eeaMeta |>
  bind_rows(usMeta) |>
  distinct()

statSf = stat |>
  left_join(allMeta, by = "station_id") |>
  filter(!is.na(lat)) |>
  mutate(
    slope = case_when(slope > 0.1 ~ 0.1,
                      slope < -0.1 ~ -0.1,
                      TRUE ~ slope
    ),
    slope_dir = ifelse(slope > 0, "up", "down")
  ) |>
  st_as_sf(coords = c("lng","lat"), crs = st_crs("WGS84"))

bb = st_bbox(statSf)


ggplot()+
  geom_sf(data = rnaturalearth::ne_coastline(returnclass = "sf"))+
  geom_sf(data = statSf,
          aes(fill = slope, colour = slope_dir, group = station_id), shape = 21, size = 2, linewidth = 2)+
  scale_x_continuous(limits = c(bb[1], bb[3]))+
  scale_y_continuous(limits = c(bb[2], bb[4]))+
  scale_colour_manual(values = c(scales::muted("blue"),scales::muted("red")))+
  scale_fill_gradient2(low = scales::muted("blue"),high = scales::muted("red"))+
  facet_wrap(~name)+
  AQVisR::AQvis_plotTheme()

statSf |>
  ggplot()+
  geom_density(aes(slope, fill = name))
