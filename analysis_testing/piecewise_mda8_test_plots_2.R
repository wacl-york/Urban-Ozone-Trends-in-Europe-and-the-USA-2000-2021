dat_sf = piecewise_dat |>
  filter(tau == 0.95) |>
  filter(country == "United States of America") |>
  st_as_sf(coords = c(lon = "lng", lat = "lat"), crs = 4326) |>
  #filter(startYear == 2000) |>
  select(station_id, pv, geometry, slope, startYear) |>
  distinct() |>
  na.omit()

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = dat_sf |> filter(pv < 0.33),
          shape = 21,
          mapping = aes(fill = slope, size = pv),
          alpha = 0.8) +
  scale_size(name = "pv", , trans = "reverse") +
  scale_fill_gradient2(
    name = "slope",
    low = "blue",       # for negative values
    mid = "white",      # zero
    high = "red",       # for positive values
    midpoint = 0
  ) +
  scale_y_continuous(limits = st_coordinates(limUS)[,2]) +
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  theme_minimal() +
  facet_wrap(~startYear)



dat_sf = piecewise_dat |>
  filter(tau == 0.5) |>
  filter(country != "United States of America") |>
  st_as_sf(coords = c(lon = "lng", lat = "lat"), crs = 4326) |>
  #filter(startYear == 2000) |>
  select(station_id, pv, geometry, slope, startYear) |>
  distinct() |>
  na.omit()

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = dat_sf,
          shape = 21,
          mapping = aes(fill = slope, size = pv),
          alpha = 0.8) +
  scale_size(name = "pv", , trans = "reverse") +
  scale_fill_gradient2(
    name = "slope",
    low = "blue",       # for negative values
    mid = "white",      # zero
    high = "red",       # for positive values
    midpoint = 0
  ) +
  scale_y_continuous(limits = st_coordinates(limEU)[,2]) +
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  theme_minimal() +
  facet_wrap(~startYear)

