library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(rnaturalearth)
library(rnaturalearthhires)

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
  collect() |>
  pivot_wider(names_from = "type") |>
  select(-stat)

segments = slopes |>
  select(station_id, name, reg, tau) |>
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
                              "name",
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

p_colours = c("#A50021",
              "#FF6400",
              "#FFBA66",
              "#A4C171",
              "#000099",
              "#1E64FF",
              "#78BCFF",
              "#A4C171"
)


slopes_year_pv = slopes_year |>
  filter(
    tau %in% c(0.25,0.5,0.75),
    seg %in% 11:14,
    fit != 0, # there are a few times the slope is zero, but we can 't plot that here so just get rid. They are always p == 1.
  ) |>
  mutate(pvStr = case_when(
    pv <= 0.05 & fit < 0 ~ "<0.05_-ve",
    between(pv, 0.05, 0.1) & fit < 0 ~ "0.05-0.1_-ve",
    between(pv, 0.1, 0.33) & fit < 0 ~ "0.1-0.33_-ve",
    pv >= 0.33 & fit < 0 ~ ">0.33_-ve",
    pv >= 0.33 & fit > 0 ~ ">0.33_+ve",
    between(pv, 0.1, 0.33) & fit > 0 ~ "0.1-0.33_+ve",
    between(pv, 0.05, 0.1) & fit > 0 ~ "0.05-0.1_+ve",
    pv <= 0.05 & fit > 0 ~ "<0.05_+ve",
    TRUE ~ NA
  ) |>
    factor(
      levels = rev(
        c(
          ">0.33_-ve",
          "0.1-0.33_-ve",
          "0.05-0.1_-ve",
          "<0.05_-ve",
          ">0.33_+ve",
          "0.1-0.33_+ve",
          "0.05-0.1_+ve",
          "<0.05_+ve"
        )
      )
    )
  )


plotDat = slopes_year_pv |>
  mutate(dir = ifelse(str_detect(pvStr, "\\+ve"), "inc", "dec")) |>
  group_by(name, tau, dir, country, year, pvStr) |>
  count() |>
  ungroup() |>
  mutate(n = ifelse(dir == "dec", n*-1, n))


g_us = plotDat |>
  filter(country == "United States of America") |>
  ggplot()+
  geom_bar(aes(year,n, fill = pvStr), stat = "identity", position = "stack")+
  geom_hline(aes(yintercept = 0))+
  scale_fill_manual(values = p_colours)+
  facet_grid(name~tau, scales = "free_y")

g_eu = plotDat |>
  filter(country != "United States of America") |>
  ggplot()+
  geom_bar(aes(year,n, fill = pvStr), stat = "identity", position = "stack")+
  geom_hline(aes(yintercept = 0))+
  scale_fill_manual(values = p_colours)+
  facet_grid(name~tau, scales = "free_y")







mycrs = 4087

world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)

lim = tibble(lng = c(-121,-40), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

plotDatSf = slopes_year_pv |>
  filter(country == "United States of America",
         year %in% c(2000, 2021),
         pvStr == "<0.05_+ve",
         name != "ox",
         tau == 0.5) |>
  rename(spc = name) |>
  group_by(station_id, spc) |>
  mutate(n = n() |>
           as.factor()) |>
  ungroup() |>
  mutate(foo = case_when(n == 2 ~ "remained_increasing",
                         n == 1 & year == 2000 ~ "stopped_increasing",
                         n == 1 & year == 2021 ~ "started_increasing"
                         )) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = plotDatSf, aes(fill = foo), size = 2, shape = 21, colour = "black")+
  scale_y_continuous(limits = st_coordinates(lim)[,2])+
  scale_x_continuous(limits = st_coordinates(lim)[,1])+
  scale_fill_manual(values = c("orange", "darkred", "darkgreen"))+
  facet_grid(spc~year)


