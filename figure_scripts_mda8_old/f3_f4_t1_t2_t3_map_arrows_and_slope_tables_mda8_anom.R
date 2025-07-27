library(sf)
library(gt)
library(DBI)
library(dplyr)
library(tidyr)
library(scico)
library(ggh4x)
library(ggtext)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)

calc_arrow_end = function(df,
                          rangeMax,
                          slope,
                          length,
                          colX = "longitude",
                          colY = "latitude",
                          outNameX = "longitude_end",
                          outNameY = "latitude_end") {

  deg2rad <- function(deg) {(deg * pi) / (180)}

  theta = (90/rangeMax)*df[[slope]]

  df[[outNameX]] = length * cos(deg2rad(theta)) + df[[colX]]
  df[[outNameY]] = length * sin(deg2rad(theta)) + df[[colY]]

  df

}

make_table = function(x, rgo = c("a", "b","c")){

  x |>
    gt() |>
    tab_spanner(
      label = "Increasing",
      columns = contains("Increasing")
    ) |>
    tab_spanner(
      label = "Decreasing",
      columns = contains("Decreasing")
    ) |>
    tab_spanner(
      label = "No Trend",
      columns = contains("No Trend")
    ) |>
    tab_row_group(
      id = "a",
      label = "Ozzz3yyy",
      rows = spc == "o3"
    ) |>
    tab_row_group(
      id = "b",
      label = "NOzzz2yyy",
      rows = spc == "no2"
    ) |>
    tab_row_group(
      id = "c",
      label = "Ozzzxyyy",
      rows = spc == "ox"
    ) |>
    cols_label_with(
      columns = contains("-"),
      fn = \(x) x |>
        stringr::str_remove("Increasing_") |>
        stringr::str_remove("Decreasing_") |>
        stringr::str_remove("No Trend_")) |>
    cols_label(
      spc = "Species",
      tau = ":tau:"
    ) |>
    row_group_order(rgo)

}


source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

min_aic = tbl(con, "min_aic_mda8_anom") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup()                                               # the differences between the locations of the change points are not substantial
# so just take the lower of the two sceanrio_idx arbitrarily

slope_segs = inner_join(
  min_aic,
  tbl(con, "slope_segs_mda8_anom"),
  by = c("station_id", "name", "reg")) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude, country) |>
              distinct(),
            by = "station_id") |>
  rename(spc = name) |>
  anti_join(tbl(con, "remove_sites"),
            by = c("spc", "station_id")
  ) |>
  collect() |>
  mutate(fit = fit*365) |>
  mutate(fit = ifelse(country == "United States of America", fit, fit/1.96),
         country = ifelse(country == "United States of America", country, "Europe"))  # ugm3 -> ppb

dbDisconnect(con, shutdown = T)


pv_opt = c("p <= 0.05 (dec)",
           "0.05 < p <= 0.1 (dec)",
           "0.1  < p <= 0.33 (dec)",
           "p > 0.33",
           "0.1  < p <= 0.33 (inc)",
           "0.05 < p <= 0.1 (inc)",
           "p <= 0.05 (inc)"
)

lineGroups = slope_segs |>
  mutate(fit = case_when(fit > 2.5 ~ 2.5,
                         fit < -2.5 ~ -2.5,
                         TRUE~fit),
         pvStr = case_when(
           pv <= 0.05 & fit < 0 ~ pv_opt[1],
           pv > 0.05 & pv <= 0.1 & fit < 0 ~ pv_opt[2],
           pv > 0.1 & pv <= 0.33 & fit < 0 ~ pv_opt[3],
           pv >= 0.33 ~ pv_opt[4],
           pv > 0.1 & pv <= 0.33 & fit > 0 ~ pv_opt[5],
           pv > 0.05 & pv <= 0.1 & fit > 0 ~ pv_opt[6],
           pv <= 0.05 & fit > 0 ~ pv_opt[7],
           TRUE ~ NA
         ) |>
           factor(levels = pv_opt)
  ) |>
  calc_arrow_end(rangeMax = 2.5,
                 slope = "fit",
                 length = 3) |>
  rename(latitude_start = latitude,
         longitude_start = longitude) |>
  mutate(rn = row_number()) |>
  pivot_longer(c(latitude_start,latitude_end, longitude_start, longitude_end),
               names_to = c("coordType","suffix"),
               names_sep = "_",
               values_to = "coord"
  ) |>
  pivot_wider(names_from = "coordType",
              values_from = "coord") |>
  select(-suffix)

lines = lineGroups |>
  st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
  group_by(rn) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

segs = tribble(
  ~seg, ~segStart, ~segEnd,
  1,  2000, 2006,
  2,  2007, 2013,
  3,  2014, 2019,
  4,  2020, 2023,
  11, 2000, 2004,
  12, 2005, 2009,
  13, 2010, 2014,
  14, 2015, 2021
)

lineDat = left_join(lineGroups, lines, "rn") |>
  select(-latitude, -longitude, -rn) |>
  st_as_sf() |>
  left_join(segs, by = "seg")



# -------------------------------------------------------------------------

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)


limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

p_colours = c(
  rgb(0, 0, 0.6),
  rgb(0.1176, 0.3922, 1),
  rgb(0.4706, 0.7373, 1),
  rgb(0.6431, 0.7569, 0.4431),
  rgb(1, 0.7294, 0.4),
  rgb(1, 0.3922, 0),
  rgb(0.6471, 0, 0.1294)
)

plotDat = lineDat |>
  filter(tau == 0.5,
         seg %in% 11:14) |>
  st_transform(mycrs) |>
  mutate(segLab = paste(segStart, segEnd, sep = " - "),
         spc = ifelse(spc == "no2",  "NO<sub>2</sub>", "O<sub>3</sub>"))

# Figure 3 Europe, 0.5. 11-14 ---------------------------------------------


geu = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = plotDat,
          mapping = aes(colour = pvStr),
          linewidth = 0.25,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_manual(values = p_colours, name = "")+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  # facet_wrap(spc~segLab)+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T)

pdf(here::here('figures_mda8','f3_eu_arrows_mda8_anom.pdf'), width = 8.3, height = 11.7)
print(geu)
dev.off()


# Figure 4 USA, 0.5, 11-14 ------------------------------------------------


gus = ggplot() +
  geom_sf(data = world, fill = "white")+
  geom_sf(data = plotDat,
          mapping = aes(colour = pvStr),
          linewidth = 0.75,
          arrow = arrow(angle = 30,
                        ends = "last",
                        type = "open",
                        length = unit(0.1, "cm"))) +
  scale_colour_manual(values = p_colours, name = "")+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  facet_nested_wrap(~spc + segLab, nrow = 4)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T)



pdf(here::here('figures_mda8','f4_usa_arrows_mda8_anom.pdf'),width = 8.3, height = 11.7)
print(gus)
dev.off()


# Tables ------------------------------------------------------------------

tableDat = lineDat |>
  st_drop_geometry() |>
  mutate(country = ifelse(country == "United States of America", country, "Europe")) |>
  mutate(Increasing = ifelse(fit > 0 & pv <= 0.33, 1, 0), # convert to just counting the increasing / decreasing
         Decreasing = ifelse(fit < 0 & pv <= 0.33, 1, 0), # then if there are two trends in the same direction in the same segment
         `No Trend` = ifelse(pv > 0.33, 1, 0)             # the rows will be identical and removed by distinct (need to hold onto station_id for this)
  ) |>
  select(spc, seg, tau, country, Increasing, Decreasing, `No Trend`, station_id) |>
  distinct() |>
  select(-station_id) |>
  group_by(spc, seg, tau, country) |>
  summarise(
    Increasing = sum(Increasing),
    Decreasing = sum(Decreasing),
    `No Trend` = sum(`No Trend`)
  ) |>
  ungroup() |>
  filter(seg %in% 11:14) |>
  left_join(segs, "seg") |>
  mutate(segLab = paste(segStart, segEnd, sep = " - ") |>
           stringr::str_remove_all("20")) |>
  select(-seg, -segStart, -segEnd) |>
  pivot_wider(values_from = c(Increasing, Decreasing, `No Trend`),
              names_from = segLab,
              names_sep = "_"
  )

tableDat |>
  filter(country == "Europe") |>
  select(-country) |>
  make_table("a") |>
  as_latex() |>
  as.character() |>
  latex_tweaks(
    caption = "Trends in O\\textsubscript{3} at sites in Europe in annual groups between 2000 and 2021 inclusive. If a site as a change point within a group, both slopes are added to the tally. Those classed as 'No Trend' are the slopes where p > 0.33",
    label = "table:europe_slope_segs_mda8_anom"
  ) |>
  writeLines(here::here('tables_mda8','t2_europe_segs_11_14_mda8_anom.txt'))


tableDat |>
  filter(country == "United States of America") |>
  select(-country) |>
  make_table("a") |>
  as_latex() |>
  as.character() |>
  latex_tweaks(
    caption = "Trends in O\\textsubscript{3} at sites in the United States of America in annual groups between 2000 and 2021 inclusive. If a site as a change point within a group, both slopes are added to the tally. Those classed as 'No Trend' are the slopes where p > 0.33",
    label = "table:usa_slope_segs"
  ) |>
  writeLines(here::here('tables_mda8','t3_usa_segs_11_14_mda8_anom.txt'))


# slope range table -------------------------------------------------------

slopeRanges = lineDat |>
  st_drop_geometry() |>
  mutate(country = ifelse(country == "United States of America", country, "Europe")) |>
  mutate(type = case_when(fit > 0 & pv <= 0.33 ~ "Increasing",
                          fit < 0 & pv <= 0.33 ~ "Decreasing",
                          pv > 0.33 ~ "No Trend")) |>
  filter(seg %in% 11:14) |>
  select(spc, country, tau, type, fit, station_id) |>
  group_by(spc, country, tau, type) |>
  summarise(
    `5 th`  = quantile(fit, probs = 0.05, na.rm = T),
    `50 th` = quantile(fit, probs = 0.50, na.rm = T),
    `95 th` = quantile(fit, probs = 0.95, na.rm = T)
  ) |>
  filter(tau == 0.5) |>
  pivot_longer(contains("th")) |>
  mutate(value = round(value, 2)) |>
  pivot_wider(names_from = c("country", "type"),
              names_sep = "_") |>
  ungroup() |>
  relocate(spc, name, contains("Increasing"))

slopeRanges |>
  arrange(spc) |>
  select(-tau) |>
  gt() |>
  tab_spanner(
    label = "Europe",
    columns = contains("Europe")
  ) |>
  tab_spanner(
    label = "United States of America",
    columns = contains("United States of America")
  ) |>
  tab_spanner(
    label = "Slope / ppbv yearwww-1yyy",
    columns = contains(c("Europe", "United States of America"))
  ) |>
  tab_row_group(
    id = "a",
    label = "Ozzz3yyy",
    rows = spc == "o3"
  ) |>
  tab_row_group(
    id = "b",
    label = "NOzzz2yyy",
    rows = spc == "no2"
  ) |>
  tab_row_group(
    id = "c",
    label = "Ozzzxyyy",
    rows = spc == "ox"
  ) |>
  cols_label(
    spc = "Species",
    name = "Percentile"
  )  |>
  row_group_order("a") |>
  cols_label_with(
    columns = contains("_"),
    fn = \(x) x |>
      stringr::str_remove("Europe_") |>
      stringr::str_remove("United States of America_")) |>
  as_latex() |>
  as.character() |>
  latex_tweaks(caption = "5 th, 50 th and 95 th percentiles of the $\\tau$ = 0.5 slopes in Europe and the USA.",
               label = "table:slope_ranges",
               sideways = F,
               adjustbox = F) |>
  writeLines(here::here('tables_mda8','t1_slope_ranges_mda8_anom.txt'))


