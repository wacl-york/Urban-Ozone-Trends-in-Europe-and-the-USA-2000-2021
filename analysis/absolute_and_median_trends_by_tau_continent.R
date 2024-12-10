library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(rnaturalearth)
library(rnaturalearthhires)
library(patchwork)
library(ggpubr)

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
            by = "station_id") |>
  mutate(fit = ifelse(country == "United States of America", fit, fit/1.96)) # ugm3 -> ppb

slopes_year = slopes_year |>
  mutate(continent = "Europe")

#slopes_year$continent[slopes_year$country != "United States of America"] = "Europe"
slopes_year$continent[slopes_year$country == "United States of America"] = "United States of America"

blacklist_sites = tbl(con, "remove_sites") |>
  #filter(spc == "o3") |>
  collect() |>
  select(station_id, name = spc)
#blacklist_sites = unique(blacklist_sites$station_id)

duplicates_test = slopes_year |>
  select(station_id, tau, name, year, continent, per_year = fit, pv) |>
  distinct() |>
  dplyr::summarise(n = dplyr::n(), .by = c(station_id, tau, name, continent, year)) |>
  dplyr::filter(n > 1L)

duplicates_test = duplicates_test[1:5]

comp_ppb_year = slopes_year |>
  select(station_id, tau, name, year, continent, per_year = fit, pv) |>
  mutate(per_year = per_year*365) |>
  distinct() |>
  select(-pv) |>
  anti_join(duplicates_test, by = c("station_id", "tau", "name", "continent", "year")) |>
  anti_join(blacklist_sites, by = c("station_id", "name"))

new_values_2002 = comp_ppb_year |>
  filter(year == 2002) |>
  select(-year) |>
  group_by(station_id, tau, name, continent) |>
  summarise_all(median) |>
  rename(value_2002 = per_year)

col = c(
  "#E4ADD6",
  "#B5549C",
  "#65014B",
  "black",
  "#0C4C00",
  "#5F903D",
  "#C0D9A1"
)

comp_plot = comp_ppb_year |>
  filter(year %in% (2002:2021)) |>
  group_by(tau, name, continent, station_id) |>
  mutate(per_year = cumsum(per_year)) |>
  left_join(new_values_2002, by = c("name", "continent", "station_id", "tau")) |>
  mutate(per_year = per_year - value_2002) |>
  ungroup() |>
  select(-value_2002) |>
  group_by(tau, name, year, continent) |>
  summarise(
    q25 = quantile(per_year, probs = 0.25),
    q75 = quantile(per_year, probs = 0.75),
    mad = mad(per_year),
    per_year = quantile(per_year, probs = 0.5)) |>
  mutate(tau = factor(tau))

#png("~/TOAR/TOAR_paper/plots/fixed_median_slopes_per_tau_continent_name_absolute_change_with_mad_ribbon.png",width = 1080*2, height = 1080*1.5, res = 300)
comp_plot |>
  ggplot()+
  #geom_ribbon(aes(x = year, ymin = per_year-mad, ymax = per_year+mad, fill = tau), alpha = 0.05) +
  geom_line(aes(x = year, y = per_year, colour = tau), linewidth = 1)+
  geom_point(aes(x = year, y = per_year, colour = factor(tau)))+
  geom_hline(aes(yintercept = 0))+
  # scale_colour_scico_d(palette = "bam")+
  # scale_fill_scico_d(palette = "bam")+
  scale_colour_manual(values = col)+
  scale_fill_manual(values = col)+
  facet_grid(name ~ continent, scale = "free_y", labeller = as_labeller(labels)) +
  theme_minimal()+
  labs(x = "Year", y = "Increased median mixing ratio since 2000 / ppbV", colour = expression(tau), fill= expression(tau))
#dev.off()

datScrape = comp_plot |>
  filter(name == "no2",
         year == 2021)

comp_plot_trends = comp_ppb_year |>
  filter(year %in% (2002:2021)) |>
  group_by(tau, name, continent, station_id) |>
  select(-station_id) |>
  group_by(tau, name, year, continent) |>
  summarise(
    q25 = quantile(per_year, probs = 0.25),
    q75 = quantile(per_year, probs = 0.75),
    mad = mad(per_year),
    per_year = quantile(per_year, probs = 0.5)) |>
  mutate(tau = factor(tau))

png("~/TOAR/TOAR_paper/plots/fixed_median_slopes_per_tau_continent_name.png",width = 1080*2, height = 1080*1.5, res = 300)
comp_plot_trends |>
  ggplot()+
  geom_ribbon(aes(x = year, ymin = per_year-mad, ymax = per_year+mad, fill = tau), alpha = 0.05) +
  geom_line(aes(x = year, y = per_year, colour = tau), linewidth = 1)+
 # geom_point(aes(x = year, y = per_year, colour = factor(tau)))+
  geom_hline(aes(yintercept = 0))+
  # scale_colour_scico_d(palette = "bam")+
  # scale_fill_scico_d(palette = "bam")+
  scale_colour_manual(values = col)+
  scale_fill_manual(values = col)+
  facet_grid(name ~ continent, scale = "free_y", labeller = as_labeller(labels)) +
  theme_minimal() +
  labs(x = "Year", y = expression("Median slope since 2000 / ppbV y "^-1), colour = expression(tau), fill= expression(tau))
dev.off()


datScrape = comp_plot_trends |>
  filter(name == "ox",
         year %in% c(2002,2003),
         tau %in% c(0.95),
         continent == "Europe")


