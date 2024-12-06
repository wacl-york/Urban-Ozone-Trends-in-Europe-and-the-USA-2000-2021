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
  mutate(continent = "North America")

slopes_year$continent[slopes_year$country != "United States of America"] = "Europe"
slopes_year$continent[slopes_year$country == "United States of America"] = "United States of America"

comp_ppb_year = slopes_year |>
 # filter(name == "o3") |>
  select(station_id, tau, name, year, continent, per_year = fit, pv) |>
  mutate(per_year = per_year*365) |>
  distinct()

# test = o3_ppb_year |>
#   filter(station_id %in% duplicate_station_id)

blacklist_sites = tbl(con, "remove_sites") |>
  #filter(spc == "o3") |>
  collect() |>
  select(station_id, name = spc)
#blacklist_sites = unique(blacklist_sites$station_id)

duplicates_test = comp_ppb_year |>
  dplyr::summarise(n = dplyr::n(), .by = c(station_id, tau, name, continent, year)) |>
  dplyr::filter(n > 1L)

duplicates_test = duplicates_test[1:5]

comp_ppb_year_wider = comp_ppb_year |>
  group_by(station_id, name, tau, continent) |>
  #filter(pv < 0.05) |>
  select(-pv) |>
  anti_join(duplicates_test, by = c("station_id", "tau", "name", "continent", "year")) |>
  anti_join(blacklist_sites, by = c("station_id", "name")) |>
  pivot_wider(names_from = year, values_from = per_year)

comp_count = comp_ppb_year_wider |>
  ungroup() |>
  select(-station_id) |>
  mutate(tau = as.character(tau)) |>
  group_by(tau, name, continent) |>
  summarise(
    across(where(is.numeric), ~ sum(. > 3, na.rm = T))
  )

comp_tau_0.5_median_yearly = comp_ppb_year |>
  select(tau, name, year, continent, per_year) |>
  group_by(tau, name, year, continent) |>
  summarise_all(median)

png("~/TOAR/TOAR_paper/plots/o3_density_ridges_by_tau_continent.png",width = 1080*2.5, height = 1080*1.5, res = 300)
ggplot(comp_tau_0.5_median_yearly |>
         filter(name == "o3",
                year %in% (2002:2020)))+
  geom_line(aes(x = year, y = per_year, colour = continent))+
  facet_wrap(~tau)

col_years = colnames(comp_ppb_year_wider[5:28])

comp_ppb_year_longer = comp_ppb_year_wider |>
  pivot_longer(cols = all_of(col_years), names_to = "year")

comp_count_longer = comp_count |>
  pivot_longer(cols = all_of(col_years), names_to = "year")

# ggplot(comp_count_longer)+
#   geom_line(aes(x = year, y = value, colour = tau, group = tau))+
#   facet_wrap(~continent+name)

comp_ppb_IQR = comp_ppb_year_longer |>
  mutate(value = as.numeric(value)) |>
  ungroup() |>
  select(-station_id) |>
  group_by(tau, continent, name, year) |>
  summarise(qLow = quantile(value, probs = 0.25, na.rm = T),
            qHigh = quantile(value, probs = 0.75, na.rm = T)) |>
  mutate(IQR = abs(qHigh-qLow)) |>
  pivot_longer(cols = c("qLow", "qHigh", "IQR"), values_to = "value", names_to = "Quant")

# ggplot(comp_ppb_IQR)+
#   geom_point(aes(x = year, y = value, colour = Quant))+
#   facet_wrap(~continent + tau + name)

perc_change_ppb_IQR = comp_ppb_IQR |>
  filter(year %in% c(2000, 2021)) |>
  filter(Quant == "IQR") |>
  select(-Quant) |>
  group_by(tau, name, continent) |>
  pivot_wider(names_from = year, values_from = value) |>
  select(tau, continent, IQR_2000 = `2000`, IQR_2021 = `2021`)

perc_change_ppb_IQR$multiplier_IQR  = 1-abs((perc_change_ppb_IQR$IQR_2021-perc_change_ppb_IQR$IQR_2000)/perc_change_ppb_IQR$IQR_2000)
perc_change_ppb_IQR$perc_reduction = 1-perc_change_ppb_IQR$multiplier_IQR

comp_ppb_IQR_wider = comp_ppb_IQR |>
  filter(year %in% c(2000, 2021)) |>
  filter(Quant %in% c("qLow", "qHigh")) |>
  group_by(tau, continent, year) |>
  pivot_wider(names_from = Quant, values_from = value) |>
  pivot_wider(names_from = year, values_from = c(qLow, qHigh)) |>
  mutate(qLow_change = qLow_2021-qLow_2000,
         qHigh_change = qHigh_2021-qHigh_2000)


lineDat = comp_ppb_year_longer |>
  filter(year %in% c(2000, 2021)) |>
  group_by(tau, continent, name, year) |>
  summarise(q25 = quantile(value, probs = 0.25, na.rm = T),
            q75 = quantile(value, probs = 0.75, na.rm = T)
            ) |>
  ungroup() |>
  pivot_longer(contains("q"), names_to = "quantiles", values_to = "q")

#ggridges::geom_density_ridges()

png("~/TOAR/TOAR_paper/plots/o3_density_ridges_by_tau_continent.png",width = 1080*2.5, height = 1080*1.5, res = 300)
ggplot(comp_ppb_year_longer |>
         filter(year %in% c(2000,2021),
                name == "o3"))+
  ggridges::geom_density_ridges(
    aes(x = as.numeric(value), y = factor(tau), fill = factor(tau)),
  )+
  geom_vline(aes(xintercept = 0), linewidth = 1, colour = "black")+
  # geom_density(aes(x = value, fill = factor(tau)), alpha = 0.2)+
  geom_vline(data = lineDat |> filter(name == "o3"),
             aes(xintercept = q,
                 group = quantiles,
                 colour = factor(tau),
                 linetype = quantiles))+
  scale_fill_scico_d(palette = "bam")+
  scale_colour_scico_d(palette = "bam")+
  scale_x_continuous(limits = c(-5,5))+
  facet_grid(year~continent)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(expression("O"[3]))
dev.off()

png("~/TOAR/TOAR_paper/plots/no2_density_ridges_by_tau_continent.png",width = 1080*2.5, height = 1080*1.5, res = 300)
ggplot(comp_ppb_year_longer |>
         filter(year %in% c(2000,2021),
                name == "no2"))+
  ggridges::geom_density_ridges(
    aes(x = as.numeric(value), y = factor(tau), fill = factor(tau)),
  )+
  geom_vline(aes(xintercept = 0), linewidth = 1, colour = "black")+
  # geom_density(aes(x = value, fill = factor(tau)), alpha = 0.2)+
  geom_vline(data = lineDat |> filter(name == "no2"),
             aes(xintercept = q,
                 group = quantiles,
                 colour = factor(tau),
                 linetype = quantiles))+
  scale_fill_scico_d(palette = "bam")+
  scale_colour_scico_d(palette = "bam")+
  scale_x_continuous(limits = c(-5,5))+
  facet_grid(year~continent)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(expression("NO"[2]))
dev.off()

png("~/TOAR/TOAR_paper/plots/ox_density_ridges_by_tau_continent.png",width = 1080*2.5, height = 1080*1.5, res = 300)
ggplot(comp_ppb_year_longer |>
         filter(year %in% c(2000,2021),
                name == "ox"))+
  ggridges::geom_density_ridges(
    aes(x = as.numeric(value), y = factor(tau), fill = factor(tau)),
  )+
  geom_vline(aes(xintercept = 0), linewidth = 1, colour = "black")+
  # geom_density(aes(x = value, fill = factor(tau)), alpha = 0.2)+
  geom_vline(data = lineDat |> filter(name == "ox"),
             aes(xintercept = q,
                 group = quantiles,
                 colour = factor(tau),
                 linetype = quantiles))+
  scale_fill_scico_d(palette = "bam")+
  scale_colour_scico_d(palette = "bam")+
  scale_x_continuous(limits = c(-5,5))+
  facet_grid(year~continent)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle(expression("O"[x]))
dev.off()

#combined = ggarrange(o3, no2, ox, nrow=1, common_legend = TRUE)

val2002 = comp_tau_0.5_median_yearly |>
  ungroup() |>
  filter(year == 2002) |>
  rename(value_2002 = per_year) |>
  select(-year)


png("~/TOAR/TOAR_paper/plots/median_slopes_per_tau_continent_name_absolute_change.png",width = 1080*2, height = 1080*1.5, res = 300)
comp_tau_0.5_median_yearly |>
  filter(year %in% (2002:2020)) |>
  group_by(tau, name, continent) |>
  mutate(per_year = cumsum(per_year)) |>
  ungroup() |>
  left_join(val2002, by = c("name", "continent", "tau")) |>
  mutate(per_year = per_year - value_2002) |>
  ggplot()+
  geom_line(aes(x = year, y = per_year, colour = factor(tau)))+
  geom_hline(aes(yintercept = 0))+
  scale_colour_scico_d(palette = "bam")+
  facet_grid(name ~ continent, scale = "free_y") +
  theme_minimal()
dev.off()

png("~/TOAR/TOAR_paper/plots/median_slopes_per_tau_continent_name.png",width = 1080*2, height = 1080*1.5, res = 300)
comp_tau_0.5_median_yearly |>
  filter(year %in% (2002:2020)) |>
  # group_by(tau, name, continent) |>
  # mutate(per_year = cumsum(per_year)) |>
  # ungroup() |>
  # left_join(val2002, by = c("name", "continent", "tau")) |>
  # mutate(per_year = per_year - value_2002) |>
  ggplot()+
  geom_line(aes(x = year, y = per_year, colour = factor(tau)))+
  geom_hline(aes(yintercept = 0))+
  scale_colour_scico_d(palette = "bam")+
  facet_grid(name ~ continent, scale = "free_y") +
  theme_minimal()
dev.off()

