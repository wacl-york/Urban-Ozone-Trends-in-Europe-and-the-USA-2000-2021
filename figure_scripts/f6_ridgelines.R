library(DBI)
library(dplyr)
library(tidyr)
library(ggh4x)
library(ggtext)
library(ggplot2)

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
  select(station_id, tau, name, year, continent, per_year = fit, pv) |>
  mutate(per_year = per_year*365) |>
  distinct()

blacklist_sites = tbl(con, "remove_sites") |>
  collect() |>
  select(station_id, name = spc)

duplicates_test = comp_ppb_year |>
  dplyr::summarise(n = dplyr::n(), .by = c(station_id, tau, name, continent, year)) |>
  dplyr::filter(n > 1L)

comp_ppb_year_longer = comp_ppb_year |>
  group_by(station_id, name, tau, continent) |>
  select(-pv) |>
  anti_join(duplicates_test, by = c("station_id", "tau", "name", "continent", "year")) |>
  anti_join(blacklist_sites, by = c("station_id", "name")) |>
  rename(value = per_year)

lineDat = comp_ppb_year_longer |>
  filter(year %in% c(2000,2019),
         name != "ox") |>
  group_by(tau, continent, name, year) |>
  summarise(q25 = quantile(value, probs = 0.25, na.rm = T),
            q75 = quantile(value, probs = 0.75, na.rm = T)
  ) |>
  ungroup() |>
  pivot_longer(contains("q"), names_to = "quantiles", values_to = "q") |>
  mutate(name = ifelse(name == "o3", "O<sub>3</sub>", "NO<sub>2</sub>"),
         quantiles = ifelse(quantiles == "q25", "25<sup>th</sup>", "75<sup>th</sup>"))

col = c(
  "#E4ADD6",
  "#B5549C",
  "#65014B",
  "black",
  "#0C4C00",
  "#5F903D",
  "#C0D9A1"
)

g1 = comp_ppb_year_longer |>
  filter(year %in% c(2000,2019),
         name != "ox") |>
  mutate(name = ifelse(name == "o3", "O<sub>3</sub>", "NO<sub>2</sub>")) |>
  ggplot()+
  ggridges::geom_density_ridges(
    aes(x = as.numeric(value), y = factor(tau), fill = factor(tau)),
  )+
  geom_vline(aes(xintercept = 0), linewidth = 1, colour = "black")+
  geom_vline(data = lineDat,
             aes(xintercept = q,
                 group = quantiles,
                 colour = factor(tau),
                 linetype = quantiles))+
  scale_colour_manual(values = col)+
  scale_fill_manual(values = col)+
  scale_x_continuous(limits = c(-2.5,2.5), name = "slope / ppb yr<sup>-1</sup>")+
  facet_nested(name + year~continent)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_markdown(),
        axis.text = element_markdown(),
        legend.text = element_markdown()
        )+
  guides(fill = guide_legend(reverse = T), colour = guide_legend(reverse = T))+
  labs(y = expression(tau), linetype = "Percentile", fill = expression(tau), colour = expression(tau))


pdf(here::here('figures','f6_ridgelines.pdf'),width = 8.3, height = 11.7)
print(g1)
dev.off()






