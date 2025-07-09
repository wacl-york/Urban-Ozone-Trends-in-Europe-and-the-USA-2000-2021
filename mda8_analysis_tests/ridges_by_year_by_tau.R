library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

min_aic = tbl(con, "min_aic_mda8") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup()

slopes = inner_join(
  tbl(con, "qr_regressions_mda8"),
  min_aic,
  by = c("scenario_idx", "name", "station_id")
) |>
  filter(
    stat == "slope"
  ) |>
  rename(spc = name) |>
  anti_join(tbl(con, "remove_sites"),
            by = c("spc", "station_id")
  ) |>
  collect() |>
  pivot_wider(names_from = "type") |>
  select(-stat)

segments = slopes |>
  select(station_id, spc, reg, tau) |>
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
                               "spc",
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
    #tau %in% c(0.25,0.5,0.75),
    seg %in% 11:14,
    fit != 0, # there are a few times the slope is zero, but we can 't plot that here so just get rid. They are always p == 1.
  ) |>
  mutate(pvStr = case_when(
    pv <= 0.05 & fit < 0 ~ "p <= 0.05 (dec)",
    between(pv, 0.05, 0.1) & fit < 0 ~ "0.05 < p <= 0.10 (dec)",
    between(pv, 0.1, 0.33) & fit < 0 ~ "0.10  < p <= 0.33 (dec)",
    pv >= 0.33 & fit < 0 ~ "p > 0.33 (dec)",
    pv >= 0.33 & fit > 0 ~ "p > 0.33 (inc)",
    between(pv, 0.1, 0.33) & fit > 0 ~ "0.10  < p <= 0.33 (inc)",
    between(pv, 0.05, 0.1) & fit > 0 ~ "0.05 < p <= 0.10 (inc)",
    pv <= 0.05 & fit > 0 ~ "p <= 0.05 (inc)",
    TRUE ~ NA
  ) |>
    factor(
      levels = rev(
        c(
          "p > 0.33 (dec)",
          "0.10  < p <= 0.33 (dec)",
          "0.05 < p <= 0.10 (dec)",
          "p <= 0.05 (dec)",
          "p > 0.33 (inc)",
          "0.10  < p <= 0.33 (inc)",
          "0.05 < p <= 0.10 (inc)",
          "p <= 0.05 (inc)"
        )
      )
    )
  )

slopes_year_pv$tau |> unique()

plotDat = slopes_year_pv |>
  mutate(dir = ifelse(str_detect(pvStr, "inc"), "inc", "dec")) |>
  group_by(spc, tau, dir, country, year, pvStr) |>
  count() |>
  ungroup() |>
  group_by(country, spc) |>
  mutate(n = ifelse(dir == "dec", n*-1, n)) |>
  filter(tau %in% c(0.10,0.5,0.90),
         spc != "ox") |>
  mutate(country = ifelse(country == "United States of America", country, "Europe"),
         spc = case_when(spc == "no2" ~ "NO<sub>2</sub>",
                         spc == "o3" ~ "O<sub>3</sub>",
                         spc == "ox" ~ "O<sub>x</sub>" ) |>
           factor(levels = c("O<sub>3</sub>", "NO<sub>2</sub>", "O<sub>x</sub>")),
         n = case_when(country == "United States of America" & year %in% c(2000, 2001, 2020, 2021) ~ NA,
                       country == "Europe" & year %in% c(2000, 2001, 2021) ~ NA,
                       TRUE ~ n
         )

  )

plotDat |>
  ggplot()+
  geom_bar(aes(year,n, fill = pvStr), stat = "identity", position = "stack")+
  geom_hline(aes(yintercept = 0))+
  scale_fill_manual(values = p_colours, name = "")+
  scale_y_continuous(name = "Number of Time Series")+
  facet_grid(tau~country, scales = "free_y")+
  theme_minimal()+
  theme(strip.text = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T)



library(ggridges)

plotDat2 = slopes_year_pv |>
  filter(tau %in% c(0.1, 0.25, 0.5, 0.75, 0.9)) |>
  mutate(country = ifelse(country == "United States of America", country, "Europe"),
         spc = case_when(spc == "no2" ~ "NO<sub>2</sub>",
                         spc == "o3" ~ "O<sub>3</sub>",
                         spc == "ox" ~ "O<sub>x</sub>" ),
         fit = ifelse(country != "United States of America", fit/1.96, fit))

plotDat2 |>
  ggplot()+
  geom_density_ridges(
    aes(x = fit, y = factor(year))
  )+
  geom_vline(aes(xintercept = 0))+
  scale_x_continuous(limits = c(-0.01,0.01))+
  facet_grid(tau~country)


plotDat2 |>
  ggplot()+
  geom_density(aes(x = fit, fill = factor(year), group = factor(year)), alpha = 0.25)+
  geom_vline(aes(xintercept = 0))+
  scale_x_continuous(limits = c(-0.01,0.01))+
  facet_grid(tau~country)
