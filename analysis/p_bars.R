library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

slope_segs = tbl(con, "slope_segs") |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, country) |>
              distinct(),
            by = "station_id") |>
  collect() |>
  mutate(country = ifelse(country != "United States of America", "Europe", country))

dbDisconnect(con, shutdown = T)

plotDat = slope_segs |>
  filter(
    country == "United States of America",
    name == "no2",
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
  ) |>
  mutate(dir = ifelse(str_detect(pvStr, "\\+ve"), "inc", "dec")) |>
  group_by(name, tau, dir, country, seg, pvStr) |>
  count() |>
  ungroup() |>
  mutate(n = ifelse(dir == "dec", n*-1, n))

p_colours = c("#A50021",
              "#FF6400",
              "#FFBA66",
              "#A4C171",
              "#000099",
              "#1E64FF",
              "#78BCFF",
              "#A4C171"
              )

plotDat |>
  ggplot()+
  geom_bar(aes(seg,n, fill = pvStr), stat = "identity", position = "stack")+
  geom_hline(aes(yintercept = 0))+
  scale_fill_manual(values = p_colours)+
  facet_wrap(~tau, nrow = 1)



