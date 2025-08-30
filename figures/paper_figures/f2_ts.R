library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggtext)
library(ggplot2)
library(stringr)
library(lubridate)

source(here::here('functions','utils.R'))

con = connect_to_db()

dbListTables(con)

types = type_table() |>
  filter(tableName != "dat_metrics")

dat = map_df(
  split(types, 1:nrow(types)), ~{

    cols = c("date", "station_id", "name", "region", "value", "mda8", "metric")

    tbl(con, .x$tableName) |>
      left_join(
        combinedMetaRegion(con) |>
          select(station_id, region),
        "station_id"
      ) |>
      select(any_of(cols)) |>
      collect() |>
      filter(
        month(date) %in% .x$months[[1]]
      ) |>
      mutate(date = floor_date(date, "year")) |>
      rename_with(\(x) ifelse(x == "mda8", "value", x)) |>
      group_by(date, name, region) |>
      summarise(mn = mean(value, na.rm = T),
                .groups = "drop") |>
      mutate(type = .x$type)
  }) |>
  mutate(avg = ifelse(str_detect(type, "mda8"), "MDA8", "Daily Means"),
         type = ifelse(type == "mda8", "mda8_all", type) |>
           str_remove("mda8_") |>
           str_remove("daily_") |>
           str_remove("all_"))

colours = c(
  "all" = "black",
  "cold" = "#1F78B4",
  "warm" = "#FF7F00",
  "day_cold" = "#FB9A99",
  "day_warm" = "#E31A1C",
  "night_cold" = "#CAB2D6",
  "night_warm" = "#6A3D9A",
  "night"= "#B2DF8A",
  "day" = "#33A02C"
)

dat |>
  filter(name == "o3") |>
  ggplot()+
  geom_line(aes(date, mn, colour = type))+
  scale_colour_manual(values = colours)+
  scale_x_datetime(name = "Date")+
  scale_y_continuous(name = "Annual O3 / ppbv")+
  facet_grid(avg~region)+
  theme_minimal()+
  theme(strip.text = element_markdown(size = 10),
        strip.placement = "outside")
