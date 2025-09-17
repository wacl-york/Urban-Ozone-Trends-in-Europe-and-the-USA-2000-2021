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
  mutate(avg = ifelse(str_detect(type, "mda8"), "MDA8", "Daily Mean"),
         type = ifelse(type == "mda8", "mda8_all", type) |>
           str_remove("mda8_") |>
           str_remove("daily_") |>
           str_remove("all_"))

dbDisconnect(con, shutdown = T)

colours = c(
  "All" = "black",
  "Cold" = "#1F78B4",
  "Warm" = "#FF7F00",
  "Day Cold" = "#FB9A99",
  "Day Warm" = "#E31A1C",
  "Night Cold" = "#CAB2D6",
  "Night Warm" = "#6A3D9A",
  "Night"= "#B2DF8A",
  "Day" = "#33A02C"
)

g1 = dat |>
  mutate(
    type = case_when(
      type == "all" ~ "All",
      type == "cold" ~ "Cold",
      type == "warm" ~ "Warm",
      type == "day" ~ "Day",
      type == "day_warm" ~ "Day Warm",
      type == "day_cold" ~ "Day Cold",
      type == "night" ~ "Night",
      type == "night_warm" ~ "Night Warm",
      type == "night_cold" ~ "Night Cold") |>
      factor(levels = c("All", "Warm", "Cold", "Day", "Night", "Day Warm", "Day Cold", "Night Warm", "Night Cold")),
    name = case_when(
      avg == "MDA8" ~ "MDA8O<sub>3</sub> / ppbv",
      name == "o3" ~ "O<sub>3</sub> / ppbv",
      name == "no2" ~ "NO<sub>2</sub> / ppbv") |>
      factor(levels = c("MDA8O<sub>3</sub> / ppbv", "O<sub>3</sub> / ppbv", "NO<sub>2</sub> / ppbv"))
  ) |> #pull(name) |> unique()
  filter(name %in% c("MDA8O<sub>3</sub> / ppbv", "O<sub>3</sub> / ppbv", "NO<sub>2</sub> / ppbv")) |>
  ggplot()+
  geom_line(aes(date, mn, colour = type))+
  scale_colour_manual(values = colours, name = "")+
  scale_x_datetime(name = "Date")+
  scale_y_continuous(name = "")+
  facet_grid(name~region, scale = "free_y", switch = "y")+
  theme_minimal()+
  theme(
    strip.text = element_markdown(size = 10),
    strip.placement = "outside")

pdf("figures/paper_figures/f01_overview.pdf", width = 7.5, height = 7.5)
print(g1)
dev.off()

# for README.md
png("figures/f01_overview.png", res = 300, width = 3000, height = 3000)
print(g1)
dev.off()

dat |>
  mutate(yr = year(date)) |>
  select(-date) |>
  filter(yr %in% c(2000,2019)) |>
  pivot_wider(values_from = mn, names_from = yr) |>
  filter(type %in% c("all", "warm"),
         name == "o3")

dat |>
  mutate(yr = year(date)) |>
  select(-date) |>
  filter(yr %in% c(2000,2019, 2020, 2022)) |>
  pivot_wider(values_from = mn, names_from = yr) |>
  filter(type %in% c("all"),
         name == "no2")
