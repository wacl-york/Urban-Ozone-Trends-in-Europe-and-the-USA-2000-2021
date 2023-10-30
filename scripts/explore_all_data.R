library(DBI)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"), read_only = FALSE)


ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2022-01-01 00:00"), "month"))

datMonth = tbl(con,"all_data") |>
  mutate(date = floor_date(date,"month")) |>
  group_by(date, name, station_id, region) |>
  summarise(value = median(value, na.rm = T)) |>
  ungroup() |>
  mutate(m = month(date)) |>
  ungroup()

coverage = tbl(con, "coverage") |>
  filter(perc >= 90) |>
  collect()

# assign into a vector as the db interface doesn't like filtering to a column here.
# I think using local(coverage$station_id) would achieve the same goal, but I'm not sure so I'll stick with this
stationsWithCoverage = coverage$station_id

dat = datMonth |>
  filter(station_id %in% stationsWithCoverage) |>
  arrange(station_id, date) |>
  collect() |>
  nest_by(name, station_id) |>
  mutate(data = ts |>
           left_join(data, by = "date") |>
           filter(!is.na(m)) |>
           mutate(season = lm(seasonModel,data = data) |>
                    predict()) |>
           list()) |>
  unnest(data)

dat |>
  group_by(region, name) |>
  select(region, name, station_id) |>
  distinct() |>
  count()

dat |>
  ggplot()+
  geom_line(aes(date, value-season, colour = station_id))+
  guides(colour = "none")+
  facet_grid(name~region, scales = "free_y")+
  AQVisR::AQvis_plotTheme()

plotly::ggplotly()



# Filter for sites that have both no2 and o3 so we can do ox --------------

datBoth = dat |>
  ungroup() |>
  nest_by(station_id,name) |>
  nest_by(station_id) |>
  mutate(nspec = nrow(data)) |>
  filter(nspec == 2) |> # if `data` after nesting has 2 rows, it means there is both no2 and o3 data
  unnest(data) |>
  unnest(data) |>
  pivot_wider(values_from = c("value","clim")) |>
  mutate(value_ox = value_no2+value_o3,
         clim_ox = clim_no2+clim_o3) |> # is it ok to get the climate average for ox by summing the climare average for NO2 and O3? TODO
  pivot_longer(cols = c(contains("clim"), contains("value")), names_sep = "_", names_to = c("type","name")) |>
  pivot_wider(names_from = "type")

datBoth |>
  group_by(region, name) |>
  select(region, name, station_id) |>
  distinct() |>
  count()

datBoth |>
  ggplot()+
  geom_line(aes(date, value, colour = station_id))+
#  geom_smooth(aes(date, value))+
  guides(colour = "none")+
  facet_grid(name~region, scales = "free_y")+
  AQVisR::AQvis_plotTheme()


# -------------------------------------------------------------------------



dbDisconnect(con, shutdown = T)
