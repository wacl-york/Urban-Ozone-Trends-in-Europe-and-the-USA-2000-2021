library(DBI)
library(dplyr)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

tbl(con, "name_station") |>
  anti_join(tbl(con, "remove_sites"), c("name" = "spc", "station_id")) |>
  group_by(name) |>
  count() |>
  arrange(name) |>
  distinct()


tbl(con, "name_station") |>
  anti_join(tbl(con, "remove_sites"), c("name" = "spc", "station_id")) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, country),
            by = "station_id"
            ) |>
  mutate(country = ifelse(country == "United States of America", country, "Europe")) |>
  group_by(name, country) |>
  distinct() |>
  count() |>
  arrange(name, country)
