library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

source(here::here('functions','utils.R'))

fileRoot = data_path("cluster_meancvi", "data")

dat = tibble(path = list.files(fileRoot, recursive = T, pattern = "stationClusters", full.names = T)) |>
  rowwise() |>
  mutate(
    region = path |>
      str_remove(fileRoot) |>
      str_split("/") |>
      pluck(1,2) |>
      word(1, sep = "_"),
    region = ifelse(region == "europe", "Europe", "United States of America"),
  ) |>
  mutate(
    data = path |>
      read.csv() |>
      tibble() |>
      mutate(station_id = as.character(station_id)) |>
      list()
  ) |>
  select(-path) |>
  unnest(data)

con = connect_to_db(read_only = FALSE)

dbWriteTable(con, "clusterTimeSeries_meancvi", dat, overwrite = TRUE)

dbDisconnect(con, shutdown = T)
