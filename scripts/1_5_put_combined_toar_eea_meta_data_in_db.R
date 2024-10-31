library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(lubridate)

# Connect to database, with read_only = F
con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# Collect TOAR meta information
toarMeta = tbl(con,"toarMeta") |>
  filter(str_detect(station_type, "urban"),
         country == "United States of America") |>
  select(station_id, latitude = lat, longitude = lng, country, station_type, date_start = data_start_date, date_end = data_end_date) |>
  mutate(station_id = as.character(station_id)) |>
  collect()

# Collect EEA meta information
eeaMeta = tbl(con,"eeaMeta") |>
  select(station_id = site, latitude, longitude, country, station_type = site_type, date_start, date_end) |>
collect()

# Bind meta information
combinedMeta = bind_rows(toarMeta, eeaMeta)

# Write combined meta information to a table in the database
dbWriteTable(con,"combinedMeta",combinedMeta)

# Disconnect from the database
dbDisconnect(con)

