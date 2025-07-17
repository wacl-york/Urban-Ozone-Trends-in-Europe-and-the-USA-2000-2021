library(DBI)
library(dplyr)

source(here::here('functions','utils.R'))

con = connect_to_db()

all_data_series = tbl(con, "all_data_series") |>
  collect() |>
  arrange(name, station_id)

series_id = as.numeric(commandArgs(trailingOnly = T)[1])+1

nm = all_data_series$name[series_id]
stn = all_data_series$station_id[series_id]

checks = list(
  remove_sites = NA,
  pre_coverage_check = NA,
  qa_threshold = NA,
  qa_variablity = NA,
  qa_persistence = NA,
  post_coverage_check = NA,
  good_series = FALSE
)
