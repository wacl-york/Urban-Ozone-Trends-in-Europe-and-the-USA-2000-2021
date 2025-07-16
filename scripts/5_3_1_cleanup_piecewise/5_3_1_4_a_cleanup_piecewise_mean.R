library(cli)
library(DBI)
library(here)
library(dplyr)
library(purrr)
library(mirai)
library(stringr)

# split into three array jobs, 1 per species.

spc_idx = as.numeric(commandArgs(trailingOnly = T)[1])+1

tableName = "piecewise_mean"

filePath = file.path(readLines(here("data_config.txt"),n = 1), "data", tableName)

files = tibble(path = list.files(filePath, recursive = T, full.names = T)) |>
  mutate(fileName = basename(path),
         species = str_remove(fileName, ".csv") |>
           word(2, sep = "_")) |>
  filter(species == c("o3", "no2", "ox")[spc_idx])

daemons(32)

dat = map(files,
          in_parallel(
            \(x){
              vroom::vroom(file = x) |>
                dplyr::mutate(station_id = as.character(station_id))
            }
          )
) |>
  bind_rows()

daemons(0)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db_mean.duckdb"), read_only = FALSE)

dbWriteTable(con, tableName, dat, append = T)

dbDisconnect(con, shutdown = T)
