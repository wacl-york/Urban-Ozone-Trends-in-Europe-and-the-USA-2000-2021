library(DBI)
library(here)
library(dplyr)
library(data.table)
library(parallel)
library(readr)


con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# pieceFiles = system('find /users/wsd500/scratch/toar/piecewise -type f -name "*.csv"', intern = T)

tableName = "aic_metrics"

# Set the path to the folder
folder_path <- here(readLines(here("data_config.txt"),n = 1),"data",tableName)

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSV files
combined_df <- csv_files |>
  lapply(read_csv, col_types = cols(station_id = col_character())) |>
  bind_rows()

if(dbExistsTable(con, tableName)){
  dbRemoveTable(con, tableName)
}

dbWriteTable(con, "aic_metrics", combined_df, overwrite = T)

dbDisconnect(con, shutdown = T)

