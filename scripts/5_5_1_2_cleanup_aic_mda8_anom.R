library(DBI)
library(here)
library(dplyr)
library(data.table)
library(parallel)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# pieceFiles = system('find /users/wsd500/scratch/toar/piecewise -type f -name "*.csv"', intern = T)

if(dbExistsTable(con, "aic_mda8_anom")){
  dbRemoveTable(con, "aic_mda8_anom")
}

header = c(
  "scenario_idx" = "numeric",
  "reg" = "character",
  "name" = "character",
  "station_id" = "character",
  "npiece" = "numeric",
  "aic" = "numeric"
)

dbCreateTable(con, "aic_mda8_anom", fields = header)

print("beginning file read")

dbExecute(con, "COPY aic_mda8_anom FROM '/mnt/scratch/projects/chem-cmde-2019/toar/data/aic_mda8_anom/*.csv' WITH (NULLSTR 'NA')")

dbDisconnect(con, shutdown = T)

