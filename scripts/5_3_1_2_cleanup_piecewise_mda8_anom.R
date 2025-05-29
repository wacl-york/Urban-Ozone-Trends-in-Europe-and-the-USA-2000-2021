library(DBI)
library(here)
library(dplyr)
library(data.table)
library(parallel)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# pieceFiles = system('find /users/wsd500/scratch/toar/piecewise -type f -name "*.csv"', intern = T)

if(dbExistsTable(con, "piecewise_mda8_anom")){
  dbRemoveTable(con, "piecewise_mda8_anom")
}

header = c(
  "x" = "numeric",
  "tau" = "numeric",
  "piecewise" = "numeric",
  "scenario_idx" = "numeric",
  "station_id" = "character",
  "name" = "character"
)

dbCreateTable(con, "piecewise_mda8_anom", fields = header)

print("beginning file read")

dbExecute(con, "COPY piecewise_mda8_anom FROM '/users/bsn502/scratch/toar/piecewise_mda8_anom/**/*.csv' WITH (NULLSTR 'NA')")

dbDisconnect(con, shutdown = T)

│
