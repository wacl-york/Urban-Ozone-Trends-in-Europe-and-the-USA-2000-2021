library(DBI)
library(here)
library(dplyr)
library(data.table)
library(parallel)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# pieceFiles = system('find /users/wsd500/scratch/toar/piecewise -type f -name "*.csv"', intern = T)

if(dbExistsTable(con, "aic")){
  dbRemoveTable(con, "aic")
}

header = c(
  "scenario_idx" = "numeric",
  "reg" = "character",
  "name" = "character",
  "station_id" = "character",
  "npiece" = "numeric",
  "aic" = "numeric"
)

dbCreateTable(con, "aic", fields = header)

print("begining file read")

dbExecute(con, "COPY aic FROM '/users/wsd500/scratch/toar/aic/*.csv' WITH (NULLSTR 'NA')")

dbDisconnect(con, shutdown = T)

