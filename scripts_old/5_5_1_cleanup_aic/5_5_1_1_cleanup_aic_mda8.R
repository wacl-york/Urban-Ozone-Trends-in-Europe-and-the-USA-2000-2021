library(DBI)
library(here)
library(dplyr)
library(data.table)
library(parallel)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# pieceFiles = system('find /users/wsd500/scratch/toar/piecewise -type f -name "*.csv"', intern = T)

tableName = "aic_mda8"

if(dbExistsTable(con, tableName)){
  dbRemoveTable(con, tableName)
}

header = c(
  "scenario_idx" = "numeric",
  "reg" = "character",
  "name" = "character",
  "station_id" = "character",
  "npiece" = "numeric",
  "aic" = "numeric"
)

dbCreateTable(con, tableName, fields = header)

print("beginning file read")

dbExecute(con, paste0("COPY",
                      tableName,
                      "FROM '",
                      here(readLines(here("data_config.txt"),n = 1),"data",tableName),
                      "*.csv' WITH (NULLSTR 'NA')"))

dbDisconnect(con, shutdown = T)

