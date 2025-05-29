library(DBI)
library(here)
library(dplyr)
library(data.table)
library(parallel)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# pieceFiles = system('find /users/wsd500/scratch/toar/piecewise -type f -name "*.csv"', intern = T)

if(dbExistsTable(con, "piecewise")){
  dbRemoveTable(con, "piecewise")
}

header = c(
  "x" = "numeric",
  "tau" = "numeric",
  "piecewise" = "numeric",
  "scenario_idx" = "numeric",
  "station_id" = "character",
  "name" = "character"
)

dbCreateTable(con, "piecewise", fields = header)

print("begining file read")

dbExecute(con, "COPY piecewise FROM '/users/wsd500/scratch/toar/piecewise/**/*.csv' WITH (NULLSTR 'NA')")

dbDisconnect(con, shutdown = T)

│
