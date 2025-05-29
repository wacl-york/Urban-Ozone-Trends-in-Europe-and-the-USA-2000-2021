library(DBI)
library(here)
library(dplyr)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

if(dbExistsTable(con, "piecewise_mda8")){
  dbRemoveTable(con, "piecewise_mda8")
}

header = c(
  "x" = "numeric",
  "tau" = "numeric",
  "piecewise" = "numeric",
  "scenario_idx" = "numeric",
  "station_id" = "character",
  "name" = "character"
)

dbCreateTable(con, "piecewise_mda8", fields = header)

print("beginning file read")

filePath = file.path(readLines(here("data_config.txt"),n = 1), "data", "piecewise_mda8","**","*.csv")

dbExecute(con, paste0("COPY piecewise_mda8 FROM '",filePath,"' WITH (NULLSTR 'NA')"))

dbDisconnect(con, shutdown = T)
