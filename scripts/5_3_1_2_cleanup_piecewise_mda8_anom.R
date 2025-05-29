library(DBI)
library(here)
library(dplyr)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

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

filePath = file.path(readLines(here("data_config.txt"),n = 1), "data", "piecewise_mda8_anom","**","*.csv")

dbExecute(con, paste0("COPY piecewise FROM '",filePath,"' WITH (NULLSTR 'NA')"))

dbDisconnect(con, shutdown = T)
