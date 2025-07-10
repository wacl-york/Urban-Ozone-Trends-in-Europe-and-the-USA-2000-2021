library(cli)
library(DBI)
library(here)
library(dplyr)
library(purrr)

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

tableName = "piecewise_mean"

if(dbExistsTable(con, tableName)){
  dbRemoveTable(con, tableName)
}

filePath = file.path(readLines(here("data_config.txt"),n = 1), "data", tableName)

stationDirs = list.dirs(filePath, recursive = F)

cli::cli_progress_bar(total = length(stationDirs))

for(i in 1:length(stationDirs)){

  speciesDir = list.dirs(stationDirs[i], recursive = F)

  for(j in 1:length(speciesDir)){

    files = list.files(speciesDir[j], full.names = T)

    dat = map_df(files, read.csv)

    dat$station_id = as.character(dat$station_id)

    dbWriteTable(con, tableName, dat, append = T)

  }

  cli::cli_progress_update()

}

dbDisconnect(con, shutdown = T)
