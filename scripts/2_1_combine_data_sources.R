library(DBI)
library(here)
library(dplyr)
library(toarR)
library(saqgetr)

insert_tbl_into_db = function(con, query, table){
  queryText = dbplyr::db_sql_render(query$src$con, query, table) |>
    as.character()

  if("all_data" %in% dbListTables(con)){
    dbExecute(con, paste0("INSERT INTO ",table," (",queryText,")"))
  }else{
    dbExecute(con, paste0("CREATE TABLE ",table," AS (", queryText,")"))
  }
}

# -------------------------------------------------------------------------


con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines("data_config.txt",n = 1),"data","db.duckdb"),
                read_only = FALSE)

# Lookup existing sites ---------------------------------------------------

if("all_data" %in% dbListTables(con)){
  added = tbl(con, "all_data") |>
    select(station_id) |>
    distinct() |>
    mutate(station_id = as.character(station_id))

  dbWriteTable(con, "all_data_stations", collect(added), overwrite = TRUE)

}else{
  temp = tribble(
    ~station_id
  ) |>
    mutate(station_id = as.character(station_id))

  dbWriteTable(con, "all_data_stations", temp,overwrite = TRUE)

  added = tbl(con, "all_data_stations")
}

# TOAR --------------------------------------------------------------------

dbWriteTable(con, "toarFlags", list_controlled_vocabulary("Data Flag"), overwrite = TRUE)

toarFlags = tbl(con, "toarFlags") |>
  select(-value) |>
  mutate(flag = as.numeric(flag))

toarMeta = tbl(con, "toarMeta") |>
  select(timeseries_id,
         station_id,
         station_type,
         name = variable_name,
         lat,
         lng)

toarData = tbl(con, "toarData") |>
  left_join(toarFlags, by = c("flags" = "name")) |>
  filter(flag <= 7) |>
  left_join(toarMeta, by = "timeseries_id") |>
  select(date,
         station_id,
         station_type,
         name,
         value,
         lat,
         lng) |>
  mutate(station_id = as.character(station_id)) |>
  anti_join(added, "station_id")


if(nrow(toarData) > 0){
  insert_tbl_into_db(con, toarData, "all_data")
}

# EEA ---------------------------------------------------------------------

eeaMeta = tbl(con, "eeaMeta") |>
  mutate(station_type = paste(site_area, site_type, sep = ".")) |>
  select(station_id = site,
         lat = latitude,
         lng = longitude,
         station_type)

eeaData = tbl(con, "eeaData") |>
  filter(flag_value == 1) |>
  left_join(eeaMeta, by = "station_id") |>
  select(date,
         station_id,
         station_type,
         name = variable,
         value,
         lat,
         lng) |>
  anti_join(added, "station_id")

if(nrow(eeaData) > 0 ){
  insert_tbl_into_db(con, eeaData, "all_data")
}


# -------------------------------------------------------------------------


dbDisconnect(con, shutdown = TRUE)
