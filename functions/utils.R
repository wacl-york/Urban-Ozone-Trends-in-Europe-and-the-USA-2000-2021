data_path = function(...){

  here::here(readLines(here::here("data_config.txt"), n = 1), "data2", ...)

}

connect_to_db = function(read_only = TRUE){
  con = DBI::dbConnect(duckdb::duckdb(),
                        dbdir = data_path("db.duckdb"),
                        read_only = read_only)

  dbExecute(con, "LOAD spatial")

  return(con)
}

log_message = function(mess, station, name, type = NULL){

  message(paste("[log]",mess, station, name, type, sep = ";"))

}

read_log = function(path){
  lines = readLines(path)
  lines = lines[grep("\\[log\\]",x = lines)]

  stringr::str_split(lines, ";") |>
    do.call(rbind, args = _) |>
    as.data.frame() |>
    setNames(c("log","message","station_id","name", "type")) |>
    dplyr::mutate(type = ifelse(type == "", NA, type))

}

get_scenario_types = function(){

  con = connect_to_db()

  on.exit(dbDisconnect(con, shutdown = T))

  tbl(con, "regression_scenarios") |>
    select(cp1, cp2, scenario_idx) |>
    distinct() |>
    collect() |>
    arrange(scenario_idx) |>
    mutate(scenarioType = case_when(
      (!is.na(cp1) & !is.na(cp2)) ~ "PQR_2",
      (!is.na(cp1) & is.na(cp2)) ~ "PQR_1",
      (is.na(cp1) & is.na(cp2)) ~ "QR"
    ))

}



combinedMetaRegion = function(con){

  dplyr::tbl(con, "combinedMeta") |>
    dplyr::mutate(region = ifelse(country == "United States of America", country, "Europe"))


}
