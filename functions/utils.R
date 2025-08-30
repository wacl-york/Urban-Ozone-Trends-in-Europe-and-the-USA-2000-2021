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


type_table = function(){
  tribble(
    ~type,                ~tableName,         ~yname,      ~months,
    "daily_all",          "dat_daily_all",    "anom",      1:12,
    "daily_all_warm",     "dat_daily_all",    "anom",      4:9,
    "daily_all_cold",     "dat_daily_all",    "anom",      c(1:3,10:12),
    "daily_day",          "dat_daily_day",    "anom",      1:12,
    "daily_day_warm",     "dat_daily_day",    "anom",      4:9,
    "daily_day_cold",     "dat_daily_day",    "anom",      c(1:3,10:12),
    "daily_night",        "dat_daily_night",  "anom",      1:12,
    "daily_night_warm",   "dat_daily_night",  "anom",      4:9,
    "daily_night_cold",   "dat_daily_night",  "anom",      c(1:3,10:12),
    "mda8",               "dat_mda8",         "mda8_anom", 1:12,
    "mda8_warm",          "dat_mda8",         "mda8_anom", 4:9,
    "mda8_cold",          "dat_mda8",         "mda8_anom", c(1:3,10:12),
    "metric_4MDA8",       "dat_metrics",      "value",     NA,
    "metric_NDGT70",      "dat_metrics",      "value",     NA,
    "metric_SOMO35",      "dat_metrics",      "value",     NA,
    "metric_3MMDA1",      "dat_metrics",      "value",     NA,
    "metric_6MMDA1",      "dat_metrics",      "value",     NA,
    "metric_AVGMDA8",     "dat_metrics",      "value",     NA
  )
}
