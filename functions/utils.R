data_path = function(...){

  here::here(readLines(here::here("data_config.txt"), n = 1), "data2", ...)

}

connect_to_db = function(read_only = TRUE){
  con = DBI::dbConnect(duckdb::duckdb(),
                        dbdir = data_path("db.duckdb"),
                        read_only = read_only)

  return(con)
}

log_message = function(mess, station, name, type = NULL){

  message(paste("[log]",mess, station, name, type, sep = ";"))

}
