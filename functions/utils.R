data_path = function(...){

  here::here(readLines(here::here("data_config.txt"), n = 1), ...)

}

connect_to_db = function(read_only = TRUE){
  con = DBI::dbConnect(duckdb::duckdb(),
                        dbdir = data_path("data2","db.duckdb"),
                        read_only = read_only)

  return(con)
}
