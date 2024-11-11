connect_to_db = function(read_only = TRUE){
  con = DBI::dbConnect(duckdb::duckdb(),
                        dbdir = here::here(readLines(here::here("data_config.txt"),n = 1),"data","db.duckdb"),
                        read_only = read_only)

  return(con)
}
