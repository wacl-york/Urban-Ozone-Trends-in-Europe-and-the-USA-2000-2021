#####################################################

### Connect to corrupt database
con = DBI::dbConnect(duckdb::duckdb(),dbdir = here::here(readLines(here::here("scratch/TOAR_paper/data_config.txt"),n = 1),"data","db_20250530_corrupt.duckdb"), read_only = FALSE)

### Or, connect to a database which has good data you want to create an RDS file from (e.g. eeaData from here as it isn't corrupt)
# con = DBI::dbConnect(duckdb::duckdb(),dbdir = here::here(readLines(here::here("data_config.txt"),n = 1),"data","db_old_use_sshfs_now.duckdb"), read_only = FALSE)

# Names of tables you wish to save as RDS files
table_names = DBI::dbListTables(con)

### Define the base directory
dat_directory = readLines(here::here("scratch/TOAR_paper/data_config.txt"), n = 1)

### For each table in the database, create an RDS file and do some error handling
for (i in table_names){
  
  tryCatch({
    tbl_rds <- dplyr::tbl(con, i) |> 
      dplyr::collect()
    
    saveRDS(tbl_rds, file = paste0(dat_directory,"data/data_tables_RDS/",i,".RDS"))
    
    message("Successfully processed: ", i)
    
  }, error = function(e) {
    message("Error processing table: ", i)
    message("Error message: ", e$message)
  })
  
}

### Disconnect from the database
DBI::dbDisconnect(con)

#####################################################
