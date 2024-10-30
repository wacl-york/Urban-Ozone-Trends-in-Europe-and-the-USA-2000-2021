library(DBI)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)
library(stringr)
library(here)

# make a function so we can use on.exit()
read_csv_write_to_db = function(filenames){
  
  on.exit(dbDisconnect(con, shutdown = T))
  
  con = dbConnect(duckdb::duckdb(),
                  dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"),
                  read_only = FALSE)
  
  for(i in 1:length(filenames)){
    
    if(i %% 10 == 0){
      print(i)
    }
    
    temp = read.csv(filenames[i])
    
    if(!dbExistsTable(con, "qr_regressions")){
      dbWriteTable(con, "qr_regressions", temp, overwrite = TRUE)
    }else{
      dbAppendTable(con, "qr_regressions", temp)
    }
    
  }
  
}

user = system("echo $USER", intern = T)

# List location of all regression qrs
# use system to list files as list.files is very slow
reg_filenames =  system('find ~/scratch/toar/regressions/ -type f -name "*.csv"', intern = TRUE)

read_csv_write_to_db(reg_filenames)
