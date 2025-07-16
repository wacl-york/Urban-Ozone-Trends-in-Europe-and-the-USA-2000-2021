library(DBI)
library(here)
library(dplyr)
library(mirai)
library(purrr)

daemons(31)

reg_dirs = list.dirs("/mnt/scratch/projects/chem-cmde-2019/toar/data/regressions_mean", recursive = F)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db_mean.duckdb"),
                read_only = FALSE)

if(dbExistsTable(con, "qr_regressions_mean")){
  dbRemoveTable(con, "qr_regressions_mean")
}

# cli::cli_progress_bar(total = length(reg_dirs))

for(i in 1:length(reg_dirs)){

  # cli::cli_progress_update()

  if(i %% 10 == 0){
    print(i)
  }

  reg_filenames =  system(paste0('find ', reg_dirs[i] ,' -type f -name "*.csv"'), intern = TRUE)

  dat = map(reg_filenames, in_parallel(\(x){

    read.csv(file = x) |>
      dplyr::mutate(station_id = as.character(station_id))
  }  )) |>
    bind_rows()

  dbWriteTable(con, "qr_regressions_mean", dat, append = T)
}

dbDisconnect(con, shutdown = T)

daemons(0)
