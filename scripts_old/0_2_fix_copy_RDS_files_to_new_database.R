library(DBI)
library(here)
library(dplyr)

# Create dataframe of data tables to write to duckdb
file_df = data.frame(full_path = list.files(file.path(readLines(here("data_config.txt"), n = 1),
                                                      "data", "data_tables_RDS"),
                                            full.names = T),
                     data_table = list.files(file.path(readLines(here("data_config.txt"), n = 1),
                                                       "data", "data_tables_RDS"),
                                             full.names = F)) |>
  mutate(data_table = gsub(".RDS", "", data_table),
         data_table = gsub("_Nov24", "", data_table))

# Create database

con = dbConnect(duckdb::duckdb(),dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db2.duckdb"), read_only = FALSE)

#file_df = file_df[1,]
# Read in and write each RDS file to the new duckdb database
for (i in 1:nrow(file_df)){

  tryCatch({

    file_rds = readRDS(file_df$full_path[i])

    dbWriteTable(con, name = paste0(file_df$data_table[i]), value = file_rds)

    rm(file_rds)

    message("Successfully written table: ", paste0(file_df$data_table[i]))

  }, error = function(e) {
    message("Error writing table: ", paste0(file_df$data_table[i]))
    message("Error message: ", e$message)
  })

}

dbDisconnect(con)
