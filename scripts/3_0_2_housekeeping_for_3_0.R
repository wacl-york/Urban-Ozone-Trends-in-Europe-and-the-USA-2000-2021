library(DBI)
library(purrr)
library(dplyr)

source(here::here('functions','utils.R'))

con = connect_to_db(FALSE)

regDirs = tibble(path = list.dirs(data_path("regs"), recursive = F)) |>
  mutate(type = basename(path),
         tableName = paste0("reg_all_",type))

for(i in 1:nrow(regDirs)){

  if(regDirs$type[i] == "logs"){

    logFiles = list.files(regDirs$path[i], pattern = ".err", full.names = T)

    log = map_df(logFiles, \(x) read_log(x))

    dbWriteTable(con, "log_3_0", log, overwrite = T)
    next
  }

  if(regDirs$type[i] == "metrics"){

    if(dbExistsTable(con, regDirs$tableName[i])){
      dbRemoveTable(con, regDirs$tableName[i])
    }

    dbExecute(
      con,
      glue::glue("
    CREATE TABLE {{regDirs$tableName[i]}} AS
      SELECT *
      FROM read_csv('{{regDirs$path[i]}}/*.csv',
        columns = {
          'stat': 'VARCHAR',
          'tau': 'DECIMAL',
          'value': 'DECIMAL',
          'type': 'VARCHAR',
          'startYear': 'DECIMAL',
          'endYear': 'DECIMAL',
          'aic': 'DECIMAL',
          'scenario_idx': 'DECIMAL',
          'station_id': 'VARCHAR',
          'name': 'VARCHAR',
          'metric': 'VARCHAR'
        },
        nullstr = ['NA','-Inf', 'Inf'])", .open = "{{", .close = "}}", sep = "")
    )

    next
  }

  if(dbExistsTable(con, regDirs$tableName[i])){
    dbRemoveTable(con, regDirs$tableName[i])
  }

  dbExecute(
    con,
    glue::glue("
    CREATE TABLE {{regDirs$tableName[i]}} AS
      SELECT *
      FROM read_csv('{{regDirs$path[i]}}/*.csv',
        columns = {
          'stat': 'VARCHAR',
          'tau': 'DECIMAL',
          'value': 'DECIMAL',
          'type': 'VARCHAR',
          'startYear': 'DECIMAL',
          'endYear': 'DECIMAL',
          'aic': 'DECIMAL',
          'scenario_idx': 'DECIMAL',
          'station_id': 'VARCHAR',
          'name': 'VARCHAR'
        },
        nullstr = ['NA','-Inf', 'Inf'])", .open = "{{", .close = "}}", sep = "")
  )

}


dbDisconnect(con, shutdown = T)
