library(DBI)
library(dplyr)
library(purrr)
library(stringr)

source(here::here('functions','utils.R'))

con = connect_to_db(FALSE)

dirTable = tibble(dirs = list.dirs(data_path("piecewise","stats", "freeTau"), recursive = F)) |>
  mutate(dirName = basename(dirs),
         tableName = str_replace(dirName, "reg_all", "piecewise_stats_freeTau")
  )

cli::cli_progress_bar(name = "stats", total = nrow(dirTable))
for(i in 1:nrow(dirTable)){

  cli::cli_progress_update()

  tn = dirTable$tableName[i]

  if(dbExistsTable(con, tn)){
    dbRemoveTable(con, tn)
  }

  if(str_detect(tn, "metrics")){

    dbExecute(
      con,
      glue::glue("
        CREATE TABLE {{tn}} AS
          SELECT *
          FROM read_csv('{{dirTable$dirs[i]}}/*.csv',
            columns = {
              'stat': 'VARCHAR',
              'tau': 'DECIMAL',
              'value': 'DECIMAL(22,16)',
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

  }else{

    dbExecute(
      con,
      glue::glue("
        CREATE TABLE {{tn}} AS
          SELECT *
          FROM read_csv('{{dirTable$dirs[i]}}/*.csv',
            columns = {
              'stat': 'VARCHAR',
              'tau': 'DECIMAL',
              'value': 'DECIMAL(22,16)',
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

}

dirTable = tibble(dirs = list.dirs(data_path("piecewise","data", "freeTau"), recursive = F)) |>
  mutate(dirName = basename(dirs),
         tableName = str_replace(dirName, "reg_all", "piecewise_data_freeTau")
  )

cli::cli_progress_bar(name = "data", total = nrow(dirTable))
for(i in 1:nrow(dirTable)){

  cli::cli_progress_update()

  tn = dirTable$tableName[i]

  if(dbExistsTable(con, tn)){
    dbRemoveTable(con, tn)
  }

  if(str_detect(tn, "metrics")){

    dbExecute(
      con,
      glue::glue("
        CREATE TABLE {{tn}} AS
          SELECT *
          FROM read_csv('{{dirTable$dirs[i]}}/*.csv',
            columns = {
              'tau': 'DECIMAL',
              'type': 'VARCHAR',
              'startYear': 'DECIMAL',
              'endYear': 'DECIMAL',
              'aic': 'DECIMAL',
              'scenario_idx': 'DECIMAL',
              'station_id': 'VARCHAR',
              'name': 'VARCHAR',
              'intercept': 'DECIMAL(22,16)',
              'slope': 'DECIMAL(22,16)',
              'date': 'TIMESTAMP',
              'timezone': 'VARCHAR',
              'value': 'DECIMAL',
              'metric': 'VARCHAR',
              'x': 'DECIMAL',
              'y': 'DECIMAL',
              'yr': 'DECIMAL',
              'piecewise': 'DECIMAL(22,16)'
            },
            nullstr = ['NA','-Inf', 'Inf'])", .open = "{{", .close = "}}", sep = "")
    )

  }else{

    if(str_detect(tn, "mda8")){

      dbExecute(
        con,
        glue::glue("
        CREATE TABLE {{tn}} AS
          SELECT *
          FROM read_csv('{{dirTable$dirs[i]}}/*.csv',
            columns = {
              'tau': 'DECIMAL',
              'type': 'VARCHAR',
              'startYear': 'DECIMAL',
              'endYear': 'DECIMAL',
              'aic': 'DECIMAL',
              'scenario_idx': 'DECIMAL',
              'station_id': 'VARCHAR',
              'name': 'VARCHAR',
              'intercept': 'DECIMAL(22,16)',
              'slope': 'DECIMAL(22,16)',
              'x': 'DECIMAL',
              'date': 'TIMESTAMP',
              'timezone': 'VARCHAR',
              'mda8': 'DECIMAL',
              'mda8_anom': 'DECIMAL',
              'y': 'DECIMAL',
              'yr': 'DECIMAL',
              'piecewise': 'DECIMAL(22,16)'
            },
            nullstr = ['NA','-Inf', 'Inf'])", .open = "{{", .close = "}}", sep = "")
      )

    }else{

      dbExecute(
        con,
        glue::glue("
        CREATE TABLE {{tn}} AS
          SELECT *
          FROM read_csv('{{dirTable$dirs[i]}}/*.csv',
            columns = {
              'tau': 'DECIMAL',
              'type': 'VARCHAR',
              'startYear': 'DECIMAL',
              'endYear': 'DECIMAL',
              'aic': 'DECIMAL',
              'scenario_idx': 'DECIMAL',
              'station_id': 'VARCHAR',
              'name': 'VARCHAR',
              'intercept': 'DECIMAL(22,16)',
              'slope': 'DECIMAL(22,16)',
              'date': 'TIMESTAMP',
              'x': 'DECIMAL',
              'timezone': 'VARCHAR',
              'value': 'DECIMAL',
              'anom': 'DECIMAL',
              'y': 'DECIMAL',
              'yr': 'DECIMAL',
              'piecewise': 'DECIMAL(22,16)'
            },
            nullstr = ['NA','-Inf', 'Inf'])", .open = "{{", .close = "}}", sep = "")
      )

    }
  }

}

dbDisconnect(con, shutdown = T)
