library(DBI)
library(here)
library(toarR)
library(dplyr)
library(stringr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"),
                read_only = FALSE)

toarMeta = tbl(con,"toarMeta") |>
  filter(data_start_date <= "2000-01-01 00:00:00", # we don't need ymd_hms here the db handles the date conversion
         data_end_date >= "2021-12-31 00:00:00",   # and actually throws an error if you do use it.
         str_detect(station_type, "urban"),
         country == "United States of America") |>
  collect()


statusPath = here("data","toar","toarDB_request_status.csv")

if(!file.exists(statusPath)){
  # if for some reason we have data in the db, but we have lost the request status file
  # we can look at the ids in the db and try to rebuild it
  if("toarData" %in% dbListTables(con)){

    completed = tbl(con, "toarData") |>
      select(timeseries_id) |>
      distinct() |>
      collect()

  }else{
    completed = tibble(timeseries_id = NULL)
  }

  requestStatus = tibble(timeseries_id = toarMeta$timeseries_id) |>
    mutate(attempts = 0,
           status = ifelse(timeseries_id %in% completed$timeseries_id,"completed","not-requested"))

  write.csv(requestStatus, statusPath, row.names = F)
}

requestStatus = read.csv(statusPath) |>
    tibble()

toDo = requestStatus |>
  filter(status == "not-requested")

for(i in 1:nrow(toDo)){

  ts_id = toDo$timeseries_id[i]

  for(j in 1:5){

    # inc attempts
    requestStatus$attempts[requestStatus$timeseries_id == ts_id] = requestStatus$attempts[requestStatus$timeseries_id == ts_id]+1

    # make request
    response = build_query("data",
                           id = ts_id) |>
      query_toar_database()

    # if something other than a tibble() is returned
    # that means the request was not status 200,
    # so try again unless its the last attempt.
    if(!"tbl_df" %in% class(response)){
      if(j == 5){
        requestStatus$status[requestStatus$ts_id == ts_id] = "failed"
        write.csv(requestStatus, statusPath, row.names = F)
      }

      next
    }

    # If the request returns no data
    # log and move onto the next country
    if(nrow(response) == 0){
      requestStatus$status[requestStatus$ts_id == ts_id] = "no-data"
      write.csv(requestStatus, statusPath, row.names = F)

      break
    }

    # Otherwise we have a successful request, save it, log it and move on
    # Format the data and store it in the db

    dat = response |>
      mutate(date = ymd_hms(datetime)) |>
      select(-datetime) |>
      relocate(date) |>
      filter(between(date, ymd_hms("2000-01-01 00:00:00"), ymd_hms("2021-12-31 00:00:00")))

    if("toarData" %in% dbListTables(con)){
      dbAppendTable(con, "toarData", dat)
    }else{
      dbWriteTable(con,"toarData",dat)
    }

    requestStatus$status[requestStatus$timeseries_id == ts_id] = "completed"
    write.csv(requestStatus, statusPath, row.names = F)

    break
  }

}

dbDisconnect(con, shutdown = T)
