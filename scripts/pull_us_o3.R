library(here)
library(dplyr)
library(purrr)
library(toarR)
library(stringr)
library(lubridate)

updateLog = function(requestStatus, status, attempt, path){
  requestStatus$status[requestStatus$station_id == thisID] = status
  requestStatus$attempts[requestStatus$station_id == thisID] = attempt
  write.csv(requestStatus, path, row.names = F)

  requestStatus

}

requestStatusPath = here("data","us","requests.csv")

stationListAllRaw = readRDS(here("data","stationList.RDS"))

maxAttempt = 5
# Filter for US Urban Sites -----------------------------------------------

stationListAll = stationListAllRaw |>
  select(-contains("ebas")) %>%
  mutate(., country = pluck(., "station", "country"),
         type_of_area = pluck(., "station", "type_of_area"),
         type = pluck(., "station", "type"),
         station_id = pluck(., "station","id"),
         station_type = interaction(type_of_area, type))

stationUS = stationListAll |>
  filter(data_start_date <= ymd_hm("2000-01-01 00:00"),
         data_end_date >= ymd_hm("2021-12-31 00:00"),
         sampling_frequency == "hourly",
         country == "United States of America",
         str_detect(station_type, "urban")
  )


# Create or load the request status ---------------------------------------

if(!file.exists(requestStatusPath)){

  requestStatus = tibble(station_id = unique(stationUS$id)) |>
    mutate(status = "not_requested",
           attempts = 0)

  write.csv(requestStatus, requestStatusPath, row.names = F)

}else{
  requestStatus = read.csv(requestStatusPath) |>
    tibble()
}

# Check that the number of requests matches the number of stations --------

if(nrow(requestStatus) != nrow(stationUS)){
  stop(paste0("Total stations (", nrow(requestStatus),") does not equal number of requests (", nrow(stationUS),")"))
}

# Loop over non-completed requests ----------------------------------------

toDo = requestStatus |>
  filter(status == "not_requested")

for(i in 1:nrow(toDo)){

  thisID = toDo$station_id[i]

  for(attempt in 1:maxAttempt){
    response = build_query("data",
                           id = thisID) |>
      query_toar_database()

    if("tbl_df" %in% class(response)){ # if the request is successful, save the data and move to next site

      filename = paste0(thisID, ".csv")

      write.csv(response, here("data","us","raw",filename))

      requestStatus = updateLog(requestStatus, "completed", attempt, requestStatusPath)

      break # break attempt loop
    }else{ # if the request was unsuccesful

      statusID = tryCatch({ # try and get the status code
        response |>
          httr::status_code()
      },
      error = function(e){e})

      if("error" %in% class(statusID)){ # if the status code can't be obtained

        requestStatus = updateLog(requestStatus, paste0("error: ",statusID), attempt, requestStatusPath)

        break # break attempt loop

      }

      if(statusID == 502){ # if API returns a bad gateway
        if(attempt == maxAttempt){ # if this is the last attempt, log and continue to next site

          requestStatus = updateLog(requestStatus, "failed", attempt, requestStatusPath)

          break # break attempt loop
        }else{ # if there are attempts left, try again
          next
        }

      } else{# if some other status is returned

        requestStatus = updateLog(requestStatus, paste0("unknown_status: ",statusID), attempt, requestStatusPath)
        break # break attempt loop
      }
    }

  }

}


