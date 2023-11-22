library(here)
library(toarR)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

# -------------------------------------------------------------------------

outputDir = here(readLines("data_config.txt",n = 1),"data","toar","stations")

countryCode = list_controlled_vocabulary("Country Code") |>
  filter(flag >= 0)

completed = tibble(path = list.files(outputDir)) |>
  mutate(cc = path |>
           str_remove("stationList_") |>
           str_remove(".RDS"))

statusPath = here(readLines("data_config.txt",n = 1),"data","toar","station_request_status.csv")

if(!file.exists(statusPath)){

  requestStatus = tibble(cc = countryCode$value) |>
    mutate(attempts = 0,
           status = ifelse(cc %in% completed$cc,"completed","not-requested"))

  write.csv(requestStatus, statusPath, row.names = F)
}

# Nambia's country code is NA so we override the na.strings here.
requestStatus = read.csv(statusPath, na.strings = "") |>
  tibble()

toDo = requestStatus |>
  filter(status == "not-requested")

for(i in 1:nrow(toDo)){

  cc = toDo$cc[i]

  for(j in 1:5){ # try the request 5 times

    # inc attempts
    requestStatus$attempts[requestStatus$cc == cc] = requestStatus$attempts[requestStatus$cc == cc]+1

    # make request
    response = build_query("search",
                           variable_id = c(5,6),
                           country = cc,
                           limit = "None",
                           sampling_frequency = "Hourly"
    ) |>
      query_toar_database()

    # if something other than a tibble() is returned
    # that means the request was not status 200,
    # so try again unless its the last attempt.
    if(!"tbl_df" %in% class(response)){
      if(j == 5){
        requestStatus$status[requestStatus$cc == cc] = "failed"
        write.csv(requestStatus, statusPath, row.names = F)
      }

      next
    }

    # If the country has no sites that fit the criteria,
    # log and move onto the next country
    if(nrow(response) == 0){
      requestStatus$status[requestStatus$cc == cc] = "no-sites"
      write.csv(requestStatus, statusPath, row.names = F)

      break
    }

    # Otherwise we have a successful request, save it, log it and move on
    # Using RDS because we end up with a nested tibble.
    filePath = here(outputDir,psate0("stationList_",cc,".RDS"))
    saveRDS(response, filePath)

    requestStatus$status[requestStatus$cc == cc] = "completed"
    write.csv(requestStatus, statusPath, row.names = F)
    break
  }


}
