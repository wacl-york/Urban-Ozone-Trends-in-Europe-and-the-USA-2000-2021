library(here)
library(toarR)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

country_code = paste0(get_base_url(),"controlled_vocabulary/Country%20Code") |>
  httr::GET() |>
  httr::content() |>
  data.table::rbindlist() |>
  tibble() |>
  filter(V1 >= 0)

completed = list.files(here("data","stations"), pattern = ".RDS") |>
  word(2, sep = "_") |>
  str_remove(".RDS")


toDo = country_code |>
  filter(V1 > max(country_code$V1[country_code$V2 %in% completed])) # we'll for the remaining sites that have and ID greater than those we downloaded
# then we don't keep trying countries that return no results

for(i in 1:nrow(toDo)){

  cc = toDo$V2[i]

  response = build_query("search",
          #               type = "Background",
           #              type_of_area = "Urban",
                         #data_start_date = "2010-12-31 23",
                         variable_id = 5,
                         country = cc,
                         limit="None") |>
    query_toar_database()

  if(nrow(response) == 0){
    next
  }else{
    stationList = response |>
      dplyr::mutate(across(contains("date"),~ymd_hms(.x)))

    saveRDS(stationList, here("data","stations",paste0("stationList_",cc,".RDS")))
    }
}

toBind = list.files(here("data","stations"), pattern = ".RDS")


stationListAll = map_df(here("data","stations",toBind),
                        ~.x |>
                          readRDS() |>
                          select(-any_of("original_units")))

saveRDS(stationListAll,here("data","stationList.RDS"))



# Get NO2 from the US only ------------------------------------------------


response = build_query("search",
                       variable_id = 6,
                       country = "US",
                       limit="None") |>
  query_toar_database()

stationListUSNO2 = response |>
  dplyr::mutate(across(contains("date"),~ymd_hms(.x)))

saveRDS(stationListUSNO2, here("data","stations_no2","stationList_US.RDS"))








