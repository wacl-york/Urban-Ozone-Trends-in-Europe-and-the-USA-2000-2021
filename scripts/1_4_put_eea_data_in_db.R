library(sf)
library(DBI)
library(lutz)
library(here)
library(dplyr)
library(saqgetr)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db(FALSE)

start = "2000-01-01"
end = "2023-12-31"

eeaMeta = get_saq_sites() |>
  filter(site_area == "urban",
         date_start <= ymd(start),
         date_end >= ymd(end))

eeaMetaSf = eeaMeta |>
  st_as_sf(coords = c("longitude", "latitude"))

eeaMeta$timezone = tz_lookup(eeaMetaSf, method = "accurate")

dbWriteTable(con, "eeaMeta",eeaMeta, overwrite = T)

if("eeaData" %in% dbListTables(con)){

  completed = tbl(con, "eeaData") |>
    select(station_id) |>
    distinct() |>
    collect()


}else{
  completed = tribble(
    ~station_id
  )
}

toDo = eeaMeta |>
  filter(!site %in% completed$station_id)

for(i in 1:nrow(toDo)){

  eeaProcesses = get_saq_processes()

  temp = get_saq_observations(toDo$site[i],variable = c("o3","no2"),
                              start = start,
                              end = end) |>
    left_join(select(eeaProcesses,process, site, period), by = c("process","site")) |>
    mutate(span =  as.numeric(date)-as.numeric(date_end)) |> # fill in some gaps in the period data
    mutate(period = ifelse(is.na(period) & between(span, -3650, -3550),"hour",period)) |>
    filter(period == "hour") |>
    select(-date_end, -summary,-period) |>
    rename(station_id = site,
           flag_value = validity)


  if(!"eeaData" %in% dbListTables(con)){
    dbWriteTable(con, "eeaData",temp)
  }else{
    dbAppendTable(con, "eeaData", temp)
  }

}

dbDisconnect(con, shutdown = TRUE)
