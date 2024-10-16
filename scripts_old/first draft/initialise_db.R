library(DBI)
library(here)
library(dplyr)
library(stringr)
library(saqgetr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"),
                read_only = FALSE)

tables = dbListTables(con)


# flags -------------------------------------------------------------------

flags = toarR:::list_controlled_vocabulary("Data Flag") |>
  select(-V2) |>
  rename(flag_value = V1,
         flag_name = V3) |>
  mutate(flag_value = as.numeric(flag_value))

dbWriteTable(con, "flags",flags, overwrite = TRUE)

# Load US data ------------------------------------------------------------

dirUS = here("data","us")

filesUS = tibble(path = list.files(dirUS,full.names = T, recursive = T, pattern = ".csv")) |>
  filter(!str_detect(path, "requests")) |>
  mutate(name = word(path, -2,-2, sep = "/") |>
           str_remove("raw_"),
         timeseries_id = word(path, -1,-1, sep = "/") |>
           str_remove(".csv"))

if("data" %in% tables){
  existingIds = tbl(con,"data") |>
    select(timeseries_id) |>
    distinct() |>
    collect()
}

toDoUS = filesUS |>
  filter(!timeseries_id %in% existingIds$timeseries_id)

for(i in 1:nrow(toDoUS)){

  temp = read.csv(toDoUS$path[i]) |>
    tibble() |>
    mutate(date = ymd_hms(datetime, tz = "UTC"),
           name = toDoUS$name[i]) |>
    select(date, value, name, flag_name = flags, timeseries_id) |>
    left_join(collect(tbl(con,"flags")), by = "flag_name") |>
    select(-flag_name) |>
    filter(between(date,ymd_hm("2000-01-01 00:00"),ymd_hm("2021-12-31 00:00")))

  if(!"data" %in% tables){
    dbWriteTable(con, "data",temp)
    tables = dbListTables(con)
  }else{
    dbAppendTable(con, "data",temp)
  }

}


# Load EEA Data -----------------------------------------------------------

dirEEA = here("data","eea","raw")

eeaProcesses = get_saq_processes()

eeaMeta = tibble(path = list.files(dirEEA,full.names = F, pattern = ".csv")) |>
  filter(path != "_.csv") |>
  mutate(site = word(path, 1, 1, sep = "_"),
         hasNO2 = str_detect(word(path,2,2,sep = "_"), "no2"),
         hasO3 = str_detect(word(path,2,2,sep = "_"), "o3")) |>
  left_join(get_saq_sites(), by = "site")

dbWriteTable(con, "eeaMeta", eeaMeta, overwrite = T)


for(i in 1:nrow(eeaMeta)){
  temp = read.csv(here(dirEEA, eeaMeta$path[i])) |>
    tibble() |>
    mutate(date = ymd_hms(date),
           date_end = ymd_hms(date_end)) |>
    left_join(select(eeaProcesses,process, site, period), by = c("process","site")) |>
    mutate(span =  as.numeric(date)-as.numeric(date_end)) |> # fill in some gaps in the period data
    mutate(period = ifelse(is.na(period) & between(span, -3650, -3550),"hour",period)) |>
    filter(period == "hour") |>
    select(-date_end, -summary,-period) |>
    rename(station_id = site,
           flag_value = validity)

  if(nrow(temp) == 0){
    next
  }

  if(!"eeaData" %in% dbListTables(con)){
    dbWriteTable(con, "eeaData",temp)
  }else{
    dbAppendTable(con, "eeaData", temp)
  }


}

# -------------------------------------------------------------------------


dbDisconnect(con, shutdown = T)
