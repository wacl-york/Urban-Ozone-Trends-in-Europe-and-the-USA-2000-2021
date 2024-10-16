library(here)
library(dplyr)
library(purrr)
library(toarR)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

stationListAllRaw = readRDS(here("data","stationList.RDS"))

stationListAll = stationListAllRaw |>
  select(-contains("ebas")) %>%
  mutate(., country = pluck(., "station", "country"),
         type_of_area = pluck(., "station", "type_of_area"),
         type = pluck(., "station", "type"),
         station_id = pluck(., "station","id"),
         station_type = interaction(type_of_area, type))

validSites = stationListAll |>
  filter(stringr::str_detect(station_type, "urban"),
         sampling_frequency == "hourly") |>
  # filter(station_type %in% c("suburban.background", "suburban.unknown","urban.background","unknown.unknown"),
  #        sampling_frequency == "hourly") |>
  select(data_start_date, data_end_date, country, station_id, station_type) |>
  filter(data_start_date > ymd("1950-01-01"),
         data_end_date < Sys.time())

firstSite = min(validSites$data_start_date) |>
  round_date("1 month")

lastSite = max(validSites$data_end_date) |>
  round_date("1 month")

nSites = tibble(date = seq(firstSite, lastSite,"1 month")) |>
  full_join(validSites, by = join_by(between(date, data_start_date, data_end_date))) |>
  mutate(hasData = ifelse(is.na(station_id), 0, 1)) |>
  select(date, country, hasData, station_type) |>
  filter(!is.na(country)) |>
  group_by(date, country, station_type) |>
  summarise(nSites = sum(hasData))

ggplot(nSites)+
  geom_point(aes(date, nSites, colour = station_type))+
  geom_vline(aes(xintercept = ymd_hm("2000-01-01 00:00")))+
  facet_wrap(~country, scales = "free_y")+
  AQVisR::AQvis_plotTheme()

nSites |>
  filter(country == "United States of America") |>
  ggplot()+
  geom_point(aes(date, nSites, colour = station_type))+
  geom_vline(aes(xintercept = ymd_hm("2000-01-01 00:00")))+
  facet_wrap(~country, scales = "free_y")+
  AQVisR::AQvis_plotTheme()

nSites |>
  filter(country %in% c("Brazil","Chile")) |>
  ggplot()+
  geom_point(aes(date, nSites, colour = station_type))+
  geom_vline(aes(xintercept = ymd_hm("2000-01-01 00:00")))+
  facet_wrap(~country, scales = "free_y")+
  AQVisR::AQvis_plotTheme()
