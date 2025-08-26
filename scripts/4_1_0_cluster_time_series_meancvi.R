library(DBI)
library(dtw)
library(dplyr)
library(tidyr)
library(stringr)
library(dtwclust)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','dtw_helpers.R'))

idx = as.numeric(commandArgs(trailingOnly = T)[1])+1

tau = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)[idx]

con = connect_to_db()

nClust = 5:75

scenarios = tribble(
  ~type,                ~tableName,         ~yname,      ~months,
  "daily_all",          "dat_daily_all",    "anom",      1:12,
  "daily_all_warm",     "dat_daily_all",    "anom",      4:9,
  "daily_all_cold",     "dat_daily_all",    "anom",      c(1:3,10:12),
  "daily_day",          "dat_daily_day",    "anom",      1:12,
  "daily_day_warm",     "dat_daily_day",    "anom",      4:9,
  "daily_day_cold",     "dat_daily_day",    "anom",      c(1:3,10:12),
  "daily_night",        "dat_daily_night",  "anom",      1:12,
  "daily_night_warm",   "dat_daily_night",  "anom",      4:9,
  "daily_night_cold",   "dat_daily_night",  "anom",      c(1:3,10:12),
  "mda8",               "dat_mda8",         "mda8_anom", 1:12,
  "mda8_warm",          "dat_mda8",         "mda8_anom", 4:9,
  "mda8_cold",          "dat_mda8",         "mda8_anom", c(1:3,10:12),
  "metric_4MDA8",       "dat_metrics",      "value",     NA,
  "metric_NDGT70",      "dat_metrics",      "value",     NA,
  "metric_SOMO35",      "dat_metrics",      "value",     NA,
  "metric_3MMDA1",      "dat_metrics",      "value",     NA,
  "metric_6MMDA1",      "dat_metrics",      "value",     NA,
  "metric_AVGMDA8",     "dat_metrics",      "value",     NA
)

datList = list()
clustList = list()
cviList = list()
clustSelect = list()
stationClustersList = list()

for(rgn in c("United States of America", "Europe")){

  for(i in 1:nrow(scenarios)){
    type = scenarios$type[i]

    # only bother with the meterics in the tau == 0.5 case - tau is not actually used for these as they are already annual, but so we dont waste time re computing them
    if(str_detect(type, "metric") & tau != 0.5){
      message(paste("[log]","Skipping Metric at non-0.5 tau",type,rgn, tau, sep = ";"))
      next
    }

    message(paste("[log]","Creating Data",type,rgn, tau, sep = ";"))

    datList[[type]] = prep_dtw_data(
      con,
      tableName = scenarios$tableName[i],
      yname = scenarios$yname[i],
      tau = tau,
      rng = rng,
      mnths = scenarios$months[[i]],
      type = type
    )

    message(paste("[log]","Clustering",type,rgn, tau, sep = ";"))

    clustList[[type]] = dtwclust::tsclust(
      series = datList[[type]],
      type = "hierarchical",
      k = nClust,
      seed = 9999,
      distance = "dtw_basic"
    )

    message(paste("[log]","Calculating CVIs",type,rgn, tau, sep = ";"))

    cviList[[type]] = purrr::map_df(clustList[[type]], cvi, type = c("Sil", "COP", "DB", "DBstar","D","CH")) |>
      # mutate(across(everything(), \(x) (x-min(x))/(max(x)-min(x)))) |>
      mutate(across(everything(), \(x) (x)/(max(x)))) |>
      mutate(nClust = nClust) |>
      pivot_longer(-nClust) |>
      mutate(
        value = case_when(
          name %in% c("COP", "DB", "DBstar") ~ 1-value, # minimise, so 1 -
          TRUE ~ value
        ),
        type = type
      )

    message(paste("[log]","Selecting Clusters",type,rgn, tau, sep = ";"))
    # Select mean of best number of clusters from the 6 CVIs
    clustSelect[[type]] = cviList[[type]] |>
      group_by(name, type) |>
      filter(value == max(value)) |>
      filter(nClust == min(nClust)) |>
      group_by(type) |>
      summarise(nClust = mean(nClust)) |>
      pull(nClust) |>
      floor()


    stationClustersList[[type]] = tibble(cluster = clustList[[type]][[which(nClust == clustSelect[[type]])]]@cluster,
                                         station_id = names(clustList[[type]][[which(nClust == clustSelect[[type]])]]@cluster),
                                         type = type,
                                         tau = tau
    )

  }

  stationClusters = bind_rows(stationClustersList)

  if(rgn == "United States of America"){
    rgn_path = "usa"
  }else{
    rgn_path = "europe"
  }

  outObjects = list(
    clustMartix_datList.RDS = datList,
    clustList.RDS = clustList,
    cviList.RDS = cviList,
    stationClusters.csv = stationClusters
  )

  dirOut = data_path("cluster_meancvi", "data",paste(rgn_path, tau,sep = "_"))

  if(!dir.exists(dirOut)){
    dir.create(dirOut, recursive = T)
  }

  paths = file.path(dirOut, names(outObjects))

  for(i in 1:length(paths)){

    message(paste("[log]","writing",names(outObjects[i]),rgn, tau, sep = ";"))

    if(str_detect(names(outObjects[i]), ".csv")){
      write.csv(outObjects[[i]], paths[i], row.names = F)
    }else{
      saveRDS(outObjects[[i]], paths[i])
    }

  }

}

dbDisconnect(con, shutdown = T)
