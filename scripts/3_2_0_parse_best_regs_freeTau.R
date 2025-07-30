library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','regression.R'))


# Setup -------------------------------------------------------------------

con = connect_to_db()

valid_series = tbl(con, "valid_series") |>
  collect() |>
  arrange(station_id, name)

min_aic_regs = tbl(con, "min_aic_regs")

scenario_data = tbl(con, "regression_scenarios") |>
  select(cp1, cp2, scenario_idx) |>
  distinct() |>
  collect() |>
  arrange(scenario_idx)

scenario_type = scenario_data |>
  mutate(scenarioType = case_when(
    is.na(cp1) & is.na(cp2) ~ "QR",
    !is.na(cp1) & is.na(cp2) ~ "PQR_1",
    !is.na(cp1) & !is.na(cp2) ~ "PQR_2"
  )) |>
  select(scenario_idx, scenarioType)

# series_id = 2
series_id = as.numeric(commandArgs(trailingOnly = T)[1])+1

nm = valid_series$name[series_id]
stn = valid_series$station_id[series_id]

warm_months = 4:9


# Regs To Do --------------------------------------------------------------
# Determing Regs to do:
# - filter out non-anom mda8 and metrics where tau != 0.5
# - filter for the min aic across all scenarios
log_message("Determining regs to do",stn, nm)
regs_to_do = min_aic_regs |>
  filter(!(str_detect(dataType, "mda8") & !str_detect(dataType, "mda8_anom"))) |>
  filter(name == nm,
         station_id == stn) |>
  collect() |>
  mutate(drop = (!is.na(metric) & tau != 0.5)) |> # only bother with 0.5 tau for the metrics
  filter(!drop) |>
  group_by(station_id, name, dataType, tau, metric) |>
  filter(aic == min(aic, na.rm = T)) |>
  ungroup() |>
  group_by(scenario_idx, station_id, name, dataType, metric) |>
  summarise(tau = c(tau) |>
              list()) |>
  ungroup() |>
  left_join(scenario_data, "scenario_idx") |>
  arrange(dataType) |>
  left_join(scenario_type, "scenario_idx")

# Get releveant data. A table per dataType
log_message("Getting Data",stn, nm)
datList = list(

  reg_all_daily_all = tbl(con, "dat_daily_all") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = anom),

  reg_all_daily_all_cold = tbl(con, "dat_daily_all") |>
    filter(name == nm,
           station_id == stn,
           !month(date) %in% warm_months) |>
    collect() |>
    mutate(y = anom),

  reg_all_daily_all_warm = tbl(con, "dat_daily_all") |>
    filter(name == nm,
           station_id == stn,
           month(date) %in% warm_months) |>
    collect() |>
    mutate(y = anom),

  reg_all_daily_day = tbl(con, "dat_daily_day") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = anom),

  reg_all_daily_day_cold = tbl(con, "dat_daily_day") |>
    filter(name == nm,
           station_id == stn,
           !month(date) %in% warm_months) |>
    collect() |>
    mutate(y = anom),

  reg_all_daily_day_warm = tbl(con, "dat_daily_day") |>
    filter(name == nm,
           station_id == stn,
           month(date) %in% warm_months) |>
    collect() |>
    mutate(y = anom),

  reg_all_daily_night = tbl(con, "dat_daily_night") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = anom),

  reg_all_daily_night_cold = tbl(con, "dat_daily_night") |>
    filter(name == nm,
           station_id == stn,
           !month(date) %in% warm_months) |>
    collect() |>
    mutate(y = anom),

  reg_all_daily_night_warm = tbl(con, "dat_daily_night") |>
    filter(name == nm,
           station_id == stn,
           month(date) %in% warm_months) |>
    collect() |>
    mutate(y = anom),

  reg_all_mda8_anom_all = tbl(con, "dat_mda8") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = mda8_anom),

  reg_all_mda8_anom_cold = tbl(con, "dat_mda8") |>
    filter(name == nm,
           station_id == stn,
           !month(date) %in% warm_months) |>
    collect() |>
    mutate(y = mda8_anom),

  reg_all_mda8_anom_warm = tbl(con, "dat_mda8") |>
    filter(name == nm,
           station_id == stn,
           month(date) %in% warm_months) |>
    collect() |>
    mutate(y = mda8_anom),

  reg_all_metrics = tbl(con, "dat_metrics") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = value)
)

dbDisconnect(con, shutdown = T)

outList = list()

for(i in 1:nrow(regs_to_do)){

  dat = datList[[regs_to_do$dataType[i]]] |>
    mutate(yr = year(date)) # year is needed to assign piecewise pieces

  dt = regs_to_do$dataType[i]

  # filter for the given metric
  if(str_detect(regs_to_do$dataType[i], "metrics")){
    dat = dat |>
      filter(metric == regs_to_do$metric[i])

    dt = paste0(dt, "_", regs_to_do$metric[i])

  }

  log_message(paste0("calculating - ",dt),stn, nm)

  # calculate regressions
  nStats = length(outList$statsList[[dt]])+1
  outList$statsList[[dt]][[nStats]] = do_qr_aic(
    dat = dat,
    cp1 = regs_to_do$cp1[i],
    cp2 = regs_to_do$cp2[i],
    calcError = TRUE,
    tau = regs_to_do$tau[[i]]
  ) |>
    left_join(regs_to_do[i,] |>
                select(tau, scenario_idx) |>
                unnest(tau),
              "tau"
    ) |>
    mutate(station_id = stn,
           name = nm)

  # calcaulte model values from regression coefficents
  nData = length(outList$dataList[[dt]])+1
  outList$dataList[[dt]][[nData]] = outList$statsList[[dt]][[nStats]] |>
    filter(type == "fit") |>
    pivot_wider(names_from = "stat") |>
    left_join(
      dat,
      by = join_by(
        between(
          y$yr,
          x$startYear,
          x$endYear,
          bounds = "[)"
        ),
        station_id,
        name
      )) |>
    mutate(piecewise = ((slope*x) + intercept))

  # If this is a metric, record the name of it in the stats table
  if(str_detect(regs_to_do$dataType[i], "metrics")){
    outList$statsList[[dt]][[nStats]] = outList$statsList[[dt]][[nStats]] |>
      mutate(metric = regs_to_do$metric[i])
  }

}

statsDat = map(outList$statsList, \(x) reduce(x, bind_rows))

if(nm == "o3"){
  statsMetrics = statsDat[str_detect(names(statsDat), "metrics_")]
  statsDat = statsDat[!str_detect(names(statsDat), "metrics_")]
  statsDat$reg_all_metrics = reduce(statsMetrics,bind_rows)
}

dirOut = data_path("piecewise","stats", "freeTau")
if(!dir.exists(dirOut)){
  dir.create(dirOut, recursive = T)
}

for(i in 1:length(statsDat)){

  type = names(statsDat)[[i]]

  log_message(paste0("writing stats - ", type), stn, nm)

  subDir = file.path(dirOut, type)

  if(!dir.exists(subDir)){
    dir.create(subDir, recursive = T)
  }

  fileName = file.path(subDir,paste0(type,"_piecewise_stats_freeTau_",stn,"_", nm, ".csv"))
  write.csv(statsDat[[i]], fileName, row.names = F)

}

datDat = map(outList$dataList, \(x) reduce(x, bind_rows))

if(nm == "o3"){
  datDatMetrics = datDat[str_detect(names(datDat), "metrics_")]
  datDat = datDat[!str_detect(names(datDat), "metrics_")]
  datDat$reg_all_metrics = reduce(datDatMetrics,bind_rows)
}

dirOut = data_path("piecewise", "data", "freeTau")

if(!dir.exists(dirOut)){
  dir.create(dirOut, recursive = T)
}

for(i in 1:length(datDat)){

  type = names(datDat)[[i]]

  log_message(paste0("writing data - ", type), stn, nm)

  subDir = file.path(dirOut, type)

  if(!dir.exists(subDir)){
    dir.create(subDir, recursive = T)
  }

  fileName = file.path(subDir,paste0(type,"_piecewise_data_freeTau_",stn,"_", nm, ".csv"))
  write.csv(datDat[[i]], fileName, row.names = F)

}
