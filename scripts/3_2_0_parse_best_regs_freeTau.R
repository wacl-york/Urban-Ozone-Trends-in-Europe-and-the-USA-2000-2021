library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','regression.R'))

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

# series_id = 1
series_id = as.numeric(commandArgs(trailingOnly = T)[1])+1

nm = valid_series$name[series_id]
stn = valid_series$station_id[series_id]

warm_months = 4:9
day_hours = 8:19

regs_to_do = min_aic_regs |>
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
  mutate(dataSource = case_when(
    str_detect(dataType, "daily_all") ~ "dat_daily_all",
    str_detect(dataType, "daily_day") ~ "dat_daily_day",
    str_detect(dataType, "daily_night") ~ "dat_daily_night",
    str_detect(dataType, "mda8_anom") ~ "dat_mda8_anom",
    str_detect(dataType, "mda8") ~ "dat_mda8",
    str_detect(dataType, "metrics") ~ "dat_metrics"
  )) |>
  arrange(dataType) |>
  left_join(scenario_type, "scenario_idx") |>
  filter(dataSource != "dat_mda8")

datList = list(
  dat_daily_all = tbl(con, "dat_daily_all") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = anom),

  dat_daily_day = tbl(con, "dat_daily_day") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = anom),

  dat_daily_night = tbl(con, "dat_daily_night") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = anom),

  dat_mda8 = tbl(con, "dat_mda8") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = mda8),

  dat_mda8_anom = tbl(con, "dat_mda8") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = mda8_anom),

  dat_metrics = tbl(con, "dat_metrics") |>
    filter(name == nm,
           station_id == stn) |>
    collect() |>
    mutate(y = value)
)

outList = list()

for(i in 1:nrow(regs_to_do)){

  dat = datList[[regs_to_do$dataSource[i]]] |>
    mutate(yr = year(date))

  dt = regs_to_do$dataType[i]

  if(regs_to_do$dataSource[i] == "dat_metrics"){
    dat = dat |>
      filter(metric == regs_to_do$metric[i])

    dt = paste0(dt, "_", regs_to_do$metric[i])

  }

  log_message(paste0("calculating - ",dt),stn, nm)


  nfree = length(outList$freeTauList[[dt]])+1

  outList$freeTauList[[dt]][[nfree]] = do_qr_aic(
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

  npiece = length(outList$piecewiseList[[dt]])+1
  outList$piecewiseList[[dt]][[npiece]] = outList$freeTauList[[dt]][[nfree]] |>
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

}

dbDisconnect(con, shutdown = T)

freeTau = map(outList$freeTauList, \(x) reduce(x, bind_rows))

if(nm == "o3"){
  freeTauMetrics = freeTau[str_detect(names(freeTau), "metrics_")]
  freeTau = freeTau[!str_detect(names(freeTau), "metrics_")]
  freeTau$reg_all_metrics = reduce(freeTauMetrics,bind_rows)
}

dirOut = data_path("piecewise","stats", "freeTau")
if(!dir.exists(dirOut)){
  dir.create(dirOut, recursive = T)
}

for(i in 1:length(freeTau)){

  type = names(freeTau)[[i]]

  log_message(paste0("writing stats - ", type), stn, nm)

  subDir = file.path(dirOut, type)

  if(!dir.exists(subDir)){
    dir.create(subDir, recursive = T)
  }

  fileName = file.path(subDir,paste0(type,"_piecewise_stats_freeTau_",stn,"_", nm, ".csv"))
  write.csv(freeTau[[i]], fileName, row.names = F)

}

piecewise = map(outList$piecewiseList, \(x) reduce(x, bind_rows))

if(nm == "o3"){
  pieceWiseMetrics = piecewise[str_detect(names(piecewise), "metrics_")]
  piecewise = piecewise[!str_detect(names(piecewise), "metrics_")]
  piecewise$reg_all_metrics = reduce(pieceWiseMetrics,bind_rows)
}

dirOut = data_path("piecewise", "data", "freeTau")

if(!dir.exists(dirOut)){
  dir.create(dirOut, recursive = T)
}

for(i in 1:length(piecewise)){

  type = names(piecewise)[[i]]

  log_message(paste0("writing data - ", type), stn, nm)

  subDir = file.path(dirOut, type)

  if(!dir.exists(subDir)){
    dir.create(subDir, recursive = T)
  }

  fileName = file.path(subDir,paste0(type,"_piecewise_data_freeTau_",stn,"_", nm, ".csv"))
  write.csv(piecewise[[i]], fileName, row.names = F)

}
