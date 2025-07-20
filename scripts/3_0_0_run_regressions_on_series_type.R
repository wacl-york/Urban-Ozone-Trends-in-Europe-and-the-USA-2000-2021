library(DBI)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','regression.R'))
tictoc::tic()
con = connect_to_db()

regression_scenarios = tbl(con, "regression_scenarios") |>
  collect() |>
  arrange(station_id, name, scenario_idx)

valid_series = tbl(con, "valid_series") |>
  collect() |>
  arrange(station_id, name)

# series_id = as.numeric(commandArgs(trailingOnly = T)[1])+1
series_id = 1056

nm = valid_series$name[series_id]
stn = valid_series$station_id[series_id]

scenarios =  regression_scenarios |>
  filter(name == nm,
         station_id == stn)

warm_months = 4:9
day_hours = 8:19


# Paths -------------------------------------------------------------------

dir_list = list(out = file.path(data_path(), "regs"))

if(!dir.exists(dir_list$out)){
  dir.create(dir_list$out)
}

path_list = list()
reg_list = list()

allTypes = tibble(type = c("daily_all", "daily_all_warm", "daily_all_cold", "daily_day", "daily_day_warm", "daily_day_cold",
                        "daily_night", "daily_night_warm", "daily_night_cold", "mda8_all", "mda8_warm", "mda8_cold",
                        "mda8_anom_all", "mda8_anom_warm", "mda8_anom_cold", "metrics")) |>
  mutate(o3Only = !str_detect(type, "daily"))

if(nm != "o3"){
  types = allTypes |>
    filter(!o3Only)
}else{
  types = allTypes
}

for(t in types$type){

  dir_list[[t]] = file.path(dir_list$out, t)
  path_list[[t]] = file.path(dir_list[[t]], paste0(t,"_", stn,"_",nm, ".csv"))
  reg_list[[t]] = list()

  if(!dir.exists(dir_list[[t]])){
    dir.create(dir_list[[t]])
  }

}

for(thisScenario in 1:nrow(scenarios)){

  scenario_idx = scenarios$scenario_idx[thisScenario]
  cp1 = scenarios$cp1[thisScenario]
  cp2 = scenarios$cp2[thisScenario]

  log_message(paste("calcualting scenario_idx",scenario_idx), stn, nm)

  # daily_all ---------------------------------------------------------------

  dat = tbl(con, "dat_daily_all") |>
    filter(
      name == nm,
      station_id == stn
    ) |>
    collect() |>
    mutate(y = anom)

  reg_list$daily_all[[thisScenario]] = dat |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )

  # daily_all_warm ----------------------------------------------------------

  reg_list$daily_all_warm[[thisScenario]] = dat |>
    filter(month(date) %in% warm_months) |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )


  # daily_all_cold ----------------------------------------------------------

  reg_list$daily_all_cold[[thisScenario]] = dat |>
    filter(!month(date) %in% warm_months) |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )

  # daily_day ---------------------------------------------------------------

  dat = tbl(con, "dat_daily_day") |>
    filter(
      name == nm,
      station_id == stn
    ) |>
    collect() |>
    mutate(y = anom)

  reg_list$daily_day[[thisScenario]] = dat |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )

  # daily_day_warm ----------------------------------------------------------
  reg_list$daily_day_warm[[thisScenario]] = dat |>
    filter(month(date) %in% warm_months) |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )


  # daily_day_cold ----------------------------------------------------------

  reg_list$daily_day_cold[[thisScenario]] = dat |>
    filter(!month(date) %in% warm_months) |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )

  # daily_night -------------------------------------------------------------

  dat = tbl(con, "dat_daily_night") |>
    filter(
      name == nm,
      station_id == stn
    ) |>
    collect() |>
    mutate(y = anom)

  reg_list$daily_night[[thisScenario]] = dat |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )

  # daily_night_warm --------------------------------------------------------

  reg_list$daily_night_warm[[thisScenario]] = dat |>
    filter(month(date) %in% warm_months) |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )

  # daily_night_cold --------------------------------------------------------

  reg_list$daily_night_cold[[thisScenario]] = dat |>
    filter(!month(date) %in% warm_months) |>
    do_qr_aic(cp1, cp2, calcError = FALSE) |>
    mutate(
      scenario_idx = scenario_idx,
      station_id = stn,
      name = nm
    )

  if(nm == "o3"){
    # mda8_all ----------------------------------------------------------------

    dat = tbl(con, "dat_mda8") |>
      filter(
        name == nm,
        station_id == stn
      ) |>
      collect() |>
      mutate(y = mda8)

    reg_list$mda8_all[[thisScenario]] = dat |>
      do_qr_aic(cp1, cp2, calcError = FALSE) |>
      mutate(
        scenario_idx = scenario_idx,
        station_id = stn,
        name = nm
      )

    # mda8_warm ---------------------------------------------------------------
    reg_list$mda8_warm[[thisScenario]] = dat |>
      filter(month(date) %in% warm_months) |>
      do_qr_aic(cp1, cp2, calcError = FALSE) |>
      mutate(
        scenario_idx = scenario_idx,
        station_id = stn,
        name = nm
      )

    # mda8_cold ---------------------------------------------------------------

    reg_list$mda8_cold[[thisScenario]] = dat |>
      filter(!month(date) %in% warm_months) |>
      do_qr_aic(cp1, cp2, calcError = FALSE) |>
      mutate(
        scenario_idx = scenario_idx,
        station_id = stn,
        name = nm
      )

    # mda8_all ----------------------------------------------------------------

    dat = tbl(con, "dat_mda8") |>
      filter(
        name == nm,
        station_id == stn
      ) |>
      collect() |>
      mutate(y = mda8_anom)

    reg_list$mda8_anom_all[[thisScenario]] = dat |>
      do_qr_aic(cp1, cp2, calcError = FALSE) |>
      mutate(
        scenario_idx = scenario_idx,
        station_id = stn,
        name = nm
      )

    # mda8_warm ---------------------------------------------------------------
    reg_list$mda8_anom_warm[[thisScenario]] = dat |>
      filter(month(date) %in% warm_months) |>
      do_qr_aic(cp1, cp2, calcError = FALSE) |>
      mutate(
        scenario_idx = scenario_idx,
        station_id = stn,
        name = nm
      )

    # mda8_cold ---------------------------------------------------------------

    reg_list$mda8_anom_cold[[thisScenario]] = dat |>
      filter(!month(date) %in% warm_months) |>
      do_qr_aic(cp1, cp2, calcError = FALSE) |>
      mutate(
        scenario_idx = scenario_idx,
        station_id = stn,
        name = nm
      )

    # metric ------------------------------------------------------------------

    dat = tbl(con, "dat_metrics") |>
      filter(
        name == nm,
        station_id == stn
      ) |>
      collect() |>
      mutate(y = value)

    metricList = list()
    metrics = dat$metric |>
      unique()

    for(i in 1:length(metrics)){

      skip = FALSE
      if(!is.na(cp1)){
        part = dat |>
          filter(date < cp1,
                 metric == metrics[i],
                 !is.na(y))

        if(nrow(part) <= 1){
          skip = TRUE
        }
      }

      if(!is.na(cp2)){
        part = dat |>
          filter(date >= cp1,
                 date < cp2,
                 metric == metrics[i],
                 !is.na(y))

        if(nrow(part) <= 1){
          skip = TRUE
        }

        part = dat |>
          filter(date >= cp2,
                 metric == metrics[i],
                 !is.na(y))

        if(nrow(part) <= 1){
          skip = TRUE
        }


      }
      if(!skip){
        metricList[[i]] = dat |>
          filter(metric == metrics[i]) |>
          do_qr_aic(cp1, cp2, calcError = FALSE) |>
          mutate(
            scenario_idx = scenario_idx,
            station_id = stn,
            name = nm,
            metric = metrics[i]
          )
      }else{
        metricList[[i]] = NULL
      }


    }

    reg_list$metrics[[thisScenario]] = bind_rows(metricList)
  }

}

reg_list_reduced = map(reg_list, \(x) reduce(x, bind_rows))


# Write -------------------------------------------------------------------

for(t in types$type){

  write.csv(reg_list_reduced[[t]], path_list[[t]], row.names = F)

}

# d/c ---------------------------------------------------------------------

dbDisconnect(con, shutdown = T)
tictoc::toc()
