library(DBI)
library(purrr)
library(dplyr)

source(here::here('functions','utils.R'))

con = connect_to_db(FALSE)

types = dbListTables(con)[stringr::str_detect(dbListTables(con), "reg_all_")]

minAicRegsList = list()

scenarioTypes = tbl(con, "regression_scenarios") |>
  select(cp1, cp2, scenario_idx) |>
  distinct() |>
  collect() |>
  arrange(scenario_idx) |>
  mutate(scenarioType = case_when(
    (!is.na(cp1) & !is.na(cp2)) ~ "PQR_2",
    (!is.na(cp1) & is.na(cp2)) ~ "PQR_1",
    (is.na(cp1) & is.na(cp2)) ~ "QR"
  ))


for(i in 1:length(types)){

  if(types[i] == "reg_all_metrics"){

    minAicRegsList[[i]] = tbl(con, types[i]) |>
      filter(stat == "slope",
             type == "fit") |>
      left_join(scenarioTypes, by = "scenario_idx", copy = TRUE) |>
      group_by(tau, station_id, name, metric, scenarioType) |>
      filter(scenarioType != "PQR_2") |>  # not allowing 2 change points in the metrics as data is annual
      filter(aic == max(aic, na.rm = T)) |>
      filter(scenario_idx == min(scenario_idx, na.rm = T)) |>
      ungroup() |>
      select(tau, scenario_idx, station_id, name, metric, scenarioType, aic) |>
      distinct() |>
      collect() |>
      mutate(dataType = types[i])

  }else{
    minAicRegsList[[i]] = tbl(con, types[i]) |>
      filter(stat == "slope",
             type == "fit")  |>
      left_join(scenarioTypes, by = "scenario_idx", copy = TRUE) |>
      group_by(tau, station_id, name, scenarioType) |>
      filter(aic == max(aic, na.rm = T)) |>
      filter(scenario_idx == min(scenario_idx, na.rm = T)) |>
      ungroup() |>
      select(tau, scenario_idx, station_id, name, scenarioType, aic) |>
      distinct() |>
      collect() |>
      mutate(dataType = types[i])
  }

}

minAicRegs = bind_rows(minAicRegsList)

dbWriteTable(con, "min_aic_regs", minAicRegs, overwrite = TRUE)

dbDisconnect(con, shutdown = T)
