library(gt)
library(DBI)
library(cli)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))
source(here::here('functions','table_utils.R'))

con = connect_to_db()

tables = dbListTables(con)[str_detect(dbListTables(con), "piecewise_stats")]

tableNameLookup = tribble(
  ~tableName,                                 ~printName,
  "piecewise_stats_freeTau_daily_all",        "Daily mean",
  "piecewise_stats_freeTau_daily_all_cold",   "Daily mean cold season",
  "piecewise_stats_freeTau_daily_all_warm",   "Daily mean warm season",
  "piecewise_stats_freeTau_daily_day",        "Daily daytime mean",
  "piecewise_stats_freeTau_daily_day_cold",   "Daily daytime mean cold season",
  "piecewise_stats_freeTau_daily_day_warm",   "Daily datyime mean warm season",
  "piecewise_stats_freeTau_daily_night",      "Daily nighttime mean",
  "piecewise_stats_freeTau_daily_night_cold", "Daily nighttime mean cold season",
  "piecewise_stats_freeTau_daily_night_warm", "Daily nighttime mean warm season",
  "piecewise_stats_freeTau_mda8_anom_all",    "MDA8O\\textsubscript{3}",
  "piecewise_stats_freeTau_mda8_anom_cold",   "MDA8O\\textsubscript{3} cold season",
  "piecewise_stats_freeTau_mda8_anom_warm",   "MDA8O\\textsubscript{3} warm season",
  "piecewise_stats_freeTau_metrics",          "O\\textsubscript metric"

)

for(rgn in c("Europe", "United States of America")){

  cli::cli_progress_bar(name = rgn, total = length(tables))

  for(i in 1:length(tables)){

    cli::cli_progress_update()
    if(str_detect(tables[i], "metric")){

      tempDat = tbl(con, tables[i]) |>
        collect() |>
        nest_by(metric) |>
        mutate(data = data |>
                 parse_piecewise_stats() |>
                 list()) |>
        unnest(data) |>
        ungroup()

      rm_var = c("region", "name", "tau")
      spc_text = "O\\textsubscript{3}"
    }else{

      tempDat = tbl(con, tables[i]) |>
        collect() |>
        parse_piecewise_stats()

      if(str_detect(tables[i], "mda8")){
        rm_var = c("region", "name")
        spc_text = "O\\textsubscript{3}"
      }else{
        rm_var = "region"
        spc_text = "O\\textsubscript{3}, NO\\textsubscript{2} and O\\textsubscript{x}"
      }

    }

    tempDat |>
      pivot_wider(
        values_from = "n",
        names_from = "type"
      ) |>
      mutate(across(
        -any_of(c("region","tau","name","year")),
        \(x) ifelse(is.na(x), 0, x)
      )) |>
      pivot_wider(names_from = "year", values_from = c("Increasing", "Decreasing", "No Trend")) |>
      filter(region == rgn) |>
      select(-all_of(rm_var)) |>
      make_table()  |>
      as_latex() |>
      as.character() |>
      latex_tweaks(
        caption = paste0(
          tableNameLookup$printName[tableNameLookup$tableName == tables[i]],
          " trend direction for ",spc_text,
          " at sites in ",rgn,
          " for selected years. Those classed as 'No Trend' are the slopes where p > 0.33"),
        label = paste0("tab:",tables[i],"-",str_replace_all(rgn," ", "_")),
        sideways = T,
        adjustbox = F
      ) |>
    writeLines(here::here('tables','overview_tables',paste0(tables[i],"-",str_replace_all(rgn," ", "_"),".txt")))

  }
}

dbDisconnect(con, shutdown = T)
