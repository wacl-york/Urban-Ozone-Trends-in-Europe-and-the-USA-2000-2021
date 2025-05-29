library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

# make function so we can use on.exit


make_piecewise = function(array_id, user){

  # Connect to database
  con = dbConnect(duckdb::duckdb(),
                  dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

  on.exit(dbDisconnect(con, shutdown = T))

  name_station = tbl(con, "name_station") |>
    collect()

  name = name_station$name[array_id]
  station_id = name_station$station_id[array_id]

  outDir = file.path(readLines(here("data_config.txt"),n = 1),'data','piecewise_mda8_anom',station_id,name)

  if(!dir.exists(outDir)){
    dir.create(outDir, recursive = T)
  }

  dat = tbl(con, "anom") |>
    filter(station_id == !!station_id,
           name == !!name) |>
    mutate(y = year(date))

  regression_scenarios = tbl(con, "regression_scenarios") |>
    filter(cp2 > cp1 | is.na(cp2) | is.na(cp1) & is.na(cp2)) |>
    select(cp1, cp2, scenario_idx) |>
    distinct() |>
    collect()

  # this is a bit of a hack to fix the issue that the startYear and endYear lost there date information along the way
  maxEndYear = dat |>
    filter(date == max(date, na.rm = TRUE)) |>
    collect()

  maxEndYear = maxEndYear$date+months(1)

  for(j in 1:nrow(regression_scenarios)){

    print(paste(j, "/", nrow(regression_scenarios)))

    regression_scenario = regression_scenarios$scenario_idx[j]

    fileOut = file.path(outDir,  paste0(paste(station_id, name, regression_scenario, sep = "_"), ".csv"))

    thisScenario = tbl(con, "qr_regressions_mda8_anom") |>
      filter(
        station_id == !!station_id,
        name == !!name,
        scenario_idx == !!regression_scenario,
        type == "fit"
      ) |>
      pivot_wider(names_from = "stat") |>
      mutate(startYear = make_date(startYear,1L,1L), # must be ints
             endYear = make_date(endYear, 1L, 1L),
             endYear = ifelse(endYear == max(endYear), !!maxEndYear, endYear)
      )

    # set bounds such that startYear <= y < endYear
    # regressions might overlap so at least this is consistent
    pieceDat = left_join(
      thisScenario,
      dat,
      join_by(
        between(y$date,
                x$startYear,
                x$endYear,
                bounds = "[)"),
        "name",
        "station_id")) |>
      collect()

    if(nrow(pieceDat) == 0){
      next
    }else{
      pieceDat = pieceDat |>
        mutate(piecewise = ((slope*x) + intercept)) |>
        arrange(x) |>
        select(x, tau, piecewise, scenario_idx, station_id, name)

      write.csv(pieceDat, fileOut, row.names = F)
    }

  }

}

user = system("echo $USER", intern = T)

args = commandArgs(trailingOnly = TRUE)
array_id = as.numeric(args[1])+1
make_piecewise(array_id, user)



