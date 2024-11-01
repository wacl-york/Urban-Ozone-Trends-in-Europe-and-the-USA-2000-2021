library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

# Connect to database
con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

name_station = tbl(con, "name_station") |>
  collect()

regression_scenarios = tbl(con, "regression_scenarios") |>
  filter(cp2 > cp1 | is.na(cp2) | is.na(cp1) & is.na(cp2)) |>
  select(cp1, cp2, scenario_idx) |>
  distinct() |>
  collect()

cli::cli_progress_bar(total = nrow(regression_scenarios)*nrow(name_station))

for(i in 1:nrow(name_station)){

  dat = tbl(con, "anom") |>
    filter(station_id == !!name_station$station_id[i],
           name == !!name_station$name[i]) |>
    mutate(y = year(date))

  # this is a bit of a hack to fix the issue that the startYear and endYear lost there date information along the way
  maxEndYear = dat |>
    filter(date == max(date, na.rm = TRUE)) |>
    collect()

  maxEndYear = maxEndYear$date+months(1)

  for(j in 1:nrow(regression_scenarios)){
    cli::cli_progress_update()

    thisScenario = tbl(con, "qr_regressions") |>
      filter(
        station_id == !!name_station$station_id[i], # name x station idx == i
        name == !!name_station$name[i],  # name x station idx == i
        scenario_idx == !!regression_scenarios$scenario_idx[j],  # regression_scenario idx == j
        type == "fit"
      ) |>
      pivot_wider(names_from = "stat") |>
      mutate(startYear = make_date(startYear,1L,1L), # must be ints
             endYear = make_date(endYear, 1L, 1L),
             endYear = ifelse(endYear == max(endYear), !!maxEndYear, endYear)
      )

    # set bounds such that startYear <= y < endYear
    # regressions might overlap so at least this is consisten
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
        select(x, tau, piecewise, scenario_idx, station_id, name) |>
        collect()

      if("piecewise" %in% dbListTables(con)){
        dbAppendTable(con, "piecewise", pieceDat)
      }else{
        dbWriteTable(con,"piecewise",pieceDat)
      }
    }

  }

}

dbDisconnect(con, shutdown = T)
