library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(quantreg)
library(lubridate)

# Take bootstrap samples of the timeseries and return quantile regression coefficients.
# used to test significance of the trend in the measured timeseries.

source(here::here('functions','mbfun.R'))

clean_qr_coef = function(fit){
  fit |>
    as_tibble() |>
    mutate(stat = c("intercept", "slope")) |>
    pivot_longer(-stat, names_to = "tau") |>
    mutate(tau = tau |>
             stringr::str_remove("tau= ") |>
             as.numeric())
}

# -------------------------------------------------------------------------

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

dat = tbl(con, "monthly_anom")

tau = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

# Calculating the fit and p values

name_station = tbl(con, "name_station") |>
  collect()

tables = dbListTables(con)

if("qr_stat" %in% tables){

  complete_name_stations = tbl(con, "qr_stat") |>
    select(name, station_id) |>
    distinct() |>
    collect()

  toDo = anti_join(name_station, complete_name_stations, by = c("name", "station_id"))

}else{

  toDo = name_station

}

pb = progress::progress_bar$new(total = nrow(toDo))

for(i in 1:nrow(toDo)){

  pb$tick()

  id = toDo$station_id[i]
  nm = toDo$name[i]

  tempDat = dat |>
    filter(station_id == id,
           name == nm) |>
    arrange(date) |>
    collect()

  if(nrow(tempDat) > 0){

    tempFit = tryCatch({
      foo <- rq(anom~x, data = tempDat, tau = tau) |>
        coef()
    },
    error = function(e){list(e)}
    )

    cleanTempFit = tempFit |>
      clean_qr_coef() |>
      mutate(type = "fit")

    tempSe = tryCatch({
      bs_results <- mbfun(anom~x,
                          data = tempDat,
                          tau = tau) |>
        replicate(1000, expr = _)
      if (length(dim(bs_results)) == 3) {
        margins <- 1:2
      } else if (length(dim(bs_results)) == 2) {
        margins <- 2
      }
      apply(bs_results, margins,sd, na.rm = T)
    },
    error = function(e){list(e)}
    )

    cleanTempSe = tempSe |>
      clean_qr_coef() |>
      mutate(type = "se")

    tempPv = tryCatch({ (pt(q = abs(tempFit/tempSe),
                            df = nrow(tempDat)-2,
                            lower.tail = F)*2)},
                      error = function(e){list(e)}
    )

    cleanTempPv = tempPv |>
      clean_qr_coef() |>
      mutate(type = "pv")

    tempStat = bind_rows(
      cleanTempFit,
      cleanTempSe,
      cleanTempPv
    ) |>
      mutate(station_id = id,
             name = nm)

    if("qr_stat" %in% tables){

      dbAppendTable(con, "qr_stat", tempStat)

    }else{

      dbWriteTable(con, "qr_stat", tempStat)
      tables = dbListTables(con)

    }

  }else{
    next
  }
}

dbDisconnect(con, shutdown = T)
