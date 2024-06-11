
qr_fun = function(con,
                  years = c(2000, 2021),
                  tblName = "qr_stat"
){

  tau = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

  # Calculating the fit and p values

  name_station = tbl(con, "name_station") |>
    collect()

  tables = dbListTables(con)

  if(tblName %in% tables){

    complete_name_stations = tbl(con, "qr_stat") |>
      select(name, station_id) |>
      distinct() |>
      filter(between(year(date), min(years), max(years))) |>
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

      tempDat = tempDat |>
        dplyr::filter(dplyr::between(lubridate::year(date), min(years), max(years)))


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

      if(tblName %in% tables){

        dbAppendTable(con, tblName, tempStat)

      }else{

        dbWriteTable(con, tblName, tempStat)
        tables = dbListTables(con)

      }

    }else{
      next
    }
  }
}
