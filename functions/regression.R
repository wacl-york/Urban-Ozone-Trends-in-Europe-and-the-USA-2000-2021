mbfun = function(formula,
                 data,
                 tau # the quantile to do qr on.
){

  n = nrow(data) # length of timeseries
  b = ceiling(n^0.25) # block length
  nblocks = ceiling(n/b)

  # create a list of sequences that are the length of a block, and overlap with the subsequent block for b-1 of the values
  # the values in the sequences correspond to row ids in the data. eventually used to select values from the data
  # the list contains enough elements such that the last sequence in the list has its final value == n
  # (n-b+1, but the values in the sequence run to b-1, so the final value == n)
  blocks = lapply(seq_len(n-b+1), function(i) seq(i, i+b-1))

  # create a vector of randomly sampled block ids - i.e. values between 1 and nblocks.
  bn = sample(1:length(blocks), nblocks, replace = T)

  # blocks[bn] randomly sample (with replacement) the blocks of data.
  # unlisting this produces a vector that will rearrange the "blocks" of the timeseries

  samp_data = data[unlist(blocks[bn]),]
  mod = quantreg::rq(formula, data = samp_data, tau = tau)

  coef(mod)
}

clean_qr_coef = function(fit){
  fit |>
    dplyr::as_tibble() |>
    dplyr::mutate(stat = c("intercept", "slope")) |>
    tidyr::pivot_longer(-stat, names_to = "tau") |>
    dplyr::mutate(tau = tau |>
                    stringr::str_remove("tau= ") |>
                    as.numeric())
}

do_qr_sub = function(dat,
                     years = c(2000,2021),
                     calcError = TRUE){

  tau = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

  if(nrow(dat) > 0){

    tempDat = dat |>
      dplyr::filter(dplyr::between(lubridate::year(date), min(years), max(years)))


    tempFit = tryCatch({
      foo <- quantreg::rq(y~x, data = tempDat, tau = tau) |>
        coef()
    },
    error = function(e){list(e)}
    )

    cleanTempFit = tempFit |>
      clean_qr_coef() |>
      dplyr::mutate(type = "fit")

    if(calcError){
      tempSe = tryCatch({
        bs_results <- mbfun(y~x,
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
        dplyr::mutate(type = "se")

      tempPv = tryCatch({ (pt(q = abs(tempFit/tempSe),
                              df = nrow(tempDat)-2,
                              lower.tail = F)*2)},
                        error = function(e){list(e)}
      )

      cleanTempPv = tempPv |>
        clean_qr_coef() |>
        dplyr::mutate(type = "pv")
    }else{

      cleanTempSe = NULL
      cleanTempPv = NULL

    }

    tempStat = dplyr::bind_rows(
      cleanTempFit,
      cleanTempSe,
      cleanTempPv
    )

    tempStat

  }else{
    NULL
  }
}

determine_scenarios = function(minDate, maxDate){

  create_scenario = function(cp1, dateRange){

    diffs = dateRange-cp1

    dateRange2 = dateRange[abs(diffs) > 365*5] # no change points within 5 years of each other

    tibble(cp1 = cp1, cp2 = dateRange2)

  }

  max_date_range = seq.Date(lubridate::ymd("2002-01-01"), lubridate::ymd("2022-01-01"), "12 month") # we know our time series can't start before 2000, so start possible scenarios from 2002

  scenarios_all = purrr::map_df(max_date_range, ~create_scenario(.x, max_date_range)) |> # 2 change points
    dplyr::bind_rows(dplyr::tibble(cp1 = max_date_range, cp2 = NA)) |> # 1 change point
    dplyr::mutate(scenario_idx = row_number())

  test = scenarios_all |>
    rowwise() |>
    dplyr::filter(
      (min(c(lubridate::year(cp1), lubridate::year(cp2)), na.rm = T) > (lubridate::year(minDate)+1)), # no change points within the first two years
      (max(c(lubridate::year(cp1), lubridate::year(cp2)), na.rm = T) < (lubridate::year(maxDate)-1))  # nor the last two years
    ) |>
    dplyr::bind_rows(dplyr::tibble(cp1 = NA, cp2 = NA, scenario_idx = nrow(scenarios_all)+1)) # 0 change points

}

do_qr_aic = function(dat, cp1, cp2, calcError = TRUE){

  switch(
    sum(!is.na(cp1), !is.na(cp2))+1,
    {
      parts = dplyr::tribble(
        ~startYear, ~endYear,
        lubridate::year(min(dat$date)), lubridate::year(max(dat$date))
      ) |>
        dplyr::mutate(piece = dplyr::row_number())
    },
    {
      parts = dplyr::tribble(
        ~startYear, ~endYear,
        lubridate::year(min(dat$date)), lubridate::year(cp1),
        lubridate::year(cp1), lubridate::year(max(dat$date))
      ) |>
        dplyr::mutate(piece = dplyr::row_number())
    },
    {
      parts = dplyr::tribble(
        ~startYear, ~endYear,
        lubridate::year(min(dat$date)), lubridate::year(cp1),
        lubridate::year(cp1), lubridate::year(cp2),
        lubridate::year(cp2), lubridate::year(max(dat$date))
      ) |>
        dplyr::mutate(piece = dplyr::row_number())
    }
  )

  regList = list()
  for(i in 1:nrow(parts)){
    regList[[i]] = do_qr_sub(
      dat,
      years = c(parts$startYear[i], parts$endYear[i]),
      calcError = calcError
    ) |>
      dplyr::mutate(
        startYear = parts$startYear[i],
        endYear = parts$endYear[i])
  }

  reg = dplyr::bind_rows(regList)

  dat = dat |>
    dplyr::mutate(yr = lubridate::year(date)) |>
    dplyr::left_join(parts, by = dplyr::join_by(between(yr, startYear, endYear, bounds = "[)")))

  tau = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

  if(nrow(parts) == 1){ # QR

    mod = quantreg::rq(y ~ x, tau = tau, data = dat)

  }else{ # PQR

    mod = quantreg::rq(y ~ x + piece + piece*x, tau = tau, data = dat)

  }

  reg |>
    dplyr::left_join(
      dplyr::tibble(aic = AIC(mod),
                    tau = tau),
      by = "tau")

}

