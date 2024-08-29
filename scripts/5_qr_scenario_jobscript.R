# Load libraries

library(dplyr)
library(tidyr)
library(DBI)
library(quantreg)
library(lubridate)

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
    as_tibble() |>
    mutate(stat = c("intercept", "slope")) |>
    pivot_longer(-stat, names_to = "tau") |>
    mutate(tau = tau |>
             stringr::str_remove("tau= ") |>
             as.numeric())
}

do_qr_sub = function(dat,
                     years = c(2000,2021)){

  tau = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

  if(nrow(dat) > 0){

    tempDat = dat |>
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

    tempStat

  }
}

# -------------------------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)
# args are: slurm array id, offset and fileoutput path root
# slurm array jobs are limited to max 10,000, we need to do ~ 130,000
# so run 13 array jobs, changing the offset by 10,000 each time (0, 10,000, ...)
# args[1]+ args[2] = sceanario number unique to job.

# arg three is the output path so we can test this locally

scenarioNumber = as.numeric(args[1])+as.numeric(args[2])
fileOutRoot = args[3]
#scenarios = readRDS(here(readLines("data_config.txt",n = 1),"data","regression_scenarios.RDS"))
scenarios = readRDS("/mnt/scratch/users/bsn502/TOAR/regression_scenarios.RDS")

cp1 = scenarios$cp1[scenarioNumber]
cp2 = scenarios$cp2[scenarioNumber]
idx = scenarios$scenario_idx[scenarioNumber]
id = scenarios$station_id[scenarioNumber]
nm = scenarios$name[scenarioNumber]

con = dbConnect(duckdb::duckdb(),
                dbdir = "/mnt/scratch/users/bsn502/TOAR/db.duckdb",
                read_only = TRUE)

dat = tbl(con, "monthly_anom") |>
  filter(station_id == id,
         name == nm) |>
  arrange(date) |>
  collect()

dbDisconnect(con, shutdown = T)

if(is.na(cp2)){
  # single cp case, so only need to fit two qrs
  parts = tribble(
    ~startYear, ~endYear,
    year(min(dat$date)), year(cp1),
    year(cp1), year(max(dat$date))
  )

}else{
  # this is the dual cp case so need to do three qrs
  parts = tribble(
    ~startYear, ~endYear,
    year(min(dat$date)), year(cp1),
    year(cp1), year(cp2),
    year(cp2), year(max(dat$date))
  )

}

regList = list()
for(i in 1:nrow(parts)){
  regList[[i]] = do_qr_sub(dat, c(parts$startYear[i], parts$endYear[i])) |>
    mutate(scenario_idx = idx,
           startYear = parts$startYear[i],
           endYear = parts$endYear[i])
}

regressions = bind_rows(regList)

fileOutName = paste0(paste("reg", nm, id, idx, sep = "_"), ".csv")
fileOutPath = paste0(fileOutRoot, id)

if(!dir.exists(fileOutPath)){
  dir.create(fileOutPath, recursive = TRUE)
}

write.csv(regressions,
          file.path(fileOutPath, fileOutName),
          row.names = F)
