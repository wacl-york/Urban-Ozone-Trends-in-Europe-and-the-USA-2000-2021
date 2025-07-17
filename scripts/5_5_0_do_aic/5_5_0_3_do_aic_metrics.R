library(DBI)
library(here)
library(dplyr)
library(tidyr)

do_aic = function(array_id, user){

  dirOut = file.path(here(readLines(here("data_config.txt"),n = 1),"data","aic_metrics"))

  if(!dir.exists(dirOut)){
    dir.create(dirOut)
  }


  con = dbConnect(duckdb::duckdb(),
                  dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

  on.exit(dbDisconnect(con, shutdown = T))

  nm_stn = tbl(con, "reg_r2_metrics") |>
    select(station_id, metric) |>
    collect() |>
    distinct()

  fileOut = file.path(dirOut, paste0("aic_metrics_",nm_stn$station_id[array_id],"_",nm_stn$metric[array_id],".csv"))

  dat = tbl(con, "reg_metrics") |>
    filter(station_id == !!nm_stn$station_id[array_id],
           metric == !!nm_stn$metric[array_id],
           reg != "loess") |> # cannot calculate logLik for LOESS
    collect() |>
    nest_by(scenario_idx, reg, metric, station_id) |>
    mutate(npiece = length(unique(data$piece)),
           ones = length(data$piece[data$piece == 1]),
           twos = length(data$piece[data$piece == 2])
           )

  listAic = list()

  listAic$qr = tryCatch({ # mainly to catch the odd QR timeout, which doesnt really affect the downstream analysis
    dat |>
      filter(reg == "qr") |>
      mutate(
        mod = list(quantreg::rq(metric_value ~ x, tau = 0.5, data = data))
      )},
    error = function(e){
      NULL
    })

  listAic$pqr = tryCatch({
    dat |>
      filter(reg %in% c("pqr_1","pqr_2"),
             npiece > 1,
             ones > 1,
             twos > 1) |>
      mutate(
        mod = list(quantreg::rq(metric_value ~ x + piece + piece*x, tau = 0.5, data = data))
      )},
    error = function(e){
      NULL
    })

  datAic = listAic |>
    bind_rows() |>
    mutate(
      aic = AIC(mod)
    ) |>
    ungroup() |>
    select(-data, -mod, -ones, -twos)

  write.csv(datAic, fileOut, row.names = F)

}

user = system("echo $USER", intern = T)

args = commandArgs(trailingOnly = TRUE)
array_id = as.numeric(args[1])+1

do_aic(array_id, user)

