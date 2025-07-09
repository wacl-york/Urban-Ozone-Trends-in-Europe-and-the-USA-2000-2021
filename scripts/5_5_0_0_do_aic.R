library(DBI)
library(here)
library(dplyr)
library(tidyr)

do_aic = function(array_id, user){

  dirOut = file.path("/users", user, "scratch", "toar", "aic")
  if(!dir.exists(dirOut)){
    dir.create(dirOut)
  }


  con = dbConnect(duckdb::duckdb(),
                  dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

  on.exit(dbDisconnect(con, shutdown = T))

  nm_stn = tbl(con, "reg_anom_r2") |>
    select(station_id, name) |>
    collect() |>
    distinct()

  fileOut = file.path(dirOut, paste0("aic_",nm_stn$station_id[array_id],"_",nm_stn$name[array_id],".csv"))

  dat = tbl(con, "reg_anom") |>
    filter(station_id == !!nm_stn$station_id[array_id],
           name == !!nm_stn$name[array_id],
           reg != "loess") |> # cannot calculate logLik for LOESS
    collect() |>
    nest_by(scenario_idx, reg, name, station_id) |>
    mutate(npiece = length(unique(data$piece)))

  datAic = list(
    dat |>
      filter(reg == "qr") |>
      mutate(
        mod = list(quantreg::rq(mda8 ~ x, tau = 0.5, data = data))
      ),
    dat |>
      filter(reg %in% c("pqr_1","pqr_2")) |>
      mutate(
        mod = list(quantreg::rq(mda8 ~ x + piece + piece*x, tau = 0.5, data = data))
      )) |>
    bind_rows() |>
    mutate(
      aic = AIC(mod)
    ) |>
    ungroup() |>
    select(-data, -mod)

  write.csv(datAic, fileOut, row.names = F)

}

user = system("echo $USER", intern = T)

args = commandArgs(trailingOnly = TRUE)
array_id = as.numeric(args[1])+1

do_aic(array_id, user)

