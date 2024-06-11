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

qr_fun(con)

qr_fun(con,
       years = c(2000, 2010),
       tblName = "qr_stat_00_10")

qr_fun(con,
       years = c(2011, 2021),
       tblName = "qr_stat_11_21")

dbDisconnect(con, shutdown = T)
