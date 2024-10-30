library(DBI)
library(dplyr)
library(tidyr)
library(here)
library(quantreg)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"),
                read_only = FALSE)

qr_regressions = tbl(con,"qr_regressions") |>
  filter(type == "fit") |>
  pivot_wider(names_from = stat, values_from = value) |>
  collect()

monthly_anom = tbl(con,"monthly_anom") |>
  collect()

for(i in 1:nrow(qr_regressions)){

  dat = monthly_anom |>
    arrange(date) |>
    filter(station_id == qr_regressions$station_id[i],
           name == qr_regressions$name[i],
           year(date) >= qr_regressions$startYear[i] & year(date)<= qr_regressions$endYear[i]) |>
    left_join(qr_regressions[i,], by = c("station_id", "name"), relationship = "many-to-many") |>
    mutate(qr_y_component = ((slope*x) + intercept),
           qr_anom_diff = abs(qr_y_component - anom))

  if("monthly_qr_regressions" %in% dbListTables(con)){
    dbAppendTable(con, "monthly_qr_regressions", dat)
  }else{
    dbWriteTable(con,"monthly_qr_regressions",dat, overwrite = TRUE)
  }

}

dbDisconnect(con, shutdown = T)
