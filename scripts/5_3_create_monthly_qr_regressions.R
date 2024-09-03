library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(quantreg)
library(lubridate)

# Connect to database
con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines("data_config.txt",n = 1),"data","db.duckdb"), read_only = F)

# Alternative path for connecting to duckdb in Viking. Remove "here" library if running in Viking.
# con = dbConnect(duckdb::duckdb(),
#                 dbdir = "/mnt/scratch/users/bsn502/TOAR/db.duckdb",
#                 read_only = FALSE)

# Collect qr_regressions table from database
qr_regressions = tbl(con,"qr_regressions") |>
  filter(type == "fit") |>
  pivot_wider(names_from = stat, values_from = value) |>
  collect()

# Collect monthly anom table from database
monthly_anom = tbl(con,"monthly_anom") |>
  collect()

# For each row in the regressions file, filter for the appropriate "monthly_anom" data, then
# calculate the y_component of the qr regression at each timepoint to create a qr regression
# timeseries. The monthly anom regressions need to be left joined to the qr regression info
# so the correct scenario idx can be identified.

for(i in 1:nrow(qr_regressions)){

  dat = monthly_anom |>
    arrange(date) |>
    filter(station_id == qr_regressions$station_id[i],
           name == qr_regressions$name[i],
           year(date) >= qr_regressions$startYear[i] & year(date)<= qr_regressions$endYear[1]) |>
    left_join(qr_regressions[i,], by = c("station_id", "name"), relationship = "many-to-many") |>
    mutate(qr_y_component = ((slope*x) + intercept),
           qr_anom_diff = abs(qr_y_component - anom))

  # Write each iteration to the database, under the table name "monthly_qr_regressions"
  if("monthly_qr_regressions" %in% dbListTables(con)){
    dbAppendTable(con, "monthly_qr_regressions", dat)
  }else{
    dbWriteTable(con,"monthly_qr_regressions",dat)
  }

}

dbDisconnect(con, shutdown = T)

# library(ggplot2)
# For testing purposes only:
# ggplot(dat)+
#   geom_point(aes(x = x, y = qr_y_component), colour = "red")+
#   geom_point(aes(x = x, y = anom))+
#   geom_path(aes(x = x, y = qr_anom_diff), colour = "blue")
