library(DBI)
library(duckdb)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)

# Source helper functions
source(here::here('functions','connect_to_db.R'))

# Connect to the database (read-only)
con = connect_to_db()

# Select and pull from database: all the timeseries with the higher r2 values. Note this currently only contains pqr_1 and pqr_2, not qr.
reg_names = tbl(con, "reg_anom_filt") |>
  select(station_id, name, scenario_idx, reg) |>
  distinct() |>
  collect()

# Select and pull from database: qr regressions
qr_slopes = tbl(con, "qr_regressions") |>
  filter(stat == "slope",
         type == "fit")|>
  select(station_id, name, scenario_idx, tau, startYear, endYear, slope = value) |>
  collect()

# Disconnect from database
dbDisconnect(con)

# Left join the two dfs to create one df
qr_reg_slope = left_join(reg_names, qr_slopes, by = c("station_id", "name", "scenario_idx"))

# Date sequence we want to forward fill into
startYear = seq(2000, 2023, by = 1)

# Create a database of all the different combinations of station_id, scenario_idx, name, tau, and reg we want to loop over
samp_dat = qr_reg_slope |>
  select(station_id, scenario_idx, name, tau, reg) |>
  distinct()

#samp_dat = samp_dat[1,]

# Initate list
regression_slope_year <- list()

# Find the slope of each year for all rows of samp_dat and dump these into a list
for (i in 1:nrow(samp_dat)) {
  select_df <- left_join(samp_dat[i,], qr_reg_slope, by = c("station_id", "scenario_idx", "name", "tau", "reg")) |>
    select(-endYear) |>
    bind_rows(as.data.frame(startYear)) |>
    distinct(startYear, .keep_all = TRUE) |>
    arrange(startYear) |>
    fill(station_id, name, scenario_idx, tau, slope, reg) |>
    drop_na(station_id)
    #drop_na(station_id, scenario_idx, tau, slope)
  #nest(annual_slopes = c(startYear, slope))
  regression_slope_year[[length(regression_slope_year) + 1]] <- select_df
}

# Convert list to dataframe
reg_slope_year_df = regression_slope_year |>
  bind_rows()

