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

# Select the timeseries with the higher r2 values.
# Join this with the qr_regressions table which includes corresponding slopes and collect.
# Note this currently only contains pqr_1 and pqr_2, not qr.
qr_reg_names_slopes = tbl(con, "reg_anom_filt") |>
  select(station_id, name, scenario_idx, reg) |>
  distinct() |>
  left_join(
    tbl(con, "qr_regressions"),
    by = c("station_id", "name", "scenario_idx")) |>
  filter(stat == "slope",
         type == "fit") |>
  select(station_id, name, scenario_idx, tau, reg, year = startYear, slope = value) |>
  collect()

# Disconnect from database
dbDisconnect(con)

# Date sequence we want to forward fill into
year = seq(2000, 2023, by = 1)

# Create a database of all the different combinations of station_id, scenario_idx, name, tau, and reg we want to loop over
samp_dat = qr_reg_names_slopes |>
  select(station_id, scenario_idx, name, tau, reg) |>
  distinct()

# Initiate list
regression_slope_year <- list()

# Find the slope of each year for all rows of samp_dat and dump these into a list
for (i in 1:nrow(samp_dat)) {
  select_df = left_join(samp_dat[i,], qr_reg_names_slopes, by = c("station_id", "scenario_idx", "name", "tau", "reg")) |>
    bind_rows(as.data.frame(year)) |>
    distinct(year, .keep_all = TRUE) |>
    arrange(year) |>
    fill(station_id, name, scenario_idx, tau, slope, reg) |>
    drop_na(station_id)
    #drop_na(station_id, scenario_idx, tau, slope)
    #nest(annual_slopes = c(year, slope))
  regression_slope_year[[length(regression_slope_year) + 1]] = select_df
}

# Convert list to dataframe
reg_slope_year_df = regression_slope_year |>
  bind_rows()

# Write this to database
#dbWriteTable(con, "annual_slopes", reg_slope_year_df, overwrite = TRUE)

