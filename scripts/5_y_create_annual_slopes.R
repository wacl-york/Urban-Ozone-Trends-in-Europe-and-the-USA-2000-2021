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

# Collect meta data
combined_meta = tbl(con, "combinedMeta") |>
  select(station_id, country) |>
  distinct() |>
  na.omit() |>
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

# Create segments
segTable = tribble(
  ~startYear, ~endYear, ~seg_id,
  2000, 2004, 1,
  2005, 2009, 2,
  2010, 2014, 3,
  2015, 2023, 4
)

# Create df where segments are bound
annual_slopes = qr_reg_names_slopes |>
  left_join(segTable, join_by(between(year, startYear, endYear))) |>
  select(-startYear, -endYear) |>
  left_join(combined_meta, by = "station_id") |>
  mutate(continent = "North America")

annual_slopes$continent[annual_slopes$country != "United States of America"] = "Europe"

#### NB this currently looks at ALL slopes. Need to break this down to

# Test plots
annual_slopes |>
ggplot() +
  geom_point(aes(x = seg_id, y = slope, colour = station_id))+
  facet_wrap(~name+continent, nrow = 3) +
  theme(legend.position = "none")

# Test plot
annual_slopes |>
  ggplot() +
  geom_point(aes(x = seg_id, y = slope, colour = station_id))+
  facet_wrap(~name+continent, nrow = 3) +
  theme(legend.position = "none")

# Test plot
annual_slopes |>
  filter(tau == 0.5) |>
  select(name, seg_id, continent, slope) |>
  group_by(name, seg_id, continent) |>
  ggplot() +
  geom_boxplot(aes(x = seg_id, y = slope, group = seg_id)) +
  facet_wrap(~ name + continent, nrow = 3, scales = "free_y") +
  theme(legend.position = "none") +
  labs(y = "Slope", x = "Segment ID")

# Write this to database
#dbWriteTable(con, "annual_slopes", reg_slope_year_df, overwrite = TRUE)

