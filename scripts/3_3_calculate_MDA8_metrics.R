library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(dbplyr)

####### SETUP ############################################################################################

# Connect to database
con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

# Select MDA8 table - warm season only (April - September)
# Convert all European values to ppb for consistency
mda8_warm = tbl(con, "mda8_o3") |>
  filter(month(local_date) %in% c(4,5,6,7,8,9),
         name == "o3") |>
  collect() |>
  mutate(mda8_ppb = ifelse(str_detect(timezone, "America"), mda8, mda8 / 1.96))

# Select MDA8 table - all seasons retained.
# Convert all European values to ppb for consistency
mda8_all = tbl(con, "mda8_o3") |>
  filter(name == "o3") |>
  collect() |>
  mutate(mda8_ppb = ifelse(str_detect(timezone, "America"), mda8, mda8 / 1.96))

##########################################################################################################

### Calculate 4MDA8 ###

### The 8-hour running mean for a particular hour is calculated on the mixing ratios
### for that hour plus the following 7 hours between the hours of 0700 and 2300 local time.
### For a given day the greatest of these 17 values is the daily maximum 8-hour average ozone.
### Based on all warm-season daily maximum 8-hour average ozone values the 4th highest value is selected.
### Fleming et al., 2018

# Group data by year, station_id and timezone, then arrange in descending order and select the 4th highest by group.
metric_4MDA8 = mda8_warm |>
  mutate(year = year(local_date)) |>
  select(year, station_id, timezone, mda8 = mda8_ppb) |>
  na.omit() |>
  group_by(year, station_id, timezone) |>
  arrange(desc(mda8), .by_group = T) |>
  mutate(rank = row_number()) |>
  filter(rank == 4) |>
  select(-rank)

##########################################################################################################

### Calculate NDGT70 ###

### Annual count of number of days of MDA8 > 70 ppb.

# Create two columns for count when mda8 > 70, and when mda8 <= 70.
# Use the absence of counts in the "YES" column to keep stations which never exceed limit.
metric_NDGT70 = mda8_all |>
  mutate(year = year(local_date)) |>
  select(year, station_id, timezone, mda8 = mda8_ppb) |>
  mutate(exceeded_limit = case_when(mda8 > 70 ~ "YES",
                   .default = "NO")) |>
  group_by(year, station_id, timezone, exceeded_limit) |>
  summarise(NDGT70 = n(), .groups = "drop") |>
  ungroup() |>
  pivot_wider(values_from = NDGT70, names_from = exceeded_limit) |>
  mutate(NDGT70 = case_when(is.na(YES) ~ 0,
                            .default = YES)) |>
  collect()


##########################################################################################################

### Calculate SOMO35 ###

### The sum of the positive differences between the daily maximum 8-h ozone mixing ratio and the cut-off
### value set at 35 ppb (70 µg m–3) calculated for all days in a year.

metric_SOMO35 = mda8_all |>
  mutate(year = year(local_date)) |>
  mutate(SOMO35 = mda8_ppb-35,
         SOMO35 = ifelse(SOMO35<0, 0, SOMO35)) |>
  select(year, station_id, timezone, SOMO35) |>
  group_by(year, station_id, timezone) |>
  summarise_all("sum")


##########################################################################################################

### Calculate 3MMDA1 ###

### Annual maximum of the three-month average of daily 1-hour maximum ozone value.
### Three month running mean values calculated were assigned to the mid-point of the 3 month period.

# Calculate the daily max hourly o3 value
o3_daily_max = tbl(con, "all_data") |>
  filter(name == "o3") |>
  mutate(day = as.Date(date)) |>
  select(day, station_id, value) |>
  group_by(day, station_id) |>
  filter(value == max(value, na.rm = T)) |>
  distinct() |>
  rename(daily_max = value)

# Define sites of interest as we are pulling from "all data" table
sites_of_interest = unique(mda8_all$station_id)

# Calculate 3MMDA1
# When there are 68 or more non-NA values (75% coverage), group by station_id and order by day, then calculate
# the mean of the daily maximum values over a centred 3-month (90 day) windows (44 days before, that day, then 45 days after)
# If there is not enough data to calculate, then NA.
metric_3MMDA1 = o3_daily_max |>
  filter(station_id %in% sites_of_interest) |>
  mutate(
    o3_3MMDA1 = sql("
      CASE
        WHEN COUNT(daily_max) OVER (
          PARTITION BY station_id
          ORDER BY day
          ROWS BETWEEN 44 PRECEDING AND 45 FOLLOWING
        ) >= 68
        THEN AVG(daily_max) OVER (
          PARTITION BY station_id
          ORDER BY day
          ROWS BETWEEN 44 PRECEDING AND 45 FOLLOWING
        )
        ELSE NULL
      END
    ")
    ) |>
  filter(day > "2000-02-13") |>
  mutate(year = year(day)) |>
  ungroup() |>
  select(year, station_id, o3_3MMDA1) |>
  group_by(year, station_id) |>
  filter(o3_3MMDA1 == max(o3_3MMDA1, na.rm = T)) |>
  distinct() |>
  collect()
  # Remove the first 44 days because there isn't a full 3-month rolling window.

# test =  metric_3MMDA1 |>
#   left_join(metric_4MDA8)

##########################################################################################################

### Calculate AVGMDA8 ###

### 6-month warm season mean of MDA8.

metric_AVGMDA8 = mda8_warm |>
  mutate(year = year(local_date)) |>
  select(year, station_id, timezone, mda8 = mda8_ppb) |>
  group_by(year, station_id, timezone) |>
  summarise_all("mean")

##########################################################################################################

dbDisconnect(con)
