library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("scratch/TOAR_paper/data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# join regeression scenarios to piecewise so we can recover piece
# grouping from the change points
piecewise_mda8 = tbl(con, "piecewise_mda8") |>
  filter(tau == 0.5) |>
  left_join(
    tbl(con, "regression_scenarios"),
    by = c("station_id","name","scenario_idx")
  )

# select relevant columns from anom data
mda8_o3 = tbl(con, "mda8_o3") |>
  select(date, station_id, name, mda8, x)

# used to recover piecewise grouping
# just set arbitrarily in the future/past
mindate = ymd("1990-01-01")
maxdate = ymd("2030-01-01")

# deal with QRs and Loess spearately due to scenario_idx
# put anom and QRs (normal and piecewise) in the same data.frame
piecewise_mda8 = piecewise_mda8 |>
  left_join(mda8_o3,
            by = c("x", "station_id", "name")) |>
  mutate(
    cp1 = ifelse(is.na(cp1), mindate, cp1),
    cp2 = ifelse(is.na(cp2), maxdate, cp2), # replace cp2 NAs with max date
    piece = case_when(
      date < cp1 ~ "1", # 1 change point
      date >= cp1 & date < cp2 ~ "2",
      date >= cp2 ~ "3"),
  ) |>
  select(-cp1, -cp2) |> # remove the filled cp1, cp2
  pivot_longer(c(piecewise), names_to = "reg") |>
  left_join( # restore the unfilled in cp1, cp2 so we can rename the qr vs piecewise reg column
    tbl(con, "regression_scenarios"),
    by = c("station_id","name","scenario_idx")
  ) |>
  mutate(reg = case_when(reg == "piecewise" & is.na(cp1) & is.na(cp2) ~ "qr",
                         reg == "piecewise" & is.na(cp2) & !is.na(cp1) ~ "pqr_1",
                         TRUE ~ "pqr_2"
  )) |>
  select(x, date,station_id, name, scenario_idx,piece, mda8, reg, value) |>
  filter(!is.na(date))

# calculate the R^2 between the tau=0.5 QRs (normal and piecewise) and the mda8 values timeseries
mda8_piecewise_r2 = piecewise_mda8 |>
  select(-x, -date) |>
  group_by(station_id, name, reg, scenario_idx) |>
  summarise(r2 = cor(mda8, value)^2) |>
  ungroup()

# Put anom and loess in the same data.frame
mda8_loess = mda8_o3 |>
  left_join(
    tbl(con, "loess_mda8"),
    by = c("x", "station_id", "name")
  ) |>
  select(x, date, station_id, name, mda8, value = loess) |>
  mutate(reg = "loess")

# calcaulte r2 between anom and loess
mda8_loess_r2 = mda8_loess |>
  select(-x, -date) |>
  group_by(station_id, name, reg) |>
  summarise(r2 = cor(mda8, value)^2) |>
  ungroup()

# get anom, QRs (normal and piecewise) and loess in the same data.frame
regs = union_all(piecewise_mda8, mda8_loess) |>
  collect()

dbWriteTable(con, "reg_mda8",regs, overwrite = T)

rm(regs)
gc()

# get r2 from all regressions in the same data.frame
regs_r2 = union_all(mda8_piecewise_r2, mda8_loess_r2) |>
  collect()

dbWriteTable(con, "reg_mda8_r2",regs_r2, overwrite = T)

dbDisconnect(con, shutdown = T)
