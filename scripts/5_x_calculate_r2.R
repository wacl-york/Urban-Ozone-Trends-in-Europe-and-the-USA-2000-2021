library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines("data_config.txt",n = 1),"data","db.duckdb"), read_only = FALSE)


# join regeression scenarios to piecewise so we can recover piece
# grouping from the change points
piecewise = tbl(con, "piecewise") |>
  filter(tau == 0.5) |>
  left_join(
    tbl(con, "regression_scenarios"),
    by = c("station_id","name","scenario_idx")
  )

# single qr data
qr = tbl(con,"qr_stat") |>
  filter(type == "fit",
         tau == 0.5) |>
  pivot_wider(names_from = "stat")

# select relevant columns from anom data
anom = tbl(con, "monthly_anom") |>
  select(date, station_id, name, anom, x)

# used to recover piecewise grouping
# just set arbitrarily in the future
maxdate = ymd("2030-01-01")

# separate piecewise from qr and loess due as scenario_idx only applies to piecewise
anom_piecewise = piecewise |>
  left_join(anom,
            by = c("x", "station_id", "name")) |>
  mutate(cp2 = ifelse(is.na(cp2), maxdate, cp2), # replace cp2 NAs with max date
         piece = case_when(date < cp1 ~ "1",
                           date >= cp1 & date < cp2 ~ "2",
                           date >= cp2 ~ "3")
  ) |>
  select(x, date,station_id, name, scenario_idx,piece, anom, piecewise) |>
  pivot_longer(c(piecewise), names_to = "reg")

anom_piecewise_r2 = anom_piecewise |>
  select(-x, -date) |>
  group_by(station_id, name, reg, scenario_idx) |>
  summarise(r2 = cor(anom, value)^2) |>
  ungroup()

anom_qr_loess = anom |>
  left_join(
    tbl(con, "loess"),
    by = c("x", "station_id", "name")
  ) |>
  left_join(qr,
            by = c("station_id","name")
  ) |>
  mutate(qr = (slope*x)+intercept,
         scenario_idx = 0) |> # calculate qr
  select(x, date, station_id, name, anom, qr, loess, scenario_idx) |>
  pivot_longer(c(qr, loess), names_to = "reg")

anom_qr_loess_r2 = anom_qr_loess |>
  select(-x, -date) |>
  group_by(station_id, name, reg, scenario_idx) |>
  summarise(r2 = cor(anom, value)^2) |>
  ungroup()

regs = union_all(anom_piecewise, anom_qr_loess) |>
  collect()

regs_r2 = union_all(anom_piecewise_r2, anom_qr_loess_r2) |>
  collect()

dbWriteTable(con, "reg_anom",regs, overwrite = T)
dbWriteTable(con, "reg_anom_r2",regs_r2, overwrite = T)

dbDisconnect(con, shutdown = T)
