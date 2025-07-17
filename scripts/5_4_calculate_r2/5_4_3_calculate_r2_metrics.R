library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)

source(here::here('functions','connect_to_db.R'))

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = FALSE)

# join regeression scenarios to piecewise so we can recover piece
# grouping from the change points
piecewise = tbl(con, "piecewise_metrics") |>
  filter(tau == 0.5) |>
  left_join(
    tbl(con, "mda8_metric_scenarios"),
    by = c("station_id","metric","scenario_idx")
  )

# select relevant columns from anom data
mda8_metrics = tbl(con, "mda8_metrics") |>
  mutate(
    year = as.integer(year),
    month = 1L,
    day = 1L,
    date = make_date(year, month, day)) |>
  select(date, station_id, metric, value, x)

# used to recover piecewise grouping
# just set arbitrarily in the future/past
mindate = ymd("1990-01-01")
maxdate = ymd("2030-01-01")

# deal with QRs and Loess spearately due to scenario_idx
# put anom and QRs (normal and piecewise) in the same data.frame
piecewise = piecewise |>
  left_join(mda8_metrics,
            by = c("x", "station_id", "metric")) |>
  mutate(
    cp1 = ifelse(is.na(cp1), mindate, cp1),
    cp2 = ifelse(is.na(cp2), maxdate, cp2), # replace cp2 NAs with max date
    piece = case_when(
      date < cp1 ~ "1", # 1 change point
      date >= cp1 & date < cp2 ~ "2",
      date >= cp2 ~ "3"),
  ) |>
  rename(metric_value = value) |>
  select(-cp1, -cp2) |> # remove the filled cp1, cp2
  pivot_longer(c(piecewise), names_to = "reg") |>
  left_join( # restore the unfilled in cp1, cp2 so we can rename the qr vs piecewise reg column
    tbl(con, "mda8_metric_scenarios"),
    by = c("station_id","metric","scenario_idx")
  ) |>
  mutate(reg = case_when(reg == "piecewise" & is.na(cp1) & is.na(cp2) ~ "qr",
                         reg == "piecewise" & is.na(cp2) & !is.na(cp1) ~ "pqr_1",
                         TRUE ~ "pqr_2"
  )) |>
  select(x, date,station_id, metric, scenario_idx,piece, reg, metric_value, value) |>
  filter(!is.na(date))

# calculate the R^2 between the tau=0.5 QRs (normal and piecewise) and the anomaly timeseries
piecewise_r2 = piecewise |>
  select(-x, -date) |>
  group_by(station_id, metric, reg, scenario_idx) |>
  summarise(r2 = cor(metric_value, value)^2) |>
  ungroup()


# get anom, QRs (normal and piecewise) and loess in the same data.frame
regs = piecewise |>
  collect()

dbWriteTable(con, "reg_metrics",regs, overwrite = T)

rm(regs)
gc()

# get r2 from all regressions in the same data.frame
regs_r2 = piecewise_r2 |>
  collect()

dbWriteTable(con, "reg_r2_metrics",regs_r2, overwrite = T)

dbDisconnect(con, shutdown = T)
