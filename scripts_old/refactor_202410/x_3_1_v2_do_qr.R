library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(quantreg)
library(lubridate)

source(here::here('functions','mbfun2.R'))

# -------------------------------------------------------------------------
configPath = here("data_config.txt")
con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(configPath,n = 1),"data","db.duckdb"), read_only = FALSE)

ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2021-12-31 00:00"), "month"))

datMonth = tbl(con,"all_data") |>
  mutate(date = floor_date(date,"month")) |>
  group_by(date, name, station_id, station_type) |>
  summarise(value = median(value, na.rm = T)) |>
  ungroup() |>
  mutate(m = month(date)) |>
  ungroup() |>
  left_join(tbl(con, "coverage"), by = c("name", "station_id"))

# coverage = tbl(con, "coverage") |>
#   filter(perc >= 90) |>
#   collect()

# assign into a vector as the db interface doesn't like filtering to a column here.
# I think using local(coverage$station_id) would achieve the same goal, but I'm not sure so I'll stick with this
#stationsWithCoverage = coverage$station_id

seasonModel = value~sin(2*pi*m/12)+cos(2*pi*m/12)+ sin(2*pi*m/6)+cos(2*pi*m/6)

dat = datMonth |>
  #filter(station_id %in% stationsWithCoverage) |>
  filter(coverage_check) |>
  arrange(station_id, date) |>
  collect() |>
  nest_by(name, station_id, station_type) |>
  rowwise() |>
  mutate(seasonality = tibble(m = 1:12,
                              season = predict(lm(seasonModel,data = data),
                                               newdata = data.frame(m = 1:12))) |>
           list()) |>
  mutate(data = ts |>
           left_join(data, by = "date") |>
           left_join(seasonality, by = "m") |>
           mutate(
             x = row_number(),
             anom = value-season) |>
           list())

dat$fit = NA
dat$fit_se = NA
dat$fit_pv = NA

saveRDS(dat, here(readLines(configPath,n = 1),"data","dataBeforeStat_v2.RDS"))

dat = readRDS(here(readLines(configPath,n = 1),"data","dataBeforeStat_v2.RDS"))

pb = progress::progress_bar$new(total = nrow(dat))

tau = c(0.05, 0.50, 0.95)

# Calculating the fit and p values

for(i in 1:nrow(dat)){
  pb$tick()
  dat$fit[i] = tryCatch({
    foo <- rq(anom~x, data = dat$data[[i]], tau = tau) |>
      coef()
    list(foo)
  },
  error = function(e){list(e)}
  )

  dat$fit_se[i] = tryCatch({
    bs_results <- mbfun2(anom~x,
                        data = dat$data[[i]],
                        tau = tau) |>
      replicate(1000, expr = _)
    if (length(dim(bs_results)) == 3) {
      margins <- 1:2
    } else if (length(dim(bs_results)) == 2) {
      margins <- 2
    }
    apply(bs_results, margins,sd, na.rm = T) |>
      list()
  },
  error = function(e){list(e)}
  )

  dat$fit_pv[i] = tryCatch({ (pt(q = abs(dat$fit[[i]]/dat$fit_se[[i]]),
                                 df = nrow(dat$data[[i]])-2,
                                 lower.tail = F)*2) |>
      list()},
      error = function(e){list(e)}
  )

}


saveRDS(dat, here(readLines(configPath,n = 1),"data","dataWithStat_v2.RDS"))

dbDisconnect(con, shutdown = T)
