library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(quantreg)
library(lubridate)

# Take bootstrap samples of the timeseries and return quantile regression coeficients.
# used to test significance of the trend in the measuered timeseries.
mbfun = function(formula,
                 data,
                 tau # the quantile to do qr on.
){

  n = nrow(data) # length of timeseries
  b = ceiling(n^0.25) # block length
  nblocks = ceiling(n/b)

  # create a list of sequences that are the length of a block, and overlap with the subsequent block for b-1 of the values
  # the values in the sequences correspond to row ids in the data. eventually used to select values from the data
  # the list contains enough elements such that the last sequence in the list has its final value == n
  # (n-b+1, but the values in the sequence run to b-1, so the final value == n)
  blocks = lapply(seq_len(n-b+1), function(i) seq(i, i+b-1))

  # create a vector of randomly sampled block ids - i.e. values between 1 and nblocks.
  bn = sample(1:length(blocks), nblocks, replace = T)

  # blocks[bn] randomly sample (with replacement) the blocks of data.
  # unlisting this produces a vector that will rearrange the "blocks" of the timeseries

  samp_data = data[unlist(blocks[bn]),]
  mod = rq(formula, data = samp_data, tau = tau)

  coef(mod)
}

# -------------------------------------------------------------------------

con = dbConnect(duckdb::duckdb(),
                dbdir = here("data","db.duckdb"), read_only = FALSE)

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
  filter(perc > 90) |>
  arrange(station_id, date) |>
  collect() |>
  nest_by(name, station_id, station_type, perc) |>
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

pb = progress::progress_bar$new(total = nrow(dat))

tau = c(0.05, 0.50, 0.95)

for(i in 1:nrow(dat)){
  pb$tick()
  dat$fit[i] = tryCatch({
    rq(anom~x, data = dat$data[[i]], tau = tau) |>
      coef() |>
      list()
  },
  error = function(e){list(e)}
  )

  dat$fit_se[i] = tryCatch({
    mbfun(value~x,
          data = dat$data[[i]],
          tau = tau) |>
      replicate(1000, expr = _) |>
      apply(2,sd, na.rm = T) |>
      list()},
    error = function(e){list(e)}
  )

  dat$fit_pv[i] = tryCatch({ (pt(q = abs(dat$fit[[i]]/dat$fit_se[[i]]),
                                 df = nrow(dat$data[[i]])-2,
                                 lower.tail = F)*2) |>
      list()},
      error = function(e){list(e)}
  )

}


saveRDS(dat, here("data","dataWithStat.RDS"))

dbDisconnect(con, shutdown = T)
