library(DBI)
library(zoo)
library(dplyr)
library(tidyr)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','rle_helpers.R'))
source(here::here('functions','mda8_helpers.R'))

# Housekeeping ------------------------------------------------------------

con = connect_to_db()

all_data_series = tbl(con, "all_data_series") |>
  collect() |>
  arrange(name, station_id)

series_id = as.numeric(commandArgs(trailingOnly = T)[1])+1

nm = all_data_series$name[series_id]
stn = all_data_series$station_id[series_id]

checks = data.frame(
  notOnRemoveList = NA,
  passPreCoverageCheck = NA,
  passPostCoverageCheck = NA,
  name = nm,
  station_id = stn,
  qa_originalRows = NA,
  qa_threshold = NA,
  qa_persistence = NA,
  qa_variablity = NA
)

dir_list = list(out = file.path(data_path(), "series"))
dir_list$checks = file.path(dir_list$out, "checks")
dir_list$hour = file.path(dir_list$out, "dat_hour")
dir_list$daily_all = file.path(dir_list$out, "dat_daily_all")
dir_list$daily_day = file.path(dir_list$out, "dat_daily_day")
dir_list$daily_night = file.path(dir_list$out, "dat_daily_night")
dir_list$mda8 = file.path(dir_list$out, "dat_mda8")
dir_list$metrics = file.path(dir_list$out, "dat_metrics")
dir_list$coverage_annual = file.path(dir_list$out, "dat_coverage_annual")
dir_list$coverage_warm = file.path(dir_list$out, "dat_coverage_warm")


path_list = list()

for(i in 1:length(dir_list)){
  if(!dir.exists(dir_list[[i]])){
    dir.create(dir_list[[i]])
  }

  path_list[[names(dir_list)[i]]] = file.path(dir_list[[i]], paste0(names(dir_list)[i],"_", stn,"_",nm, ".csv"))

}

skip = FALSE # set TRUE if a check fails, then we don't bother with the subsequent checks

# Remove Sites Due to Visual Inspection ----------------------------------

log_message("Remove Sites",stn, nm)

remove_sites = tbl(con, "remove_sites") |>
  collect() |>
  filter(stn %in% station_id) |>
  filter(nm %in% name)

if(nrow(remove_sites) > 0){
  checks$notOnRemoveList = FALSE
  log_message("Skip",stn, nm)
  skip = TRUE
  write.csv(checks, path_list$checks, row.names = F)
}else{
  checks$notOnRemoveList = TRUE
}

# Pre Coverage ------------------------------------------------------------
if(!skip){
  log_message("Pre Coverage",stn, nm)
  coverage_total = tbl(con, "coverage_total") |>
    filter(station_id == stn,
           name == nm) |>
    collect()

  if(!coverage_total$total_coverage_check){
    checks$passPreCoverageCheck = FALSE
    log_message("Skip",stn, nm)
    skip = TRUE
    write.csv(checks, path_list$checks, row.names = F)

  }else{
    checks$passPreCoverageCheck = TRUE
  }
}

# QA ----------------------------------------------------------------------

if(!skip){

  ## Get Data ----------------------------------------------------------------

  log_message("Get Data",stn, nm)

  dat = tbl(con, "all_data") |>
    filter(station_id == stn,
           name == nm) |>
    select(date, value) |>
    collect() |>
    arrange(date) |>
    mutate(day = floor_date(date, "day"),
           m = month(date))

  originalRows = nrow(dat)
  checks$qa_originalRows = originalRows

  ## Threshold ---------------------------------------------------------------
  log_message("QA Threshold",stn, nm)

  dat = dat |>
    filter(!is.na(value),
           between(value, 0, 500))

  checks$qa_threshold = originalRows-nrow(dat)

  originalRows = nrow(dat)


  ## Persistence -------------------------------------------------------------

  log_message("QA Persistence",stn, nm)

  valueRle = rle(dat$value) |>
    tidy_rle() |>
    filter(lengths > 8) |>
    arrange(desc(lengths))

  if(nrow(valueRle) > 0){
    rleIdx = valueRle |>
      index_from_tidy_rle()

    dat = dat |>
      mutate(idx = row_number()) |>
      filter(!idx %in% rleIdx$idx) |>
      select(-idx)

  }

  checks$qa_persistence = originalRows-nrow(dat)
  originalRows = nrow(dat)


  ## Variation ---------------------------------------------------------------

  log_message("QA Variation",stn, nm)

  daily = dat |>
    group_by(day) |>
    summarise(minimum = min(value, na.rm = T),
              maximum = max(value, na.rm = T),
              variation = maximum-minimum) |>
    filter(variation <= 2)

  dat = dat |>
    filter(!day %in% daily$day) |>
    select(-day)

  checks$qa_variablity = originalRows-nrow(dat)

}

# Post Coverage -----------------------------------------------------------

if(!skip){
  log_message("Post QA Coverage",stn, nm)

  startDate = ymd_hm("2000-01-01 00:00")
  endDate = ymd_hm("2022-01-01 00:00")

  ts = tibble(date = seq(startDate, endDate, "hour"))
  x = nrow(ts)

  coverage_after_QA = dat |>
    filter(!is.na(value),
           between(date, startDate, endDate)) |>
    count() |>
    mutate(perc = (n/x)*100) |>
    collect() |>
    mutate(total_coverage_check = perc >= 80)

  if(!coverage_after_QA$total_coverage_check){
    log_message("Skip",stn, nm)
    skip = TRUE
    write.csv(checks, path_list$checks, row.names = F)
  }else{
    checks$passPostCoverageCheck = TRUE
    write.csv(checks, path_list$checks, row.names = F)
  }

}


if(!skip){
  # Create Climatology ------------------------------------------------------
  log_message("Checks passed - creating data",stn, nm)
  log_message("Climatology",stn, nm)

  clim = dat |>
    select(m, value) |>
    group_by(m) |>
    summarise(clim_mean = mean(value)) |>
    ungroup()


  # Calculate Anom --------------------------------------------------------

  dat = dat |>
    left_join(clim, "m") |>
    mutate(anom = value-clim_mean)

  # Dat Hour --------------------------------------------------------------

  log_message("make hour_dat",stn, nm)

  tzMeta = tbl(con, "combinedMeta") |>
    filter(station_id == stn) |>
    pull(timezone) |>
    unique()

  tsHour = tibble(date = seq(min(ymd_hms("2000-01-01 00:00:00")), max(ymd_hms("2023-12-31 00:00:00")), "hour")) |>
    mutate(x = row_number())

  hour_dat = dat |>
    left_join(tsHour,y = _, "date") |>
    mutate(timezone = tzMeta) |>
    rowwise() |>
    mutate(
      local_date = with_tz(date, timezone)
    ) |>
    ungroup() |>
    filter(!is.na(x)) |>
    mutate(station_id = stn,
           name = nm) |>
    select(x, date, local_date, station_id, name, timezone, value, anom)


  # Dat day -----------------------------------------------------------------

  log_message("make daily_dat",stn, nm)

  tsDay = tibble(date = seq(min(ymd("2000-01-01")), max(ymd("2023-12-31")), "day")) |>
    mutate(x = row_number())

  log_message("make daily_all_dat",stn, nm)

  daily_all_dat = hour_dat |>
    mutate(date = floor_date(date, "day")) |>
    group_by(date, station_id, name, timezone) |>
    summarise(value = mean(value, na.rm = T)) |>
    left_join(tsDay, y = _, "date") |>
    mutate(m = month(date)) |>
    left_join(clim, "m") |>
    mutate(anom = value-clim_mean) |>
    select(-m, -clim_mean)

  log_message("make daily_day_dat",stn, nm)

  daily_day_dat = hour_dat |>
    filter(hour(local_date) %in% 8:19) |>
    mutate(date = floor_date(date, "day")) |>
    group_by(date, station_id, name, timezone) |>
    summarise(value = mean(value, na.rm = T)) |>
    left_join(tsDay, y = _, "date") |>
    mutate(m = month(date)) |>
    left_join(clim, "m") |>
    mutate(anom = value-clim_mean) |>
    select(-m, -clim_mean)

  log_message("make daily_night_dat",stn, nm)

  daily_night_dat = hour_dat |>
    filter(!hour(local_date) %in% 8:19) |>
    mutate(date = floor_date(date, "day")) |>
    group_by(date, station_id, name, timezone) |>
    summarise(value = mean(value, na.rm = T)) |>
    left_join(tsDay, y = _, "date") |>
    mutate(m = month(date)) |>
    left_join(clim, "m") |>
    mutate(anom = value-clim_mean) |>
    select(-m, -clim_mean)

  # Annual Coverage ---------------------------------------------------------
  log_message("make coverage_annual",stn, nm)
  coverage_annual = hour_dat |>
    mutate(date = floor_date(date, "year")) |>
    mutate(value = ifelse(is.na(value), 0, 1)) |>
    group_by(date) |>
    summarise(n = sum(value)) |>
    mutate(perc = (n/8760)*100,
           coverage_check = perc > 60,
           station_id = stn,
           name = nm)

  # Warm Coverage -----------------------------------------------------------
  log_message("make coverage_warm",stn, nm)
  coverage_warm = hour_dat |>
    filter(month(date) %in% c(4,5,6,7,8,9)) |>
    mutate(date = floor_date(date, "year")) |>
    mutate(value = ifelse(is.na(value), 0, 1)) |>
    group_by(date) |>
    summarise(n = sum(value)) |>
    mutate(perc = (n/4380)*100,
           coverage_check = perc > 60,
           station_id = stn,
           name = nm)

  # Extra O3 calcs ----------------------------------------------------------

  if(nm == "o3"){
    ## MDA8 --------------------------------------------------------------------
    log_message("make mda8 dat",stn, nm)

    tsDay = tibble(day = seq(min(ymd("2000-01-01")), max(ymd("2023-12-31")), "day")) |>
      mutate(x = row_number())

    mda8_dat = hour_dat |>
      select(local_date, value) |>
      mutate(
        mda8 = rollapply(
          data = value,
          width = 8,
          FUN = calc_hour8_o3,
          fill = NA,
          align = "left")) |>
      mutate(
        mda8 = ifelse(hour(local_date) %in% 0:6, NA, mda8),
        day = date(local_date)
      ) |>
      group_by(day) |>
      filter(mda8 == max_or_empty(mda8)) |>
      filter(row_number() == 1) |> # Sometimes there are multiple MDA8s that are the same, just take the first one (we only need one per day)
      ungroup() |>
      left_join(tsDay,y = _, by = "day") |>
      filter(!is.na(x)) |>
      mutate(station_id = stn,
             name = nm,
             timezone = tzMeta,
             m = month(day)) |>
      left_join(clim, "m") |>
      mutate(mda8_anom = mda8-clim_mean) |>
      select(x, date = day, station_id, name, timezone, mda8, mda8_anom)

    ## Metrics -----------------------------------------------------------------
    log_message("Metrics",stn, nm)

    mda8_warm = mda8_dat |>
      filter(month(date) %in% c(4,5,6,7,8,9))

    tsYear = tibble(date = seq(min(ymd_hms("2000-01-01 00:00:00")), max(ymd_hms("2023-12-31 00:00:00")), "year")) |>
      mutate(x = row_number())

    metrics = list()


    ### Calculate 4MDA8 ---------------------------------------------------------
    log_message("4MDA8",stn, nm)
    ### The 8-hour running mean for a particular hour is calculated on the mixing ratios
    ### for that hour plus the following 7 hours between the hours of 0700 and 2300 local time.
    ### For a given day the greatest of these 17 values is the daily maximum 8-hour average ozone.
    ### Based on all warm-season daily maximum 8-hour average ozone values the 4th highest value is selected.
    ### Fleming et al., 2018

    # Group data by year, station_id and timezone, then arrange in descending order and select the 4th highest by group.
    metrics$metric_4MDA8 = mda8_warm |>
      mutate(date = floor_date(date, "year")) |>
      rename(value = mda8) |>
      group_by(date) |>
      arrange(desc(value),.by_group = T) |>
      mutate(rank = row_number()) |>
      filter(rank == 4) |>
      ungroup() |>
      select(-rank) |>
      mutate(metric = "4MDA8") |>
      select(date, station_id, name, timezone, value, metric) |>
      left_join(
        select(coverage_warm, date, coverage_check),
        "date"
      ) |>
      mutate(value = ifelse(coverage_check, value, NA)) |>
      select(-coverage_check)


    ### Calculate NDGT70 --------------------------------------------------------

    log_message("NDGT70",stn, nm)
    ### Annual count of number of days of MDA8 > 70 ppb.
    metrics$metric_NDGT70 = mda8_dat |>
      mutate(
        date = floor_date(date, "year"),
        exceeded_limit = mda8 > 70
      ) |>
      filter(!is.na(exceeded_limit)) |>
      group_by(date, station_id, name, timezone) |>
      summarise(value = sum(exceeded_limit, na.rm = T)) |>
      ungroup() |>
      mutate(metric = "NDGT70") |>
      left_join(
        select(coverage_annual, date, coverage_check),
        "date"
      ) |>
      mutate(value = ifelse(coverage_check, value, NA)) |>
      select(-coverage_check)


    ### Calculate SOMO35 --------------------------------------------------------
    log_message("SOMO35",stn, nm)
    ### The sum of the positive differences between the daily maximum 8-h ozone mixing ratio and the cut-off
    ### value set at 35 ppb (70 µg m–3) calculated for all days in a year.

    metrics$metric_SOMO35 = mda8_dat |>
      mutate(
        date = floor_date(date, "year"),
        SOMO35 = mda8-35) |>
      mutate(SOMO35 = ifelse(is.na(SOMO35), 0, SOMO35), # replace missing vals with 0 to preseve years with no data (coverage check will replace with NA)
             SOMO35 = ifelse(SOMO35 < 0, 0, SOMO35)) |> # and replace negs with 0 to preserve years where SOMO35 == 0
      select(date, station_id, name, timezone, value = SOMO35) |>
      group_by(date, station_id, name, timezone) |>
      summarise(value = sum(value, na.rm = T)) |>
      mutate(metric = "SOMO35")|>
      left_join(
        select(coverage_annual, date, coverage_check),
        "date"
      ) |>
      mutate(value = ifelse(coverage_check, value, NA)) |>
      select(-coverage_check)


    ### Calculate 3MMDA1 --------------------------------------------------------
    log_message("3MMDA1",stn, nm)
    ### Annual maximum of the three-month average of daily 1-hour maximum ozone value.
    ### Three month running mean values calculated were assigned to the mid-point of the 3 month period.

    # Calculate the daily max hourly o3 value

    calc_3mmda1 = function(x){

      y = x[!is.na(x)]

      if(length(y) < 68){ # An 3mmda1 is valid if 68 of 90 hours are present in period
        return(NA)
      }

      mean(x, na.rm = T)

    }

    metrics$metric_3MMDA1 = hour_dat |>
      mutate(date = floor_date(date, "day")) |>
      select(date, station_id, name, timezone, value) |>
      group_by(date) |>
      filter(value == max_or_empty(value)) |>
      ungroup() |>
      mutate(
        x_3mmda1 = rollapply(
          data = value,
          width = 90,
          FUN = calc_3mmda1,
          fill = NA,
          align = "center")
      ) |>
      select(date, station_id, name, timezone, value = x_3mmda1) |>
      mutate(
        date = floor_date(date, "year"),
        metric = "3MMDA1"
      ) |>
      group_by(date) |>
      filter(value == max_or_empty(value)) |>
      distinct() |> # sometimes there are 2 max values per year, and this messes up the regressions downstream.
      ungroup() |>
      left_join(
        select(coverage_annual, date, coverage_check),
        "date"
      ) |>
      mutate(value = ifelse(coverage_check, value, NA)) |>
      select(-coverage_check)

    ### Calculate 6MMDA1 --------------------------------------------------------
    log_message("6MMDA1",stn, nm)
    ### Annual maximum of the three-month average of daily 1-hour maximum ozone value.
    ### Three month running mean values calculated were assigned to the mid-point of the 3 month period.

    # Calculate the daily max hourly o3 value

    calc_6mmda1 = function(x){

      y = x[!is.na(x)]

      if(length(y) < 182*0.75){
        return(NA)
      }

      mean(x, na.rm = T)

    }

    metrics$metric_6MMDA1 = hour_dat |>
      mutate(date = floor_date(date, "day")) |>
      select(date, station_id, name, timezone, value) |>
      group_by(date) |>
      filter(value == max_or_empty(value)) |>
      ungroup() |>
      mutate(
        x_6mmda1 = rollapply(
          data = value,
          width = 182,
          FUN = calc_6mmda1,
          fill = NA,
          align = "center")
      ) |>
      select(date, station_id, name, timezone, value = x_6mmda1) |>
      mutate(
        date = floor_date(date, "year"),
        metric = "6MMDA1"
      ) |>
      group_by(date) |>
      filter(value == max_or_empty(value)) |>
      ungroup() |>
      distinct() |> # sometimes there are 2 max values per year, and this messes up the regressions downstream.
      left_join(
        select(coverage_annual, date, coverage_check),
        "date"
      ) |>
      mutate(value = ifelse(coverage_check, value, NA)) |>
      select(-coverage_check)


    ### Calculate AVGMDA8 -------------------------------------------------------
    log_message("AVGMDA8",stn, nm)
    ### 6-month warm season mean of MDA8.

    metrics$metric_AVGMDA8 = mda8_warm |>
      mutate(date = floor_date(date, "year")) |>
      select(date, station_id, name, timezone, value = mda8) |>
      group_by(date, station_id, name, timezone) |>
      summarise(value = mean(value, na.rm = T)) |>
      mutate(metric = "AVGMDA8") |>
      left_join(
        select(coverage_warm, date, coverage_check),
        "date"
      ) |>
      mutate(value = ifelse(coverage_check, value, NA)) |>
      select(-coverage_check)

    metric_dat = bind_rows(metrics) |>
      left_join(tsYear, "date")

    ## Write Data - O3 Only --------------------------------------------------------------
    log_message("Write mda8_dat",stn, nm)
    write.csv(mda8_dat, path_list$mda8, row.names = F)

    log_message("Write metric_dat",stn, nm)
    write.csv(metric_dat, path_list$metrics, row.names = F)

  }


  # Write Data --------------------------------------------------------------

  log_message("Write coverage_warm",stn, nm)
  write.csv(coverage_warm, path_list$coverage_warm, row.names = F)

  log_message("Write coverage_annual",stn, nm)
  write.csv(coverage_annual, path_list$coverage_annual, row.names = F)

  log_message("Write hour_dat",stn, nm)
  write.csv(hour_dat, path_list$hour, row.names = F)

  log_message("Write daily_all_dat",stn, nm)
  write.csv(daily_all_dat, path_list$daily_all, row.names = F)

  log_message("Write daily_day_dat",stn, nm)
  write.csv(daily_day_dat, path_list$daily_day, row.names = F)

  log_message("Write daily_night_dat",stn, nm)
  write.csv(daily_night_dat, path_list$daily_night, row.names = F)

}

dbDisconnect(con, shutdown = T)
