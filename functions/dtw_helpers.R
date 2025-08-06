prep_dtw_data = function(con,
                         tableName,
                         yname,
                         tau,
                         rng,
                         mnths,
                         type
){
  yname = sym(yname)

  if(stringr::str_detect(tableName, "metric")){

    tsPad = tibble(date = seq(ymd_hms("2000-01-01 00:00:00"), max(ymd_hms("2023-12-31 00:00:00")), "year")) |>
      mutate(x = row_number())

    mtric = str_remove(type, "metric_")

    datRaw = tbl(con, tableName) |>
      left_join(
        combinedMetaRegion(con) |>
          select(region, station_id),
        by = "station_id"
      ) |>
      filter(
        name == "o3",
        region == rgn,
        metric == mtric
      )

    dat = datRaw |>
      group_by(station_id) |>
      mutate(y = ifelse(is.na(!!yname), mean(!!yname, na.rm = T), !!yname),
             y = (y-mean(y, na.rm = T))/sd(y, na.rm = T)
      ) |>
      select(date, station_id, y, value) |>
      collect() |>
      mutate(date = floor_date(date, "month")) |>
      group_by(date, station_id) |>
      summarise(y = quantile(y, probs = tau, na.rm = T), .groups = "drop") |>
      left_join(tsPad, "date") |>
      select(-date) |>
      arrange(station_id,x) |>
      pivot_wider(names_from = "station_id", values_from = "y") |>
      select(-x) |>
      as.matrix() |>
      t()

    #
    dat

  }else{

    tsPad = tibble(date = seq(min(ymd_hms("2000-01-01 00:00:00")), max(ymd_hms("2023-12-31 00:00:00")), "month")) |>
      mutate(m = month(date)) |>
      filter(m %in% mnths) |>
      mutate(x = row_number()) |>
      select(-m)

    datRaw = tbl(con, tableName) |>
      left_join(
        combinedMetaRegion(con) |>
          select(region, station_id),
        by = "station_id"
      ) |>
      filter(
        name == "o3",
        region == rgn
      ) |>
      mutate(m = month(date)) |>
      filter(m %in% mnths)

    season_anom = datRaw |>
      group_by(m, station_id) |>
      summarise(season = mean(!!yname, na.rm = T), .groups = "drop")

    dat = datRaw |>
      left_join(
        season_anom,
        by = c("station_id", "m")
      ) |>
      group_by(station_id) |>
      mutate(y = ifelse(is.na(!!yname), season, !!yname),
             y = (y-mean(y, na.rm = T))/sd(y, na.rm = T)
      ) |>
      select(date, station_id, y) |>
      collect() |>
      mutate(date = floor_date(date, "month")) |>
      group_by(date, station_id) |>
      summarise(y = quantile(y, probs = tau, na.rm = T), .groups = "drop") |>
      left_join(tsPad, "date") |>
      select(-date) |>
      arrange(station_id,x) |>
      pivot_wider(names_from = "station_id", values_from = "y") |>
      select(-x) |>
      as.matrix() |>
      t()

    #
    dat
  }
}
