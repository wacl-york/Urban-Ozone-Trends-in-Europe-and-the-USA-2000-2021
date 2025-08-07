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
          select(region, station_id) |>
          distinct(),
        by = "station_id"
      ) |>
      filter(
        name == "o3",
        region == rgn,
        metric == mtric
      )

    dat = datRaw |>
      select(-x) |>
      collect() |>
      nest_by(station_id) |>
      mutate(data = data |>
               left_join(tsPad, y = _, "date") |>
               list()) |>
      unnest(data) |>
      group_by(station_id) |>
      mutate(y = ifelse(is.na(!!yname), mean(!!yname, na.rm = T), !!yname),
             y = (y-mean(y, na.rm = T))/sd(y, na.rm = T)
      ) |>
      select(x, station_id, y) |>
      mutate(y = ifelse(is.na(y), 0, y)) |>  # if y is still NA, it is because the sd is NA. Happens with metrics where all can be 0 e.g NDGT70 - replace with 0
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
          select(region, station_id) |>
          distinct(),
        by = "station_id"
      ) |>
      filter(
        name == "o3",
        region == rgn
      ) |>
      mutate(m = month(date)) |>
      filter(m %in% mnths) |>
      select(-x)

    season_anom = datRaw |>
      group_by(m, station_id) |>
      summarise(season = mean(!!yname, na.rm = T), .groups = "drop")

    dat = datRaw |>
      left_join(
        season_anom,
        by = c("station_id", "m")
      ) |>
      collect() |>
      nest_by(station_id) |>
      mutate(data = data |>
               mutate(date = floor_date(date, "month")) |>
               left_join(tsPad, y = _, "date") |>
               list()
               ) |>
      unnest(data) |>
      group_by(station_id) |>
      mutate(y = ifelse(is.na(!!yname), season, !!yname),
             y = (y-mean(y, na.rm = T))/sd(y, na.rm = T)
      ) |>
      select(x, station_id, y) |>
      group_by(x, station_id) |>
      summarise(y = quantile(y, probs = tau, na.rm = T), .groups = "drop") |>
      arrange(station_id,x) |>
      pivot_wider(names_from = "station_id", values_from = "y") |>
      select(-x) |>
      as.matrix() |>
      t()

    #
    dat
  }
}
