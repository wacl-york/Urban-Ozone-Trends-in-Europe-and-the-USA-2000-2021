pad_time_series = function(.data, period) {
  # Check for presence of date column in .data
  if (!"date" %in% names(.data)) {
    stop("Column `date` not found")
  }

  # Check data column is formated as a POSIXct
  if (!"POSIXct" %in% class(.data$date)) {
    stop("date must be of class POSIXct")
  }

  ts = data.frame(date = seq(from = min(.data$date, na.rm = T),
                             to = max(.data$date, na.rm = T),
                             by = period))

  # Make tibble if .data is a tibble
  if ("tbl_df" %in% class(.data)) {
    ts = dplyr::tibble(ts)
  }

  dplyr::left_join(ts, .data, "date")

}
