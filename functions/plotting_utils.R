expand_slopes = function(df){
  years = tibble::tibble(year = min(df$startYear):max(df$endYear))

  years |>
    dplyr::left_join(df, join_by(between(year, startYear, endYear, bounds = "[]")))
}

make_pvStr = function(df){

  df |>
    dplyr::mutate(
      pvStr = dplyr::case_when(
        pv <= 0.05 & fit < 0 ~ "p <= 0.05 (dec)",
        dplyr::between(pv, 0.05, 0.1) & fit < 0 ~ "0.05 < p <= 0.10 (dec)",
        dplyr::between(pv, 0.1, 0.33) & fit < 0 ~ "0.10  < p <= 0.33 (dec)",
        pv >= 0.33 & fit < 0 ~ "p > 0.33 (dec)",
        pv >= 0.33 & fit > 0 ~ "p > 0.33 (inc)",
        dplyr::between(pv, 0.1, 0.33) & fit > 0 ~ "0.10  < p <= 0.33 (inc)",
        dplyr::between(pv, 0.05, 0.1) & fit > 0 ~ "0.05 < p <= 0.10 (inc)",
        pv <= 0.05 & fit > 0 ~ "p <= 0.05 (inc)",
        TRUE ~ NA
      ) |>
        factor(
          levels = rev(
            c(
              "p > 0.33 (dec)",
              "0.10  < p <= 0.33 (dec)",
              "0.05 < p <= 0.10 (dec)",
              "p <= 0.05 (dec)",
              "p > 0.33 (inc)",
              "0.10  < p <= 0.33 (inc)",
              "0.05 < p <= 0.10 (inc)",
              "p <= 0.05 (inc)"
            )
          )
        )
    )

}


format_spc_name = function(df){
  df |>
    dplyr::mutate(
      name = dplyr::case_when(name == "no2" ~ "NO<sub>2</sub>",
                            name == "o3" ~ "O<sub>3</sub>",
                            name == "ox" ~ "O<sub>x</sub>" ) |>
        factor(levels = c("O<sub>3</sub>", "NO<sub>2</sub>", "O<sub>x</sub>")),
      n = dplyr::case_when(country == "United States of America" & year %in% c(2000:2001, 2020:2023) ~ NA,
                           country == "Europe" & year %in% c(2000:2001, 2021:2023) ~ NA,
                           TRUE ~ n
      ))
}

make_slopeBin = function(df) {
  df |>
    dplyr::mutate(
      slopeBin = dplyr::case_when(
        fit <= -1                      ~ "slope <= -1",
        fit > -1 & fit <= -0.67        ~ "-1 < slope <= -0.67",
        fit > -0.67 & fit <= -0.33     ~ "-0.67 < slope <= -0.33",
        fit > -0.33 & fit < 0          ~ "-0.33 < slope < 0",
        fit == 0                       ~ "slope = 0",
        fit > 0 & fit <= 0.33          ~ "0 < slope <= 0.33",
        fit > 0.33 & fit <= 0.67       ~ "0.33 < slope <= 0.67",
        fit > 0.67 & fit < 1           ~ "0.67 < slope < 1",
        fit >= 1                       ~ "slope >= 1",
        TRUE ~ NA
      ) |>
        factor(
          levels = c(
            "slope >= 1",
            "0.67 < slope < 1",
            "0.33 < slope <= 0.67",
            "0 < slope <= 0.33",
            "slope = 0",
            "-0.33 < slope < 0",
            "-0.67 < slope <= -0.33",
            "-1 < slope <= -0.67",
            "slope <= -1"
          )
        )
    )
}

calc_arrow_end = function(df,
                          rangeMax,
                          slope,
                          length,
                          colX = "longitude",
                          colY = "latitude",
                          outNameX = "longitude_end",
                          outNameY = "latitude_end") {

  deg2rad <- function(deg) {(deg * pi) / (180)}

  theta = (90/rangeMax)*df[[slope]]

  df[[outNameX]] = length * cos(deg2rad(theta)) + df[[colX]]
  df[[outNameY]] = length * sin(deg2rad(theta)) + df[[colY]]

  df

}
