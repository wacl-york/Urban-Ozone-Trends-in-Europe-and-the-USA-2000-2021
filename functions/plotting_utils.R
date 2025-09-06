expand_slopes = function(df){
  years = tibble::tibble(year = min(df$startYear):max(df$endYear))

  years |>
    dplyr::left_join(df, dplyr::join_by(between(year, startYear, endYear, bounds = "[]")))
}

make_pvStr = function(df, n = c(7,8)[2]){

  if(n == 8){
    df = df |>
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

  if(n == 7){

    pv_opt = c("p <= 0.05 (dec)",
               "0.05 < p <= 0.1 (dec)",
               "0.1  < p <= 0.33 (dec)",
               "p > 0.33",
               "0.1  < p <= 0.33 (inc)",
               "0.05 < p <= 0.1 (inc)",
               "p <= 0.05 (inc)"
    )

    df = df |>
      dplyr::mutate(
        pvStr = dplyr::case_when(
          pv <= 0.05 & fit < 0 ~ pv_opt[1],
          pv > 0.05 & pv <= 0.1 & fit < 0 ~ pv_opt[2],
          pv > 0.1 & pv <= 0.33 & fit < 0 ~ pv_opt[3],
          pv >= 0.33 ~ pv_opt[4],
          pv > 0.1 & pv <= 0.33 & fit > 0 ~ pv_opt[5],
          pv > 0.05 & pv <= 0.1 & fit > 0 ~ pv_opt[6],
          pv <= 0.05 & fit > 0 ~ pv_opt[7],
          TRUE ~ NA
        ) |>
          factor(levels = pv_opt),
      )
  }

  df

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

get_slopes = function(tableName){
  dplyr::tbl(con, tableName) |>
    dplyr::filter(stat == "slope") |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::across(tidyselect::any_of(c("station_id", "name", "tau", "metric")))) |>
    tidyr::pivot_wider(names_from = "type") |>
    dplyr::select(-stat) |>
    dplyr::nest_by(dplyr::across(tidyselect::any_of(c("station_id", "name", "tau", "metric")))) |>
    dplyr::mutate(data = data |>
                    expand_slopes() |>
                    list()) |>
    tidyr::unnest(data)
}

make_slopes_plotDat = function(dat){

  dat |>
    filter(
      fit != 0, # there are a few times the slope is zero, but we can't plot that here so just get rid. They are always p == 1.
    ) |>
    make_pvStr() |>
    left_join(tbl(con, "combinedMeta") |>
                select(station_id, country) |>
                distinct() |>
                collect(),
              "station_id") |>
    mutate(country = ifelse(country == "United States of America", country, "Europe"),
           dir = ifelse(str_detect(pvStr, "inc"), "inc", "dec")) |>
    group_by(across(any_of(c("name", "country", "tau", "dir", "year", "pvStr", "metric")))) |>
    count() |>
    ungroup() |>
    mutate(n = ifelse(dir == "dec", n*-1, n)) |>
    format_spc_name()
}

arrow_plot = function(dat,
                      name,
                      region,
                      type,
                      rm,
                      yrs = c(2002, 2006, 2010, 2014, 2018, 2022),
                      mycrs = 4087
){

  p_colours = c(
    rgb(0, 0, 0.6),
    rgb(0.1176, 0.3922, 1),
    rgb(0.4706, 0.7373, 1),
    rgb(0.6431, 0.7569, 0.4431),
    rgb(1, 0.7294, 0.4),
    rgb(1, 0.3922, 0),
    rgb(0.6471, 0, 0.1294)
  )

  world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
    st_transform(mycrs)

  if(region == "Europe"){
    lim = tibble(lng = c(-20,35), lat = c(25,65)) |>
      st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
      st_transform(mycrs)
  }else{
    lim = tibble(lng = c(-130,-50), lat = c(25,50)) |>
      st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
      st_transform(mycrs)
  }

  plotDat = dat |>
    filter(
      name == name,
      year %in% yrs
    )

  ggplot() +
    geom_sf(data = world, fill = "white")+
    geom_sf(data = plotDat,
            mapping = aes(colour = pvStr),
            linewidth = 0.25,
            arrow = arrow(angle = 30,
                          ends = "last",
                          type = "open",
                          length = unit(0.1, "cm"))) +
    scale_colour_manual(values = p_colours, name = "")+
    scale_y_continuous(limits = st_coordinates(lim)[,2])+
    scale_x_continuous(limits = st_coordinates(lim)[,1])+
    facet_grid(tau~year)+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          strip.text = element_markdown(),
          legend.position = "bottom",
          legend.byrow = T)+
    ggtitle(paste0(type, " - arrow range = +/- ",rm))

}
