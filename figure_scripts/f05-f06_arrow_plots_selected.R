library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(mirai)
library(ggtext)
library(stringr)
library(ggplot2)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))

parse_type = function(type){

  if(stringr::str_detect(type, "all")){
    return("MDA8O<sub>3</sub>")
  }

  if(stringr::str_detect(type, "warm")){
    return("MDA8O<sub>3</sub> Warm Season")
  }

  if(stringr::str_detect(type, "cold")){
    return("MDA8O<sub>3</sub> Cold Season")
  }

}

con = connect_to_db()

tables = c("piecewise_stats_freeTau_mda8_anom_all", "piecewise_stats_freeTau_mda8_anom_cold", "piecewise_stats_freeTau_mda8_anom_warm")

mycrs = 4087 #8857

datRaw = map(
  tables,
  ~{

    source(here::here('functions','plotting_utils.R'))
    temp = .x |>
      get_slopes() |>
      dplyr::ungroup() |>
      filter(tau %in% c(0.05, 0.5, 0.95))

    if("metric" %in% names(temp)){
      temp$type = temp$metric
    }else{
      temp$type = .x
      temp$fit = temp$fit*365
    }

    temp

  },
  .progress = TRUE
) |>
  bind_rows()

slopes = datRaw |>
  make_pvStr(7) |>
  group_by(type) |>
  mutate(
    fit_capped = case_when(
      fit > 2.5 ~ 2.5,
      fit < -2.5 ~ -2.5,
      TRUE ~ fit
    )) |>
  ungroup()

line_groups = slopes |>
  left_join(
    combinedMetaRegion(con) |>
      select(station_id, latitude, longitude, region) |>
      collect() |>
      distinct(),
    by = "station_id"
  ) |>
  nest_by(type) |>
  mutate(rm = max(abs(data$fit_capped))) |>
  mutate(data = data |>
           calc_arrow_end(rangeMax = rm,
                          slope = "fit_capped",
                          length = 3) |>
           rename(latitude_start = latitude,
                  longitude_start = longitude) |>
           mutate(rn = row_number()) |>
           pivot_longer(c(latitude_start,latitude_end, longitude_start, longitude_end),
                        names_to = c("coordType","suffix"),
                        names_sep = "_",
                        values_to = "coord"
           ) |>
           pivot_wider(names_from = "coordType",
                       values_from = "coord") |>
           select(-suffix) |>
           list())

tictoc::tic()
mirai::daemons(3)
lines = line_groups |>
  ungroup() |>
  mutate(lines = map(data,
                     in_parallel(
                       ~{
                         .x |>
                           sf::st_as_sf(coords = c("longitude", "latitude"),  crs = 4326) |>
                           dplyr::group_by(rn) |>
                           dplyr::summarise(do_union = FALSE) |>
                           sf::st_cast("LINESTRING")
                       }
                     )
  )
  )
mirai::daemons(0)
tictoc::toc()


lineDat = lines |>
  rowwise() |>
  mutate(
    lineDat = data |>
      left_join(lines, "rn") |>
      select(-latitude, -longitude, -rn) |>
      st_as_sf() |>
      # select(-pvStr) |>
      make_pvStr(7) |>
      mutate(tau = paste0("&tau; = ", tau)) |>
      list(),
    titleText = parse_type(type),
  ) |>
  ungroup() |>
  rowwise() |>
  mutate(
    g_eu_o3 = arrow_plot(
      dat = lineDat,
      name = "o3",
      region = "Europe",
      type = type,
      rm = rm,
      worldScale = "small",
      yrs = c(2004, 2018),
      titleText = titleText
        ) |>
      list(),
    g_us_o3 = arrow_plot(
      dat = lineDat,
      region ="United States of America",
      name = "o3",
      type = type,
      rm = rm,
      worldScale = "small",
      yrs = c(2004, 2018),
      titleText = titleText
    ) |>
      list()
  ) |>
  mutate(paper = str_detect(type, "warm"))

dirOutP = here::here('figures','paper_figures')
dirOutSI = here::here('figures','si_figures')

if(!dir.exists(dirOutP)){
  dir.create(dirOutP, recursive = T)
}

if(!dir.exists(dirOutSI)){
  dir.create(dirOutSI, recursive = T)
}




grDevices::cairo_pdf(
  here::here(dirOutP, paste0("f05_o3_map_mda8_warm_eu_o3.pdf")),
  width = 5.6, height = 7
)
print(lineDat$g_eu_o3[lineDat$type == "piecewise_stats_freeTau_mda8_anom_warm"])
dev.off()

grDevices::cairo_pdf(
  here::here(dirOutP, paste0("f06_o3_map_mda8_warm_us_o3.pdf")),
  width = 5.6, height = 7
)
print(lineDat$g_us_o3[lineDat$type == "piecewise_stats_freeTau_mda8_anom_warm"])
dev.off()


grDevices::cairo_pdf(
  here::here(dirOutSI, paste0("fS05_o3_map_mda8_all_eu_o3.pdf")),
  width = 5.6, height = 7
)
print(lineDat$g_eu_o3[lineDat$type == "piecewise_stats_freeTau_mda8_anom_all"])
dev.off()

grDevices::cairo_pdf(
  here::here(dirOutSI, paste0("fS06_o3_map_mda8_all_us_o3.pdf")),
  width = 5.6, height = 7
)
print(lineDat$g_us_o3[lineDat$type == "piecewise_stats_freeTau_mda8_anom_all"])
dev.off()


grDevices::cairo_pdf(
  here::here(dirOutSI, paste0("fS07_o3_map_mda8_cold_eu_o3.pdf")),
  width = 5.6, height = 7
)
print(lineDat$g_eu_o3[lineDat$type == "piecewise_stats_freeTau_mda8_anom_cold"])
dev.off()

grDevices::cairo_pdf(
  here::here(dirOutSI, paste0("fS08_o3_map_mda8_cold_us_o3.pdf")),
  width = 5.6, height = 7
)
print(lineDat$g_us_o3[lineDat$type == "piecewise_stats_freeTau_mda8_anom_cold"])
dev.off()





dbDisconnect(con, shutdown = T)
