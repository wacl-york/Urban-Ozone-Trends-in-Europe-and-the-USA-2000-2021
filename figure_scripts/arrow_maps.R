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

con = connect_to_db()

tables = dbListTables(con)[str_detect(dbListTables(con), "piecewise_stats_freeTau")]

mycrs = 4087 #8857

datRaw = map(
  tables,
  ~{

    source(here::here('functions','plotting_utils.R'))
    temp = .x |>
      get_slopes() |>
      dplyr::ungroup()

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
      fit > quantile(fit, 0.99) ~ quantile(fit, 0.99),
      fit < quantile(fit, 0.01) ~ quantile(fit, 0.01),
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
mirai::daemons(18)
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
  mutate(lineDat = data |>
           left_join(lines, "rn") |>
           select(-latitude, -longitude, -rn) |>
           st_as_sf() |>
           # select(-pvStr) |>
           make_pvStr(7) |>
           list()
  ) |>
  ungroup() |>
  rowwise() |>
  mutate(
    g_eu_o3 = arrow_plot(lineDat, "Europe", "o3", type, rm) |>
      list(),
    g_eu_no2 = arrow_plot(lineDat, "Europe", "no2", type, rm) |>
      list(),
    g_us_o3 = arrow_plot(lineDat, "United States of America", "o3", type, rm) |>
      list(),
    g_us_no2 = arrow_plot(lineDat, "United States of America", "no2", type, rm) |>
      list()
  )


dirOut = data_path("figures", "o3_map")

if(!dir.exists(dirOut)){
  dir.create(dirOut, recursive = T)
}

saveRDS(lineDat, data_path("figures", "o3_map_data.RDS"))

for(i in 1:nrow(lineDat)){

  pdf(
    here::here(dirOut, paste0("o3_map_",lineDat$type[[i]],"_eu_o3.pdf")),
    width = 24, height = 16
  )
  print(lineDat$g_eu_o3[[i]])
  dev.off()

  pdf(
    here::here(dirOut, paste0("o3_map_",lineDat$type[[i]],"_eu_no2.pdf")),
    width = 24, height = 16
  )
  print(lineDat$g_eu_no2[[i]])
  dev.off()

  pdf(
    here::here(dirOut, paste0("o3_map_",lineDat$type[[i]],"_us_o3.pdf")),
    width = 24, height = 16
  )
  print(lineDat$g_us_o3[[i]])
  dev.off()

  pdf(
    here::here(dirOut, paste0("o3_map_",lineDat$type[[i]],"_us_no2.pdf")),
    width = 24, height = 16
  )
  print(lineDat$g_us_no2[[i]])
  dev.off()

}

dbDisconnect(con, shutdown = T)
