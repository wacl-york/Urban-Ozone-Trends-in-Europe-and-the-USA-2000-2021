library(sf)
library(DBI)
library(here)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

source(here::here('functions','utils.R'))

con = connect_to_db()

mycrs = 4087

dat = tbl(con, "dat_metrics") |>
  left_join(
    combinedMetaRegion(con) |>
      select(station_id, region, latitude, longitude) |>
      distinct(),
    "station_id"
  ) |>
  mutate(
    value_bin = case_when( # no YOU'RE an obnoxious case_when()
      metric == "4MDA8" & value < 30.5 ~ "0 - 30 ppb",
      metric == "4MDA8" & value >= 30.5 & value < 40.5 ~ "31 - 40 ppb",
      metric == "4MDA8" & value >= 40.5 & value < 50.5 ~ "41 - 50 ppb",
      metric == "4MDA8" & value >= 50.5 & value < 60.5 ~ "51 - 60 ppb",
      metric == "4MDA8" & value >= 60.5 & value < 70.5 ~ "61 - 70 ppb",
      metric == "4MDA8" & value >= 70.5 & value < 75.5 ~ "71 - 75 ppb",
      metric == "4MDA8" & value >= 75.5 & value < 85.5 ~ "76 - 85 ppb",
      metric == "4MDA8" & value >= 85.5 & value < 100.5 ~ "86 - 100 ppb",
      metric == "4MDA8" & value >= 100.5 ~ ">= 100 ppb",
      metric == "NDGT70" & value < 0.5 ~ "0 days",
      metric == "NDGT70" & value >= 0.5 & value < 3.5 ~ "0 - 3 days",
      metric == "NDGT70" & value >= 3.5 & value < 6.5 ~ "4 - 6 days",
      metric == "NDGT70" & value >= 6.5 & value < 15.5 ~ "7 - 15 days",
      metric == "NDGT70" & value >= 15.5 & value < 25.5 ~ "16 - 25 days",
      metric == "NDGT70" & value >= 25.5 ~ ">= 26 days",
      metric == "SOMO35" & value < 1000.5 ~ "0 - 1000 ppb day",
      metric == "SOMO35" & value >= 1000.5 & value < 2000.5 ~ "1001 - 2000 ppb day",
      metric == "SOMO35" & value >= 2000.5 & value < 3000.5 ~ "2001 - 3000 ppb day",
      metric == "SOMO35" & value >= 3000.5 & value < 4000.5 ~ "3001 - 4001 ppb day",
      metric == "SOMO35" & value >= 4000.5 & value < 5000.5 ~ "4001 - 5000 ppb day",
      metric == "SOMO35" & value >= 5000.5 & value < 6000.5 ~ "5001 - 6000 ppb day",
      metric == "SOMO35" & value >= 6000.5 & value < 7000.5 ~ "6001 - 7000 ppb day",
      metric == "SOMO35" & value >= 7000.5 & value < 8000.5 ~ "7001 - 8000 ppb day",
      metric == "SOMO35" & value >= 8000.5 ~ ">= 8001 ppb day",
      metric %in% c("3MMDA1","6MMDA1") & value < 30.5 ~ "0 - 30 ppb",
      metric %in% c("3MMDA1","6MMDA1") & value >= 30.5 & value < 40.5 ~ "31 - 40 ppb",
      metric %in% c("3MMDA1","6MMDA1") & value >= 40.5 & value < 50.5 ~ "41 - 50 ppb",
      metric %in% c("3MMDA1","6MMDA1") & value >= 50.5 & value < 60.5 ~ "51 - 60 ppb",
      metric %in% c("3MMDA1","6MMDA1") & value >= 60.5 & value < 70.5 ~ "61 - 70 ppb",
      metric %in% c("3MMDA1","6MMDA1") & value >= 70.5 & value < 80.5 ~ "71 - 80 ppb",
      metric %in% c("3MMDA1","6MMDA1") & value >= 80.5 & value < 90.5 ~ "81 - 90 ppb",
      metric %in% c("3MMDA1","6MMDA1") & value >= 90.5 ~ ">= 91 ppb",
      metric == "AVGMDA8" & value < 35.5 ~ "0 - 35 ppb",
      metric == "AVGMDA8" & value >= 35.5 & value < 40.5 ~ "36 - 40 ppb",
      metric == "AVGMDA8" & value >= 40.5 & value < 45.5 ~ "41 - 45 ppb",
      metric == "AVGMDA8" & value >= 45.5 & value < 50.5 ~ "46 - 50 ppb",
      metric == "AVGMDA8" & value >= 50.5 & value < 55.5 ~ "51 - 55 ppb",
      metric == "AVGMDA8" & value >= 55.5 & value < 60.5 ~ "56 - 60 ppb",
      metric == "AVGMDA8" & value >= 60.5 & value < 65.5 ~ "61 - 65 ppb",
      metric == "AVGMDA8" & value >= 65.5 ~ ">= 66 ppb",
      .default = NA)
  ) |>
  collect() |>
  mutate(
    value_bin = factor(
      value_bin,
      levels = c(
        "0 - 30 ppb",
        "0 - 35 ppb",
        "36 - 40 ppb",
        "31 - 40 ppb",
        "41 - 50 ppb",
        "41 - 45 ppb",
        "46 - 50 ppb",
        "51 - 60 ppb",
        "51 - 55 ppb",
        "56 - 60 ppb",
        "61 - 70 ppb",
        "61 - 65 ppb",
        ">= 66 ppb",
        "71 - 75 ppb",
        "76 - 85 ppb",
        "71 - 80 ppb",
        "81 - 90 ppb",
        ">= 91 ppb",
        "86 - 100 ppb",
        ">= 100 ppb",
        "0 days",
        "0 - 3 days",
        "4 - 6 days",
        "7 - 15 days",
        "16 - 25 days",
        ">= 26 days",
        "0 - 1000 ppb day",
        "1001 - 2000 ppb day",
        "2001 - 3000 ppb day",
        "3001 - 4001 ppb day",
        "4001 - 5000 ppb day",
        "5001 - 6000 ppb day",
        "6001 - 7000 ppb day",
        "7001 - 8000 ppb day",
        ">= 8001 ppb day"
      )
    )
  ) |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "WGS84"
  ) |>
  st_transform(mycrs)

dbDisconnect(con, shutdown = T)

p_colours = list(
  "4MDA8" = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue","forestgreen", "gold", "chocolate1", "firebrick2", "firebrick4"),
  "NDGT70" = c("midnightblue","dodgerblue","gold", "chocolate1", "firebrick2", "firebrick4"),
  "SOMO35" = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue","forestgreen", "gold", "chocolate1", "firebrick2", "firebrick4"),
  "3MMDA1" = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue","forestgreen", "gold", "chocolate1", "firebrick4"),
  "6MMDA1" = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue","forestgreen", "gold", "chocolate1", "firebrick4"),
  "AVGMDA8" = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue","forestgreen", "gold", "chocolate1", "firebrick4")
)

world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
  st_transform(mycrs)

dirOut = here::here('figures','si_figures','metric_maps')

if(!dir.exists(dirOut)){
  dir.create(dirOut, recursive = T)
}

for(mtric in unique(dat$metric)){
  for(rgn in unique(dat$region)){

    plotDat = dat |>
      mutate(year = year(date)) |>
      filter(
        region == rgn,
        metric == mtric,
        !is.na(value),
        year %in% 2000:2021
      ) |>
      arrange(value)

    if(rgn == "Europe"){
      lim = tibble(lng = c(-20,35), lat = c(25,65)) |>
        st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
        st_transform(mycrs)
      h = 11
    }else{
      lim = tibble(lng = c(-130,-50), lat = c(25,50)) |>
        st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
        st_transform(mycrs)
      h = 7
    }

    g1 = ggplot()+
      geom_sf(data = world)+
      geom_sf(data = plotDat,
                 aes(colour = value_bin))+
      scale_colour_manual(values = p_colours[[mtric]], name = "")+
      scale_y_continuous(limits = st_coordinates(lim)[,2])+
      scale_x_continuous(limits = st_coordinates(lim)[,1])+
      facet_wrap(~year)+
      ggtitle(mtric)+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),
            legend.position = "bottom",
            legend.byrow = T)

    pdf(here(dirOut, paste0("metric_map_",str_replace_all(rgn, " ", "-"),"_", mtric, ".pdf")), width = 11, height = h)
    print(g1)
    dev.off()

  }
}
