library(sf)
library(DBI)
library(dplyr)
library(tidyr)
library(ggh4x)
library(ggtext)
library(ggplot2)
library(patchwork)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','dtw_helpers.R'))
source(here::here('functions','plotting_utils.R'))

con = connect_to_db()
mycrs = 4087

world = rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,30), lat = c(35,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

dbListTables(con)

opts = tribble(
  ~piecewiseTable, ~clusterType,
  "piecewise_stats_freeTau_daily_all",        "daily_all",
  "piecewise_stats_freeTau_daily_all_cold",   "daily_all_cold",
  "piecewise_stats_freeTau_daily_all_warm",   "daily_all_warm",
  "piecewise_stats_freeTau_daily_day",        "daily_day",
  "piecewise_stats_freeTau_daily_day_cold",   "daily_day_cold",
  "piecewise_stats_freeTau_daily_day_warm",   "daily_day_warm",
  "piecewise_stats_freeTau_daily_night",      "daily_night",
  "piecewise_stats_freeTau_daily_night_cold", "daily_night_cold",
  "piecewise_stats_freeTau_daily_night_warm", "daily_night_warm",
  "piecewise_stats_freeTau_mda8_anom_all",    "mda8",
  "piecewise_stats_freeTau_mda8_anom_cold",   "mda8_warm",
  "piecewise_stats_freeTau_mda8_anom_warm",   "mda8_cold"
)

for(i in 1:nrow(opts)){
  datCluster = tbl(con, "clusterTimeSeries_meancvi") |>
    collect() |>
    reindex_clusters() |>
    ungroup() |>
    filter(type == opts$clusterType[i]) |>
    select(-type)

  datMetrics = tbl(con, "dat_metrics") |>
    pivot_wider(names_from = "metric") |>
    mutate(year = year(date)) |>
    select(-name, -date, -timezone, -x) |>
    collect()

  dat = tbl(con, opts$piecewiseTable[i]) |>
    pivot_wider(names_from = "type") |>
    filter(stat == "slope") |>
    collect() |>
    left_join(datCluster, c("station_id", "tau")) |>
    expand_slopes() |>
    left_join(datMetrics, c("station_id", "year")) |>
    mutate(fit = fit*365,
           se = se*365) |>
    filter(
      year %in% c(2004, 2018),
           cluster != 99,
           tau %in% c(0.05, 0.5, 0.95)) |>
    pivot_longer(c("6MMDA1", "SOMO35", "4MDA8", "NDGT70", "3MMDA1", "AVGMDA8"), names_to = "metric") |>
    filter(!metric %in% c("4MDA8", "NDGT70"))

  mods = dat |>
    nest_by(tau, region, metric, year) |>
    mutate(mod = lm(fit~ value, data = data, weights = 1/(data$se)) |>
             list(),
           slope = coef(mod)[2],
           int = coef(mod)[1],
           r2 = summary(mod)$r.squared) |>
    left_join(
      dat |>
        group_by(tau, region) |>
        summarise(
          ypos = min(fit, na.rm = T)   + (max(fit, na.rm = T)   - min(fit, na.rm = T))*0.8
        ) ,
      by = c("region", "tau")
    ) |>
    left_join(
      dat |>
        group_by(metric, year) |>
        summarise(
          xpos = min(value, na.rm = T) + (max(value, na.rm = T) - min(value, na.rm = T))*0.2
        ) ,
      by = c("metric", "year")
    )

  g1 = dat |>
    arrange(cluster) |>
    ggplot()+
    geom_point(aes(value, fit, colour = factor(cluster)))+
    geom_hline(yintercept = 0)+
    geom_abline(data = mods,
                aes(slope = slope, intercept = int))+
    geom_text(
      data = mods,
      aes(x = xpos, y = ypos, label = paste0("R2:", round(r2, 2), "\n m:", round(slope, 2)))
    )+
    facet_nested(region + tau ~ metric+year, scale = "free")+
    theme_minimal()+
    ggtitle(opts$clusterType[i])

  dirOut = here::here('figures','slope_metric_cluster_scatter')

  if(!dir.exists(dirOut)){
    dir.create(dirOut)
  }

  pdf(file.path(dirOut, paste0("slope_scatter_",opts$clusterType[i], ".pdf")), width = 11, height = 8)
  print(g1)
  dev.off()


  if(i == 10){

    scatter = list()
    maps = list()

    for(rgn in c("Europe", "United States of America")){
      modsSub = mods |>
        filter(
          metric == "6MMDA1",
          tau == 0.95,
          region == rgn
        )

      if(rgn != "Europe"){
        modsSub$xpos = 50
      }

      datSub = dat |>
        filter(
          metric == "6MMDA1",
          tau == 0.95,
          region == rgn
        ) |>
        arrange(cluster)


      cluster_col = c(
        "1" = scales::hue_pal()(7)[1],
        "2" = scales::hue_pal()(7)[2],
        "3" = scales::hue_pal()(7)[3],
        "4" = scales::hue_pal()(7)[4],
        "5" = scales::hue_pal()(7)[5],
        "6" = scales::hue_pal()(7)[6],
        "7" = scales::hue_pal()(7)[7]
      )

      datSubSf = datSub |>
        left_join(
          combinedMetaRegion(con) |>
            select(station_id, latitude, longitude) |>
            collect() |>
            distinct(),
          by = "station_id"
        ) |>
        st_as_sf(
          coords = c("longitude", "latitude"),
          crs = "WGS84"
        ) |>
        st_transform(mycrs)

      scatter[[rgn]] = datSub |>
        ggplot()+
        geom_point(aes(value, fit, colour = factor(cluster)))+
        geom_hline(yintercept = 0)+
        geom_abline(data = modsSub,
                    aes(slope = slope, intercept = int))+
        geom_text(
          data = modsSub,
          aes(x = xpos, y = ypos, label = paste0("R2:", round(r2, 2), "\n m:", round(slope, 2)))
        )+
        scale_colour_manual(values = cluster_col, name = "Cluster")+
        scale_y_continuous(name = "MDA8 Trend")+
        scale_x_continuous(name = "6MMDA1")+
        facet_nested(~year, scale = "free")+
        theme_minimal()+
        guides(color = "none")

      maps[[rgn]] = ggplot()+
        geom_sf(data = world)+
        geom_sf(data = datSubSf, aes(colour = factor(cluster)))+
        scale_colour_manual(values = cluster_col, name = "Cluster")+
        theme_minimal()+
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.major = element_blank(),
              strip.text = element_markdown(),
              legend.position = "bottom",
              legend.byrow = T)+
        guides(color = "none")

        if(rgn == "Europe"){

          maps[[rgn]] = maps[[rgn]]+
            scale_y_continuous(limits = st_coordinates(limEU)[,2])+
            scale_x_continuous(limits = st_coordinates(limEU)[,1])

        }else{
          maps[[rgn]] = maps[[rgn]]+
            scale_y_continuous(limits = st_coordinates(limUS)[,2])+
            scale_x_continuous(limits = st_coordinates(limUS)[,1])
        }
    }

    g2 = list(scatter$Europe,
         maps$Europe,
         scatter$`United States of America`,
         maps$`United States of America`) |>
      wrap_plots()

    png(data_path("figures", "acc", "scatter_map.png"),res = 300, width = 3615, height = 2542)
    print(g2)
    dev.off()

  }

}

dbDisconnect(con, shutdown = T)


