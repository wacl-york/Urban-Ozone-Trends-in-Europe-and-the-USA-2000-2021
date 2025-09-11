library(DBI)
library(here)
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

dbListTables(con)

opts = tribble(
  ~piecewiseTable, ~clusterType, ~title,
  "piecewise_stats_freeTau_mda8_anom_all",    "mda8", "MDA8O<sub>3</sub> Trend / ppbv yr<sup>-1</sup>",
  "piecewise_stats_freeTau_mda8_anom_cold",   "mda8_warm", "MDA8O<sub>3</sub> Warm Season Trend / ppbv yr<sup>-1</sup>",
  "piecewise_stats_freeTau_mda8_anom_warm",   "mda8_cold", "MDA8O<sub>3</sub> Cold Season Trend / ppbv yr<sup>-1</sup>"
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
    # filter(!metric %in% c("4MDA8", "NDGT70"))
    filter(metric == "6MMDA1") |>
    arrange(cluster) |>
    mutate(
      cluster = as.character(cluster),
      cluster = ifelse(cluster == 99, "No Cluster", cluster),
      cluster = factor(cluster,
                       levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "No Cluster")),
      tau = paste0("&tau; = ", tau)
    ) |>
    make_pvStr(7) |>
    mutate(pvStr = factor(pvStr, levels = rev(levels(pvStr))))

  g1 = dat |>
    ggplot()+
    geom_point(aes(value, fit, colour = cluster))+
    geom_hline(yintercept = 0)+
    facet_nested(region + tau ~ year, scale = "free")+
    scale_y_continuous(name = opts$title[i])+
    scale_x_continuous(name = "6MMDA1 / ppbv")+
    scale_colour_manual(
      name = "Cluster",
      values = c("1" = "#E41A1C",
                 "2" = "#377EB8",
                 "3" = "#4DAF4A",
                 "4" = "#984EA3",
                 "5" = "#FF7F00",
                 "6" = "#FFFF33",
                 "7" = "#A65628",
                 "8" = "#F781BF",
                 "9" = "#8DD3C7",
                 "10" = "#B3DE69",
                 "No Cluster" = "#999999"
      ))+
    theme_minimal()+
    theme(
      axis.title = element_markdown(),
      strip.text = element_markdown()
    )

  g2 = dat |>
    ggplot()+
    geom_point(aes(value, fit, colour = pvStr))+
    geom_hline(yintercept = 0)+
    facet_nested(region + tau ~ year, scale = "free")+
    scale_y_continuous(name = opts$title[i])+
    scale_x_continuous(name = "6MMDA1 / ppbv")+
    scale_colour_manual(
      name = "",
      values = c(
        "p <= 0.05 (inc)" = "#A50021",
        "0.05 < p <= 0.1 (inc)" = "#FF6400",
        "0.1  < p <= 0.33 (inc)" = "#FFBA66",
        "p > 0.33"  = "#A4C171",
        "0.1  < p <= 0.33 (dec)"  = "#78BCFF",
        "0.05 < p <= 0.1 (dec)" = "#1E64FF",
        "p <= 0.05 (dec)" = "#000099"
      )
    )+
  theme_minimal()+
  theme(
    axis.title = element_markdown(),
    strip.text = element_markdown()
  )

  dirOutP = here::here('figures','paper_figures','mda8_6mmda1')
  dirOutSI = here::here('figures','si_figures','mda8_6mmda1')

  if(!dir.exists(dirOutP)){
    dir.create(dirOutP)
  }

  if(!dir.exists(dirOutSI)){
    dir.create(dirOutSI)
  }

  grDevices::cairo_pdf(here(dirOutP, paste0(opts$clusterType[i], "_cluster_mda8_6mmda1.pdf")), width = 11, height = 7)
  print(g1)
  dev.off()

  grDevices::cairo_pdf(here(dirOutSI, paste0(opts$clusterType[i], "_sig_mda8_6mmda1.pdf")), width = 11, height = 7)
  print(g2)
  dev.off()

}

dbDisconnect(con, shutdown = T)
