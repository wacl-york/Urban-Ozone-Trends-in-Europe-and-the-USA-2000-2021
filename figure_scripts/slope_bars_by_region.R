library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)

source(here::here('functions','utils.R'))

expand_slopes = function(df){
  years = tibble(year = min(df$startYear):max(df$endYear))

  years |>
    left_join(df, join_by(between(year, startYear, endYear, bounds = "[]")))
}

make_slopeBin = function(df) {
  df |>
    mutate(slopeBin = case_when(
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


format_spc_name = function(df){
  df |>
    mutate(name = case_when(name == "no2" ~ "NO<sub>2</sub>",
                            name == "o3" ~ "O<sub>3</sub>",
                            name == "ox" ~ "O<sub>x</sub>" ) |>
             factor(levels = c("O<sub>3</sub>", "NO<sub>2</sub>", "O<sub>x</sub>")),
           n = case_when(country == "United States of America" & year %in% c(2000:2001, 2020:2023) ~ NA,
                         country == "Europe" & year %in% c(2000:2001, 2021:2023) ~ NA,
                         TRUE ~ n
           ))
}

# -------------------------------------------------------------------------


con = connect_to_db()

tables = dbListTables(con)[str_detect(dbListTables(con), "piecewise_stats_freeTau")]
#tables = "piecewise_stats_freeTau_metrics"

p_colours = c("#A50021",
              "#FF6400",
              "#FFBA66",
              "#A4C171",
              "#000099",
              "#1E64FF",
              "#78BCFF",
              "#A4C171"
)

clean_df = tibble(
  name    = factor(),     # <fct>
  country = character(),  # <chr>
  region  = character(),  # <chr>
  tau     = double(),     # <dbl>
  dir     = character(),  # <chr>
  year    = integer(),    # <int>
  slopeBin  = character(),     # <chr>
  n       = double(),     # <dbl>
  table    = character()   # <chr>
)

clean_df_metrics = tibble(
  name    = factor(),     # <fct>
  country = character(),  # <chr>
  region  = character(),  # <chr>
  tau     = double(),     # <dbl>
  dir     = character(),  # <chr>
  year    = integer(),    # <int>
  slopeBin   = factor(),     # <chr>
  metric  = character(),  # <chr>
  n       = double(),     # <dbl>
  table    = character()   # <chr>
)

for(i in 1:length(tables)){

  tableName = tables[i]

  plotList = list()

  if(str_detect(tableName, "metric")){

    slopes = tbl(con, tableName) |>
      filter(stat == "slope") |>
      collect() |>
      arrange(station_id, name, tau) |>
      pivot_wider(names_from = "type") |>
      select(-stat) |>
      nest_by(station_id, name, tau, metric) |>
      mutate(data = data |>
               expand_slopes() |>
               list()) |>
      unnest(data)

    plotDat = slopes |>
      filter(
        fit != 0, # there are a few times the slope is zero, but we can 't plot that here so just get rid. They are always p == 1.
      ) |>
      make_slopeBin()|>
      mutate(dir = ifelse(fit > 0, "inc", "dec")) |>
      left_join(tbl(con, "combinedMeta") |>
                  select(station_id, country, timezone) |>
                  distinct() |>
                  collect(),
                "station_id") |>
      mutate(region = case_when(country %in% c("austria", "belgium", "switzerland", "czechia", "germany", "denmark",
                                               "estonia", "finland", "france", "united_kingdom", "ireland", "iceland",
                                               "netherlands", "norway", "poland", "sweden", "slovakia") ~ "Northern and Central Europe",
                                country %in% c("bulgaria", "greece", "italy", "portugal", "spain", "slovenia") ~ "Southern Europe",
                                timezone %in% c("America/Chicago", "America/New_York") ~ "Eastern and Central US",
                                timezone %in% c("America/Los_Angeles", "America/Phoenix", "America/Denver", "Pacific/Honolulu") ~ "Western US and Hawaii",
                                .default = NA),
                                country = ifelse(country == "United States of America", country, "Europe")) |>
      group_by(name, country, tau, region, dir, year, slopeBin, metric) |>
      count() |>
      ungroup() |>
      mutate(n = ifelse(dir == "dec", n*-1, n)) |>
      mutate(table = paste0(tableName)) |>
      format_spc_name()

    plotList$metric = plotDat |>
      ggplot()+
      geom_bar(aes(year,n, fill = slopeBin), stat = "identity", position = "stack")+
      geom_hline(aes(yintercept = 0))+
      scale_fill_manual(values = p_colours, name = "")+
      scale_y_continuous(name = "Number of Time Series")+
      facet_grid(country~metric, scales = "free_y")+
      theme_minimal()+
      theme(strip.text = element_markdown(),
            legend.position = "bottom",
            legend.byrow = T)

    clean_df_metrics = bind_rows(clean_df_metrics, plotDat)

  }else{

    slopes = tbl(con, tableName) |>
      filter(stat == "slope") |>
      collect() |>
      arrange(station_id, name, tau) |>
      pivot_wider(names_from = "type") |>
      select(-stat) |>
      nest_by(station_id, name, tau) |>
      mutate(data = data |>
               expand_slopes() |>
               list()) |>
      unnest(data) |>
      mutate(fit = fit*365)

    plotDat = slopes |>
      filter(
        fit != 0, # there are a few times the slope is zero, but we can 't plot that here so just get rid. They are always p == 1.
      ) |>
      make_slopeBin() |>
      mutate(dir = ifelse(fit > 0, "inc", "dec")) |>
      left_join(tbl(con, "combinedMeta") |>
                  select(station_id, country, timezone) |>
                  distinct() |>
                  collect(),
                "station_id") |>
      mutate(region = case_when(country %in% c("austria", "belgium", "switzerland", "czechia", "germany", "denmark",
                                               "estonia", "finland", "france", "united_kingdom", "ireland", "iceland",
                                               "netherlands", "norway", "poland", "sweden", "slovakia") ~ "Northern and Central Europe",
                                country %in% c("bulgaria", "greece", "italy", "portugal", "spain", "slovenia") ~ "Southern Europe",
                                timezone %in% c("America/Chicago", "America/New_York") ~ "Eastern and Central US",
                                timezone %in% c("America/Los_Angeles", "America/Phoenix", "America/Denver", "Pacific/Honolulu") ~ "Western US and Hawaii",
                                .default = NA),
             country = ifelse(country == "United States of America", country, "Europe")) |>
      group_by(name, country, region, tau, dir, year, slopeBin) |>
      count() |>
      ungroup() |>
      mutate(n = ifelse(dir == "dec", n*-1, n)) |>
      mutate(table = paste0(tableName)) |>
      format_spc_name()

    if(str_detect(tableName, "mda8")){

      species = "O<sub>3</sub>"

    }else{
      species = c("O<sub>3</sub>", "NO<sub>2</sub>", "O<sub>x</sub>")
    }

    for(spc in species){

      plotList[[spc]] = plotDat |>
        filter(name == spc,
               tau %in% c(0.05, 0.5, 0.95)) |>
        ggplot()+
        geom_bar(aes(year,n, fill = slopeBin), stat = "identity", position = "stack")+
        geom_hline(aes(yintercept = 0))+
        scale_fill_manual(values = p_colours, name = "")+
        scale_y_continuous(name = "Number of Time Series")+
        facet_grid(country~region~tau, scales = "free_y")+
        theme_minimal()+
        theme(strip.text = element_markdown(),
              legend.position = "bottom",
              legend.byrow = T)+
        ggtitle(spc)
    }

    clean_df = bind_rows(clean_df, plotDat)

  }

  fileOut = here::here("figures", paste0("slope_bars_by_region_", str_remove(tableName, "piecewise_stats_"), ".pdf"))

  pdf(fileOut,width = 10, height = 8)
  print(plotList)
  dev.off()

  table_df  = clean_df
  table_df_metrics = clean_df_metrics


}


dbDisconnect(con, shutdown = T)

table_df = table_df |>
  group_by(name, country, region, tau, year, table) |>
  mutate(n_sum = sum(abs(n), na.rm = T)) |>
  ungroup() |>
  group_by(name, country, region, tau, dir, year, slopeBin, table) |>
  mutate(perc_n = (abs(n)/n_sum)*100) |>
  ungroup()


daily_day = table_df |>
  filter(name == "O<sub>3</sub>",
         table == "piecewise_stats_freeTau_daily_day_warm",
         tau %in% c(0.5),
         year %in% c(2002:2020),
         #year %in% c(2002, 2020),
         #region %in% c("Southern Europe", "Northern and Central Europe"),
         region == "Southern Europe")
         #region == "Northern and Central Europe",
         #pvStr == "p <= 0.05 (inc)"
         #pvStr == "p <= 0.05 (dec)",
         #pvStr %in% c("p <= 0.05 (inc)", "0.05 < p <= 0.10 (inc)", "0.10  < p <= 0.33 (inc)", "p > 0.33 (inc)")

daily_day_pos_neg_perc = table_df |>
  filter(name == "O<sub>3</sub>",
         table == "piecewise_stats_freeTau_daily_day_warm") |>
  select(name, country, region, tau, dir, year, n, table) |>
  group_by(name, country, region, tau, year, table) |>
  mutate(year_sum_region = sum(abs(n), na.rm = T)) |>
  ungroup() |>
  group_by(name, country, region, dir, tau, year, table) |>
  mutate(year_sum_dir = sum(abs(n), na.rm = T)) |>
  ungroup() |>
  mutate(perc_year_sum_dir = (year_sum_dir/year_sum_region)*100) |>
  filter(tau %in% c(0.5),
         year %in% c(2002:2020),
         dir == "inc",
         #region == "Northern and Central Europe"),
         region == "Southern Europe")

##########################################

daily_day = table_df |>
  filter(name == "O<sub>3</sub>",
         table == "piecewise_stats_freeTau_mda8_anom_warm",
         tau %in% c(0.5),
         year %in% c(2002:2020),
         #year %in% c(2002, 2020),
         #region %in% c("Southern Europe", "Northern and Central Europe"),
         region == "Southern Europe")

daily_day_pos_neg_perc = table_df |>
  filter(name == "O<sub>3</sub>",
         table == "piecewise_stats_freeTau_mda8_anom_warm") |>
  select(name, country, region, tau, dir, year, n, table) |>
  group_by(name, country, region, tau, year, table) |>
  mutate(year_sum_region = sum(abs(n), na.rm = T)) |>
  ungroup() |>
  group_by(name, country, region, dir, tau, year, table) |>
  mutate(year_sum_dir = sum(abs(n), na.rm = T)) |>
  ungroup() |>
  mutate(perc_year_sum_dir = (year_sum_dir/year_sum_region)*100) |>
  filter(tau %in% c(0.05),
         year %in% c(2002:2020),
         dir == "inc",
         #region == "Northern and Central Europe")
         region == "Southern Europe")
