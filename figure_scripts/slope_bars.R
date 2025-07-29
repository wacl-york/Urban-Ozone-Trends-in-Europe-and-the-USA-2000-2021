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

p_colours = c("#A50021",
              "#FF6400",
              "#FFBA66",
              "#A4C171",
              "#000099",
              "#1E64FF",
              "#78BCFF",
              "#A4C171"
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
      make_slopeBin() |>
      mutate(dir = ifelse(fit > 0, "inc", "dec")) |>
      left_join(tbl(con, "combinedMeta") |>
                  select(station_id, country) |>
                  distinct() |>
                  collect(),
                "station_id") |>
      mutate(country = ifelse(country == "United States of America", country, "Europe")) |>
      group_by(name, country, tau, year, dir, metric, slopeBin) |>
      count() |>
      ungroup() |>
      mutate(n = ifelse(dir == "dec", n*-1, n)) |>
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
      mutate(dir = ifelse(fit > 0, "inc", "dec")) |>
      make_slopeBin() |>
      left_join(tbl(con, "combinedMeta") |>
                  select(station_id, country) |>
                  distinct() |>
                  collect(),
                "station_id") |>
      mutate(country = ifelse(country == "United States of America", country, "Europe")) |>
      group_by(name, country, tau, year, dir, slopeBin) |>
      count() |>
      ungroup() |>
      mutate(n = ifelse(dir == "dec", n*-1, n)) |>
      format_spc_name()

    if(str_detect(tableName, "mda8")){

      species = "O<sub>3</sub>"

    }else{
      species = c("O<sub>3</sub>", "NO<sub>2</sub>", "O<sub>x</sub>")
    }

    for(spc in species){

      plotList[[spc]] = plotDat |>
        filter(name == spc) |>
        ggplot()+
        geom_bar(aes(year,n, fill = slopeBin), stat = "identity", position = "stack")+
        geom_hline(aes(yintercept = 0))+
        scale_fill_manual(values = p_colours, name = "")+
        scale_y_continuous(name = "Number of Time Series")+
        facet_grid(country~tau, scales = "free_y")+
        theme_minimal()+
        theme(strip.text = element_markdown(),
              legend.position = "bottom",
              legend.byrow = T)+
        ggtitle(spc)
    }

  }

  fileOut = here::here("figures", paste0("slope_bars_", str_remove(tableName, "piecewise_stats_"), ".pdf"))

  pdf(fileOut,width = 12, height = 8)
  print(plotList)
  dev.off()

}


dbDisconnect(con, shutdown = T)
