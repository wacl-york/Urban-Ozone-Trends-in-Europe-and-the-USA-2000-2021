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

make_pvStr = function(df){

  df |>
    mutate(pvStr = case_when(
      pv <= 0.05 & fit < 0 ~ "p <= 0.05 (dec)",
      between(pv, 0.05, 0.1) & fit < 0 ~ "0.05 < p <= 0.10 (dec)",
      between(pv, 0.1, 0.33) & fit < 0 ~ "0.10  < p <= 0.33 (dec)",
      pv >= 0.33 & fit < 0 ~ "p > 0.33 (dec)",
      pv >= 0.33 & fit > 0 ~ "p > 0.33 (inc)",
      between(pv, 0.1, 0.33) & fit > 0 ~ "0.10  < p <= 0.33 (inc)",
      between(pv, 0.05, 0.1) & fit > 0 ~ "0.05 < p <= 0.10 (inc)",
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
      make_pvStr() |>
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
                                country = ifelse(country == "United States of America", country, "Europe"),
                    dir = ifelse(str_detect(pvStr, "inc"), "inc", "dec")) |>
      group_by(name, country, tau, region, dir, year, pvStr, metric) |>
      count() |>
      ungroup() |>
      mutate(n = ifelse(dir == "dec", n*-1, n)) |>
      format_spc_name()

    plotList$metric = plotDat |>
      ggplot()+
      geom_bar(aes(year,n, fill = pvStr), stat = "identity", position = "stack")+
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
      unnest(data)

    plotDat = slopes |>
      filter(
        fit != 0, # there are a few times the slope is zero, but we can 't plot that here so just get rid. They are always p == 1.
      ) |>
      make_pvStr() |>
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
             country = ifelse(country == "United States of America", country, "Europe"),
             dir = ifelse(str_detect(pvStr, "inc"), "inc", "dec")) |>
      group_by(name, country, region, tau, dir, year, pvStr) |>
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
        filter(name == spc,
               tau %in% c(0.05, 0.5, 0.95)) |>
        ggplot()+
        geom_bar(aes(year,n, fill = pvStr), stat = "identity", position = "stack")+
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

  }

  fileOut = here::here("figures", paste0("significance_bars_by_region_", str_remove(tableName, "piecewise_stats_"), ".pdf"))

  pdf(fileOut,width = 8, height = 6)
  print(plotList)
  dev.off()

}


dbDisconnect(con, shutdown = T)
