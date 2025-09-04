library(sf)
library(DBI)
library(ggh4x)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))

################################################################################

clean_df = tibble(
  metric = character(),
  station_id = character(),
  name    = factor(),     # <fct>
  tau     = double(),     # <dbl>
  year    = integer(),    # <int>
  startYear = double(),
  endYear = double(),
  aic = double(),
  scenario_idx = double(),
  country = character(),  # <chr>
  fit = double(),
  se = double(),
  pv = double(),
  pvStr = factor(),
  #rm = double(),
  type = character()
)

# clean_df_metrics = tibble(
#   name    = factor(),     # <fct>
#   country = character(),  # <chr>
#   region  = character(),  # <chr>
#   tau     = double(),     # <dbl>
#   dir     = character(),  # <chr>
#   year    = integer(),    # <int>
#   pvStr   = factor(),     # <fct>
#   metric  = character(),  # <chr>
#   n       = double(),     # <dbl>
#   table    = character()   # <chr>
# )

###############################################################################

con = connect_to_db()

tables = dbListTables(con)[str_detect(dbListTables(con), "piecewise_stats_freeTau")]

pv_opt = c("p <= 0.05 (dec)",
           "0.05 < p <= 0.1 (dec)",
           "0.1  < p <= 0.33 (dec)",
           "p > 0.33",
           "0.1  < p <= 0.33 (inc)",
           "0.05 < p <= 0.1 (inc)",
           "p <= 0.05 (inc)"
)



for(i in 1:length(tables)){

  tableName = tables[i]

  tableName = tables[i]
  if(str_detect(tableName, "metric")){
    groupVars = c("station_id", "name", "tau", "metric")
    facetFormula = metric~factor(year)
  }else{
    groupVars = c("station_id", "name", "tau")
    facetFormula = tau~factor(year)
  }

  slopes = tbl(con, tableName) |>
    filter(stat == "slope") |>
    left_join(
      tbl(con, "combinedMeta") |>
        select(station_id, latitude, longitude, country) |>
        distinct(),
      by = "station_id") |>
    collect() |>
    arrange(pick(all_of(groupVars))) |>
    pivot_wider(names_from = "type") |>
    select(-stat) |>
    nest_by(pick(all_of(groupVars))) |>
    mutate(data = data |>
             expand_slopes() |>
             list()) |>
    unnest(data) |>
    mutate(fit = fit*365) |>
    ungroup() |>
    filter(between(fit, quantile(fit, 0.01), quantile(fit, 0.99))) |>
    mutate(
      pvStr = case_when(
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

  if(str_detect(tableName, "metric")){

    range_maxs = slopes |>
      mutate(fit = abs(fit)) |>
      group_by(metric) |>
      summarise(rm = quantile(fit, 0.99))

    lineGroups = slopes |>
      nest_by(metric) |>
      left_join(range_maxs, "metric") |>
      mutate(data = data |>
               calc_arrow_end(rangeMax = rm,
                              slope = "fit",
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
               list()) |>
      unnest(data)

  }else{
    lineGroups = slopes |>
    calc_arrow_end(rangeMax = 2,
                   slope = "fit",
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
      select(-suffix)
  }

  plotDat = lineGroups |>
    filter(name == "o3",
           year %in% c(2002, 2006, 2010, 2014, 2018, 2022)) |>
    mutate(name = ifelse(name == "no2",  "NO<sub>2</sub>", "O<sub>3</sub>") |>
             factor(levels = c( "O<sub>3</sub>","NO<sub>2</sub>"))) |>
    select(-c(se, rn)) |>
    mutate(type = paste0(tableName))

  clean_df = bind_rows(clean_df, plotDat)

}


#############################################

combined_meta = tbl(con, "combinedMeta") |>
  collect() |>
  select(station_id, country, timezone, station_type) |>
  distinct() |>
  mutate(region = case_when(country %in% c("austria", "belgium", "switzerland", "czechia", "germany", "denmark",
                                           "estonia", "finland", "france", "united_kingdom", "ireland", "iceland",
                                           "netherlands", "norway", "poland", "sweden", "slovakia") ~ "Northern and Central Europe",
                            country %in% c("bulgaria", "greece", "italy", "portugal", "spain", "slovenia") ~ "Southern Europe",
                            timezone %in% c("America/Chicago", "America/New_York", "America/Indiana/Indianapolis",
                                            "America/Kentucky/Louisville", "America/Detroit", "America/North_Dakota/Beulah"
                                            ) ~ "Eastern and Central US",
                            timezone %in% c("America/Los_Angeles", "America/Phoenix", "America/Denver", "America/Boise") ~ "Western US",
                            timezone %in% c("Pacific/Honolulu", "America/Puerto_Rico", )
                            timezone %in% c("America/Anchorage")
                            .default = NA))


site_count = combined_meta |>
  distinct() |>
  select(region) |>
  group_by(region) |>
  count()

dat_explr = clean_df |>
  left_join(combined_meta, by = c("country", "station_id")) |>
  mutate(region = case_when(country %in% c("austria", "belgium", "switzerland", "czechia", "germany", "denmark",
                                           "estonia", "finland", "france", "united_kingdom", "ireland", "iceland",
                                           "netherlands", "norway", "poland", "sweden", "slovakia") ~ "Northern and Central Europe",
                            country %in% c("bulgaria", "greece", "italy", "portugal", "spain", "slovenia") ~ "Southern Europe",
                            timezone %in% c("America/Chicago", "America/New_York") ~ "Eastern and Central US",
                            timezone %in% c("America/Los_Angeles", "America/Phoenix", "America/Denver", "Pacific/Honolulu") ~ "Western US and Hawaii",
                            .default = NA))

dat_sig_inc = dat_explr |>
  filter(pvStr == "p <= 0.05 (inc)") |>
  select(tau, year, region, fit, pv, type) |>
  group_by(tau, year, region, type) |>
  distinct() |>
  count()



dbDisconnect(con, shutdown = T)

