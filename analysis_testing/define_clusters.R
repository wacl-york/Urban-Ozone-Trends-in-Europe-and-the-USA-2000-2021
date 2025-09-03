library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))

# -------------------------------------------------------------------------

con = connect_to_db()

#tables = dbListTables(con)[str_detect(dbListTables(con), "cluster")]

cluster_timeseries <- tbl(con, "clusterTimeSeries") |>
  collect() |>
  filter((region == "Europe" & type == "mda8") |
           (region == "United States of America" & type == "mda8_warm"),
         tau == 0.95) |>
  mutate(cluster_region = case_when(region == "Europe" & cluster == 1 ~ "Central Europe",
                                    region == "Europe" & cluster == 2 ~ "Southern Europe",
                                    region == "Europe" & cluster == 8 ~ "Far Eastern Europe",
                                    region == "Europe" & cluster == 9 ~ "Eastern Europe",
                                    region == "Europe" & cluster == 12 ~ "North Eastern Europe",
                                    region == "Europe" & cluster == 18 ~ "South Western Europe",
                                    region == "Europe" & cluster == 29 ~ "North Western Europe",
                                    region == "Europe" & cluster == 33 ~ "Western Europe",
                                    region == "United States of America" & cluster == 1 ~ "Gulf of Mexico",
                                    region == "United States of America" & cluster == 4 ~ "South Western USA",
                                    region == "United States of America" & cluster == 6 ~ "South and West USA",
                                    region == "United States of America" & cluster == 22 ~ "Florida",
                                    region == "United States of America" & cluster == 26 ~ "Ohio Valley",
                                    region == "United States of America" & cluster == 28 ~ "Northeastern USA")) |>
  na.omit(cluster_region)

#dbDisconnect(con)

#####################################################################################################################

cluster_timeseries <- tbl(con, "clusterTimeSeries") |>
  collect() |>
  filter((region == "Europe" & type == "mda8") |
           (region == "United States of America" & type == "mda8_warm"),
         tau == 0.95) |>
  mutate(cluster_region = case_when(region == "Europe" & cluster == 1 ~ "C EU",
                                    region == "Europe" & cluster == 2 ~ "S EU",
                                    region == "Europe" & cluster == 8 ~ "FE EU",
                                    region == "Europe" & cluster == 9 ~ "E Eu",
                                    region == "Europe" & cluster == 12 ~ "NE EU",
                                    region == "Europe" & cluster == 18 ~ "SW EU",
                                    region == "Europe" & cluster == 29 ~ "NW EU",
                                    region == "Europe" & cluster == 33 ~ "W EU",
                                    region == "United States of America" & cluster == 1 ~ "Gulf of Mexico",
                                    region == "United States of America" & cluster == 4 ~ "SW US",
                                    region == "United States of America" & cluster == 6 ~ "S & W USA",
                                    region == "United States of America" & cluster == 22 ~ "Florida",
                                    region == "United States of America" & cluster == 26 ~ "Ohio Valley",
                                    region == "United States of America" & cluster == 28 ~ "Northeastern USA")) |>
  na.omit(cluster_region) |>
  select(station_id, cluster_region)

dbDisconnect(con)
