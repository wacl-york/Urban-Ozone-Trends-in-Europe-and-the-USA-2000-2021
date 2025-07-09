library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(dbplyr)
library(wesanderson)
library(ggpubr)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

timezone_dat = tbl(con, "mda8_o3") |>
  select(station_id, timezone) |>
  distinct()

mda8_metrics = tbl(con, "mda8_metrics") |>
  left_join(timezone_dat, by = "station_id") |>
  collect()

station_id = unique(mda8_metrics$station_id)

pdf(here("analysis","mda8_metrics_inspect.pdf"), width = 16, height = 9)
for(i in station_id){

temp = mda8_metrics |>
  filter(station_id == paste0(i))
  #filter(station_id == "14561")

g = ggplot(temp)+
  geom_point(aes(x = year, y = value, colour = metric))+
  geom_line(aes(x = year, y = value, colour = metric)) +
  facet_wrap(~metric, scales = "free") +
  scale_colour_manual(values = wes_palette("Darjeeling1"))+
  theme_pubr() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  ggtitle(paste0(i))

print(g)

}
dev.off()

