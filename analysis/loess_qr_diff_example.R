library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

qr_50 = tbl(con, "qr_stat") %>%
  filter(type == "fit",
         tau == 0.5) %>%
  pivot_wider(names_from = "stat")

dat = tbl(con, "monthly_anom") |>
  left_join(tbl(con, "loess"), by = c("name", "station_id", "x")) |>
  left_join(qr_50, by = c("name", "station_id")) |>
  mutate(qr = (slope*x)+intercept,
         diff = qr-loess,
         diff = abs(diff)) |>
  collect()

diffSum = dat |>
  select(station_id, name, diff) |>
  group_by(station_id, name) |>
  summarise(diffSum = sum(diff, na.rm = T)) |>
  arrange(desc(diffSum)) |>
  ungroup() |>
  mutate(idx = row_number())

diffSum |>
  ggplot()+
  geom_histogram(aes(diffSum), binwidth = 100)+
  theme_classic()


pdf(here("diff_test.pdf"), width = 12, height = 9)
for(i in 1:nrow(diffSum)){
  g = dat |>
    filter(station_id == diffSum$station_id[i],
           name == diffSum$name[i]) |>
    select(date, anom, loess, qr, diff) |>
    pivot_longer(-date) |>
    ggplot()+
    geom_line(aes(date, value, colour = name))+
    geom_hline(aes(yintercept = 0))+
    ggtitle(label = paste0(diffSum$station_id[i], " - ", diffSum$name[i], " - ", round(diffSum$diffSum[i], 2)))+
    theme_classic()

  print(g)
}

dev.off()

