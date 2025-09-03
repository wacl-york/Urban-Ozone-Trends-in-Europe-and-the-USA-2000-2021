library(ggplot2)

dat = tbl(con, "piecewise_stats_daily_day") |>
  filter(station_id == "1138",
         name == "o3") |>
  collect()

dat |>
  group_by(interaction(startYear, endYear)) |>
  mutate(piece = cur_group_id()) |>
  ungroup() |>
  ggplot()+
  geom_point(aes(date, anom))+
  geom_line(aes(date, piecewise, colour = factor(tau)), linewidth = 3)



