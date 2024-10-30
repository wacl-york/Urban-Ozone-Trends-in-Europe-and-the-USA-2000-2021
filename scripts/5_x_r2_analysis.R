library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines("data_config.txt",n = 1),"data","db.duckdb"), read_only = TRUE)

tbl(con, "reg_anom_r2") |>
  group_by(station_id, name, reg) |>
  filter(r2 == max(r2, na.rm = TRUE)) |>
  ungroup() |>
  filter(station_id == "8227")

best_regs = tbl(con, "reg_anom_r2") |>
  group_by(station_id, name, reg) |>
  filter(r2 == max(r2, na.rm = TRUE)) |>
  ungroup() |>
  select(-scenario_idx) |>
  pivot_wider(names_from = "reg", values_from = "r2") |>
  mutate(norm_reg = qr) |>
  pivot_longer(c(loess, qr, piecewise), names_to = "reg", values_to = "r2")

order_o3 = best_regs |>
  filter(reg == "loess",
         name == "o3") |>
  arrange(r2) |>
  select(station_id) |>
  collect() |>
  pluck("station_id")

order_no2 = best_regs |>
  filter(reg == "loess",
         name == "no2") |>
  arrange(r2) |>
  select(station_id) |>
  collect() |>
  pluck("station_id")

# -------------------------------------------------------------------------


g_o3 = best_regs |>
  filter(name == "o3") |>
  collect() |>
  mutate(station_id = factor(station_id, levels = !!order_o3)) |>
  ggplot()+
  geom_point(aes(r2, station_id, colour = reg))+
  scale_y_discrete(guide = guide_axis(n.dodge = 3))+
  facet_wrap(~name, ncol = 1)+
  AQVisR::AQvis_plotTheme()

g_no2 = best_regs |>
  filter(name == "no2") |>
  collect() |>
  mutate(station_id = factor(station_id, levels = !!order_no2)) |>
  ggplot()+
  geom_point(aes(r2, station_id, colour = reg))+
  scale_y_discrete(guide = guide_axis(n.dodge = 3), position = "right")+
  facet_wrap(~name, ncol = 1)+
  AQVisR::AQvis_plotTheme()

g_o3+g_no2

# -------------------------------------------------------------------------

best_regs_diff = best_regs |>
  mutate(r2_diff = r2-norm_reg)

order_o3_norm = best_regs_diff |>
  collect() |>
  filter(reg == "piecewise",
         name == "o3") |>
  arrange(r2_diff) |>
  select(station_id) |>
  pluck("station_id")

order_no2_norm = best_regs_diff |>
  collect() |>
  filter(reg == "piecewise",
         name == "no2") |>
  arrange(r2_diff) |>
  select(station_id) |>
  pluck("station_id")

g_o3_norm = best_regs_diff |>
  filter(name == "o3") |>
  collect() |>
  mutate(station_id = factor(station_id, levels = !!order_o3_norm)) |>
  ggplot()+
  geom_point(aes(r2_diff, station_id, shape = reg, colour = r2), size = 2)+
  scale_y_discrete(guide = guide_axis(n.dodge = 3))+
  scico::scale_color_scico(palette = "batlow", limits = c(0,1))+
  # scale_colour_viridis_c(limits = c(0,1))+
  facet_wrap(~name, ncol = 1)+
  AQVisR::AQvis_plotTheme()

g_no2_norm = best_regs_diff |>
  filter(name == "no2") |>
  collect() |>
  mutate(station_id = factor(station_id, levels = !!order_no2_norm)) |>
  ggplot()+
  geom_point(aes(r2_diff, station_id, shape = reg, colour = r2), size = 2)+
  scale_y_discrete(guide = guide_axis(n.dodge = 3), position = "right")+
  scico::scale_color_scico(palette = "batlow", limits = c(0,1))+
  # scale_colour_viridis_c(limits = c(0,1))+
  facet_wrap(~name, ncol = 1)+
  AQVisR::AQvis_plotTheme()

wrap_plots(g_o3_norm,g_no2_norm, guides = "collect") & theme(legend.position = 'bottom', axis.text.y = element_blank())



toDo = best_regs_diff |>
  filter(reg == "loess") |>
  arrange(desc(r2), station_id) |>
  collect()

pdf("reg_inspect.pdf", width = 16, height = 9)
for(i in 1:nrow(toDo)){

  stn = toDo$station_id[i]
  nm = toDo$name[i]
  idx = tbl(con, "reg_anom_r2") |>
    group_by(station_id, name, reg) |>
    filter(r2 == max(r2, na.rm = TRUE),
           name == !!nm,
           reg == "piecewise") |>
    ungroup() |>
    filter(station_id == !!stn) |>
    collect() |>
    pluck("scenario_idx")

  r2_dat = best_regs_diff |>
    filter(name == nm) |>
    filter(station_id == stn) |>
    collect()

  st = paste(r2_dat$reg, round(r2_dat$r2, 3), sep = ": ", collapse = " | ")



  g = tbl(con, "reg_anom") |>
    filter(station_id == !!stn,
           scenario_idx %in% c(idx,0),
           name == !!nm) |>
    ggplot()+
    geom_line(aes(date, anom))+
    geom_point(aes(date, value, colour = reg))+
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
    labs(title = paste(stn, nm),
         subtitle = st)

  print(g)

}
dev.off()






