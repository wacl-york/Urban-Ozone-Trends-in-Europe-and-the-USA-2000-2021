library(DBI)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)

con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

best_regs = tbl(con, "reg_anom_r2") |>
  group_by(station_id, name, reg) |>
  filter(r2 == max(r2, na.rm = TRUE)) |>
  ungroup() |>
  select(-scenario_idx) |>
  pivot_wider(names_from = "reg", values_from = "r2") |>
  mutate(norm_reg = qr) |>
  pivot_longer(-c(station_id, name, norm_reg), names_to = "reg", values_to = "r2")

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

order_ox = best_regs |>
  filter(reg == "loess",
         name == "ox") |>
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

g_ox = best_regs |>
  filter(name == "ox") |>
  collect() |>
  mutate(station_id = factor(station_id, levels = !!order_ox)) |>
  ggplot()+
  geom_point(aes(r2, station_id, colour = reg))+
  scale_y_discrete(guide = guide_axis(n.dodge = 3), position = "right")+
  facet_wrap(~name, ncol = 1)+
  AQVisR::AQvis_plotTheme()

g_o3+g_no2+g_ox

# -------------------------------------------------------------------------

best_regs_diff = best_regs |>
  mutate(r2_diff = r2-norm_reg)

order_o3_norm = best_regs_diff |>
  collect() |>
  filter(reg == "pqr_1",
         name == "o3") |>
  arrange(r2_diff) |>
  select(station_id) |>
  pluck("station_id")

order_no2_norm = best_regs_diff |>
  collect() |>
  filter(reg == "pqr_1",
         name == "no2") |>
  arrange(r2_diff) |>
  select(station_id) |>
  pluck("station_id")

order_ox_norm = best_regs_diff |>
  collect() |>
  filter(reg == "pqr_1",
         name == "ox") |>
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

g_ox_norm = best_regs_diff |>
  filter(name == "ox") |>
  collect() |>
  mutate(station_id = factor(station_id, levels = !!order_ox_norm)) |>
  ggplot()+
  geom_point(aes(r2_diff, station_id, shape = reg, colour = r2), size = 2)+
  scale_y_discrete(guide = guide_axis(n.dodge = 3), position = "right")+
  scico::scale_color_scico(palette = "batlow", limits = c(0,1))+
  # scale_colour_viridis_c(limits = c(0,1))+
  facet_wrap(~name, ncol = 1)+
  AQVisR::AQvis_plotTheme()

wrap_plots(g_o3_norm,g_no2_norm,g_ox_norm, guides = "collect") & theme(legend.position = 'bottom', axis.text.y = element_blank())


meta = tbl(con, "combinedMeta") |>
  select(station_id, country) |>
  collect() |>
  unique()

toDo = best_regs_diff |>
  filter(reg == "loess") |>
  # arrange(desc(r2), station_id) |>
  collect() |>
  left_join(meta, "station_id") |>
  arrange(country, desc(r2))

cli::cli_progress_bar(total = nrow(toDo))

pdf(here("analysis","reg_inspect_2.pdf"), width = 16, height = 9)
for(i in 1:nrow(toDo)){

  cli::cli_progress_update()
  stn = toDo$station_id[i]
  nm = toDo$name[i]
  cnt = toDo$country[i]

  idx_pqr_1 = tbl(con, "reg_anom_r2") |>
    group_by(station_id, name, reg) |>
    filter(r2 == max(r2, na.rm = TRUE),
           name == !!nm,
           reg %in% c("pqr_1")) |>
    ungroup() |>
    filter(station_id == !!stn) |>
    collect() |>
    pluck("scenario_idx")

  idx_pqr_2 = tbl(con, "reg_anom_r2") |>
    group_by(station_id, name, reg) |>
    filter(r2 == max(r2, na.rm = TRUE),
           name == !!nm,
           reg %in% c("pqr_2")) |>
    ungroup() |>
    filter(station_id == !!stn) |>
    collect() |>
    pluck("scenario_idx")


  r2_dat = best_regs_diff |>
    filter(name == nm) |>
    filter(station_id == stn) |>
    collect()

  st = paste(r2_dat$reg, round(r2_dat$r2, 3), sep = ": ", collapse = " | ")

  plotDat = bind_rows(
    tbl(con, "reg_anom") |>
    filter(station_id == !!stn,
           reg == "pqr_1",
           scenario_idx %in% c(idx_pqr_1,0),
           name == !!nm) |>
      collect(),
    tbl(con, "reg_anom") |>
      filter(station_id == !!stn,
             reg == "pqr_2",
             scenario_idx %in% c(idx_pqr_2,0),
             name == !!nm) |>
      collect(),
    tbl(con, "reg_anom") |>
      filter(station_id == !!stn,
             reg %in% c("loess", "qr"),
             name == !!nm) |>
      collect()
  )

  g = plotDat |>
    ggplot()+
    geom_line(aes(date, anom))+
    geom_line(aes(date, value, colour = reg), size = 2)+
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", guide = guide_axis(n.dodge = 2))+
    labs(title = paste(cnt, stn, nm),
         subtitle = st)+
    facet_wrap(~reg)+
    AQVisR::AQvis_plotTheme()


  print(g)

}
dev.off()

dbDisconnect(con, shutdown = T)






