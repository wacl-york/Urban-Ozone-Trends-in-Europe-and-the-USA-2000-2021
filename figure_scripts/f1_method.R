library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(scico)
library(ggtext)
library(ggplot2)
library(lubridate)
library(patchwork)
library(shadowtext)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

stn = "gb0566a"


# Dat - season ------------------------------------------------------------

dat = tbl(con, "anom") |>
  filter(station_id == !!stn,
         name == "o3") |>
  collect() |>
  arrange(date)


g1 = dat |>
  select(-anom) |>
  pivot_longer(c(value, season), names_to = "grp") |>
  mutate(grp = ifelse(grp == "value", "Measured", "Seasonality")) |>
  ggplot()+
  geom_line(aes(date, value/1.96, colour = grp, linewidth = grp))+
  annotate(geom = "shadowtext",
           x = ymd_hms("2000-01-01 00:00:00"),
           y = 50,
           label = "A")+
  scale_colour_manual(values = c("black", "red"), name = "")+
  scale_linewidth_manual(values = c(0.5, 0.75), name = "")+
  scale_y_continuous(name = "O<sub>3</sub> / ppbv")+
  scale_x_datetime(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
  theme_classic()+
  theme(axis.title.y = element_markdown(),
        legend.position = "bottom")


# Anom - regs -------------------------------------------------------------

min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id, reg) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup() |>
  filter(station_id == !!stn,
         name == "o3")

loess = tbl(con, "loess") |>
  filter(station_id == !!stn,
         name == "o3")

dat2 = tbl(con,"reg_anom") |>
  filter(station_id == !!stn,
         name == "o3") |>
  left_join(min_aic, c("scenario_idx", "name", "station_id", "reg")) |>
  mutate(scenario_idx = ifelse(reg == "loess", 0, aic),
         piece = ifelse(reg == "loess", "1", piece)) |>
  collect() |>
  mutate(aic = ifelse(reg %in% c("qr","loess"), 0, aic)) |>
  filter(!is.na(aic)) |>
  select(-aic, -r2, -npiece, -scenario_idx) |>
  pivot_wider(names_from = "reg") |>
  pivot_longer(c(loess, pqr_1, pqr_2,qr), names_to = "reg") |>
  arrange(date)

g2 = dat2 |>
  mutate(reg = case_when(reg == "loess" ~ "LOESS",
                         reg == "qr" ~ "Quantile Regression",
                         reg == "pqr_1" ~ "Piecewise QR - 1 CP",
                         reg == "pqr_2" ~ "Piecewise QR - 2 CP")) |>
  ggplot()+
  geom_line(aes(date,
                anom/1.96))+
  geom_line(aes(date,
                value/1.96,
                colour = reg,
                group = interaction(reg, piece)),
            linewidth = 1)+
  annotate(geom = "shadowtext",
           x = ymd_hms("2000-01-01 00:00:00"),
           y = 35,
           label = "B")+
  scale_colour_manual(values = c("#FF0000", "#00A08A", "#F98400", "#5BBCD6"), name = "")+
  scale_y_continuous(name = "O<sub>3</sub> Anomaly / ppbv")+
  scale_x_datetime(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
  theme_classic()+
  theme(axis.title.y = element_markdown(),
        legend.position = "bottom")


# anom - tau --------------------------------------------------------------

scen = min_aic |>
  filter(reg == "pqr_2") |>
  collect() |>
  pluck("scenario_idx")

dat3 = tbl(con, "piecewise") |>
  filter(station_id == !!stn,
         name == "o3",
         scenario_idx == !!scen) |>
  collect() |>
  left_join(x = dat, y = _, by = c("x", "station_id", "name")) |>
  left_join(dat2 |>
              filter(reg == "pqr_2",
                     !is.na(value)) |>
              select(x, piece),
            by = c("x"))

dbDisconnect(con, shutdown = T)

g3 = dat3 |>
  ggplot()+
  geom_line(aes(date,
                anom/1.96))+
  geom_line(aes(date,
                piecewise/1.96,
                colour = as.factor(tau),
                group = interaction(tau, piece)),
            linewidth = 1)+
  annotate(geom = "shadowtext",
           x = ymd_hms("2000-01-01 00:00:00"),
           y = 35,
           label = "C")+
  scale_colour_manual(values = c(
    "#E4ADD6",
    "#B5549C",
    "#65014B",
    "grey50",
    "#0C4C00",
    "#5F903D",
    "#C0D9A1"
  ),
  name = "Quantile")+
  # scale_colour_scico_d(palette = "vik", name = "Quantile")+
  scale_y_continuous(name = "O<sub>3</sub> Anomaly / ppbv")+
  scale_x_datetime(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
  theme_classic()+
  theme(axis.title.y = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T)


# -------------------------------------------------------------------------




p1 = g1/g2/g3

scale = 7.5

pdf(here::here('figures','f1_method.pdf'), width = scale, height = (scale*1.414), paper = "a4")
p1
dev.off()

