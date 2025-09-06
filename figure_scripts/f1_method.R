library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggplot2)
library(patchwork)
library(lubridate)
library(shadowtext)

source(here::here('functions','utils.R'))
source(here::here('functions','regression.R'))
source(here::here('functions','plotting_utils.R'))

con = connect_to_db()

stn = "gb0566a"
nm = "o3"

plotDat1 = tbl(con, "all_data") |>
  filter(
    name == nm,
    station_id == stn
  ) |>
  collect() |>
  mutate(m = month(date)) |>
  group_by(m) |>
  mutate(Seasonality = mean(value)) |>
  rename(Measured = value) |>
  pivot_longer(c(Measured, Seasonality), names_to = "type")


g1 = plotDat1 |>
  ggplot()+
  geom_line(aes(date, value, colour = type))+
  # annotate(geom = "shadowtext",
  #          x = ymd_hms("2000-01-01 00:00:00"),
  #          y = 50,
  #          label = "A")+
  scale_colour_manual(values = c("black", "red"), name = "")+
  scale_linewidth_manual(values = c(0.5, 0.75), name = "")+
  scale_y_continuous(name = "O<sub>3</sub> / ppbv")+
  scale_x_datetime(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
  theme_classic()+
  theme(axis.title.y = element_markdown(),
        legend.position = "bottom")


# -------------------------------------------------------------------------

scenario_data = tbl(con, "regression_scenarios") |>
  select(cp1, cp2, scenario_idx) |>
  distinct() |>
  collect() |>
  arrange(scenario_idx)

scenario_type = scenario_data |>
  mutate(scenarioType = case_when(
    is.na(cp1) & is.na(cp2) ~ "QR",
    !is.na(cp1) & is.na(cp2) ~ "PQR_1",
    !is.na(cp1) & !is.na(cp2) ~ "PQR_2"
  )) |>
  select(scenario_idx, scenarioType)

regs_to_do = tbl(con, "min_aic_regs") |>
  filter(
    name == nm,
    station_id == stn,
    tau == 0.5,
    dataType == "reg_all_mda8_anom_all"
  ) |>
  collect() |>
  left_join(scenario_data, "scenario_idx")

dat = tbl(con, "dat_mda8") |>
  filter(name == nm,
         station_id == stn) |>
  collect() |>
  mutate(
    y = mda8_anom,
    yr = year(date)
  )

statsList = list()
datList = list()

for(i in 1:nrow(regs_to_do)){
  statsList[[i]] = do_qr_aic(
    dat = dat,
    cp1 = regs_to_do$cp1[i],
    cp2 = regs_to_do$cp2[i],
    calcError = TRUE,
    tau = regs_to_do$tau[[i]]
  ) |>
    left_join(regs_to_do[i,] |>
                select(tau, scenario_idx) |>
                unnest(tau),
              "tau"
    ) |>
    mutate(station_id = stn,
           name = nm)

  datList[[i]] = statsList[[i]] |>
    filter(type == "fit") |>
    pivot_wider(names_from = "stat") |>
    left_join(
      dat,
      by = join_by(
        between(
          y$yr,
          x$startYear,
          x$endYear,
          bounds = "[)"
        ),
        station_id,
        name
      )) |>
    mutate(piecewise = ((slope*x) + intercept),
           scenarioType = regs_to_do$scenarioType[[i]]
    )

}

regsDat = datList |>
  bind_rows()

g2 = regsDat |>
  ggplot()+
  geom_line(aes(date, mda8_anom))+
  geom_line(aes(date, piecewise, colour = scenarioType))+
  # annotate(geom = "shadowtext",
  #          x = ymd("2000-01-01"),
  #          y = 35,
  #          label = "B")+
  scale_colour_manual(values = c("#FF0000", "#00A08A", "#F98400"), name = "")+
  scale_y_continuous(name = "MDA8O<sub>3</sub> Anomaly / ppbv")+
  scale_x_date(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
  theme_classic()+
  theme(axis.title.y = element_markdown(),
        legend.position = "bottom")


# -------------------------------------------------------------------------

g3 = tbl(con, "piecewise_data_freeTau_mda8_anom_all") |>
  filter(
    name == nm,
    station_id == stn
  ) |>
  collect() |>
  ggplot()+
  geom_line(aes(date, y))+
  geom_line(aes(date, piecewise, group = interaction(endYear, tau)),
            colour = "black",
            linewidth = 2)+
  geom_line(aes(date, piecewise, colour = factor(tau), group = interaction(endYear, tau)),
            linewidth = 1)+
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
  scale_y_continuous(name = "MDA8O<sub>3</sub> Anomaly / ppbv")+
  scale_x_datetime(name = "Date", date_breaks = "2 years", date_labels = "%Y") +
  theme_classic()+
  theme(axis.title.y = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T)

p1 = g1/g2/g3
p2 = g1/g3

png(data_path("figures", "acc", "f1_method.png"), res = 300, width = 3615, height = 2542)
print(p1)
dev.off()

png(data_path("figures", "acc", "f1_method_v2.png"), res = 300, width = 3615, height = (2542)*0.7)
print(p2)
dev.off()

dbDisconnect(con, shutdown = T)
