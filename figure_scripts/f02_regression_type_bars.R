library(sf)
library(gt)
library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)
library(lubridate)
library(patchwork)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))
source(here::here('functions','table_utils.R'))

con = connect_to_db()

# -------------------------------------------------------------------------

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)

limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,33), lat = c(33,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

# -------------------------------------------------------------------------

regs_by_type = tbl(con, "min_aic_regs") |>
  filter(!(str_detect(dataType, "mda8") & !str_detect(dataType, "mda8_anom"))) |>
  filter(!str_detect(dataType, "metric")) |>
  group_by(station_id, name, tau, dataType) |>
  filter(aic == min(aic, na.rm = T)) |>
  mutate(rank = case_when( # sometimes (~22 series) AIC is tied. Here we will select QR > PQR_1 > PQR_2
    scenarioType == "QR" ~ scenario_idx,
    scenarioType == "PQR_1" ~ scenario_idx+1000,
    scenarioType == "PQR_2" ~ scenario_idx+2000
  )) |>
  filter(rank == min(rank, na.rm = T)) |>
  select(-rank) |>
  collect() |>
  ungroup() |>
  left_join(
    combinedMetaRegion(con) |>
      select(station_id, region, latitude, longitude) |>
      collect() |>
      distinct(),
    "station_id") |>
  mutate(scenarioType = factor(scenarioType, levels = c("QR", "PQR_1", "PQR_2")))


# Main Plot ---------------------------------------------------------------

g1 = regs_by_type |>
  filter(name == "o3",
         dataType %in% c("reg_all_mda8_anom_all", "reg_all_mda8_anom_warm", "reg_all_mda8_anom_cold"),
         tau %in% c(0.05, 0.5, 0.95)) |>
  mutate(dataType = case_when(
    dataType == "reg_all_mda8_anom_all" ~ "MDA8O<sub>3</sub>",
    dataType == "reg_all_mda8_anom_warm" ~ "MDA8O<sub>3</sub> Warm Season",
    dataType == "reg_all_mda8_anom_cold" ~ "MDA8O<sub>3</sub> Cold Season"
  )) |>
  ggplot()+
  geom_bar(aes(factor(tau), fill = scenarioType), stat = "count", position = "dodge")+
  scale_x_discrete(name = "&tau;")+
  scale_y_continuous(name = "Number of Sites",expand = c(0,0))+
  scale_fill_manual(
    name = "Regression Type",
    values = c(PQR_1 = "#FF0000", PQR_2 = "#00A08A", QR = "#F2AD00"))+
  facet_grid(region~dataType, scale = "free_y")+
  theme_minimal()+
  theme(
    strip.text = element_markdown(size = 10),
    axis.title = element_markdown(),
    axis.text.x = element_text(angle = 285, vjust = 0.5)
  )

grDevices::cairo_pdf(here::here('figures','paper_figures','f02_regression_type_bars_o3.pdf'), width = 7.5, height = 5) # to get the tau to write properly use cairo_pdf
print(g1)
dev.off()


# SI Plots ----------------------------------------------------------------
regions = c("Europe", "United States of America")

for(i in 1:length(regions)){

  rgn = regions[i]
  title = paste0("O<sub>3</sub> - ", rgn)

  g2 = regs_by_type |>
    left_join(
      tbl(con, "regression_scenarios") |>
        collect(),
      by = c("station_id", "name", "scenario_idx")
    ) |>
    pivot_longer(contains("cp"),names_to = "cp") |>
    mutate(cp_year = year(value)) |>
    filter(name == "o3",
           region == rgn,
           tau %in% c(0.05, 0.5, 0.95),
           dataType %in% c("reg_all_mda8_anom_all", "reg_all_mda8_anom_warm", "reg_all_mda8_anom_cold")) |>
    mutate(
      dataType = case_when(
        dataType == "reg_all_mda8_anom_all" ~ "MDA8O<sub>3</sub>",
        dataType == "reg_all_mda8_anom_warm" ~ "MDA8O<sub>3</sub> Warm Season",
        dataType == "reg_all_mda8_anom_cold" ~ "MDA8O<sub>3</sub> Cold Season"
      ),
      tau = paste0("&tau; = ", tau)
    ) |>
    ggplot()+
    geom_histogram(aes(cp_year, fill = scenarioType), binwidth = 1, position = "stack")+
    facet_grid(tau~dataType, scale = "free_y")+
    scale_x_continuous(name = "Change Point Year")+
    scale_y_continuous(name = "Number of Sites", expand = c(0,0))+
    scale_fill_manual(
      name = "Regression Type",
      values = c(PQR_1 = "#FF0000", PQR_2 = "#00A08A"))+
    theme_minimal()+
    theme(
      strip.text = element_markdown(size = 10),
      axis.title = element_markdown(),
      axis.text.x = element_text(angle = 285, vjust = 0.5),
      title = element_markdown(),
      legend.position = "bottom"
    )+
    ggtitle(title)


  grDevices::cairo_pdf(here::here('figures','si_figures',paste0("fS",str_pad(i, 2, pad = "0"),"_cp_year_o3_",str_replace_all(rgn, " ", "-"),".pdf")), width = 7.5, height = 7.5)
  print(g2)
  dev.off()
}

# Map ---------------------------------------------------------------------

regs_by_type_sf = regs_by_type |>
  filter(name == "o3",
         tau %in% c(0.05, 0.5, 0.95),
         dataType %in% c("reg_all_mda8_anom_all", "reg_all_mda8_anom_warm", "reg_all_mda8_anom_cold")) |>
  st_as_sf(coords = c("longitude","latitude"), crs = "WGS84") |>
  st_transform(mycrs) |>
  mutate(dataType = case_when(
    dataType == "reg_all_mda8_anom_all" ~ "MDA8O<sub>3</sub>",
    dataType == "reg_all_mda8_anom_warm" ~ "MDA8O<sub>3</sub> Warm Season",
    dataType == "reg_all_mda8_anom_cold" ~ "MDA8O<sub>3</sub> Cold Season"
  ),
  tau = paste0("&tau; = ", tau)
  )

g_eu = ggplot()+
  geom_sf(data = world)+
  geom_sf(data = regs_by_type_sf, aes(fill = scenarioType), shape = 21)+
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1])+
  scale_fill_manual(name = "Regression Type",
                    values = c(PQR_1 = "#FF0000", PQR_2 = "#00A08A", QR = "#F2AD00"))+
  facet_grid(tau~dataType)+
  theme_minimal()+
  theme(
    strip.text = element_markdown(),
    legend.position = "bottom",
    panel.grid.major = element_blank()
  )

g_us = ggplot()+
  geom_sf(data = world)+
  geom_sf(data = regs_by_type_sf, aes(fill = scenarioType), shape = 21)+
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1])+
  scale_fill_manual(name = "Regression Type",
                    values = c(PQR_1 = "#FF0000", PQR_2 = "#00A08A", QR = "#F2AD00"))+
  facet_grid(tau~dataType)+
  theme_minimal()+
  theme(
    strip.text = element_markdown(size = 6),
    legend.position = "bottom",
    panel.grid.major = element_blank()
  )

grDevices::cairo_pdf(here::here('figures','si_figures','fS03_regression_type_map_eu.pdf'), width = 10, height = 7)
print(g_eu)
dev.off()

grDevices::cairo_pdf(here::here('figures','si_figures','fS04_regression_type_map_us.pdf'), width = 10, height = 7)
print(g_us)
dev.off()

# Table -------------------------------------------------------------------

tableDat = regs_by_type |>
  group_by(tau, name, dataType, region, scenarioType) |>
  count() |>
  group_by(tau, name, dataType, region) |>
  mutate(perc = (n/sum(n))*100,
         perc = round(perc)) |>
  select(-n) |>
  pivot_wider(values_from = "perc", names_from = c("name","scenarioType"), names_sep = "_") |>
  relocate(tau, dataType, o3_QR, o3_PQR_1, o3_PQR_2, no2_QR, no2_PQR_1, no2_PQR_2, ox_QR, ox_PQR_1, ox_PQR_2)

tableDat |>
  group_by(region) |>
  gt() |>
  tab_spanner(
    label = "NOzzz2yyy / %",
    columns = contains("no2_")
  ) |>
  tab_spanner(
    label = "Ozzz3yyy / %",
    columns = contains("o3_")
  ) |>
  tab_spanner(
    label = "Ozzzxyyy / %",
    columns = contains("ox_")
  ) |>
  cols_label_with(
    columns = contains("_"),
    fn = \(x) x |>
      stringr::str_remove("o3_") |>
      stringr::str_remove("no2_") |>
      stringr::str_remove("ox_")) |>
  cols_label(
    tau = ":tau:",
    dataType = "Type"
  ) |>
  as_latex() |>
  as.character() |>
  latex_tweaks(caption = "a",
               label = "b",
               sideways = F,
               adjustbox = F
  ) |>
  str_replace_all("NA", " - ") |>
  writeLines(here::here('tables','regression_type_table.txt'))

dbDisconnect(con, shutdown = T)
