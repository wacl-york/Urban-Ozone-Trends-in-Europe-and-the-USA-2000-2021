library(gt)
library(DBI)
library(dplyr)
library(tidyr)
library(ggtext)
library(stringr)
library(ggplot2)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','table_utils.R'))

con = connect_to_db()

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
      select(station_id, region) |>
      collect() |>
      distinct(),
    "station_id")

g1 = regs_by_type |>
  filter(name == "o3",
         dataType %in% c("reg_all_daily_all", "reg_all_mda8_anom_all", "reg_all_mda8_anom_warm")) |>
  mutate(dataType = case_when(
    dataType == "reg_all_daily_all" ~ "Daily Mean",
    dataType == "reg_all_mda8_anom_all" ~ "MDA8O<sub>3</sub>",
    dataType == "reg_all_mda8_anom_warm" ~ "MDA8O<sub>3</sub> Warm Season",
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

grDevices::cairo_pdf("figures/paper_figures/regression_type_bars_o3.pdf", width = 7.5, height = 5) # to get the tau to write properly use cairo_pdf
print(g1)
dev.off()

for(spc in c("o3", "no2", "ox")){
  g1_all = regs_by_type |>
    filter(name == spc) |>
    mutate(dataType = str_remove(dataType, "reg_all_")) |>
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
      strip.text = element_markdown(size = 5),
      axis.title = element_markdown(),
      axis.text.x = element_text(angle = 285, vjust = 0.5)
    )

  grDevices::cairo_pdf(paste0("figures/paper_figures/regression_type_bars_",spc,"_all.pdf"), width = 12, height = 5) # to get the tau to write properly use cairo_pdf
  print(g1_all)
  dev.off()
}


for(rgn in c("Europe", "United States of America")){
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
           dataType %in% c("reg_all_daily_all", "reg_all_mda8_anom_all", "reg_all_mda8_anom_warm")) |>
    mutate(
      dataType = case_when(
        dataType == "reg_all_daily_all" ~ "Daily Mean",
        dataType == "reg_all_mda8_anom_all" ~ "MDA8O<sub>3</sub>",
        dataType == "reg_all_mda8_anom_warm" ~ "MDA8O<sub>3</sub> Warm Season"
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
      axis.text.x = element_text(angle = 285, vjust = 0.5)
    )+
    ggtitle(rgn)


  grDevices::cairo_pdf(paste0("figures/paper_figures/cp_year_",str_replace_all(rgn, " ", "-"),".pdf"), width = 7.5, height = 7.5)
  print(g2)
  dev.off()

}


# temp |>
#   group_by(tau, name, dataType, region, scenarioType) |>
#   count() |>
#   group_by(tau, name, dataType, region) |>
#   mutate(perc = (n/sum(n))*100,
#          perc = round(perc)) |>
#   select(-n) |>
#   pivot_wider(values_from = "perc", names_from = c("name","scenarioType"), names_sep = "_") |>
#   # filter(dataType %in% c("reg_all_daily_all", "reg_all_mda8_anom_all")) |>
#   # mutate(dataType = case_when(
#   #   dataType == "reg_all_daily_all" ~ "Daily Mean",
#   #   dataType == "reg_all_mda8_anom_all" ~ "MDA8Ozzz3yyy"
#   # )) |>
#   # filter(tau %in% c(0.25, 0.5, 0.75)) |>
#   relocate(tau, dataType, o3_QR, o3_PQR_1, o3_PQR_2, no2_QR, no2_PQR_1, no2_PQR_2, ox_QR, ox_PQR_1, ox_PQR_2) |>
#   group_by(region) |>
#   gt() |>
#   tab_spanner(
#     label = "NOzzz2yyy / %",
#     columns = contains("no2_")
#   ) |>
#   tab_spanner(
#     label = "Ozzz3yyy / %",
#     columns = contains("o3_")
#   ) |>
#   tab_spanner(
#     label = "Ozzzxyyy / %",
#     columns = contains("ox_")
#   ) |>
#   cols_label_with(
#     columns = contains("_"),
#     fn = \(x) x |>
#       stringr::str_remove("o3_") |>
#       stringr::str_remove("no2_") |>
#       stringr::str_remove("ox_")) |>
#   cols_label(
#     tau = ":tau:",
#     dataType = "Type"
#   ) |>
#   as_latex() |>
#   as.character() |>
#   latex_tweaks(caption = "a",
#                label = "b",
#                sideways = F,
#                adjustbox = F
#                ) |>
#   str_replace_all("NA", " - ")

dbDisconnect(con, shutdown = T)
