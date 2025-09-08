library(gt)
library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(ggh4x)
library(ggtext)
library(ggplot2)
library(stringr)
library(patchwork)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))
source(here::here('functions','table_utils.R'))

make_slope_sig_counts_table = function(dat){
  dat |>
    filter(dir != 0) |>
    ungroup() |>
    mutate(dir = ifelse(dir == 1, "Increasing", "Decreasing")) |>
    select(year, tau, dataType, region, value, dir, groupType) |>
    pivot_wider(names_from = dir, values_from = value) |>
    pivot_wider(names_from = tau, values_from = c(Decreasing, Increasing), names_sep = "_") |>
    pivot_wider(names_from = year, values_from = contains("_"), names_sep = "_") |>
    group_by(dataType) |>
    select(
      region,
      dataType,
      groupType,
      contains("0.05"),
      contains("0.5"),
      contains("0.95")
    ) |>
    mutate(
      dataType = case_when(
        dataType == "mda8_anom_all" ~ "MDA8Ozzz3yyy",
        dataType == "mda8_anom_warm" ~ "MDA8Ozzz3yyy Warm Season",
        dataType == "mda8_anom_cold" ~ "MDA8Ozzz3yyy Cold Season"
      ),
      groupType = case_when(
        groupType == "modCert" ~ "p < 0.10",
        groupType == "modCertMedSlope" ~ "p < 0.10, $|$slope$|$ > 0.5",
        groupType == "unfiltered" ~ "All Sites",
        TRUE ~ groupType
      )
    )
}

# Get Data ----------------------------------------------------------------

con = connect_to_db()

dbListTables(con)

tables = c("piecewise_stats_freeTau_mda8_anom_all", "piecewise_stats_freeTau_mda8_anom_cold", "piecewise_stats_freeTau_mda8_anom_warm")

tableName = "piecewise_stats_freeTau_mda8_anom_all"

dat = map_df(
  tables,
  ~{
    tbl(con, .x) |>
      filter(stat == "slope",
             tau %in% c(0.05, 0.5, 0.95)) |>
      pivot_wider(
        names_from = "type"
      ) |>
      mutate(dir = case_when(
        fit > 0 ~ 1,
        fit < 0 ~ -1,
        fit == 0 ~ 0
      ),
      fit = fit*365,
      dataType = str_remove(.x, "piecewise_stats_freeTau_")
      ) |>
      collect()
  }
) |>
  left_join(
    combinedMetaRegion(con) |>
      select(station_id, region) |>
      collect() |>
      distinct(),
    "station_id"
  ) |>
  expand_slopes() |>
  filter(year %in% c(2004, 2018))

dbDisconnect(con, shutdown = T)

totals = dat |>
    group_by(year, tau, name, dataType, region) |>
    count(name = "total")

counts = list(
  dat |>
    group_by(year, tau, name, dir, dataType, region) |>
    count() |>
    mutate(groupType = "unfiltered"),
  dat |>
    filter(pv < 0.10) |>
    group_by(year, tau, name, dir, dataType, region) |>
    count() |>
    mutate(groupType = "modCert"),
  dat |>
    filter(pv < 0.10,
           abs(fit) > 0.5) |>
    group_by(year, tau, name, dir, dataType, region) |>
    count() |>
    mutate(groupType = "modCertMedSlope")
) |>
  bind_rows() |>
  left_join(totals,
            by = c("year", "tau", "name", "dataType", "region")) |>
  ungroup() |>
  mutate(perc = (n/total)*100)

# Make Plot ---------------------------------------------------------------

plotDat = counts |>
  select(-total, -perc) |>
  pivot_wider(values_from = "n",
              names_from = "groupType") |>
  mutate(
    diff_unfiltered = unfiltered-modCert,
    diff_modCert = modCert-modCertMedSlope,
    diff_modCertMedSlope = modCertMedSlope
  ) |>
  select(-unfiltered, -modCert, -modCertMedSlope)

plotForLegend = plotDat |>
  pivot_longer(contains("diff"), values_to = "n",names_to = "groupType") |>
  filter(dir != 0) |>
  mutate(
    groupType = case_when(
      dir < 0 & groupType == "diff_modCert" ~ "p < 0.10 (dec)",
      dir < 0 & groupType == "diff_modCertMedSlope" ~ "p < 0.10, slope < -0.5 ppbv / yr<sup>-1</sup>",
      dir > 0 & groupType == "diff_modCert" ~ "p < 0.10 (inc)",
      dir > 0 & groupType == "diff_modCertMedSlope" ~ "p < 0.10, slope < +0.5 ppbv / yr<sup>-1</sup>",
      dir > 0 & groupType == "diff_unfiltered" ~ "All Sites",
      TRUE ~ groupType
    ) |>
      factor(levels = c(
        "p < 0.10, slope < +0.5 ppbv / yr<sup>-1</sup>",
        "p < 0.10 (inc)",
        "All Sites",
        "p < 0.10 (dec)",
        "p < 0.10, slope < -0.5 ppbv / yr<sup>-1</sup>"
      ))
  ) |>
  filter(!is.na(groupType)) |>
  ggplot()+
  geom_bar(aes(factor(tau),n, fill = groupType), position = "stack", stat = "identity")+
  geom_hline(yintercept = 0, colour = "black")+
  scale_fill_manual(
    name = "",
    values = c(
      "p < 0.10, slope < +0.5 ppbv / yr<sup>-1</sup>" = "#FF6400",
      "p < 0.10 (inc)" = "#FFBA66",
      "all sites (inc)" = "#A4C171",
      "All Sites" = "#A4C171",
      "p < 0.10 (dec)" = "#78BCFF",
      "p < 0.10, slope < -0.5 ppbv / yr<sup>-1</sup>" = "#1E64FF"
    )
  )+
  facet_nested(region~dataType+year, scales = "free_y")+
  theme_minimal()+
  theme(
    legend.text = element_markdown(),
    strip.text = element_markdown(),
    legend.position = "bottom"
  )

legend = plotForLegend |>
  ggpubr::get_legend()


g1 = plotDat |>
  pivot_longer(contains("diff"), values_to = "n",names_to = "groupType") |>
  filter(dir != 0) |>
  mutate(n = dir*n,
         dataType = case_when(
           dataType == "mda8_anom_all" ~ "MDA8O<sub>3</sub>",
           dataType == "mda8_anom_warm" ~ "MDA8O<sub>3</sub> Warm Season",
           dataType == "mda8_anom_cold" ~ "MDA8O<sub>3</sub> Cold Season"
         ),
         groupType = case_when(
           dir < 0 & groupType == "diff_modCert" ~ "p < 0.10 (dec)",
           dir < 0 & groupType == "diff_modCertMedSlope" ~ "p < 0.10, slope < -0.5 ppbv / yr<sup>-1</sup>",
           dir < 0 & groupType == "diff_unfiltered" ~ "all sites (dec)",
           dir > 0 & groupType == "diff_modCert" ~ "p < 0.10 (inc)",
           dir > 0 & groupType == "diff_modCertMedSlope" ~ "p < 0.10, slope < +0.5 ppbv / yr<sup>-1</sup>",
           dir > 0 & groupType == "diff_unfiltered" ~ "all sites (inc)",
           TRUE ~ groupType
         ) |>
           factor(levels = c(
             "p < 0.10, slope < +0.5 ppbv / yr<sup>-1</sup>",
             "p < 0.10 (inc)",
             "all sites (inc)",
             "p < 0.10, slope < -0.5 ppbv / yr<sup>-1</sup>",
             "p < 0.10 (dec)",
             "all sites (dec)"
           ))
  ) |>
  ggplot()+
  geom_bar(aes(factor(tau),n, fill = groupType), position = "stack", stat = "identity")+
  geom_hline(yintercept = 0, colour = "black")+
  scale_fill_manual(
    name = "",
    values = c(
      "p < 0.10, slope < +0.5 ppbv / yr<sup>-1</sup>" = "#FF6400",
      "p < 0.10 (inc)" = "#FFBA66",
      "all sites (inc)" = "#A4C171",
      "all sites (dec)" = "#A4C171",
      "p < 0.10 (dec)" = "#78BCFF",
      "p < 0.10, slope < -0.5 ppbv / yr<sup>-1</sup>" = "#1E64FF"
    )
  )+
  scale_x_discrete(name = "&tau;")+
  scale_y_continuous(name = "Number of Sites")+
  guides(fill = "none")+
  facet_nested(region~dataType+year, scales = "free_y")+
  theme_minimal()+
  theme(
    legend.text = element_markdown(),
    strip.text = element_markdown(),
    axis.title = element_markdown()
  )

p1 = wrap_plots(g1,legend, heights = c(9,1))

grDevices::cairo_pdf("figures/paper_figures/slope_sig_counts.pdf", width = 11, height = 7)
print(p1)
dev.off()

# Make Table --------------------------------------------------------------

tabData = list(
  n = counts |>
    mutate(value = n) |>
    make_slope_sig_counts_table(),
  perc = counts |>
    mutate(value = round(perc,1)) |>
    make_slope_sig_counts_table()
)

tabGt = list()

for(rgn in c("Europe", "United States of America")){
  for(tabType in c("n", "perc")){
    tabGt[[rgn]][[tabType]] = tabData[[tabType]] |>
      filter(region == rgn) |>
      select(-region) |>
      mutate(across(everything(), \(x) ifelse(is.na(x), 0,x))) |>
      gt() |>
      tab_spanner("Decreasing", columns = contains("Decreasing_0.05"), id = "dec_0.05") |>
      tab_spanner("Increasing", columns = contains("Increasing_0.05"), id = "inc_0.05") |>
      tab_spanner("Decreasing", columns = contains("Decreasing_0.5"),  id = "dec_0.5") |>
      tab_spanner("Increasing", columns = contains("Increasing_0.5"),  id = "inc_0.5") |>
      tab_spanner("Decreasing", columns = contains("Decreasing_0.95"), id = "dec_0.95") |>
      tab_spanner("Increasing", columns = contains("Increasing_0.95"), id = "inc_0.95") |>
      tab_spanner(":tau: = 0.05", columns = contains("0.05")) |>
      tab_spanner(":tau: = 0.50", columns = contains("0.5")) |>
      tab_spanner(":tau: = 0.95", columns = contains("0.95")) |>
      cols_label(
        contains("2004") ~ "2004",
        contains("2018") ~ "2018",
        groupType = "")

  }
}

# write these separatly so we can just view the data without overwriting
for(rgn in c("Europe", "United States of America")){
  for(tabType in c("n", "perc")){
    tabGt[[rgn]][[tabType]] |>
      as_latex() |>
      as.character() |>
      latex_tweaks(caption = "a",label = "b") |>
      writeLines(here::here('tables',paste0("slope_sig_counts_",tabType,"_", str_replace_all(rgn, " ", "-"), ".txt")))
  }
}

tabGt$Europe$perc
