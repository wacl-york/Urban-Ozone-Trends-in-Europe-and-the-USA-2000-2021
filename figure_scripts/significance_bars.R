library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(ggh4x)
library(ggtext)
library(stringr)
library(ggplot2)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))

get_slopes = function(tableName){
  tbl(con, tableName) |>
    filter(stat == "slope") |>
    collect() |>
    arrange(station_id, name, tau) |>
    pivot_wider(names_from = "type") |>
    select(-stat) |>
    nest_by(across(any_of(c("station_id", "name", "tau", "metric")))) |>
    mutate(data = data |>
             expand_slopes() |>
             list()) |>
    unnest(data)
}

make_slopes_plotDat = function(dat){

  dat |>
    filter(
      fit != 0, # there are a few times the slope is zero, but we can't plot that here so just get rid. They are always p == 1.
    ) |>
    make_pvStr() |>
    left_join(tbl(con, "combinedMeta") |>
                select(station_id, country) |>
                distinct() |>
                collect(),
              "station_id") |>
    mutate(country = ifelse(country == "United States of America", country, "Europe"),
           dir = ifelse(str_detect(pvStr, "inc"), "inc", "dec")) |>
    group_by(across(any_of(c("name", "country", "tau", "dir", "year", "pvStr", "metric")))) |>
    count() |>
    ungroup() |>
    mutate(n = ifelse(dir == "dec", n*-1, n)) |>
    format_spc_name()
}

# -------------------------------------------------------------------------


con = connect_to_db()

tables = dbListTables(con)[str_detect(dbListTables(con), "piecewise_stats_freeTau")]

p_colours = c("#A50021",
              "#FF6400",
              "#FFBA66",
              "#A4C171",
              "#000099",
              "#1E64FF",
              "#78BCFF",
              "#A4C171"
)

dirOut = here::here("figures","si_figures","significance_bars")

if(!dir.exists(dirOut)){
  dir.create(dirOut)
}


for(i in 1:length(tables)){

  tableName = tables[i]

  slopes = get_slopes(tableName)

  plotDat = slopes |>
    make_slopes_plotDat() |>
    mutate(
      tau = paste0("&tau; = ", tau)
    )


  if(str_detect(tableName, "metric")){

    g1 = plotDat |>
      ggplot()+
      geom_bar(aes(year,n, fill = pvStr), stat = "identity", position = "stack")+
      geom_hline(aes(yintercept = 0))+
      scale_fill_manual(values = p_colours, name = "")+
      scale_y_continuous(name = "Number of Time Series")+
      scale_x_continuous(name = "Year")+
      facet_grid(country~metric, scales = "free_y")+
      theme_minimal()+
      theme(strip.text = element_markdown(),
            legend.position = "bottom",
            legend.byrow = T)

    fileOut = here::here(dirOut , "significance_bars_metrics.pdf")

    grDevices::cairo_pdf(fileOut, width = 11, height = 8)
    print(g1)
    dev.off()

  }else{

    if(str_detect(tableName, "mda8")){
      species = c(o3 = "O<sub>3</sub>")
    }else{
      species = c(o3 = "O<sub>3</sub>", no2 =  "NO<sub>2</sub>", ox = "O<sub>x</sub>")
    }

    for(i in 1:length(species)){

      g1 = plotDat |>
        filter(name == species[i]) |>
        ggplot()+
        geom_bar(aes(year,n, fill = pvStr), stat = "identity", position = "stack")+
        geom_hline(aes(yintercept = 0))+
        scale_fill_manual(values = p_colours, name = "")+
        scale_y_continuous(name = "Number of Time Series")+
        scale_x_continuous(name = "Year")+
        facet_grid(country~tau, scales = "free_y")+
        theme_minimal()+
        theme(strip.text = element_markdown(),
              title = element_markdown(),
              legend.position = "bottom",
              legend.byrow = T)+
        ggtitle(paste0(species[i], " - ",str_remove(tableName, "piecewise_stats_freeTau_")))

      fileOut = here::here(dirOut , paste0("significance_bars_", str_remove(tableName, "piecewise_stats_"), "_", names(species)[i], ".pdf"))

      grDevices::cairo_pdf(fileOut, width = 11, height = 8)
      print(g1)
      dev.off()

    }

  }


}


figTables = c("piecewise_stats_freeTau_mda8_anom_all","piecewise_stats_freeTau_mda8_anom_cold","piecewise_stats_freeTau_mda8_anom_warm")


dat = map_df(
  figTables,
  ~{ .x |>
      get_slopes() |>
      make_slopes_plotDat() |>
      mutate(tbl = .x)
  }
) |>
  filter(tau %in% c(0.05, 0.5, 0.95)) |>
  mutate(tbl = case_when(
    tbl == "piecewise_stats_freeTau_mda8_anom_all" ~ "MDA8O<sub>3</sub>",
    tbl == "piecewise_stats_freeTau_mda8_anom_warm" ~ "MDA8O<sub>3</sub> Warm Season",
    tbl == "piecewise_stats_freeTau_mda8_anom_cold" ~ "MDA8O<sub>3</sub> Cold Season"
  ),
  tau = paste0("&tau; = ", tau)
  )

g2 = dat |>
  ggplot()+
  geom_bar(aes(year,n, fill = pvStr), stat = "identity", position = "stack")+
  geom_hline(aes(yintercept = 0))+
  scale_fill_manual(values = p_colours, name = "")+
  scale_y_continuous(name = "Number of Time Series")+
  scale_x_continuous(name = "Year")+
  facet_nested(country + tau ~ tbl, scale = "free_y")+
  theme_minimal()+
  theme(strip.text = element_markdown(),
        title = element_markdown(),
        legend.position = "bottom",
        legend.byrow = T)

grDevices::cairo_pdf(here::here('figures','paper_figures','signifcance_bars.pdf'), width = 11, height = 8)
print(g2)
dev.off()

dbDisconnect(con, shutdown = T)
