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

grDevices::cairo_pdf(here::here('figures','paper_figures','f03_signifcance_bars.pdf'), width = 11, height = 8)
print(g2)
dev.off()

dbDisconnect(con, shutdown = T)
