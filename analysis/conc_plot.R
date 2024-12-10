library(here)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggplot2)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

datConc = tbl(con, "all_data") |>
  rename(spc = name) |>
  anti_join(tbl(con, "remove_sites"),
            by = c("spc", "station_id")) |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, country),
            by = "station_id") |>
  mutate(date = date_trunc("year", date),
         country = ifelse(country == "United States of America", country, "Europe"),
         value = ifelse(country == "United States of America", value, value/1.96)
         ) |>
  group_by(date, spc, country) |>
  summarise(q25 = quantile(value, probs = 0.25, na.rm = T),
            q50 = quantile(value, probs = 0.50, na.rm = T),
            q75 = quantile(value, probs = 0.75, na.rm = T)) |>
  collect()


dbDisconnect(con, shutdown = T)


g1 = datConc |>
  mutate(spc = case_when(spc == "no2" ~ "NO<sub>2</sub> / ppbv",
                         spc == "o3" ~ "O<sub>3</sub> / ppbv") |>
           factor(levels = c("O<sub>3</sub> / ppbv", "NO<sub>2</sub> / ppbv"))
         ) |>
  ggplot()+
  geom_ribbon(aes(date, ymin = q25, ymax = q75), alpha = 0.5)+
  geom_line(aes(date, q50))+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "")+
  facet_grid(spc~country, scale = "free_y",switch = "y")+
  theme_minimal()+
  theme(strip.text = element_markdown(size = 10),
        strip.placement = "outside")

png("plots/o3_no2_conc.png", res = 300,  width = 4000, height = 3000)
g1
dev.off()
