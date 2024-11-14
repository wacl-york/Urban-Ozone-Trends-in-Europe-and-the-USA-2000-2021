library(scico)
library(dplyr)
library(tidyr)
library(ggplot2)

source(here::here('functions','connect_to_db.R'))

con = connect_to_db()

r2_comp = tbl(con, "pqr_r2_max") |>
  collect() |>
  bind_rows(
    tbl(con, "reg_anom_r2") |>
      filter(reg %in% c("loess", "qr")) |>
      collect()
  ) |>
  select(-scenario_idx) |>
  distinct() |>
  pivot_wider(names_from = "reg", values_from = "r2")


r2_relative_to_loess = r2_comp |>
  pivot_longer(c(pqr_2, pqr_1, qr), names_to = "reg", values_to = "r2") |>
  mutate(perc_increase = ((r2-loess)/r2)*100,
         abs_change = r2-loess) |>
  arrange(desc(perc_increase))

r2_relative_to_loess |>
  mutate(perc_increase = ifelse(perc_increase < -1e2, -1e2, perc_increase)) |>
  ggplot()+
  geom_density(aes(perc_increase, fill = reg), alpha = 0.5)+
  facet_wrap(~name)

r2_relative_to_loess |>
  # mutate(perc_increase = ifelse(perc_increase < -1e2, -1e2, perc_increase)) |>
  ggplot()+
  geom_density(aes(abs_change, fill = reg), alpha = 0.5)+
  geom_vline(aes(xintercept = 0))+
  facet_wrap(~name)

# -------------------------------------------------------------------------

r2_relative_to_qr = r2_comp |>
  pivot_longer(c(pqr_2, pqr_1, loess), names_to = "reg", values_to = "r2") |>
  mutate(perc_increase = ((r2-qr)/r2)*100,
         abs_change = r2-qr) |>
  arrange(desc(perc_increase))

r2_relative_to_qr |>
  filter(reg != "loess") |>
  ggplot()+
  geom_density(aes(perc_increase, fill = reg), alpha = 0.5)+
  geom_vline(aes(xintercept = 0))+
  facet_wrap(~name)

r2_relative_to_qr |>
  filter(reg != "loess") |>
  ggplot()+
  geom_density(aes(abs_change, fill = reg), alpha = 0.5)+
  geom_vline(aes(xintercept = 0))+
  facet_wrap(~name)


# -------------------------------------------------------------------------

r2_relative_to_pqr1 = r2_comp |>
  pivot_longer(c(pqr_2, qr, loess), names_to = "reg", values_to = "r2") |>
  mutate(perc_increase = ((r2-pqr_1)/r2)*100) |>
  arrange(desc(perc_increase))

r2_relative_to_pqr1 |>
  filter(reg != "loess") |>
  mutate(perc_increase = ifelse(perc_increase < -1e2, -1e2, perc_increase)) |>
  ggplot()+
  geom_density(aes(perc_increase, fill = reg), alpha = 0.5)+
  geom_vline(aes(xintercept = 0))+
  facet_wrap(~name)


r2_relative_to_pqr1 |>
  filter(perc_increase < -1e2)



# -------------------------------------------------------------------------


r2_comp |>
  pivot_longer(-c(station_id, name), names_to = "reg", values_to = "r2") |>
  filter(reg != "loess") |>
  group_by(station_id, name) |>
  filter(r2 == max(r2)) |>
  group_by(reg) |>
  count()



DBI::dbDisconnect(con, shutdown = T)
