library(ggplot2)

ldat = tbl(con, "loess") |>
  filter(station_id == "8169",
         name == "no2") |>
  collect() |>
  arrange(x)

regressions  |>
  pivot_wider(names_from = "stat") |>
  filter(type == "fit") |>
  ggplot()+
  geom_abline(aes(slope = slope,
                  intercept = intercept,
                  colour = as.factor(startYear)))+
  facet_wrap(~tau)

regressions |>
  pivot_wider(names_from = "stat") |>
  filter(type == "fit",
         tau == 0.5,
         startYear == 2000) |>
  ggplot()+
  geom_abline(aes(slope = slope,
                  intercept = intercept,
                  colour = as.factor(startYear)))

plot(dat$x, dat$anom)
abline(4.21,-0.15, col = "red")
abline(-1.12, 0.0285, col = "blue")
abline(3.42, -0.0241, col = "green")
abline(v = 25)
abline(v = 85)
lines(ldat$x, ldat$loess, col = "orange")


fits = regressions |>
  pivot_wider(names_from = "stat") |>
  filter(type == "fit",
         tau == 0.5)

test = dat |>
  mutate(qr_piece = case_when(
    x <= 25 ~ fits$slope[1]*x+fits$intercept[1],
    between(x, 26, 84) ~ fits$slope[2]*x+fits$intercept[2],
    x >= 85 ~ fits$slope[3]*x+fits$intercept[3]
  ))

plot(test$x, test$qr_piece)


test2 = test |>
  select(x, anom, qr_piece) |>
  left_join(select(ldat, loess, x), "x")

test2 |>
  pivot_longer(-x) |>
  ggplot()+
  geom_point(aes(x, value, colour = name))


test2 |>
  mutate(diff = (loess-qr_piece)^2,
         diff_mn = (loess-mean(qr_piece))^2) |>
  summarise(r2 = 1-(sum(diff, na.rm = T)/sum(diff_mn, na.rm = T)))








