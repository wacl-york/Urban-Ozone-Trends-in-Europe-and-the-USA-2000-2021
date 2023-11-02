eeaMeta = tbl(con, "eeaMeta") |>
  collect()


firstSite = min(eeaMeta$date_start) |>
  round_date("1 month")

lastSite = max(eeaMeta$date_end) |>
  round_date("1 month")

nSites = tibble(date = seq(firstSite, lastSite,"1 month")) |>
  full_join(eeaMeta, by = join_by(between(date, date_start, date_end))) |>
  mutate(hasData = ifelse(is.na(site), 0, 1)) |>
  mutate(station_type = interaction(site_area, site_type)) |>
  select(date, country, hasData, station_type) |>
  filter(!is.na(country)) |>
  group_by(date, country, station_type) |>
  summarise(nSites = sum(hasData))

ggplot(nSites)+
  geom_point(aes(date, nSites, colour = station_type))+
  geom_vline(aes(xintercept = ymd_hm("2000-01-01 00:00")))+
  facet_wrap(~country, scales = "free_y")+
  AQVisR::AQvis_plotTheme()
