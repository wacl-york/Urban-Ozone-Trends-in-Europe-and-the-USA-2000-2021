library(DBI)
library(dplyr)

source(here::here('functions','utils.R'))

con = connect_to_db(FALSE)

# Sites to remove - determined via inspection -----------------------------
remove_sites_inspect = tribble(
  ~station_id, ~name,
  "bg0013a", "o3",
  "bg0043a", "o3",
  "bg0040a", "no2",
  "bg0052a", "o3",
  "es1529a", "o3",
  "es1529a", "ox",
  "gr0030a", "o3",
  "it0963a", "no2",
  "pt03072", "no2",
  "gr0031a", "ox",
  "ie0028a", "no2",
  "es1131a", "o3"
)

# Have we removed site used in an Ox calculation?  ------------------------
# Lets remove it.
remove_sites_ox = tbl(con, "all_data_series") |>
  filter(name == "ox") |>
  filter(station_id %in% remove_sites_inspect$station_id) |>
  collect()

remove_sites = remove_sites_inspect |>
  bind_rows(remove_sites_ox) |>
  distinct()

dbWriteTable(con, "remove_sites", remove_sites, overwrite = T)

dbDisconnect(con, shutdown = T)
