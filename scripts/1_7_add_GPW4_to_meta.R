library(sf)
library(DBI)
library(here)
library(terra)
library(dplyr)

source(here::here('functions','data_path.R'))
source(here::here('functions','connect_to_db.R'))

popDat = list.files(data_path("data", "GPW4"), full.names = T)

ra = rast(popDat)

con = connect_to_db(FALSE)

meta = tbl(con, "combinedMeta") |>
  collect()

metaSf = meta |>
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("WGS84"))

temp = terra::extract(ra, metaSf) |>
  setNames(c("ID", "GPW4_2000", "GPW4_2020")) |>
  select(-ID) |>
  tibble()

meta = bind_cols(meta, temp)

dbWriteTable(con, "combinedMeta", meta, overwrite = TRUE)

dbDisconnect(con, shutdown = T)
