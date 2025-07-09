library(DBI)
library(here)
library(dplyr)
library(tidyr)

# Connect to database
con = dbConnect(duckdb::duckdb(),
                dbdir = here(readLines(here("data_config.txt"),n = 1),"data","db.duckdb"), read_only = TRUE)

# Pull scenarios
reg_scen = tbl(con, "regression_scenarios") |>
  filter(name == "o3") |>
  select(-name) |>
  distinct() |>
  collect()

# Convert to be compatible with multiple metrics, and drop cp2.
metric_scen = tibble(metric = c("3MMDA1", "4MDA8", "AVGMDA8", "NDGT70", "SOMO35")) |>
  rowwise() |>
  mutate(data = list(reg_scen)) |>
  unnest(data) |>
  filter(is.na(cp2))

# Save scenarios RDS file
# saveRDS(metric_scen, file = "/mnt/scratch/projects/chem-cmde-2019/toar/data/mda8_metric_scenarios.RDS")

# Write table to database.
dbWriteTable(con, "mda8_metric_scenarios", metric_scen)

# Disconnect from database.
dbDisconnect(con, shutdown = T)

