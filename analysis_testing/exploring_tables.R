library(DBI)
library(dplyr)
library(stringr)

source(here::here('functions','utils.R'))

con = connect_to_db()



tables = dbListTables(con)


tables[str_detect(tables, "dat_")]

tables[str_detect(tables, "reg_all")]


tables[str_detect(tables, "piecewise_stats")]
tables[str_detect(tables, "piecewise_data")]


tbl(con, "piecewise_data_daily_all")

