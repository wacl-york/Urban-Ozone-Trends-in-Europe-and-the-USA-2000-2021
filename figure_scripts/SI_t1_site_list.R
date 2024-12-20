library(gt)
library(DBI)
library(dplyr)
library(stringr)

source(here::here('functions','connect_to_db.R'))

latex_tweaks = function(x, caption, label, sideways = T, adjustbox = T){
  y = x |>
    stringr::str_replace_all("zzz", "\\\\textsubscript{") |>
    stringr::str_replace_all("yyy", "}") |> # gross
    stringr::str_remove_all("\\\\fontsize\\{12.0pt\\}\\{14.4pt\\}\\\\selectfont\\n") |>
    stringr::str_split("\\n", simplify = T)

  if(sideways){
    y = y |>
      stringr::str_replace_all("table", "sidewaystable")
  }
  y = y[y != ""]

  y1 = y[1:2]

  yn = y[length(y)]

  ymid = y[-c(2, length(y))]

  a = c(y1,
        paste0("\\caption{",caption,"}\\\\"),
        paste0("\\label{",label,"}\\\\"))

  if(adjustbox){
    a = c(a,
          "\\begin{adjustbox}{width=\\textwidth}",
          ymid,
          "\\end{adjustbox}",
          yn)
  } else{
    a = c(a, ymid, yn)
  }

  a
}

con = connect_to_db()

min_aic = tbl(con, "min_aic") |>
  group_by(name, station_id) |>
  filter(aic == min(aic, na.rm = T)) |>
  filter(scenario_idx == min(scenario_idx, na.rm = T)) |> # a handful of sites have multiple scenarios that have identical AIC.
  ungroup() |>
  select(scenario_idx, name, station_id)


si_table = tbl(con, "combinedMeta") |>
  inner_join(tbl(con, "name_station"), by = "station_id") |>
  left_join(min_aic, by = c("name", "station_id")) |>
  anti_join(tbl(con, "remove_sites"), by = c("station_id", "name" = "spc")) |>
  left_join(tbl(con, "regression_scenarios"), by = c("name", "station_id", "scenario_idx")) |>
  select(
    `Station ID` = station_id,
    Latitude = latitude,
    Longitude = longitude,
    Country = country,
    `Station Type` = station_type,
    Species = name,
    `Change Point 1` = cp1,
    `Change Point 2` = cp2) |>
  distinct() |>
  collect() |>
  mutate(Country = str_to_title(Country),
         `Station Type` = ifelse(str_detect(`Station Type`, "urban"), `Station Type`, paste0("Urban ", `Station Type`)) |>
           str_replace("\\.", " ") |>
           str_to_title(),
         `Station Type` = str_remove(`Station Type`, "Unknown") |>
           str_trim(),
         Species = case_when(Species == "o3" ~ "Ozzz3yyy",
                             Species == "no2" ~ "NOzzz2yyy",
                             Species == "ox" ~ "Ozzzxyyy"),
         Latitude = round(Latitude, 4),
         Longitude = round(Longitude, 4)
         ) |>
  arrange(Country, `Station ID`)


si_table |>
  mutate(across(contains("Point"), as.character)) |>
  gt()  |>
  tab_options(latex.use_longtable = TRUE) |>
  as_latex() |>
  as.character() |>
  latex_tweaks(caption = "List of Sites",
               label = "table:site_list",
               sideways = F,
               adjustbox = F) |>
  writeLines(here::here('tables','SI_1_site_list.txt'))
