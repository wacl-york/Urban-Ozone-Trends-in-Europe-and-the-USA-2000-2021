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

  y1 = y[1]

  yn = y[length(y)]

  ymid = y[-c(1, length(y))]

  a = c(y1,
        paste0("\\caption{",caption,"}"),
        paste0("\\label{",label,"}"))

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

tbl(con, "remove_sites") |>
  rename(`Station ID` = station_id, Species = spc) |>
  collect() |>
  mutate(Species = case_when(Species == "o3" ~ "Ozzz3yyy",
                      Species == "no2" ~ "NOzzz2yyy",
                      Species == "ox" ~ "Ozzzxyyy")) |>
  gt() |>
  as_latex() |>
  as.character() |>
  latex_tweaks(caption = "Time series removed after visual inspection",
               label = "table:remove_sites",
               sideways = F,
               adjustbox = F) |>
  writeLines(here::here('tables','SI_2_remove_sites.txt'))


