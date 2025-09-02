make_table = function(x){

  tab = x |>
    gt() |>
    tab_spanner(
      label = "Increasing",
      columns = contains("Increasing")
    ) |>
    tab_spanner(
      label = "Decreasing",
      columns = contains("Decreasing")
    ) |>
    tab_spanner(
      label = "No Trend",
      columns = contains("No Trend")
    ) |>
    cols_label_with(
      columns = contains("_"),
      fn = \(x) x |>
        stringr::str_remove("Increasing_") |>
        stringr::str_remove("Decreasing_") |>
        stringr::str_remove("No Trend_"))


  if("name" %in% names(x)){
    tab = tab |>
      cols_label(
        name = "Species",
        tau = ":tau:"
      )

    spc = unique(x$name)

    rgo = c()

    if("o3" %in% spc){
      tab = tab |>
        tab_row_group(
          id = "a",
          label = "Ozzz3yyy",
          rows = name == "o3"
        )
      rgo = c(rgo, "a")
    }

    if("no2" %in% spc){
      tab = tab |>
        tab_row_group(
          id = "b",
          label = "NOzzz2yyy",
          rows = name == "no2"
        )
      rgo = c(rgo, "b")
    }

    if("ox" %in% spc){
      tab = tab |>
        tab_row_group(
          id = "c",
          label = "Ozzzxyyy",
          rows = name == "ox"
        )
      rgo = c(rgo, "c")
    }

    if(length(rgo) > 0){
      tab = tab |>
        row_group_order(rgo)
    }
  }else{
    if("metric" %in% names(x)){
      tab = tab |>
        cols_label(
          metric = "Metric"
        )
    }else{
      tab = tab |>
        cols_label(
          tau = ":tau:"
        )
    }
  }

  tab

}


latex_tweaks = function(x, caption, label, sideways = T, adjustbox = T){
  y = x |>
    stringr::str_replace_all("o3", "") |>
    stringr::str_replace_all("no2", "") |>
    stringr::str_replace_all("ox", "") |>
    stringr::str_replace_all(":tau:", "$\\\\tau$") |>
    stringr::str_replace_all("www", "\\\\textsuperscript{") |>
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


parse_piecewise_stats = function(dat){
  dat |>
    expand_slopes() |>
    filter(
      year %in% c(2000, 2004, 2008, 2012, 2016, 2020),
      stat == "slope",
    ) |>
    pivot_wider(names_from = "type") |>
    nest_by(station_id, name, year, tau) |>
    mutate(data = data |> # if the year we have picked is a year that the slope changes in, then there will be two entries for that year.
             filter(endYear == max(endYear)) |> # pick one that ends later
             filter(scenario_idx == max(scenario_idx)) |>  # odd case that there are equal AICs that have made it this far (1 site AFAIK)
             list()
    ) |>
    unnest(data) |>
    ungroup() |>
    mutate(type = case_when(
      fit > 0 & pv < 0.33 ~ "Increasing",
      fit < 0 & pv < 0.33 ~ "Decreasing",
      pv >= 0.33 ~ "No Trend",
      TRUE ~ "No Trend" # for the NDGT70 metric, no trend can exist as NDGT70 is always 0, so catch these here (2-6 sites)
    )) |>
    left_join(
      combinedMetaRegion(con) |>
        select(station_id, region) |>
        distinct() |>
        collect(),
      "station_id"
    ) |>
    group_by(region, tau, name, type, year) |>
    count() |>
    ungroup()
}
