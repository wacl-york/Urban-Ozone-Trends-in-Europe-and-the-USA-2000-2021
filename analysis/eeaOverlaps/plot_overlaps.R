library(saqgetr)
library(here)
library(dplyr)
library(ggplot2)
library(patchwork)
library(progress)
library(tidyr)

plot_overlapping_data = function(site, var = "o3"){

  raw = saqgetr::get_saq_observations(site,
                                      variable = var,
                                      start = "2000-01-01",
                                      end = "2021-12-31")

  test = raw |>
    select(-date_end) |>
    filter(summary == 1)

  a = test |>
    ggplot()+
    geom_line(aes(date, value, colour = as.factor(process)))

  b = test |>
    ggplot()+
    geom_line(aes(date, value, colour = as.factor(process)))+
    facet_wrap(~as.factor(process), ncol = 1)

  g = a+b

  return(g)
}

end_overlaps_start = function(df){

  overlaps = vector(length = nrow(df))

  for(i in 1:nrow(df)){

    starts = df$date_start[df$date_start > df$date_start[i]] # filter for all starts that are greater than this start

    if(length(starts) == 0){
      return(-1)
    }

    overlaps = sum(starts < df$date_end[i]) # count overlaps

    return(overlaps)

  }

}

proc = get_saq_processes()

o3_overlaps = proc |>
  mutate(id = paste0(site, variable, date_start, date_end), # this should remove cases of true duplicated data.
         dup = duplicated(id)) |>
  filter(!dup,
         period == "hour",
         variable == "o3") |>
  nest_by(site, variable) |>
  mutate(overlaps = end_overlaps_start(data)) |>
  filter(overlaps > 0) |>
  unnest(data)


# good example is es1131a


plot_overlapping_data("es1131a")
