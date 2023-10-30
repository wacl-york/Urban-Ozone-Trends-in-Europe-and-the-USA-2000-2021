library(here)
library(dplyr)
library(stringr)
library(saqgetr)
library(lubridate)

start = "2000-01-01"
end = "2021-12-31"

eeaMeta = get_saq_sites() |>
  filter(site_area == "urban",
         date_start <= ymd(start),
         date_end >= ymd(end))

complete = list.files(here("data","eea","raw")) |>
  word(1,1, sep = "_")

toDo = eeaMeta |>
  filter(!site %in% complete)

for(i in 1:nrow(toDo)){


  temp = get_saq_observations(toDo$site[i],variable = c("o3","no2"),
                              start = start,
                              end = end)

  filename = paste0(unique(temp$site),
                    "_",
                    paste0(unique(temp$variable),collapse = "-"),
                    ".csv")

  write.csv(temp,
            here("data","eea","raw",filename),row.names = F)

}
