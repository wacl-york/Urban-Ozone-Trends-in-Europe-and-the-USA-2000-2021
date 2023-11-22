library(dplyr)
library(saqgetr)
library(lubridate)

find_overlaps = function(df){

  df = df |>
    filter(!is.na(date_start),
           !is.na(date_end))

  if(nrow(df) == 0){
    return(tibble())
  }

  ts = tibble(date = seq(ymd_hm("2000-01-01 00:00"), ymd_hm("2022-01-01 00:00"), "hour"))

  for(i in 1:nrow(df)){

    nm = paste0("x",i)

    thisTs = tibble(date = seq(df$date_start[i], df$date_end[i], "hour")) |>
      mutate(!!nm := date)

    ts = left_join(ts, thisTs, by = "date")
  }

  ts = ts |>
    mutate(across(contains("x"), \(x) !is.na(x)),
           tot = rowSums(across(contains("x"))))

  ts_rle = ts$tot |>
    rle() |>
    wsdmiscr::tidy_rle()

  ts_rle$date_start = ts$date[ts_rle$idxStart]
  ts_rle$date_end = ts$date[ts_rle$idxEnd]

  ts_rle |>
    filter(values > 1,
           lengths > 24) # overlap for longer than 24 hours.

}

eeaProcesses = get_saq_processes()

start = "2000-01-01"
end = "2021-12-31"

eeaMeta = get_saq_sites() |>
  filter(site_area == "urban",
         date_start <= ymd(start),
         date_end >= ymd(end))

proc = eeaProcesses |>
  filter(period == "hour",
         variable %in% c("o3", "no2"),
         site %in% eeaMeta$site) |>
  nest_by(site,variable) |>
  mutate(overlaps = find_overlaps(data) |>
           list())


sig_overlaps = proc |>
  mutate(n = nrow(overlaps)) |>
  filter(n > 0) |>
  mutate(longest_overlap = max(purrr::pluck(overlaps, "lengths"))) |>
  arrange(desc(longest_overlap)) |>
  mutate(overlap_status = NA,
         overlap_table = NA)

pb = progress::progress_bar$new(total = nrow(sig_overlaps))

for(i in 1:nrow(sig_overlaps)){
  pb$tick()
  temp = saqgetr::get_saq_observations(site = sig_overlaps$site[i],
                                       variable = sig_overlaps$variable[i],
                                       start = start,
                                       end = end) |>
    mutate(process = paste0("x",process)) |>
    filter(summary == 20)

  if(nrow(temp) == 0){

    sig_overlaps$overlap_status[[i]] = "site has no data"
    next

  }

  process_names = unique(temp$process)


  step1 = temp |>
    pivot_wider(names_from = "process")


  for(j in 1:length(process_names)){

    step1[[paste0(process_names[j],"_flag")]] = !is.na(step1[[process_names[j]]])

  }

  step2 = step1 |>
    mutate(tot = rowSums(across(contains("_flag")))) |>
    select(-contains("_flag")) |>
    filter(tot > 1) |>
    pivot_longer(all_of(process_names), names_to = "process")

  sig_overlaps$overlap_table[[i]] = list(step2)

  if(nrow(step2) < 24 & nrow(step2) > 0){
    sig_overlaps$overlap_status[[i]] = "less than 24 hours of overlap"
    next
  }

  if(nrow(step2) == 0){
    sig_overlaps$overlap_status[[i]] = "no data in overlapping processes"
    next
  }

  sig_overlaps$overlap_status[[i]] = "overlap"

}


actual_overlaps = sig_overlaps |>
  mutate(nrow_overlap = nrow(overlap_table[[1]]) |>
           list()) |>
  unnest(nrow_overlap) |>
  filter(nrow_overlap != 0)


actual_overlaps = step2 |>
  group_by(process) |>
  summarise(value = sum(value, na.rm = T))

step2 |>
  select(-date_end) |>
  left_join(sig_overlaps$overlaps[[4]], by = join_by(between(date, date_start, date_end))) |>
  group_by(process,idxStart) |>
  summarise(value = sum(value, na.rm = T))

step2 |>
  ggplot()+
  geom_point(aes(date, value, colour = process))

temp |>
  ggplot()+
  geom_line(aes(date, value, colour = process))+
  facet_wrap(~process, ncol = 1)
