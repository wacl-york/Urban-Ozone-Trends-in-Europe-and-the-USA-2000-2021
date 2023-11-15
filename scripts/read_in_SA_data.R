library(tidyverse)
library(lubridate)

file_name = list.files("Z:/Global_AQ_Data/South America/", recursive = F)

i = file_name[150]

met_info = read.csv(paste0("Z:/Global_AQ_Data/South America/",i), header = F)

met_info = met_info$V1[str_detect(met_info$V1, "^#")]
met_info = data.frame(met_info)

met_info <- met_info %>% 
  mutate(metric = str_extract(met_info,
                              "[^:]+"))
met_info <- met_info %>% 
  mutate(result = str_remove(met_info,
                             "[^:]+"))

met_info$result = gsub(": ", "", met_info$result)
met_info$result = gsub(":", "", met_info$result)
met_info$metric = gsub("#", "", met_info$metric)

met_info = met_info %>% 
  select(metric, result) %>%
  filter(metric %in% c("Station_id", "Station_country", "Station_city", "Station_name", "Station_type", "Station_type_of_area")) %>% 
  distinct() %>% 
  pivot_wider(names_from = metric, values_from = result)

met_info$Parameter = paste0(i)

met_info <- met_info %>% 
  mutate(Parameter = str_extract(Parameter,
                                 "[^_]+"))

met_full_file = met_info[0,]

rm(i)

for(i in file_name){
  
  file_name = list.files("Z:/Global_AQ_Data/South America/", recursive = F)
  
  met_info = read.csv(paste0("Z:/Global_AQ_Data/South America/",i), header = F)
  
  met_info = met_info$V1[str_detect(met_info$V1, "^#")]
  met_info = data.frame(met_info)
  
  met_info <- met_info %>% 
    mutate(metric = str_extract(met_info,
                                "[^:]+"))
  met_info <- met_info %>% 
    mutate(result = str_remove(met_info,
                               "[^:]+"))
  
  met_info$result = gsub(": ", "", met_info$result)
  met_info$result = gsub(":", "", met_info$result)
  met_info$metric = gsub("#", "", met_info$metric)
  
  met_info = met_info %>% 
    select(metric, result) %>%
    filter(metric %in% c("Station_id", "Station_country", "Station_city", "Station_name", "Station_type", "Station_type_of_area")) %>% 
    distinct() %>% 
    pivot_wider(names_from = metric, values_from = result)
  
  met_info$Parameter = paste0(i)
  
  met_info <- met_info %>% 
    mutate(Parameter = str_extract(Parameter,
                                   "[^_]+"))
  
  met_full_file = bind_rows(met_full_file, met_info)
}



file_name = list.files("Z:/Global_AQ_Data/South America/", recursive = F)

i = file_name[150]

met_info = read.csv(paste0("Z:/Global_AQ_Data/South America/",i), header = F)

met_info = met_info$V1[str_detect(met_info$V1, "^#")]
met_info = data.frame(met_info)

met_info <- met_info %>% 
  mutate(metric = str_extract(met_info,
                              "[^:]+"))
met_info <- met_info %>% 
  mutate(result = str_remove(met_info,
                             "[^:]+"))

met_info$result = gsub(": ", "", met_info$result)
met_info$result = gsub(":", "", met_info$result)
met_info$metric = gsub("#", "", met_info$metric)

met_info = met_info %>% 
  select(metric, result) %>%
  filter(metric %in% c("Station_id", "Station_country", "Station_city", "Station_name", "Station_type", "Station_type_of_area")) %>% 
  distinct() %>% 
  pivot_wider(names_from = metric, values_from = result)

met_info$Parameter = paste0(i)

met_info <- met_info %>% 
  mutate(Parameter = str_extract(Parameter,
                                 "[^_]+"))

met_full_file = met_info[0,]

rm(i)

for(i in file_name){
  
  file_name = list.files("Z:/Global_AQ_Data/South America/", recursive = F)
  
  met_info = read.csv(paste0("Z:/Global_AQ_Data/South America/",i), header = F)
  
  met_info = met_info$V1[str_detect(met_info$V1, "^#")]
  met_info = data.frame(met_info)
  
  met_info <- met_info %>% 
    mutate(metric = str_extract(met_info,
                                "[^:]+"))
  met_info <- met_info %>% 
    mutate(result = str_remove(met_info,
                               "[^:]+"))
  
  met_info$result = gsub(": ", "", met_info$result)
  met_info$result = gsub(":", "", met_info$result)
  met_info$metric = gsub("#", "", met_info$metric)
  
  met_info = met_info %>% 
    select(metric, result) %>%
    filter(metric %in% c("Station_id", "Station_country", "Station_city", "Station_name", "Station_type", "Station_type_of_area")) %>% 
    distinct() %>% 
    pivot_wider(names_from = metric, values_from = result)
  
  met_info$Parameter = paste0(i)
  
  met_info <- met_info %>% 
    mutate(Parameter = str_extract(Parameter,
                                   "[^_]+"))
  
  met_full_file = bind_rows(met_full_file, met_info)
}


file_name = list.files("Z:/Global_AQ_Data/South America/", recursive = F)

i = file_name[1]

data_file = read_delim(paste0("Z:/Global_AQ_Data/South America/",i), delim = ";", comment = "#")
data_file$filename = paste0(i)
data_file$Parameter = paste0(i)
data_file$Station_id = paste0(i)

data_file <- data_file %>% 
  mutate(Parameter = str_extract(Parameter,
                                 "[^_]+"))

data_file <- data_file %>% 
  mutate(Station_id = str_remove(Station_id,
                                 "[^_]+"))

data_file$Station_id = gsub("_", "", data_file$Station_id)
data_file$Station_id = gsub(".csv", "", data_file$Station_id)

data_full_frame = data_file[0,]


file_name = list.files("Z:/Global_AQ_Data/South America/", recursive = F)

for(i in file_name){
  
  data_file = read_delim(paste0("Z:/Global_AQ_Data/South America/",i), delim = ";", comment = "#")
  data_file$filename = paste0(i)
  data_file$Parameter = paste0(i)
  data_file$Station_id = paste0(i)
  
  data_file <- data_file %>% 
    mutate(Parameter = str_extract(Parameter,
                                   "[^_]+"))
  
  data_file <- data_file %>% 
    mutate(Station_id = str_remove(Station_id,
                                   "[^_]+"))
  
  data_file$Station_id = gsub("_", "", data_file$Station_id)
  data_file$Station_id = gsub(".csv", "", data_file$Station_id)
  
  data_full_frame = bind_rows(data_full_frame, data_file)
  
}

data_and_met = left_join(data_full_frame, met_full_file, by = c("Station_id", "Parameter"), relationship = "many-to-many")


met_



# multidecadal_data = data_and_met %>% 
#   filter(Time <= "2001-01-01 00:00:00" )
# 
# O3_data = data_and_met %>% 
#   filter(Parameter == "O3")
# 
# ggplot(O3_data)+
#   geom_point(aes(x = Time, y = Value, colour = Station_id))+
#   facet_wrap(~Station_country)
