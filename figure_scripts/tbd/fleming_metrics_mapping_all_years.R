library(sf)
library(gt)
library(DBI)
library(dplyr)
library(tidyr)
library(scico)
library(ggh4x)
library(ggtext)
library(ggplot2)
library(rnaturalearth)
#library(rnaturalearthhires)
library(ggpubr)
library(stringr)
library(here)
library(lubridate)

source(here::here('functions','utils.R'))
source(here::here('functions','plotting_utils.R'))

con = connect_to_db()

######

metrics = tbl(con, "dat_metrics") |>
  left_join(tbl(con, "combinedMeta") |>
              select(station_id, latitude, longitude, country) |>
              distinct(),
            by = "station_id") |>
  collect()

mycrs = 4087 #8857

world = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") |>
  st_transform(mycrs)

#########################

metrics_binned = metrics |>
  mutate(year = year(date)) |>
  select(-c(date,name)) |>
  group_by(year, metric, station_id, latitude, longitude, country, timezone) |>
  summarise_all("mean", na.rm = T)

metrics_sf = st_as_sf(metrics_binned, coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs) |>
  na.omit()

metrics_4MDA8_sf = st_as_sf(metrics_binned, coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs) |>
  na.omit() |>
  filter(metric == "4MDA8") |>
  mutate(value_bin = case_when(value < 30.5 ~ "0 - 30 ppb",
                               value >= 30.5 & value < 40.5 ~ "31 - 40 ppb",
                               value >= 40.5 & value < 50.5 ~ "41 - 50 ppb",
                               value >= 50.5 & value < 60.5 ~ "51 - 60 ppb",
                               value >= 60.5 & value < 70.5 ~ "61 - 70 ppb",
                               value >= 70.5 & value < 75.5 ~ "71 - 75 ppb",
                               #value >= 75.5 & value < 80.5 ~ "76 - 80 ppb",
                               value >= 75.5 & value < 85.5 ~ "76 - 85 ppb",
                               #value >= 80.5 & value < 85.5 ~ "81 - 85 ppb",
                               value >= 85.5 & value < 100.5 ~ "86 - 100 ppb",
                               value >= 100.5 ~ ">= 100 ppb",
                               .default = NA)) |>
  arrange(value)


limUS = tibble(lng = c(-130,-50), lat = c(25,50)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

limEU = tibble(lng = c(-20,35), lat = c(25,65)) |>
  st_as_sf(coords = c("lng", "lat"), crs = st_crs("WGS84")) |>
  st_transform(mycrs)

metrics_4MDA8_sf$value_bin <- factor(metrics_4MDA8_sf$value_bin,
                                     levels=c("0 - 30 ppb",
                                              "31 - 40 ppb",
                                              "41 - 50 ppb",
                                              "51 - 60 ppb",
                                              "61 - 70 ppb",
                                              "71 - 75 ppb",
                                              "76 - 85 ppb",
                                              #"81 - 85 ppb",
                                              "86 - 100 ppb",
                                              ">= 100 ppb"
                                     ))

plot_4MDA8_EU = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_4MDA8_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("4MDA8") +
  labs(colour = "4MDA8") +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick2", "firebrick4")) +
  # scale_color_manual(values = c("mediumslateblue","mediumblue","dodgerblue",
  #                               "forestgreen", "gold", "chocolate1", "firebrick2", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

plot_4MDA8_US = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_4MDA8_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("4MDA8") +
  labs(colour = "4MDA8") +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick2", "firebrick4")) +
  # scale_color_manual(values = c("mediumslateblue","mediumblue","dodgerblue",
  #                               "forestgreen", "gold", "chocolate1", "firebrick2", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

##################

metrics_NDGT70_sf = st_as_sf(metrics_binned, coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs) |>
  na.omit() |>
  filter(metric == "NDGT70") |>
  mutate(value_bin = case_when(value < 0.5 ~ "0 days",
                               value >= 0.5 & value < 3.5 ~ "0 - 3 days",
                               value >= 3.5 & value < 6.5 ~ "4 - 6 days",
                               value >= 6.5 & value < 15.5 ~ "7 - 15 days",
                               value >= 15.5 & value < 25.5 ~ "16 - 25 days",
                               value >= 25.5 ~ ">= 26 days",
                               .default = NA)) |>
  arrange(value)

metrics_NDGT70_sf$value_bin <- factor(metrics_NDGT70_sf$value_bin,
                                      levels=c("0 days",
                                               "0 - 3 days",
                                               "4 - 6 days",
                                               "7 - 15 days",
                                               "16 - 25 days",
                                               ">= 26 days"))


plot_NDGT70_EU = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_NDGT70_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("NDGT70") +
  labs(colour = "NDGT70") +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  scale_color_manual(values = c("midnightblue","dodgerblue",
                                "gold", "chocolate1", "firebrick2", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))


plot_NDGT70_US = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_NDGT70_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("NDGT70") +
  labs(colour = "NDGT70") +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  scale_color_manual(values = c("midnightblue","dodgerblue",
                                "gold", "chocolate1", "firebrick2", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

##############################################

timezone_dat = tbl(con, "combinedMeta") |>
  select(station_id, timezone) |>
  distinct() |>
  collect()

metrics_SOMO35_sf = st_as_sf(metrics_binned, coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs) |>
  left_join(timezone_dat, by = "station_id") |>
  na.omit() |>
  filter(metric == "SOMO35") |>
  mutate(value_bin = case_when(value < 1000.5 ~ "0 - 1000 ppb day",
                               value >= 1000.5 & value < 2000.5 ~ "1001 - 2000 ppb day",
                               value >= 2000.5 & value < 3000.5 ~ "2001 - 3000 ppb day",
                               value >= 3000.5 & value < 4000.5 ~ "3001 - 4001 ppb day",
                               value >= 4000.5 & value < 5000.5 ~ "4001 - 5000 ppb day",
                               value >= 5000.5 & value < 6000.5 ~ "5001 - 6000 ppb day",
                               value >= 6000.5 & value < 7000.5 ~ "6001 - 7000 ppb day",
                               value >= 7000.5 & value < 8000.5 ~ "7001 - 8000 ppb day",
                               value >= 8000.5 ~ ">= 8001 ppb day",
                               .default = NA)) |>
  arrange(value)

metrics_SOMO35_sf$value_bin <- factor(metrics_SOMO35_sf$value_bin,
                                      levels=c("0 - 1000 ppb day",
                                               "1001 - 2000 ppb day",
                                               "2001 - 3000 ppb day",
                                               "3001 - 4001 ppb day",
                                               "4001 - 5000 ppb day",
                                               "5001 - 6000 ppb day",
                                               "6001 - 7000 ppb day",
                                               "7001 - 8000 ppb day",
                                               ">= 8001 ppb day"))


plot_SOMO35_EU = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_SOMO35_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("SOMO35") +
  labs(colour = "SOMO35") +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick2", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))


plot_SOMO35_US = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_SOMO35_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("SOMO35") +
  labs(colour = "SOMO35") +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick2", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))


###################################################

metrics_3MMDA1_sf = st_as_sf(metrics_binned, coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs) |>
  na.omit() |>
  filter(metric == "3MMDA1") |>
  mutate(value = ifelse(str_detect(country, "United States of America"), value, value / 1.96)) |>
  mutate(value_bin = case_when(value < 30.5 ~ "0 - 30 ppb",
                               value >= 30.5 & value < 40.5 ~ "31 - 40 ppb",
                               value >= 40.5 & value < 50.5 ~ "41 - 50 ppb",
                               value >= 50.5 & value < 60.5 ~ "51 - 60 ppb",
                               value >= 60.5 & value < 70.5 ~ "61 - 70 ppb",
                               value >= 70.5 & value < 80.5 ~ "71 - 80 ppb",
                               value >= 80.5 & value < 90.5 ~ "81 - 90 ppb",
                               value >= 90.5 ~ ">= 91 ppb",
                               .default = NA)) |>
  arrange(value)


metrics_3MMDA1_sf$value_bin <- factor(metrics_3MMDA1_sf$value_bin,
                                      levels=c("0 - 30 ppb",
                                               "31 - 40 ppb",
                                               "41 - 50 ppb",
                                               "51 - 60 ppb",
                                               "61 - 70 ppb",
                                               "71 - 80 ppb",
                                               "81 - 90 ppb",
                                               ">= 91 ppb"
                                      ))

plot_3MMDA1_EU = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_3MMDA1_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("3MMDA1") +
  labs(colour = "3MMDA1") +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

plot_3MMDA1_US = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_3MMDA1_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("3MMDA1") +
  labs(colour = "3MMDA1") +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

###################################################

metrics_6MMDA1_sf = st_as_sf(metrics_binned, coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs) |>
  na.omit() |>
  filter(metric == "6MMDA1") |>
  mutate(value = ifelse(str_detect(country, "United States of America"), value, value / 1.96)) |>
  mutate(value_bin = case_when(value < 30.5 ~ "0 - 30 ppb",
                               value >= 30.5 & value < 40.5 ~ "31 - 40 ppb",
                               value >= 40.5 & value < 50.5 ~ "41 - 50 ppb",
                               value >= 50.5 & value < 60.5 ~ "51 - 60 ppb",
                               value >= 60.5 & value < 70.5 ~ "61 - 70 ppb",
                               value >= 70.5 & value < 80.5 ~ "71 - 80 ppb",
                               value >= 80.5 & value < 90.5 ~ "81 - 90 ppb",
                               value >= 90.5 ~ ">= 91 ppb",
                               .default = NA)) |>
  arrange(value)


metrics_6MMDA1_sf$value_bin <- factor(metrics_6MMDA1_sf$value_bin,
                                      levels=c("0 - 30 ppb",
                                               "31 - 40 ppb",
                                               "41 - 50 ppb",
                                               "51 - 60 ppb",
                                               "61 - 70 ppb",
                                               "71 - 80 ppb",
                                               "81 - 90 ppb",
                                               ">= 91 ppb"
                                      ))

plot_6MMDA1_EU = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_6MMDA1_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("6MMDA1") +
  labs(colour = "6MMDA1") +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

plot_6MMDA1_US = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_6MMDA1_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("6MMDA1") +
  labs(colour = "6MMDA1") +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

##################

metrics_AVGMDA8_sf = st_as_sf(metrics_binned, coords = c("longitude", "latitude"), crs = st_crs("WGS84")) |>
  st_transform(mycrs) |>
  na.omit() |>
  filter(metric == "AVGMDA8") |>
  mutate(value_bin = case_when(value < 35.5 ~ "0 - 35 ppb",
                               value >= 35.5 & value < 40.5 ~ "36 - 40 ppb",
                               value >= 40.5 & value < 45.5 ~ "41 - 45 ppb",
                               value >= 45.5 & value < 50.5 ~ "46 - 50 ppb",
                               value >= 50.5 & value < 55.5 ~ "51 - 55 ppb",
                               value >= 55.5 & value < 60.5 ~ "56 - 60 ppb",
                               value >= 60.5 & value < 65.5 ~ "61 - 65 ppb",
                               value >= 65.5 ~ ">= 66 ppb",
                               .default = NA)) |>
  arrange(value)


metrics_AVGMDA8_sf$value_bin <- factor(metrics_AVGMDA8_sf$value_bin,
                                       levels=c("0 - 35 ppb",
                                                "36 - 40 ppb",
                                                "41 - 45 ppb",
                                                "46 - 50 ppb",
                                                "51 - 55 ppb",
                                                "56 - 60 ppb",
                                                "61 - 65 ppb",
                                                ">= 66 ppb"
                                       ))

plot_AVGMDA8_EU = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_AVGMDA8_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("AVGMDA8") +
  labs(colour = "AVGMDA8") +
  scale_y_continuous(limits = st_coordinates(limEU)[,2])+
  scale_x_continuous(limits = st_coordinates(limEU)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

plot_AVGMDA8_US = ggplot()+
  geom_sf(data = world, fill = "white")+
  geom_sf(data = metrics_AVGMDA8_sf,
          mapping = aes(colour = value_bin)) +
  facet_wrap(~year) +
  ggtitle("AVGMDA8") +
  labs(colour = "AVGMDA8") +
  scale_y_continuous(limits = st_coordinates(limUS)[,2])+
  scale_x_continuous(limits = st_coordinates(limUS)[,1]) +
  scale_color_manual(values = c("midnightblue", "mediumslateblue","mediumblue","dodgerblue",
                                "forestgreen", "gold", "chocolate1", "firebrick4")) +
  theme_pubr() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

#################################################

dbDisconnect(con, shutdown = T)

############### combined plots ##################

pdf(here("analysis","metrics_inspect_all_years.pdf"), width = 16, height = 9)

print(plot_4MDA8_EU)
print(plot_4MDA8_US)
print(plot_NDGT70_EU)
print(plot_NDGT70_US)
print(plot_SOMO35_EU)
print(plot_SOMO35_US)
print(plot_3MMDA1_EU)
print(plot_3MMDA1_US)
print(plot_6MMDA1_EU)
print(plot_6MMDA1_US)
print(plot_AVGMDA8_EU)
print(plot_AVGMDA8_US)

dev.off()





