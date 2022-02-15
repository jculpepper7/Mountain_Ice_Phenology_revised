## The objective of this script is to:
## 1. Read in combined in situ data and remote data. 
## 2. Detect ice phenology from remote data.
## 2a. Detect first instance of freeze
## 2b. Detect dates of freeze and breakup of the longest freeze period
## 2c. Detect total number of freeze periods
## 3. Bind separate data frames into a single data frame for further analysis. 
## 4. Write a new csv file with the aggregated data for further analysis in other scripts.

#load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(plotly)
library(zoo)
library(here)
library(janitor)
library(padr)
library(modelr)
library(viridis)
#for spatial plots
library(ggmap)
library(usmap)

# 1. Read in data --------------------------------------------------------------

#Import cleaned and merged remotely sensed data from script 06

all_remote_ice_data <- read_csv(here("data/remote/aqua_terra_merged_clean.csv"))


# all_remote_ice_data <- all_remote_ice_data %>%
#   filter(date > "2000-08-01") #remove dates prior to August in the year 2000, as it gives a false ice on value in March or April (from the previous years freeze event)

# 2. Detect ice phenology from median values -----------------------------------

#NOTE: This code is a modification of script 04. Check if theres an issue, as I 
#have eliminated some lines owing to the data cleaning process in script 06

# 2a. Create a dataframe with no missing dates and rolling median windows------- 

all_remote_w_median <- all_remote_ice_data %>%
  na.omit() %>%
  mutate(date = ymd(date)) %>% #ensure that date is in date format
  select(hylak_id, date, full_merge) %>% #reorder columns
  group_by(hylak_id) %>%
  pad(break_above = 15) %>% #insert missing observations by day
  mutate(full_merge_fill = na.approx(full_merge)) %>% #this fills NAs with a linear interpolation (from zoo package)
  mutate(frac_31day_med = zoo::rollmedian(full_merge_fill, k = 31, fill = NA)) %>%
  pivot_longer(!c(hylak_id, date), names_to = "median_val", values_to = "median_iceFrac") %>% #change from wide to long format
  arrange(median_val) %>% #reorganize rows to group by rolling median from 3-31 rather than date
  mutate(year = year(date), #add year column
         month = month(date), #add month column to make a water year column
         water_year = ifelse(month>=10, year+1, year)) %>% #add a water year column
  select(-month) %>% #remove month column (no need after making water year column)
  ungroup

# 2b. Detect ice phenology from median values ----------------------------------

#first create a dataframe with ice on values

remote_iceOn <- all_remote_w_median %>%
  group_by(hylak_id, water_year, median_val) %>%
  mutate(median_iceFrac = rollapply(median_iceFrac, width = 21, min, align = "left", fill = NA, na.rm = TRUE)) %>%
  #mutate(median_iceFrac = rollmedian(median_iceFrac, k = 21, min, align = "left", fill = NA, na.rm = TRUE)) %>%
  filter(median_iceFrac >= 0.8) %>%
  filter(row_number() == 1) %>%
  rename(ice_on_water_year = water_year) %>%
  ungroup

#Second, create a dataframe with ice off values

remote_iceOff <- all_remote_w_median %>%
  group_by(hylak_id, year, median_val) %>%
  #filter(date > "2000-08-01") %>%
  mutate(median_iceFrac = rollapply(median_iceFrac, width = 28, max, align = "left", fill = NA, na.rm = TRUE)) %>% 
  filter(median_iceFrac <= 0.2) %>% 
  filter(row_number() == 1) %>%
  rename(ice_off_water_year = water_year) %>%
  ungroup

#Create new dataframes for ice on and ice off-----------------------------------

remote_iceOn_final <- remote_iceOn %>%
  filter(median_val == "full_merge") %>%
  arrange(hylak_id, date)

remote_iceOff_final <- remote_iceOff %>%
  filter(median_val == "frac_31day_med") %>%
  arrange(hylak_id, date)

#Load hydroLAKES data and join to ice on and ice off data-----------------------

# #hydrolakes data (see script # 00 for source and link to data)
hydro_lakes <- read_csv(here('data/hydroLAKES/HydroLAKES_points_v10.csv')) %>%
  clean_names()

#join hydroLAKES to ice on data

iceOn_full_data <- remote_iceOn_final %>%
  left_join(hydro_lakes, keep = TRUE) %>%
  select(-median_val, -median_iceFrac)

#groupby and run regressions
all_regress <-  iceOn_full_data %>% 
  mutate(ice_on_yday = yday(date)) %>%
  group_by(hylak_id) %>%
  do(mod1 = lm(ice_on_yday ~ year, data = .)) %>%
  ungroup

#use broom the extract the slope and rsq per group
glance <-all_regress %>% 
  mutate(tidy = map(mod1, broom::tidy),
         glance = map(mod1, broom::glance),
         augment = map(mod1, broom::augment),
         rsq = glance %>% map_dbl('r.squared'),
         slope = tidy %>% map_dbl(function(x) x$estimate[2])) %>%
  select(hylak_id, slope)

iceOn_full_w_slope <- iceOn_full_data %>%
  left_join(glance, by = 'hylak_id')


#join hydroLAKES to ice off data------------------------------------------------

iceOff_full_data <- remote_iceOff_final %>%
  left_join(hydro_lakes, by = 'hylak_id') %>%
  select(-median_val, -median_iceFrac) 

mean(iceOff_full_data$elevation) #1066.67
median(iceOff_full_data$elevation) #1066.67
mean(iceOff_full_data$lake_area) #42.5
median(iceOff_full_data$lake_area) #6.17
range(iceOff_full_data$lake_area)#0.4 495.62

#groupby and run regressions
all_regress <-  iceOff_full_data %>%
  filter(elevation > 1500) %>%
  mutate(ice_off_yday = yday(date)) %>%
  group_by(hylak_id) %>%
  do(mod1 = lm(ice_off_yday ~ year, data = .)) %>%
  ungroup

#use broom the extract the slope and rsq per group
glance <-all_regress %>% 
  mutate(tidy = map(mod1, broom::tidy),
         glance = map(mod1, broom::glance),
         augment = map(mod1, broom::augment),
         rsq = glance %>% map_dbl('r.squared'),
         slope = tidy %>% map_dbl(function(x) x$estimate[2])) %>%
  select(hylak_id, slope)

iceOff_full_w_slope <- iceOff_full_data %>%
  right_join(glance, by = 'hylak_id') %>%
  filter(elevation > 1500,
         slope > -10,
         slope < 5) 

ggplot(data = iceOff_full_w_slope, aes(x = year, y = ))
#Build a map of lakes with slopes-----------------------------------------------

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


library("ggplot2")
theme_set(theme_bw())
library("sf")       


library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-127.15, -115.12), ylim = c(28.65, 50.97), expand = FALSE)+
  geom_point(data = iceOff_full_data, aes(x = pour_long, y = pour_lat, size = lake_area, color = elevation))+
  scale_colour_viridis()+
  xlab("Longitude")+
  ylab("Latitude")
  #coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

ggsave(here("output/figs/lake_map.jpeg"), dpi = 700) #, width = 15, height = 15, units = "in"


#Plot the same map; however, color by slope of ice off--------------------------

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-127.15, -115.12), ylim = c(28.65, 50.97), expand = FALSE)+
  geom_point(data = iceOff_full_w_slope, aes(x = pour_long, y = pour_lat, color = slope), size = 4)+
  #geom_jitter()+
  scale_colour_viridis()+
  xlab("Longitude")+
  ylab("Latitude")

ggsave(here("output/figs/lake_map_ice_off.jpeg"), dpi = 700) 

mean(iceOff_full_w_slope$slope) #-0.9998539
median(iceOff_full_w_slope$slope) #-0.8091474
range(iceOff_full_w_slope$slope) #-5.485714, 4.178862



#Plot the same map; however, color by slope of ice on--------------------------

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-127.15, -115.12), ylim = c(28.65, 50.97), expand = FALSE)+
  geom_point(data = iceOn_full_w_slope, aes(x = pour_long, y = pour_lat, size = lake_area, color = slope))+
  scale_colour_viridis()+
  xlab("Longitude")+
  ylab("Latitude")

ggsave(here("output/figs/lake_map_ice_on.jpeg"), dpi = 700)

mean(iceOn_full_w_slope$slope, na.rm = TRUE) #-2.03884
median(iceOn_full_w_slope$slope, na.rm = TRUE) #-1.247955
range(iceOn_full_w_slope$slope, na.rm = TRUE) #-44.75510, 33.87634





















all_ice <-  iceOff_full_data %>%
  filter(elevation > 1500) %>%
  mutate(ice_off_yday = yday(date)) %>%
  ungroup

min(all_ice$pour_lat)

low_lat <- all_ice %>%
  filter(pour_lat < 35)


ggplot(data = all_ice, aes(x = year, y = ice_off_yday))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~hylak_id)
ggsave(here('output/figs/faceted_high_lakes.jpeg'), dpi = 700)
