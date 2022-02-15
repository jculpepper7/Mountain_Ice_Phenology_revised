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

# 1. Read in data -----------------------------------------------------------

all_insitu_ice_data <- read_csv(here('data/combined/all_insitu_ice_data.csv'))

#load and clean remotely sensed data

castle_remote <- read_csv(here('data/castle/castle_terra_aqua_merged.csv'))

#eliminate outlier code (discussed with Rina on 9/30/21)

castle_remote <- castle_remote %>%
  clean_names() %>%
  mutate(lake = c('castle')) %>%
  select(lake, date, ice_modis)

diff <- as.data.frame(diff(castle_remote$ice_modis))



# castle_outlier_removal <- castle_remote %>%
#   mutate(diff = diff(median_iceFrac))




castle_remote <- castle_remote %>%
  clean_names() %>%
  mutate(lake = c("castle")) %>% #add name of lake
  select(lake, date, ice_modis) %>% #select and reorder relevant columns
  mutate(frac_03day_med = zoo::rollmedian(ice_modis, k = 3, fill = NA), #add rolling medians from 3-31 days
         frac_05day_med = zoo::rollmedian(ice_modis, k = 5, fill = NA),
         frac_07day_med = zoo::rollmedian(ice_modis, k = 7, fill = NA),
         frac_09day_med = zoo::rollmedian(ice_modis, k = 9, fill = NA),
         frac_11day_med = zoo::rollmedian(ice_modis, k = 11, fill = NA),
         frac_13day_med = zoo::rollmedian(ice_modis, k = 13, fill = NA),
         frac_15day_med = zoo::rollmedian(ice_modis, k = 15, fill = NA),
         frac_17day_med = zoo::rollmedian(ice_modis, k = 17, fill = NA),
         frac_19day_med = zoo::rollmedian(ice_modis, k = 19, fill = NA),
         frac_21day_med = zoo::rollmedian(ice_modis, k = 21, fill = NA),
         frac_23day_med = zoo::rollmedian(ice_modis, k = 23, fill = NA),
         frac_25day_med = zoo::rollmedian(ice_modis, k = 25, fill = NA),
         frac_27day_med = zoo::rollmedian(ice_modis, k = 27, fill = NA),
         frac_29day_med = zoo::rollmedian(ice_modis, k = 29, fill = NA),
         frac_31day_med = zoo::rollmedian(ice_modis, k = 31, fill = NA)) %>%
  select(-ice_modis) %>% #remove the raw data
  pivot_longer(!c(lake, date), names_to = "median_val", values_to = "median_iceFrac") %>% #change from wide to long format
  arrange(median_val) %>% #reorganize rows to group by rolling median from 3-31 rather than date
  mutate(year = year(date), #add year column
         month = month(date), #add month column to make a water year column
         water_year = ifelse(month>=10, year+1, year)) %>% #add a water year column
  select(-month) #remove month column (no need after making water year column)
 
  
 
# 2. Detect ice phenology from median values -------------------------------

castle_medianVal_iceOn <- castle_remote %>%
  #select(date, year, water_year, w_year, frac_03day_med) %>%
  group_by(water_year, median_val) %>%
  mutate(median_iceFrac = rollapply(median_iceFrac, width = 14, min, align = "left", fill = NA, na.rm = TRUE)) %>%
  filter(median_iceFrac >= 0.8) %>%
  filter(row_number() == 1) %>%
  ungroup

# castle_medianVal_iceOn_different <- castle_remote %>%
#   #select(date, year, water_year, w_year, frac_03day_med) %>%
#   group_by(water_year, median_val) %>%
#   mutate(median_iceFrac = rollapply(median_iceFrac, width = case_when(time_length(date, "days") >= 20), min, align = "left", fill = NA, na.rm = TRUE)) %>%
#   filter(median_iceFrac >= 0.8) %>%
#   filter(row_number() == 1) %>%
#   ungroup

#filter for just castle to run the MAD function
castle_insitu <- all_insitu_ice_data %>%
  filter(lake == "castle")

castle_medianVal_iceOff <- castle_remote %>%
  #select(date, year, water_year, w_year, frac_21day_med) %>%
  group_by(year, median_val) %>%
  mutate(median_iceFrac = rollapply(median_iceFrac, width = 14, max, align = "left", fill = NA, na.rm = TRUE)) %>% 
  filter(median_iceFrac <= 0.2) %>% 
  filter(row_number() == 1) %>%
  select(-median_iceFrac) %>%
  pivot_wider(names_from = median_val, values_from = date) %>%
  arrange(year)
  #right_join(castle_insitu, by = "year", suffix = c("_remote", "_insitu")) #%>%
#mutate(yday_remote = yday(date_remote)) %>% #these lines of code are to double check that shifting to yday would not be an issue because the abs(mean()) of date was not working in other lakes
#mutate(yday_insitu = yday(date_insitu))
#ungroup

castle_insitu <- all_insitu_ice_data %>%
  filter(lake == "castle")

abs(mean(castle_grouped21_off_threshold_diff$date_remote - castle_grouped21_off_threshold_diff$date_insitu))  
# ggplot(data = castle_remote) +
#   geom_line(mapping = aes(x = date, y = ice_modis))



# 2. Detect ice phenology ---------------------------------------------------

# 2a. First ice occurrence ---------------------------------------------------


