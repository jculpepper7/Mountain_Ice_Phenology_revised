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
library(Metrics)

# 1. Read in data --------------------------------------------------------------

#Import in situ data

#all_insitu_ice_data <- read_csv(here('data/combined/all_insitu_ice_data.csv'))
all_insitu_w_water_year <- read_csv(here('data/combined/all_insitu_water_year.csv')) %>%
  mutate(
    ice_on_insitu = ymd(ice_on_insitu),
    ice_off_insitu = ymd(ice_off_insitu),
    lakename = tolower(lakename)
  )

#Import cleaned and merged remotely sensed data from script 06

#all_remote_ice_data <- read_csv(here("data/remote/aqua_clean.csv")) %>%
#all_remote_ice_data <- read_csv(here("data/remote/terra_clean.csv")) %>% select(-X1, full_merge = ice_modis.MODISTerra) %>%
all_remote_ice_data <- read_csv(here("data/remote/aqua_terra_merged_clean_outlier_removed.csv")) %>%
  mutate(
    lakename = tolower(lakename),
    date = ymd(date)
  )

# 2. Create columns that give the uncertainty-----------------------------------
# Uncertainty, here, is cloudy days where ice fraction is completely obscured.

ice_data_uncertainty <- all_remote_ice_data %>%
  na.omit() %>%
  group_by(lakename) %>%
  arrange(lakename, date) %>%
  mutate(
    #include days since and until next image to offer some metric of uncertainty
    days_since_last_image = date - lag(date),
    days_until_next_image = lead(date) - date,
    water_year = ifelse(month(date)>=10, year(date)+1, year(date)),
    season = 
      ifelse(month(date) >= 12 | month(date) < 3, 'winter', 
       ifelse(month(date) >= 3 & month(date) < 6, 'spring',
         ifelse(month(date) >= 6 & month(date) < 9, 'summer', 'fall'))),
    #attempt to flag false negatives (i.e. topographic shadow or black ice)
    #attempt to flag false positives (i.e. missed cloud pixels)
    # false_negative = ifelse(full_merge <= 0.2 & lag(full_merge) >= 0.2 & lead(full_merge) >= 0.2, TRUE, FALSE),
    # false_positive = ifelse(full_merge >= 0.8 & lag(full_merge) <= 0.2 & lead(full_merge) <=0.2, TRUE, FALSE),
    false_negative = ifelse(full_merge <= 0.2 & lag(full_merge) > 0.2 & lead(full_merge) > 0.2, TRUE, FALSE),
    false_positive = ifelse(full_merge >= 0.8 & lag(full_merge) <= 0.2 & lead(full_merge) <=0.2, TRUE, FALSE),
    # Mark observations that do not have an image within 5, 10, and 15 days
    # filter_5day = ifelse(days_since_last_image > 5 & days_until_next_image > 5, TRUE, FALSE),
    # filter_10day = ifelse(days_since_last_image > 10 & days_until_next_image > 10, TRUE, FALSE),
    # filter_15day = ifelse(days_since_last_image > 15 & days_until_next_image > 15, TRUE, FALSE)
    # filter_5day = ifelse(days_since_last_image > 5, TRUE, FALSE),
    # filter_10day = ifelse(days_since_last_image > 10, TRUE, FALSE),
    # filter_15day = ifelse(days_since_last_image > 15, TRUE, FALSE)
  ) %>%
  #filter(false_negative == FALSE) %>% 
  #filter(false_positive == FALSE) %>%
  ungroup

ice_off_insitu <- all_insitu_w_water_year %>%
  select(lakename, ice_off_insitu, ice_off_water_year)

ice_on_insitu <- all_insitu_w_water_year %>%
  select(lakename, ice_on_insitu, ice_on_water_year)

ice_data_uncertainty_2 <- ice_data_uncertainty %>%
  full_join(ice_on_insitu, by = c('lakename', 'water_year' = 'ice_on_water_year')) %>%
  full_join(ice_off_insitu, by = c('lakename', 'water_year' = 'ice_off_water_year')) %>%
  group_by(water_year, lakename) %>%
  mutate(
    diff_on = date - ice_on_insitu,
    diff_off = date - ice_off_insitu
  ) %>%
  group_by(lakename, water_year) %>%
  mutate(
    ice_on_5day_check = any(between(diff_on, -5, 5)),
    ice_on_10day_check = any(between(diff_on, -10, 10)),
    ice_on_15day_check = any(between(diff_on, -15, 15)),
    ice_off_5day_check = any(between(diff_off, -5, 5)),
    ice_off_10day_check = any(between(diff_off, -10, 10)),
    ice_off_15day_check = any(between(diff_off, -15, 15))
  )


# 3. Detect ice phenology from median values -----------------------------------

# 2a. Create a dataframe with no missing dates and rolling median windows------- 

# all_remote_w_median <- all_remote_ice_data %>%
#   na.omit() %>%
#   #filter(lakename != "morskie_oko") %>%
#   mutate(date = ymd(date)) %>% #ensure that date is in date format
#   select(lakename, date, full_merge) %>% #reorder columns
#   group_by(lakename) %>%
#   pad() %>% #insert missing observations by day
#   mutate(full_merge_fill = na.approx(full_merge)) %>% #this fills NAs with a linear interpolation (from zoo package)
#   mutate(frac_03day_med = zoo::rollmedian(full_merge_fill, k = 3, fill = NA), #add rolling medians from 3-31 days
#          frac_05day_med = zoo::rollmedian(full_merge_fill, k = 5, fill = NA),
#          frac_07day_med = zoo::rollmedian(full_merge_fill, k = 7, fill = NA),
#          frac_09day_med = zoo::rollmedian(full_merge_fill, k = 9, fill = NA),
#          frac_11day_med = zoo::rollmedian(full_merge_fill, k = 11, fill = NA),
#          frac_13day_med = zoo::rollmedian(full_merge_fill, k = 13, fill = NA),
#          frac_15day_med = zoo::rollmedian(full_merge_fill, k = 15, fill = NA),
#          frac_17day_med = zoo::rollmedian(full_merge_fill, k = 17, fill = NA),
#          frac_19day_med = zoo::rollmedian(full_merge_fill, k = 19, fill = NA),
#          frac_21day_med = zoo::rollmedian(full_merge_fill, k = 21, fill = NA),
#          frac_23day_med = zoo::rollmedian(full_merge_fill, k = 23, fill = NA),
#          frac_25day_med = zoo::rollmedian(full_merge_fill, k = 25, fill = NA),
#          frac_27day_med = zoo::rollmedian(full_merge_fill, k = 27, fill = NA),
#          frac_29day_med = zoo::rollmedian(full_merge_fill, k = 29, fill = NA),
#          frac_31day_med = zoo::rollmedian(full_merge_fill, k = 31, fill = NA)) %>%
#   pivot_longer(!c(lakename, date), names_to = "median_val", values_to = "median_iceFrac") %>% #change from wide to long format
#   arrange(median_val) %>% #reorganize rows to group by rolling median from 3-31 rather than date
#   mutate(year = year(date), #add year column
#          month = month(date), #add month column to make a water year column
#          water_year = ifelse(month>=10, year+1, year)) %>% #add a water year column
#   select(-month) %>% #remove month column (no need after making water year column)
#   ungroup
# 

# 2b. Detect ice phenology from median values -------------------------------

#first create a dataframe with ice on values

remote_iceOn <- ice_data_uncertainty_2 %>%
#remote_iceOn <- all_remote_w_median %>%
  #filter(median_val == "full_merge") %>%
  #filter(median_val == "full_merge" ) %>% #for test of Inf values #& lakename == 'albion'
  #remove median_val as grouping variable when above filter is in use
  group_by(lakename, water_year) %>% #, medial_val
  filter(date > '2000-09-30') %>%
  #mutate(median_iceFrac = rollapply(median_iceFrac, width = 19, min, align = "left", fill = NA, na.rm = TRUE)) %>% #21  #after looking at the MAE, MDAE, and RMSE through Metrics pkg, 19 days for width performs the best with MAE = 22, MDAE = 14, RMSE = 29.2
  #slice_max(full_merge >= 0.8, with_ties = FALSE) %>%
  filter(full_merge >= 0.2) %>% #when using slice_min() (below) uncomment this line
  slice_min(date) %>% #instead of slice_max() above?
  rename(date_ice_on = date) %>% #remove when not going through all columns
  mutate(ice_on_w_uncertainty = date_ice_on - days_since_last_image/2) %>%
  #forgot to add uncertainty range
  ungroup() #%>%
#select(lakename, date_ice_on = date, water_year, median_val) #year, 


look <- all_remote_ice_data %>%
  filter(lakename == 'albion')



#Pruned remote_iceOn dataset for right_join() with remote_iceOff

remote_ice_on_trim <- remote_iceOn %>%
  #filter(median_val == 'full_merge') %>%
  select(lakename, date_ice_on, water_year) #median_val, 


#Second, create a dataframe with ice off values

#remote_iceOff <- all_remote_w_median %>%
remote_iceOff <- ice_data_uncertainty_2 %>%
  left_join(., y = remote_ice_on_trim) %>%
  group_by(lakename, water_year) %>%
  filter(date > "2000-09-30") %>% #remtve this filter
  #found this rollapply() solution here: https://stackoverflow.com/questions/31373256/r-selecting-first-of-n-consecutive-rows-above-a-certain-threshold-value
  #mutate(median_iceFrac = rollapply(median_iceFrac, width = 2, max, align = "left", fill = NA, na.rm = TRUE)) %>% #28 
  #filter(median_iceFrac <= 0.2 & date > date_ice_on) %>% 
  #filter(row_number() == 1) %>%
  filter(full_merge < 0.2 & date > date_ice_on) %>%
  slice_min(date) %>%
  rename(date_ice_off = date) %>%
  mutate(ice_duration = date_ice_off - date_ice_on) %>%
  mutate(ice_off_w_uncertainty = date_ice_off - days_since_last_image/2) %>%
  ungroup



#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

#######################################################
#Create new dataframes for ice on and ice off that can offer a performance stat (MAD)


####
#Add in situ data to remote ice phenology

#ALREADY PAIRED IN ice_data_uncertainty_2, so I don't need this step in this script
# #ice on dates
# remote_insitu_merge_iceOn_dates <- remote_iceOn %>%
#   rename(ice_on_water_year = water_year) %>%
#   inner_join(all_insitu_w_water_year, by = c("lakename", "ice_on_water_year")) %>% #inner_join() works better than right_join(), as it does not include erroneous NA values, such as the Castle ice on dates, since we do not have Castle ice on dates.
#   select(-ice_off_insitu, -ice_off_insitu_yday, -ice_off_water_year) %>%
#   arrange(lakename, ice_on_water_year) 
# 
# 
# #ice off dates
# remote_insitu_merge_iceOff_dates <- remote_iceOff %>%
#   rename(ice_off_water_year = water_year) %>%
#   inner_join(all_insitu_w_water_year, by = c("lakename", "ice_off_water_year")) %>%
#   select(-ice_on_insitu, -ice_on_insitu_yday, -ice_on_water_year) %>%
#   arrange(lakename, ice_off_water_year) 


#Write the csvs of ice on and ice off

#ice on dates
write_csv(remote_iceOn, here("data/combined/remote_insitu_iceOn_dates_update_merge_2022.04.01.csv"))


#ice off dates
write_csv(remote_iceOff, here("data/combined/remote_insitu_iceOff_dates_update_merge_2022.04.01.csv"))


