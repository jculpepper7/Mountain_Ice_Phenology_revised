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

all_remote_ice_data <- read_csv(here("data/remote/aqua_terra_merged_clean_outlier_removed.csv")) %>%
  mutate(
    lakename = tolower(lakename)
  )


# 2. Detect ice phenology from median values -----------------------------------

# 2a. Create a dataframe with no missing dates and rolling median windows------- 

all_remote_w_median <- all_remote_ice_data %>%
  na.omit() %>%
  #filter(lakename != "morskie_oko") %>%
  mutate(date = ymd(date)) %>% #ensure that date is in date format
  select(lakename, date, full_merge) %>% #reorder columns
  group_by(lakename) %>%
  #pad() %>% #insert missing observations by day
  mutate(full_merge_fill = na.approx(full_merge),
         cubic_spline = smooth.spline(full_merge)) %>% #this fills NAs with a linear interpolation (from zoo package)
  mutate(frac_03day_med = zoo::rollmedian(full_merge_fill, k = 3, fill = NA), #add rolling medians from 3-31 days
         frac_05day_med = zoo::rollmedian(full_merge_fill, k = 5, fill = NA),
         frac_07day_med = zoo::rollmedian(full_merge_fill, k = 7, fill = NA),
         frac_09day_med = zoo::rollmedian(full_merge_fill, k = 9, fill = NA),
         frac_11day_med = zoo::rollmedian(full_merge_fill, k = 11, fill = NA),
         frac_13day_med = zoo::rollmedian(full_merge_fill, k = 13, fill = NA),
         frac_15day_med = zoo::rollmedian(full_merge_fill, k = 15, fill = NA),
         frac_17day_med = zoo::rollmedian(full_merge_fill, k = 17, fill = NA),
         frac_19day_med = zoo::rollmedian(full_merge_fill, k = 19, fill = NA),
         frac_21day_med = zoo::rollmedian(full_merge_fill, k = 21, fill = NA),
         frac_23day_med = zoo::rollmedian(full_merge_fill, k = 23, fill = NA),
         frac_25day_med = zoo::rollmedian(full_merge_fill, k = 25, fill = NA),
         frac_27day_med = zoo::rollmedian(full_merge_fill, k = 27, fill = NA),
         frac_29day_med = zoo::rollmedian(full_merge_fill, k = 29, fill = NA),
         frac_31day_med = zoo::rollmedian(full_merge_fill, k = 31, fill = NA)) %>%
  pivot_longer(!c(lakename, date), names_to = "median_val", values_to = "median_iceFrac") %>% #change from wide to long format
  arrange(median_val) %>% #reorganize rows to group by rolling median from 3-31 rather than date
  mutate(year = year(date), #add year column
         month = month(date), #add month column to make a water year column
         water_year = ifelse(month>=10, year+1, year)) %>% #add a water year column
  select(-month) %>% #remove month column (no need after making water year column)
  ungroup


# 2b. Detect ice phenology from median values -------------------------------

#first create a dataframe with ice on values

remote_iceOn <- all_remote_w_median %>%
  #filter(median_val == "full_merge") %>%
  filter(median_val == "full_merge" ) %>% #for test of Inf values #& lakename == 'albion'
  #remove median_val as grouping variable when above filter is in use
  group_by(lakename, water_year) %>% #, medial_val
  filter(date > '2000-09-30') %>%
  #filter(median_iceFrac >= 0.8) %>%
  #mutate(median_iceFrac = rollapply(median_iceFrac, width = 19, min, align = "left", fill = NA, na.rm = TRUE)) %>% #21  #after looking at the MAE, MDAE, and RMSE through Metrics pkg, 19 days for width performs the best with MAE = 22, MDAE = 14, RMSE = 29.2
  #filter(median_iceFrac >= 0.8) %>% #when using slice_min() (below) uncomment this line
  slice_max(median_iceFrac >= 0.8, with_ties = FALSE) %>%
  #slice_min(date) %>% #instead of slice_max() above?
  #filter(row_number() == 1) %>%
  rename(date_ice_on = date) %>% #remove when not going through all columns
  ungroup() #%>%
#select(lakename, date_ice_on = date, water_year, median_val) #year, 


look <- all_remote_w_median %>%
  filter(lakename == 'albion' & median_val == 'full_merge')



#Pruned remote_iceOn dataset for right_join() with remote_iceOff

remote_ice_on_trim <- remote_iceOn %>%
  filter(median_val == 'full_merge') %>%
  select(lakename, date_ice_on, water_year) #median_val, 


#Second, create a dataframe with ice off values

remote_iceOff <- all_remote_w_median %>%
  select(-year) %>%
  #left_join(., y= remote_iceOn) %>%
  left_join(., y = remote_ice_on_trim) %>%
  group_by(lakename, water_year, median_val) %>% #, median_val
  #filter(median_val == "frac_31day_med") %>%
  #filter(date > date_ice_on) %>%
  filter(date > "2000-09-30") %>%
  #found this rollapply() solution here: https://stackoverflow.com/questions/31373256/r-selecting-first-of-n-consecutive-rows-above-a-certain-threshold-value
  mutate(median_iceFrac = rollapply(median_iceFrac, width = 2, max, align = "left", fill = NA, na.rm = TRUE)) %>% #28 
  filter(median_iceFrac <= 0.2 & date > date_ice_on) %>% 
  # slice(n()) %>%
  filter(row_number() == 1) %>%
  rename(date_ice_off = date) %>%
  mutate(ice_duration = date_ice_off - date_ice_on) %>%
  select(lakename, ice_off_water_year = water_year, date_ice_on, date_ice_off, ice_duration) %>%
  #select(lakename, water_year, date_ice_off) %>%
  # mutate(
  #   #ice_on_water_year = ifelse(month(date_ice_on) >= 10, year(date_ice_on)+1, year(date_ice_on)),
  #   ice_off_water_year = ifelse(month(date_ice_off) >= 10, year(date_ice_off)+1, year(date_ice_off))
  #   ) %>%
  # rename(ice_off_water_year = water_year) %>%
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


#ice on dates
remote_insitu_merge_iceOn_dates <- remote_iceOn %>%
  select(-year) %>%
  rename(ice_on_water_year = water_year) %>%
  #select(-median_iceFrac) %>% #comment out when using one median_val
  pivot_wider(names_from = median_val, values_from = date_ice_on) %>% #comment out when using one median_val
  inner_join(all_insitu_w_water_year, by = c("lakename", "ice_on_water_year")) %>% #inner_join() works better than right_join(), as it does not include erroneous NA values, such as the Castle ice on dates, since we do not have Castle ice on dates.
  #right_join(all_insitu_w_water_year, by = c("lakename")) %>%
  select(-ice_off_insitu, -ice_off_insitu_yday, -ice_off_water_year) %>%
  arrange(lakename, ice_on_water_year) #%>%
#na.omit()
#select(-year.y, -year.x, -ice_off_water_year)
#select(-year, -ice_off_water_year)




#ice off dates
remote_insitu_merge_iceOff_dates <- remote_iceOff %>%
  #filter(ice_duration != 1) %>%
  select(-c(date_ice_on, ice_duration)) %>%
  #select(-median_iceFrac) %>% #eliminated this column in line 178 (remote_iceOff -> select() step)
  pivot_wider(names_from = median_val, values_from = date_ice_off) %>%
  inner_join(all_insitu_w_water_year, by = c("lakename", "ice_off_water_year")) %>%
  select(-ice_on_insitu, -ice_on_insitu_yday, -ice_on_water_year) %>%
  arrange(lakename, ice_off_water_year) #%>%
#select(-year.x, -year.y, -ice_on_water_year)
#select(-year, -ice_on_water_year)
#na.omit()


#Write the csvs of ice on and ice off

#ice on dates
write_csv(remote_insitu_merge_iceOn_dates, here("data/combined/remote_insitu_iceOn_dates_update_2022.03.23.csv"))
#testing for 100% for ice on and 0% for ice off (excluding morskie_oko)
#write_csv(remote_insitu_merge_iceOn_dates, here("data/combined/remote_insitu_iceOn_dates_no_oko.csv"))

#ice off dates
write_csv(remote_insitu_merge_iceOff_dates, here("data/combined/remote_insitu_iceOff_dates_update_2022.03.23.csv"))
#testing for 100% for ice on and 0% for ice off (excluding morskie_oko)
#write_csv(remote_insitu_merge_iceOff_dates, here("data/combined/remote_insitu_iceOff_dates_no_oko.csv"))

#Mean Absolute Difference (MAD)-------------------------------------------------

#get the MAD for each rolling median for each lake
#solution to the MAD found through StackOverflow: 
#https://stackoverflow.com/questions/69921659/apply-a-custom-function-across-certain-columns-in-a-dataframe-in-r/69922305#69922305

#ice on MAD
ice_on_med_test <- remote_insitu_merge_iceOn_dates %>%
  na.omit() %>%
  #group_by(lakename) %>% # f you do not group by lake name, it will get the total MAE
  summarise(across(
    #.cols = 3:20, #without full_merge_fill
    .cols = 3:4, #with full_merge_fill
    #.fns = ~ abs(mean(na.rm = TRUE, interval(.x, ice_on_insitu) %/% days(1))) #NOTE: 02.18.2022 This is not MAE its the absolute value of the mean difference
    .fns = ~ Metrics::mae(predicted = as.numeric(.x), actual = as.numeric(ice_on_insitu))
    #.fns = ~ Metrics::mdae(predicted = as.numeric(.x), actual = as.numeric(ice_on_insitu))
    #.fns = ~ Metrics::rmse(predicted = as.numeric(.x), actual = as.numeric(ice_on_insitu))
  ))
ice_on_med_test

#ice off MAD
ice_off_med_test <- remote_insitu_merge_iceOff_dates %>%
  na.omit() %>%
  #group_by(lakename) %>%
  #filter(lakename != "morskie_oko") %>%
  summarise(across(
    #.cols = 2:18, #without full_merge_fill
    .cols = 3:20, #with full_merge_fill
    #.cols = 2:5,
    #.fns = ~ abs(mean(na.rm = TRUE, interval(.x, ice_off_insitu) %/% days(1)))
    .fns = ~ Metrics::mae( predicted = .x, actual = ice_off_insitu)
  ))
ice_off_med_test
#get the mean values per column (across lakes)

#ice on
ice_on_med_mean_vals <- ice_on_med_test %>%
  select(-lakename, -ice_on_insitu) %>%
  colMeans(na.rm = TRUE) %>%
  data.frame()
ice_on_med_mean_vals
#ice off
ice_off_med_mean_vals <- ice_off_med_test %>%
  select(-lakename, -ice_off_insitu) %>%
  colMeans(na.rm = TRUE) %>%
  data.frame()
ice_off_med_mean_vals


