## The goal of this script is to: 
##
## 1. Import MODIS Aqua and Terra data
## 2. Merge Aqua and Terra
## 3. Create cleaned csv file for further analysis 

# Import libraries--------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(plotly)
library(zoo)
library(here)
library(janitor)
library(padr)

# 1. Import data----------------------------------------------------------------

# import raw remote data from GEE

all_remote <- read_csv(here('data/combined/modis_allLakes_output_Terra_aqua_merged_raw.csv'))

# 2. Merge Aqua and Terra data--------------------------------------------------

# 2a. Separate Aqua and Terra data for merge------------------------------------

separate_aqua_terra <- all_remote %>%
  #improves column names from GEE output
  clean_names() %>%
  #Focusing on these lakes for now
  filter(lakename %in% c("Albion", "Castle", "Lunz", "Morskie_Oko", "Silver")) %>%
  filter(cloud_modis < 0.2) %>%
  #pivot wider and give h, cloud, and ice variables their own columns with pivot wider
  pivot_wider(names_from = source, names_sep = ".", values_from = c(ice_modis, cloud_modis, h)) 

# 2b. Prep Aqua and Terra for merge into a single column------------------------

aqua_terra_initial <- separate_aqua_terra %>%
  arrange(lakename, date) %>%
  #Add a difference column to detect sensor overlap (both sensors off an image on the same day)
  mutate(modis_diff_no_overlap = ice_modis.MODISTerra - ice_modis.MODISAqua) %>%
  #Filter to have only observations with NA in difference column, which will 
  #give a dataframe with only observations that don't overlap between Aqua and Terra
  filter(is.na(modis_diff_no_overlap)) %>%
  #Create a new column for merged Aqua and Terr observations (for non-overlapping observations)
  mutate(merged_remote_no_overlap = ifelse(is.na(ice_modis.MODISTerra), ice_modis.MODISAqua, ice_modis.MODISTerra))

# 2c. Amend overlapping data to merge with dataframe from step 2b---------------

#Evaluate Aqua data without NA values
check_aqua <- separate_aqua_terra %>%
  #select(-MODISTerra, -h) %>%
  select(-ice_modis.MODISTerra, -h.MODISTerra, -cloud_modis.MODISTerra) %>%
  na.omit() %>%
  group_by(lakename) %>%
  mutate(diff_ice_modis_aqua = c(ice_modis.MODISAqua[1], diff(ice_modis.MODISAqua)),
         diff_nonzero = ifelse(diff_ice_modis_aqua !=0, TRUE, FALSE),
         ice_modis_aqua_new = ifelse(diff_nonzero == TRUE & dplyr::lag(ice_modis.MODISAqua)==dplyr::lead(ice_modis.MODISAqua), lag(ice_modis.MODISAqua), ice_modis.MODISAqua)) %>%
  ungroup

double_check_aqua <- check_aqua %>%
  mutate(diff_dbl_chk_aqua = ice_modis_aqua_new - ice_modis.MODISAqua) %>%
  mutate(which_dates_diff_aqua = ifelse(diff_dbl_chk_aqua != 0, FALSE, TRUE)) %>%
  filter(which_dates_diff_aqua == FALSE)

#Evaluate Terra data without NA values
check_terra <- separate_aqua_terra %>%
  #select(-MODISTerra, -h) %>%
  select(-ice_modis.MODISAqua, -h.MODISAqua, -cloud_modis.MODISAqua) %>%
  na.omit() %>%
  group_by(lakename) %>%
  mutate(diff_ice_modis_terra = c(ice_modis.MODISTerra[1], diff(ice_modis.MODISTerra)),
         diff_nonzero = ifelse(diff_ice_modis_terra !=0, TRUE, FALSE),
         ice_modis_terra_new = ifelse(diff_nonzero == TRUE & dplyr::lag(ice_modis.MODISTerra)==dplyr::lead(ice_modis.MODISTerra), lag(ice_modis.MODISTerra), ice_modis.MODISTerra)) %>%
  ungroup

double_check_terra <- check_terra %>%
  mutate(diff_dbl_chk_terra = ice_modis_terra_new - ice_modis.MODISTerra) %>%
  mutate(which_dates_diff_terra = ifelse(diff_dbl_chk_terra != 0, FALSE, TRUE)) %>%
  filter(which_dates_diff_terra == FALSE)

#Merge Aqua and Terra data without NA values to detect overlapping data 
#(i.e reflectance values on the same day from the different sensors)
check_merge <- full_join(check_aqua, check_terra) %>%
  arrange(lakename, date) %>%
  #eliminate NA values
  na.omit() %>%
  #The diff of the sensor columns will tell us where the sensors disagree.
  mutate(modis_diff = ice_modis_aqua_new - ice_modis_terra_new) 

#Look at the basic stats to check for discrepancies when the two sensors have images on the same day
mean(check_merge$modis_diff) #0.002472321
max(check_merge$modis_diff) #1
median(check_merge$modis_diff) #0
min(check_merge$modis_diff) #-1
# These values indicate that generally, the sensors agree, but at times are opposed. 
# Proceed by eliminating large opposing values (>0.5 or <-0.5) and taking average values between smaller opposing values

#Get a dataframe with only equivalent sensor data
non_opposing_values <- check_merge %>%
  filter(modis_diff == 0) %>%
  #select(date, lakename, cloud_modis, modis_diff)
  select(date, lakename, ice_modis.MODISAqua, ice_modis.MODISTerra, modis_diff)

#Get a dataframe with only senor data that disagree, eliminate large disagreements, 
#and take the average of the remaining disagreements
opposing_values <- check_merge %>%
  filter(modis_diff != 0) %>% #eliminate non-opposing values #421 total observations
  filter(modis_diff < 0.5 & modis_diff > -0.5) %>% #eliminate values too
  mutate(modis_mean = (ice_modis.MODISAqua + ice_modis.MODISTerra)/2) %>%
  select(date, lakename, modis_mean)

#Combine the non_opposing and amended opposing data
merge_aqua_terra_small <- full_join(non_opposing_values, opposing_values) %>% 
  arrange(lakename, date) %>%
  mutate(merge_small = ifelse(is.na(modis_mean), ice_modis.MODISTerra, modis_mean)) %>%
  select(date, lakename, merge_small)
#calling this "merge_aqua_terra_small" b/c I will add this to the full data set 
#for a merge of all usable data

# 2d. Merge non-overlapping and overlapping data from 2b & 2c-------------------

modis_full_merge <- full_join(aqua_terra_initial, merge_aqua_terra_small) %>%
  arrange(lakename, date) %>%
  mutate(full_merge = ifelse(is.na(merge_small), merged_remote_no_overlap, merge_small)) %>%
  select(date, lakename, full_merge)
#select(date, lakename, cloud_modis.MODISTerra, cloud_modis.MODISAqua, full_merge)

#####
#just exploratory. will delete.
#quick glance at the data
modis_full_merge %>%
  na.omit() %>%
  ggplot(mapping = aes(x = date, y = full_merge))+
  geom_line(size = 1.5)+
  theme_classic()+
  facet_wrap(~lakename)

# 3. Create cleaned, merged csv for further analysis----------------------------

write_csv(modis_full_merge, here("data/remote/aqua_terra_merged_clean.csv"))



