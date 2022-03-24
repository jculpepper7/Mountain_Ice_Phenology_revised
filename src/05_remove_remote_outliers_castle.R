## The goal of this script is to remove outliers from the 
## raw MODIS (Aqua/Terra merged) dataset from Google Earth Engine (GEE)
##
## Steps:
## 1. Import data
## 2. Plot raw data
## 3. Use simple algorithm to remove "spikes" from the dataset (due to cloud or topographic shadow)
## 3a. Use differencing to detect when data point is non-zero from previous data point
## 3b. For raw GEE data point of each non-zero differenced value, check if previous day is equal to next day
## 3c. If previous and next day are not equal, ignore
## 3d. If previous and next day are equal, revert current day to previous day
## 4. Plot before and after data together

# Import libraries--------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(plotly)
library(zoo)
library(here)
library(janitor)

# 1. Import data----------------------------------------------------------------

# import raw remote data from GEE

castle_remote <- read_csv(here('data/castle/castle_terra_aqua_merged.csv'))

# 2. Plot raw data-----------------------------------------------------------

remote_raw_plt <- 
  ggplot(data = castle_remote, mapping = aes(x = date, y = iceModis)) + 
  geom_line(size = 2, aes(color = "Raw")) +
  xlab("Date") +
  ylab("Ice Fraction (%)") +
  ggtitle("Raw Ice Fraction") +
  theme_classic() +
  theme(text = element_text(size = 35),
        plot.title = element_text(hjust = 0.5)) +
  labs(colour = "")

# 3. Algorithm to remove outliers-----------------------------------------------

# 3a - d. Eliminate outliers----------------------------------------------------
castle_remote <- castle_remote %>%
  clean_names() %>% # clean column names
  select(date, ice_modis) %>% #choose relevant columns and reorder
  mutate(diff_ice_modis = c(NA,diff(ice_modis)),
         diff_nonzero = ifelse(diff_ice_modis!=0, TRUE, FALSE),
         ice_modis_new = ifelse(diff_nonzero == TRUE & dplyr::lag(ice_modis)==dplyr::lead(ice_modis), lag(ice_modis), ice_modis))
         
  
# 4. Plot cleaned ice fraction--------------------------------------------------

remote_cleaned_plt <- 
  ggplot(data = castle_remote, mapping = aes(x = date, y = ice_modis_new)) + 
  geom_line(size = 2, aes(color = "Raw")) +
  xlab("Date") +
  ylab("Ice Fraction (%)") +
  ggtitle("Cleaned Ice Fraction") +
  theme_classic() +
  theme(text = element_text(size = 35),
        plot.title = element_text(hjust = 0.5)) +
  labs(colour = "")

#ggplotly(remote_cleaned_plt)

#take a look at the raw and cleaned datasets using 'patchwork' package

compare_plt <- remote_raw_plt / remote_cleaned_plt

ggplotly(compare_plt)

# 5. Create new CSV cleaned file in the data folder-----------------------------

#select only relevant columns
castle_remote_cleaned <- castle_remote %>%
  select(date, ice_modis_new)

#write csv file to folder
write_csv(castle_remote_cleaned, here("data/remote/castle_remote_cleaned.csv"))

