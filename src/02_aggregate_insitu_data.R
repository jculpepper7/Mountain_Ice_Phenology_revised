## The objective of this script is to:
## 1. Read in the raw data of in situ validated ice phenology. 
##    For information on the acquisition of ice phenology data, see script 00_get_ice_data.R
## 2. Make column headers uniform
## 3. Bind separate data frames into a single data frame for further analysis. 
## 4. Add water year for ice off and ice on
## 5. Write a new csv file with the aggregated data for further analysis in other scripts.


# Load packages --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(here)

# 1. Read in data ------------------------------------------------------

albion <- read_csv(here("data/albion/albion_in_situ.csv"))
castle <- read_csv(here("data/castle/castle_in_situ.csv"))
lunz <- read_csv(here("data/lunz/lunz_in_situ.csv"))
morskie_oko <- read_csv(here("data/morskie_oko/morskie_oko_in_situ.csv"))
silver <- read_csv(here("data/silver/silver_in_situ.csv"))

# 2. Make data frames uniform ------------------------------------------

# Clean Albion data 

albion_insitu <- albion %>%
  clean_names() %>%
  mutate(ice_on_insitu = mdy(ice_on), # Fix dates using lubridate
         ice_off_insitu = mdy(ice_off),
         ice_on_insitu_yday = yday(ice_on_insitu), #add the ordinal day for ice on and off
         ice_off_insitu_yday = yday(ice_off_insitu)) %>%
  select(-ice_on, -ice_off) #remove columns with incorrect formatting

#Take a look
head(albion_insitu)

# ----------------------------------------------------------------------

# Clean Castle data

castle_insitu <- castle %>%
  clean_names() %>%
  rename(ice_off_insitu_yday = ice_off) %>%
  mutate(ice_off_insitu = make_date(year) + ice_off_insitu_yday - 1, #add the actual date rather than just the day of year
         lake = c("castle")) %>%  #add the ordinal day for ice off
  select(-name) %>% #remove unnecessary column
  select(lake, year, ice_off_insitu, ice_off_insitu_yday) %>% #reorder columns to be uniform with other dataframes
  filter(year >= 2000) #filter out rows that cannot be used in analysis (MODIS only dates back to 2000)
  
#Take a look
head(castle_insitu)

# ----------------------------------------------------------------------

# Clean Lunz data

lunz_insitu <- lunz %>%
  clean_names() %>%
  mutate(lake = c("lunz"),
         ice_on_insitu = mdy(ice_on), # Fix dates using lubridate
         ice_off_insitu = mdy(ice_off),
         ice_on_insitu_yday = yday(ice_on_insitu), #add the ordinal day for ice on and off
         ice_off_insitu_yday = yday(ice_off_insitu)) %>%
  select(-ice_on, -ice_off, -days) %>% #remove columns with incorrect formatting
  select(lake, year, ice_on_insitu, ice_off_insitu, ice_on_insitu_yday, ice_off_insitu_yday) #reorder columns to be uniform with other dataframes

#Take a look
head(lunz_insitu)

# ----------------------------------------------------------------------

# Clean Morskie Oko data

morskie_oko_insitu <- morskie_oko %>%
  clean_names() %>%
  mutate(ice_on_insitu = mdy(ice_on), # Fix dates using lubridate
         ice_off_insitu = mdy(ice_off),
         ice_on_insitu_yday = yday(ice_on_insitu), #add the ordinal day for ice on and off
         ice_off_insitu_yday = yday(ice_off_insitu)) %>%
  select(-ice_on, -ice_off) #remove columns with incorrect formatting

#Take a look
head(morskie_oko_insitu)

# ----------------------------------------------------------------------

# Clean Silver data

silver_insitu <- silver %>%
  clean_names() %>%
  mutate(ice_on_insitu = mdy(ice_on), # Fix dates using lubridate
         ice_off_insitu = mdy(ice_off),
         ice_on_insitu_yday = yday(ice_on_insitu), #add the ordinal day for ice on and off
         ice_off_insitu_yday = yday(ice_off_insitu)) %>%
  select(-ice_on, -ice_off) #remove columns with incorrect formatting

#Take a look
head(silver_insitu)

# 3. Bind separate data frames ------------------------------------------

all_insitu_ice_data <- bind_rows(albion_insitu, castle_insitu, lunz_insitu, morskie_oko_insitu, silver_insitu)

# 4. Add water year to both the ice on and ice off dates

all_insitu_w_water_year <- all_insitu_ice_data %>%
  rename(lakename = lake) %>%
  mutate(ice_on_month = month(ice_on_insitu),
         ice_off_month = month(ice_off_insitu),
         ice_on_year = year(ice_on_insitu),
         ice_off_year = year(ice_off_insitu)) %>%
  mutate(ice_on_water_year = ifelse(ice_on_month >= 10, ice_on_year+1, ice_on_year),
         ice_off_water_year = ifelse(ice_off_month >= 10, ice_off_year+1, ice_off_year)) %>%
  select(-ice_on_month, -ice_off_month, -year, -ice_on_year, -ice_off_year)

# 5. Write CSV with new data --------------------------------------------

write_csv(all_insitu_ice_data, here("data/combined/all_insitu_ice_data.csv"))
write_csv(all_insitu_w_water_year, here("data/combined/all_insitu_water_year.csv"))
