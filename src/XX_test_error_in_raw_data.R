## The goal of this script is to test the error in the raw, unaltered data
## from Google Earth Engine
## I want to determine if the MODIS ice fraction from Aqua and Terra do or 
## do not agree with the in situ days.
## I'll check when the days overlap (between MODIS and in situ) and then
## I'll test whether the MODIS observation agrees with the in situ data
## if not, there may be an issue with the GEE data acquisition

# Add libraries
library(tidyverse)
library(lubridate)
library(plotly)

# 1. import data----------------------------------------------------------------

# 1a. Import GEE data-----------------------------------------------------------

all_remote <- read_csv(here('data/combined/modis_allLakes_output_Terra_Aqua_merged_raw.csv')) %>%
  filter(lakename %in% c("Albion", "Castle", "Lunz", "Morskie_Oko", "Muressan", "Silver"))
all_nor_lakes <- read_csv(here('data/combined/modis_norwegian_lakes.csv')) 

all_remote <- bind_rows(all_remote, all_nor_lakes)

# 1b. Import in situ data-------------------------------------------------------

all_insitu_w_water_year <- read_csv(here('data/combined/all_insitu_water_year.csv')) %>%
  mutate(
    ice_on_insitu = ymd(ice_on_insitu),
    ice_off_insitu = ymd(ice_off_insitu),
    lakename = tolower(lakename)
  )

# 2. Separate Aqua and Terra ice fractions--------------------------------------

# 2a. Use pivot_wider() to separate long-form data------------------------------
separate_aqua_terra <- all_remote %>%
  clean_names() %>%
  mutate(
    lakename = tolower(lakename)
  ) %>%
  filter(cloud_modis < 0.1) %>%
  #pivot wider and give h, cloud, and ice variables their own columns with pivot wider
  pivot_wider(names_from = source, names_sep = ".", values_from = c(ice_modis, cloud_modis, h))

# 2b. Create a TERRA dataset----------------------------------------------------

# Total number of observations (with NA omitted = 20398)

terra <- separate_aqua_terra %>%
  select(date, 
         lakename, 
         terra_ice_fraction = ice_modis.MODISTerra, 
         terra_cloud = cloud_modis.MODISTerra,
         terra_threshold = h.MODISTerra
         ) %>%
  na.omit

# 2c. Create an AQUA dataset----------------------------------------------------

# Total number of observations (with NA omitted = 17479)

aqua <- separate_aqua_terra %>%
  select(date,
         lakename,
         aqua_ice_fraction = ice_modis.MODISAqua,
         aqua_cloud = cloud_modis.MODISAqua,
         aqua_threshold = h.MODISAqua
        ) %>%
  na.omit

# 3. Merge Aqua and Terra datasets (separately) with in situ data---------------

# 3a. Limit in situ data to lakename, water_year, ice_on_insitu-----------------

# Create an ice on dataset
ice_on <- all_insitu_w_water_year %>%
  select(lakename, ice_on_insitu, ice_on_water_year)

# Create an ice off dataset
ice_off <- all_insitu_w_water_year %>%
  select(lakename, ice_off_insitu, ice_off_water_year)

# 3b. Add ice_on_water_year to the remote data and merge in situ----------------

# Terra merge with in situ

terra_on <- terra %>%
  mutate(
    ice_on_water_year = ifelse(year(date)>=10, year(date)+1, year(date))
  ) %>%
  inner_join(ice_on, by = c('lakename', 'ice_on_water_year'))

terra_off <- terra %>%
  mutate(
    ice_off_water_year = ifelse(year(date)>=10, year(date)+1, year(date))
  ) %>%
  inner_join(ice_off, by = c('lakename', 'ice_off_water_year'))

# Aqua merge with in situ

aqua_on <- aqua %>%
  mutate(
    ice_on_water_year = ifelse(year(date)>=10, year(date)+1, year(date))
  ) %>%
  inner_join(ice_on, by = c('lakename', 'ice_on_water_year'))

aqua_off <- aqua %>%
  mutate(
    ice_off_water_year = ifelse(year(date)>=10, year(date)+1, year(date))
  ) %>%
  inner_join(ice_off, by = c('lakename', 'ice_off_water_year'))

# 4. Check where there are images on the same day-------------------------------

terra_on_insitu_test <- terra_on %>%
  filter(date == ice_on_insitu)

terra_off_insitu_test <- terra_off %>%
  filter(date == ice_off_insitu)

aqua_on_insitu_test <- aqua_on %>%
  filter(date == ice_on_insitu)

aqua_off_insitu_test <- aqua_off %>%
  filter(date == ice_off_insitu)














