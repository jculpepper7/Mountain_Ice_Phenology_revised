## The goal of this script is to:
##
## 1. Make violin plots of Sen's slope across lat and elev gradients

# 1. Load libraries-------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)

# 2. Import data----------------------------------------------------------------

# MK test is already loaded, but I should probably write it as a file
mk_results <- read_csv(here('data/n_american_ice_phenology/mk_results.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id)
  )

# Take a look

str(mk_results) #looks good

# 3. Make elevation groups and latitude groups as factors----------------------- 

# Create new columns with elevaiton and latitude ranges as factors
# In this case, low, medium and high, but others may be useful

mk_results_groupings <- mk_results %>% 
  mutate(
    elevation_grouping = as.factor(ifelse(between(elevation, 1500, 2000), 'low', 
                                ifelse(between(elevation, 2001, 2500), 'medium', 'high'))),
    latitude_grouping = as.factor(ifelse(between(pour_lat, 30, 45), 'low', 
                                ifelse(between(pour_lat, 46, 55), 'medium', 'high')))
  )

# Take a look

str(mk_results_groupings)

# 4. Violin plots (or something)------------------------------------------------

# Plots for elevation groups

mk_results_groupings %>% 
  filter(event == 'ice_on') %>% 
  ggplot() +
  theme_bw()+
  geom_violin(aes(elevation_grouping, sen, fill = elevation_grouping), draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_jitter(aes(elevation_grouping, sen), height = 0, width = 0.2, color = "black", alpha = 0.3)+
  ggtitle('Ice On - Sen Slope by Elevation') +
  scale_x_discrete(limits = c('low', 'medium', 'high'))
  
mk_results_groupings %>% 
  filter(event == 'ice_off') %>% 
  ggplot() +
  theme_bw()+
  geom_violin(aes(elevation_grouping, sen, fill = elevation_grouping), draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_jitter(aes(elevation_grouping, sen), height = 0, width = 0.2, color = "black", alpha = 0.3) +
  ggtitle('Ice Off - Sen Slope by Elevation') +
  scale_x_discrete(limits = c('low', 'medium', 'high'))

#plots for latitude groups

mk_results_groupings %>% 
  filter(event == 'ice_on') %>% 
  ggplot() +
  theme_bw()+
  geom_violin(aes(latitude_grouping, sen, fill = latitude_grouping), draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_jitter(aes(elevation_grouping, sen), height = 0, width = 0.2, color = "black", alpha = 0.3)+
  ggtitle('Ice On - Sen Slope by Latitude') +
  scale_x_discrete(limits = c('low', 'medium', 'high'))

mk_results_groupings %>% 
  filter(event == 'ice_off') %>% 
  ggplot() +
  theme_bw()+
  geom_violin(aes(latitude_grouping, sen, fill = latitude_grouping), draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_jitter(aes(elevation_grouping, sen), height = 0, width = 0.2, color = "black", alpha = 0.3) +
  ggtitle('Ice Off - Sen Slope by Latitude') +
  scale_x_discrete(limits = c('low', 'medium', 'high'))


  
  
  
  
  
  
