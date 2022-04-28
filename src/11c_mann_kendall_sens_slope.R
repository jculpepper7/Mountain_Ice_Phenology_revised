## The goal of this script is to:
## Run Mann Kendall and Sen's Slope on North American lakes data

# Load relevant libraries-------------------------------------------------------

library(tidyverse)
library(here)
library(Kendall)
library(openair)
library(wql)
library(janitor)
library(viridis)

# Import necessary data---------------------------------------------------------

# Import North American ice phenology
n_american_lakes <- 
  list.files(
    path = here('data/n_american_ice_phenology'),
    pattern = "*.csv",
    full.names = TRUE
  ) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>% 
  na.omit()

# Take a look
str(n_american_lakes)

# Revise data structure
n_american_lakes <- n_american_lakes %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    water_year = as.numeric(water_year),
    sdoy_fit = as.numeric(sdoy_fit),
    event = as.factor(event)
  )

#Import HydroLAKES data
hydro_lakes <- read_csv(here('data/remote/sierra_lake_ice_data/HydroLAKES_N_America.csv'), guess_max = 5000) %>%
  clean_names() %>%
  select(Hylak_id = hylak_id, pour_lat, pour_long, elevation, lake_area) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id) #Convert to factor to match N. American data
  ) %>% 
  arrange(Hylak_id)
  
str(hydro_lakes)

# Join N. American data with hydrolakes

n_american_lakes_1 <- n_american_lakes %>% 
  inner_join(hydro_lakes)

str(n_american_lakes_1)

n_american_lakes_1 %>% count(Hylak_id) #3,084 total lakes

# 1. Mann-Kendall and Sen's Slope on N. American Ice Phenology------------------

mk_sen_test <- n_american_lakes_1 %>%
  na.omit() %>%
  group_by(Hylak_id, event) %>%
  filter(elevation >= 1500,
         n() >= 20) %>% #filtering out any lakes with fewer than 20 rows of data
  summarize(sen = mannKen(sdoy_fit)$sen.slope,             
            rel_slope = mannKen(sdoy_fit)$sen.slope.rel,   
            p.value = mannKen(sdoy_fit)$p.value) %>%
  ungroup

#how many lakeswith 20 years of data?
mk_sen_test %>% count(Hylak_id) #filtering out any lakes with fewer than 20 rows of data leaves 2,410 lakes greater than 1500m in elevation 

# 2. ICE ON summary stats-------------------------------------------------------

mk_sen_test_pos_ice_on <- mk_sen_test %>%
  filter(event == 'ice_on',
         sen > 0) %>%  
  summarise(max = max(sen),
            min = min(sen),
            median = median(sen),                         #Total number of lakes that have 20 years of ice on = 2384
            percent_pos = n()/2384,                       #41% with positive slope (later ice on)
            mean_ice_on_trend = mean(sen),                #mean slope = 0.42 days per year
            mean_ice_on_relative_slope = mean(rel_slope)) #mean rel_slope = 0.0049

mk_sen_test_neg_ice_on <- mk_sen_test %>%
  filter(event == 'ice_on',
         sen < 0) %>%  
  summarise(max = max(sen),
            min = min(sen),
            median = median(sen),
            percent_neg = n()/2384,                       #48% with negative slope (earlier ice on)
            mean_ice_on_trend = mean(sen),                #mean slope = -0.59 days per year
            mean_ice_on_relative_slope = mean(rel_slope)) #mean rel_slope = -0.0058

# 2. ICE OFF summary stats------------------------------------------------------

mk_sen_test_pos_ice_off <- mk_sen_test %>%
  filter(event == 'ice_off',
         sen > 0) %>%  
  summarise(max = max(sen),
            min = min(sen),
            median = median(sen),                          #Total number of lakes that have 20 years of ice off = 1682
            percent_pos = n()/1682,                        #35% with positive slope (later ice off)
            mean_ice_off_trend = mean(sen),                #mean slope = 0.3 days per year
            mean_ice_off_relative_slope = mean(rel_slope)) #mean rel_slope = 0.00106, max = 1.47, min = 0.0278

mk_sen_test_neg_ice_off <- mk_sen_test %>%
  filter(event == 'ice_off',
         sen < 0) %>%  
  summarise(max = max(sen),
            min = min(sen),
            median = median(sen),
            percent_neg = n()/1682,                       #53% with negative slope (earlier ice off)
            mean_ice_off_trend = mean(sen),                #mean slope = -0.49 days per year
            mean_ice_off_relative_slope = mean(rel_slope)) #mean rel_slope = -0.00166, max = -0.0278, min = -2

mk_sen_test_ice_off_no_trend <- mk_sen_test %>%
  filter(event == 'ice_off',
         sen == 0) %>%  
  summarise(percent_neg = n()/184)

# 3. ICE DURATION---------------------------------------------------------------  

# ice_on_grouping <- iceOn_full_data %>%
#   na.omit() %>%
#   group_by(hylak_id) %>%
#   filter(elevation >= 1500,
#          n() >= 20) %>% #filtering out any lakes with fewer than 10 rows of data
#   mutate(ice_on_yday = yday(date),
#          yday_w_year = ifelse(ice_on_yday >= 274, ice_on_yday - 273, ice_on_yday + 92)) %>%
#   ungroup %>% 
#   select(hylak_id, water_year = ice_on_water_year, elevation, ice_on_yday = yday_w_year)
# 
# 
# ice_off_grouping <- iceOff_full_data %>%
#   na.omit() %>%
#   group_by(hylak_id) %>%
#   filter(elevation >= 1500,
#          n() >= 20) %>% 
#   mutate(ice_off_yday = yday(date),
#          yday_w_year = ifelse(ice_off_yday >= 274, ice_off_yday - 273, ice_off_yday + 92)) %>%
#   ungroup %>% 
#   select(hylak_id, water_year = ice_off_water_year, elevation, ice_off_yday = yday_w_year)
# 
# #Group ice on and ice off for DURATION
# 
# grouped_ice <- ice_on_grouping %>% 
#   inner_join(ice_off_grouping) %>% 
#   mutate(
#     duration = ice_off_yday - ice_on_yday
#   ) %>% 
#   filter(duration > 14)
# 
# unique(grouped_ice$hylak_id)  
# 
# 
# duration_trend <- grouped_ice %>% 
#   group_by(hylak_id) %>%
#   summarize(duration_tau = MannKendall(duration)$tau,
#             duration_p.value = MannKendall(duration)$sl,
#             duration_sen = mannKen(duration)$sen.slope,
#             duration_rel_slope = mannKen(duration)$sen.slope.rel) %>%
#   ungroup
# 
# unique(duration_trend$hylak_id)
# 
# mean_duration_trend <- duration_trend %>% 
#   select(duration_tau, duration_sen, duration_rel_slope) %>% 
#   summarise(tau_mean = mean(duration_tau),
#             sen_mean = mean(duration_sen),
#             sen_median = median(duration_sen),
#             rel_sen_mean = mean(duration_rel_slope))
# 
# 
# duration_trend %>% 
#   filter(duration_tau < 0)
# unique(mean_duration_trend)
# # With 20 years of data, a total of 284 lakes
# # Total of 206 with pos trend which is 72% of lakes with 20 years of data
# # Average loss of 0.17 per year or 1.7 days per decade for 
# # 
# # 402 lakes have 20 years of data for ice off / 380 with a negative trend
# # 0.55 days per dacade later ice formation
# # 5.5 days per decade




#Visualize the Sen's slope relationships on a N. American map-------------------

#Build a map of lakes with slopes-----------------------------------------------

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
library("sf")       
library("rnaturalearth")
library("rnaturalearthdata")

#Found mapping code here: https://evamaerey.github.io/flipbooks/geom_sf/geom_sf.html#32

#Prepare N. American map
theme_set(theme_bw())

na <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf") %>% 
  select(name, continent, geometry) %>% 
  filter(continent == 'North America')

# Add lat and long to trend data

trend_plotting_data <- mk_sen_test %>% 
  inner_join(hydro_lakes)
  
#write_csv(trend_plotting_data, here('data/n_american_ice_phenology/results/mk_results.csv'))

#ICE ON plot--------------------------------------------------------------------

# Add the suitability dataset to give opacity as suitability probability

rs_results_tbl_ice_on <- read_csv(here('data/n_american_ice_phenology/results/rs_results_tbl_ice_on.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    .pred_class = as.factor(.pred_class)
  ) %>% 
  select(Hylak_id, class = .pred_class, prob_suitable = .pred_suitable, prob_unsuitable = .pred_unsuitable)

# Join results table with ice on plotting data

ice_on_trend_plotting_data <- trend_plotting_data %>%  #2384 lakes with 20 years of ice off data
  filter(event == 'ice_on') %>% 
  inner_join(rs_results_tbl_ice_on) %>% 
  filter(class == 'suitable')                          #727 suitable for ice on

# mid <- mean(ice_on_trend_plotting_data$sen)
mid <- 0

#ice_on_map <- 
ggplot(data = ice_on_trend_plotting_data) +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-154, -104), ylim = c(33, 65.5), expand = FALSE)+
  geom_point(aes(x = pour_long, y = pour_lat, color = sen), size = 5)+
  scale_colour_gradient2(midpoint = mid, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = 'Ice On Trend \nSlope Magnitude')

#save a copy of the ice on plot
ggsave(here("output/maps/ice_on_map_p.value_2.jpeg"), dpi = 700) #, width = 15, height = 15, units = "in"

#ICE OFF plot--------------------------------------------------------------------

# Add the suitability dataset to give opacity as suitability probability

rs_results_tbl_ice_off <- read_csv(here('data/n_american_ice_phenology/results/rs_results_tbl_ice_off.csv')) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id),
    .pred_class = as.factor(.pred_class)
  ) %>% 
  select(Hylak_id, class = .pred_class, prob_suitable = .pred_suitable, prob_unsuitable = .pred_unsuitable)

# Join results table with ice off trend plotting data

ice_off_trend_plotting_data <- trend_plotting_data %>%   #1,672 lakes with 20 years of ice off data
  filter(event == 'ice_off') %>% 
  inner_join(rs_results_tbl_ice_off) %>% 
  filter(class == 'suitable')                            #1,545 suitable for ice off

#mid <- mean(ice_off_trend_plotting_data$sen)
mid <- 0

#ice_on_map <- 
ggplot(data = ice_off_trend_plotting_data) +
  ggplot2::geom_sf(data = na) +
  coord_sf(xlim = c(-154, -104), ylim = c(33, 65.5), expand = FALSE)+
  geom_point(aes(x = pour_long, y = pour_lat, color = sen), size = 5)+
  scale_colour_gradient2(midpoint = mid, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+  #found this code here: https://www.datanovia.com/en/blog/ggplot-gradient-color/
  #scale_colour_gradient(low = 'red', high = 'blue', space = 'Lab')+
  #geom_point(aes(x = pour_long, y = pour_lat, fill = sen), pch = 21, size = 4, stroke = 0.5)+
  #scale_fill_gradient2(midpoint = mid, low = 'red', mid = 'white', high = 'blue', space = 'Lab')+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = 'Ice Off Trend \nSlope Magnitude')

#coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

#save a copy of the ice on plot
ggsave(here("output/maps/ice_off_map_p.value.jpeg"), dpi = 700) #, width = 15, height = 15, units = "in"


























