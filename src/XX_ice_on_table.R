library(tidyverse)
library(ggpubr)
library(lubridate)
library(plotly)

#Import remote data with cloudy images 
#Also, clean the dataset column names and date conventions to make sure that it is 
#interoperable with ice detection dataset
remote_w_clouds_ice_on <- read_csv(here('data/combined/modis_allLakes_output_Terra_Aqua_merged_raw.csv')) %>%
  clean_names() %>%
  select(-h) %>%
  filter(lakename %in% c('Albion', 'Castle', 'Lunz', 'Morskie_Oko', 'Silver')) %>%
  pivot_wider(names_from = source, values_from = cloud_modis) %>%
  rename(ice_on_insitu = date,
         terra_cloud = MODISTerra,
         aqua_cloud = MODISAqua) %>%
  mutate(
    lakename = tolower(lakename),
    terra_cloud = ifelse(terra_cloud >= 0.1, TRUE, FALSE),
    aqua_cloud = ifelse(aqua_cloud >= 0.1, TRUE, FALSE)
  ) 
 

# remote_iceOn <- all_remote_w_median %>%
#   #filter(median_val == "full_merge") %>%
#   #remove median_val as grouping variable when above filter is in use
#   group_by(lakename, water_year, median_val) %>% #, medial_val
#   filter(date > '2000-10-01') %>% #start at 2001 water year (otherwise will have a misleading ice on in March of 2000)
#   mutate(median_iceFrac = rollapply(median_iceFrac, width = 19, min, align = "left", fill = NA, na.rm = TRUE)) %>% #21
#   filter(median_iceFrac >= 0.8) %>%
#   filter(row_number() == 1) %>%
#   rename(date_ice_on = date) %>% #remove when not going through all columns
#   ungroup() #%>%

# remote_insitu_merge_iceOn_dates <- remote_iceOn %>%
#   select(-year) %>%
#   rename(ice_on_water_year = water_year) %>%
#   select(-median_iceFrac) %>% #comment out when using one median_val
#   pivot_wider(names_from = median_val, values_from = date_ice_on) %>% #comment out when using one median_val
#   inner_join(all_insitu_w_water_year, by = c("lakename", "ice_on_water_year")) %>% #inner_join() works better than right_join(), as it does not include erroneous NA values, such as the Castle ice on dates, since we do not have Castle ice on dates.
#   #right_join(all_insitu_w_water_year, by = c("lakename")) %>%
#   select(-ice_off_insitu, -ice_off_insitu_yday, -ice_off_water_year) %>%
#   arrange(lakename, ice_on_water_year) #%>%

# ice_on_med_test <- remote_insitu_merge_iceOn_dates %>%
#   na.omit() %>%
#   #group_by(lakename) %>% # f you do not group by lake name, it will get the total MAE
#   #filter(lakename != "morskie_oko") %>%
#   summarise(across(
#     .cols = 3:20, #without full_merge_fill
#     #.cols = 2:5, #with full_merge_fill
#     .fns = ~ mean(na.rm = TRUE, interval(.x, ice_on_insitu) %/% days(1)) #NOTE: 02.18.2022 This is not MAE its the absolute value of the mean difference (which is absolute value of the mean bias error MBE - see Smejkalova et al., 2016)
#     #.fns = ~ Metrics::mdae(predicted = as.numeric(.x), actual = as.numeric(ice_on_insitu))
#     #.fns = ~ Metrics::mae(predicted = as.numeric(.x), actual = as.numeric(ice_on_insitu))
#     #.fns = ~ Metrics::rmse(predicted = as.numeric(.x), actual = as.numeric(ice_on_insitu))
#   ))
# ice_on_med_test


cloud_window_5day <- read_csv(here('data/remote/15days_cloud_window.csv')) %>%
  select(2:4) %>%
  rename(
    lakename = 'lake',
    water_year = 'water year'
  )
  


remote_insitu_merge_iceOn_dates_plt <- remote_iceOn %>%
  filter(median_val == 'full_merge') %>%
  rename(ice_on_water_year = water_year) %>%
  #pivot_wider(names_from = median_val, values_from = date_ice_off) %>%
  inner_join(all_insitu_w_water_year, by = c("lakename", "ice_on_water_year")) %>%
  rename(water_year = 'ice_on_water_year') %>% #this is just to test the cloud windows for the inner_join below
  select(-ice_off_insitu, -ice_off_insitu_yday, -ice_off_water_year, -year, -median_val, -median_iceFrac) %>%
  #left_join(remote_w_clouds_ice_on, by = c('lakename', 'ice_on_insitu'))
  inner_join(cloud_window_5day, by = c('lakename', 'water_year')) #%>%
  #arrange(lakename, ice_on_water_year, median_val)

remote_insitu_merge_iceOn_dates_plt <- remote_insitu_merge_iceOn_dates_plt[remote_insitu_merge_iceOn_dates_plt$remote_near_on_date == TRUE,]


Metrics::mae(actual = as.numeric(remote_insitu_merge_iceOn_dates_plt$ice_on_insitu), predicted = as.numeric(remote_insitu_merge_iceOn_dates_plt$date_ice_on))
mean(remote_insitu_merge_iceOn_dates_plt$date_ice_on - remote_insitu_merge_iceOn_dates_plt$ice_on_insitu)

y <- remote_insitu_merge_iceOn_dates_plt %>%
  #filter(median_val == 'frac_31day_med') %>%
  mutate(
    #ice_on_remote_yday = yday(full_merge),
    #ice_on_remote_yday = yday(date_ice_on), #for "update" data
    yday_insitu_october = ifelse(yday(ice_on_insitu) >= 274, yday(ice_on_insitu) - 273, yday(ice_on_insitu) + 92),
    #yday_insitu_october = ifelse(year(ice_off_insitu) %% 4 == 0 & ice_off_insitu_yday > 59, yday_insitu_october + 1, yday_insitu_october),
    yday_remote_october = ifelse(yday(date_ice_on) >= 274, yday(date_ice_on) - 273, yday(date_ice_on) + 92),
    #diff_date = ice_on_insitu - full_merge,
    #diff_date = ice_on_insitu - date_ice_on, #for "update" data
    #diff_yday = yday_insitu_october - yday_remote_october
  ) %>%
  ggplot(aes(x = yday_insitu_october, y = yday_remote_october))+ #data = remote_insitu_merge_iceOff_dates_plt
  #one_to_one_iceoff <- ggplot(data = remote_insitu_merge_iceOff_dates_plt)+
  geom_abline(slope = 1, intercept = 0, size = 1.5)+
  #geom_point(aes(shape = lakename, fill = lakename), size = 10, stroke = 3)+
  geom_point(aes(color = terra_cloud), size = 5)+ #for cloud coloring
  #geom_smooth(aes(x = yday_insitu_october, y = yday_remote_october))+
  theme_classic()+
  #ylim(c(0,366))+
  #xlim(c(0,366))+
  #scale_shape_manual("Lake:", values = c(15,16,17,18))+
  #scale_shape_manual("Lake:", values = c(21,22,23,24,25))+
  scale_fill_viridis_d("Lake:", direction = -1)+
  #scale_color_viridis_d("Lake:", direction = 1, option = 'inferno')+
  ylab("Ice On - MODIS [day]")+
  xlab("Ice On - In Situ [day]")+
  #facet_wrap(~lakename, scales = 'free')+
  theme(
    text = element_text(size = 20),
    legend.position = "bottom"
  ) +
  #geom_smooth(method = 'lm', se = TRUE)+
  #stat_regline_equation(
    #aes(label = paste(..adj.rr.label..))
  #)+
  ggtitle('Ice On (Terra Clouds, n = 96); MAE = 23.53 days; MBE = -4.29days')
y
#facet_wrap(~median_val + ice_off_water_year)
#one_to_one_iceoff
ggplotly(y)

ggsave(here('ice_on_15day_cloud_filter.png'), dpi = 500, width = 15, height = 9)

