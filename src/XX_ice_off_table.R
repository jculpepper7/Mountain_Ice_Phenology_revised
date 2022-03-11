# load libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(here)
library(plotly)
library(Metrics)
library(ggpubr)



#Import remote data with cloudy images 
#Also, clean the dataset column names and date conventions to make sure that it is 
#interoperable with ice detection dataset
remote_w_clouds_ice_off <- read_csv(here('data/combined/modis_allLakes_output_Terra_Aqua_merged_raw.csv')) %>%
  clean_names() %>%
  select(-h) %>%
  filter(lakename %in% c('Albion', 'Castle', 'Lunz', 'Morskie_Oko', 'Silver')) %>%
  pivot_wider(names_from = source, values_from = cloud_modis) %>%
  rename(ice_off_insitu = date,
         terra_cloud = MODISTerra,
         aqua_cloud = MODISAqua) %>%
  mutate(
    lakename = tolower(lakename),
    terra_cloud = ifelse(terra_cloud >= 0.1, TRUE, FALSE),
    aqua_cloud = ifelse(aqua_cloud >= 0.1, TRUE, FALSE)
  ) 



remote_iceOff <- all_remote_w_median %>%
  select(-year) %>%
  left_join(., y = remote_ice_on_trim) %>%
  group_by(lakename, water_year, median_val) %>% #, median_val #water_year
  #filter(date > "2000-09-30") %>%
  #found this rollapply() solution here: https://stackoverflow.com/questions/31373256/r-selecting-first-of-n-consecutive-rows-above-a-certain-threshold-value
  mutate(median_iceFrac = rollapply(median_iceFrac, width = 97, max, align = "left", fill = NA, na.rm = TRUE)) %>% #28 
  filter(median_iceFrac <= 0.2 & date > date_ice_on) %>% #& date > date_ice_on
  filter(row_number() == 1) %>%
  rename(date_ice_off = date) %>%
  select(lakename, ice_off_water_year = water_year, date_ice_off, median_val) %>%
  ungroup

####################################
#ice off dates
remote_insitu_merge_iceOff_dates <- remote_iceOff %>%
  pivot_wider(names_from = median_val, values_from = date_ice_off) %>%
  inner_join(all_insitu_w_water_year, by = c("lakename", "ice_off_water_year")) %>%
  select(-ice_on_insitu, -ice_on_insitu_yday, -ice_on_water_year) %>%
  arrange(lakename, ice_off_water_year) %>%
  mutate(
    diff = frac_31day_med - ice_off_insitu
  )
  #na.omit()
mean(remote_insitu_merge_iceOff_dates$diff, na.rm = TRUE)
############################
mae_97 <- remote_insitu_merge_iceOff_dates %>%
  na.omit() %>%
  summarise(across(
    .cols = 3:20, #with full_merge_fill
    .fns = ~ Metrics::mae( predicted = as.numeric(.x), actual = as.numeric(ice_off_insitu))
  )) %>%
  mutate(
    width = '97 day'
  ) %>%
select(19, 1:18)
#ice_off_med_test

off_mae_table_no_w_year <- bind_rows(mae_2, mae_3, mae_4, mae_5, mae_6, mae_7, mae_8, mae_9, mae_10,
                             mae_11, mae_12, mae_13, mae_14, mae_15, mae_16, mae_17, mae_18, mae_19, mae_20,
                             mae_21, mae_22, mae_23, mae_24, mae_25, mae_26, mae_27, mae_28, mae_29, mae_30, mae_31)

write_csv(off_mae_table_no_w_year, here('data/off_mae_table_no_w_year_2022.02.22.csv'))

############################
mdae_100 <- remote_insitu_merge_iceOff_dates %>%
  na.omit() %>%
  summarise(across(
    .cols = 3:20, #with full_merge_fill
    .fns = ~ Metrics::mdae( predicted = as.numeric(.x), actual = as.numeric(ice_off_insitu))
  )) %>%
  mutate(
    width = '100 day'
  ) %>%
  select(19, 1:18)
#ice_off_med_test

off_mdae_table_no_w_year <- bind_rows(mdae_2, mdae_3, mdae_4, mdae_5, mdae_6, mdae_7, mdae_8, mdae_9, mdae_10,
                            mdae_11, mdae_12, mdae_13, mdae_14, mdae_15, mdae_16, mdae_17, mdae_18, mdae_19, mdae_20,
                            mdae_21, mdae_22, mdae_23, mdae_24, mdae_25, mdae_26, mdae_27, mdae_28, mdae_29, mdae_30, mdae_31)

write_csv(off_mdae_table_no_w_year, here('data/off_mdae_table_no_w_year_2022.02.22.csv'))

############################
rmse_100 <- remote_insitu_merge_iceOff_dates %>%
  na.omit() %>%
  summarise(across(
    .cols = 3:20, #with full_merge_fill
    .fns = ~ Metrics::rmse( predicted = as.numeric(.x), actual = as.numeric(ice_off_insitu))
  )) %>%
  mutate(
    width = '100 day'
  ) %>%
  select(19, 1:18)
#ice_off_med_test

off_rmse_table_no_w_year <- bind_rows(rmse_2, rmse_3, rmse_4, rmse_5, rmse_6, rmse_7, rmse_8, rmse_9, rmse_10,
                            rmse_11, rmse_12, rmse_13, rmse_14, rmse_15, rmse_16, rmse_17, rmse_18, rmse_19, rmse_20,
                            rmse_21, rmse_22, rmse_23, rmse_24, rmse_25, rmse_26, rmse_27, rmse_28, rmse_29, rmse_30, rmse_31)


write_csv(off_rmse_table_no_w_year, here('data/off_rmse_table_no_w_year_2022.02.22.csv'))















cloud_window_5day <- read_csv(here('data/remote/15days_cloud_window.csv')) %>%
  select(2,3,5) %>%
  rename(
    lakename = 'lake',
    water_year = 'water year'
  )




remote_insitu_merge_iceOff_dates_plt <- remote_iceOff %>%
  filter(median_val == 'full_merge') %>%
  #rename(ice_off_water_year = water_year) %>% #for cloud test
  #pivot_wider(names_from = median_val, values_from = date_ice_off) %>%
  inner_join(all_insitu_w_water_year, by = c("lakename", "ice_off_water_year")) %>%
  rename(water_year = ice_off_water_year) %>% #for cloud test to use inner join below
  select(-ice_on_insitu, -ice_on_insitu_yday, -ice_on_water_year) %>%
  #arrange(lakename, ice_off_water_year, median_val) %>%
  #inner_join(cloud_window_5day, by = c('lakename', 'water_year'))
  left_join(remote_w_clouds_ice_off, by = c('lakename', 'ice_off_insitu'))
  #slice(-63) #%>%
  #na.omit()


remote_insitu_merge_iceOff_dates_plt <- remote_insitu_merge_iceOff_dates_plt[remote_insitu_merge_iceOff_dates_plt$remote_near_off_date == TRUE,]


Metrics::mae(actual = as.numeric(remote_insitu_merge_iceOff_dates_plt$ice_off_insitu), predicted = as.numeric(remote_insitu_merge_iceOff_dates_plt$date_ice_off))
mean(remote_insitu_merge_iceOff_dates_plt$date_ice_off - remote_insitu_merge_iceOff_dates_plt$ice_off_insitu)




all_remote_w_median %>%
  filter(
    median_val == 'full_merge' & lakename == 'albion' & year == c(2011:2016)
  ) %>%
ggplot( mapping = aes(x = date, y = median_iceFrac))+
  geom_hline(yintercept = 0.8, linetype = 'dashed')+
  geom_line(size = 1.5, color = "#56B4E9")+
  theme_classic()+
  ylab('Ice Fraction')+
  xlab('Year')+
  theme(text = element_text(size = 25))

ggsave(here('output/example_ice_fraction.png'), dpi = 500, width = 15, height = 10)

#mae(predicted = as.numeric(remote_insitu_merge_iceOff_dates_plt$date_ice_off), actual = as.numeric(remote_insitu_merge_iceOff_dates_plt$ice_off_insitu))

x <- remote_insitu_merge_iceOff_dates_plt %>%
  #filter(median_val == 'frac_31day_med') %>%
  mutate(
    #ice_on_remote_yday = yday(full_merge),
    #ice_on_remote_yday = yday(date_ice_on), #for "update" data
    yday_insitu_october = ifelse(yday(ice_off_insitu) >= 274, yday(ice_off_insitu) - 273, yday(ice_off_insitu) + 92),
    #yday_insitu_october = ifelse(year(ice_off_insitu) %% 4 == 0 & ice_off_insitu_yday > 59, yday_insitu_october + 1, yday_insitu_october),
    yday_remote_october = ifelse(yday(date_ice_off) >= 274, yday(date_ice_off) - 273, yday(date_ice_off) + 92),
    #diff_date = ice_on_insitu - full_merge,
    #diff_date = ice_on_insitu - date_ice_on, #for "update" data
    #diff_yday = yday_insitu_october - yday_remote_october
  ) %>%
ggplot(aes(x = yday_insitu_october, y = yday_remote_october))+ #data = remote_insitu_merge_iceOff_dates_plt
#one_to_one_iceoff <- ggplot(data = remote_insitu_merge_iceOff_dates_plt)+
  geom_abline(slope = 1, intercept = 0, size = 1.5)+
  #geom_point(aes(shape = lakename, fill = lakename), size = 10, stroke = 3)+
  geom_point(aes( color = aqua_cloud), size = 5)+ #for cloud coloring
  #geom_smooth(aes(x = yday_insitu_october, y = yday_remote_october))+
  theme_classic()+
  theme_classic()+
  #ylim(c(90,300))+
  #xlim(c(90,300))+
  #scale_shape_manual("Lake:", values = c(15,16,17,18))+
  scale_shape_manual("Lake:", values = c(21,22,23,24,25))+
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
 # )+
  #ggtitle('Ice Off (Terra Cloud, n = 114); MAE = 14.02 days; MBE = 1.64 days')
  ggtitle('Ice Off (Terra Cloud, n = 114)') #for cloud coloring title
  #facet_wrap(~median_val + ice_off_water_year)
x
#one_to_one_iceoff
#ggplotly(x)

ggsave(here('ice_off_15day_cloud_filter.png'), dpi = 500, width = 15, height = 9)
