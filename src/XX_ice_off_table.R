# load libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(here)
library(plotly)
library(Metrics)
library(ggpubr)

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
  arrange(lakename, ice_off_water_year) #%>%
  #na.omit()

############################
mae_90 <- remote_insitu_merge_iceOff_dates %>%
  na.omit() %>%
  summarise(across(
    .cols = 3:20, #with full_merge_fill
    .fns = ~ Metrics::mae( predicted = as.numeric(.x), actual = as.numeric(ice_off_insitu))
  )) %>%
  mutate(
    width = '90 day'
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



















remote_insitu_merge_iceOff_dates_plt <- remote_iceOff %>%
  filter(median_val == 'frac_31day_med') %>%
  #pivot_wider(names_from = median_val, values_from = date_ice_off) %>%
  inner_join(all_insitu_w_water_year, by = c("lakename", "ice_off_water_year")) %>%
  select(-ice_on_insitu, -ice_on_insitu_yday, -ice_on_water_year) %>%
  arrange(lakename, ice_off_water_year, median_val) %>%
  slice(-63) #%>%
  #na.omit()

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
  geom_point(aes(shape = lakename, fill = lakename), size = 10, stroke = 3)+
  #geom_smooth(aes(x = yday_insitu_october, y = yday_remote_october))+
  theme_classic()+
  theme_classic()+
  #ylim(c(0,366))+
  #xlim(c(0,366))+
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
  geom_smooth(method = 'lm', se = FALSE)+
  stat_regline_equation(
    aes(label = paste(..adj.rr.label..))
  )+
  ggtitle('Ice Off with Ice On Filter (width = 97 days); MAE = 8.42 days')
  #facet_wrap(~median_val + ice_off_water_year)
#one_to_one_iceoff
ggplotly(x)

ggsave(here('output/ice_off_ice_on_filter_width_97days_2.png'), dpi = 500, width = 15, height = 9)
