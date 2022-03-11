

#add the cloudy data to the slice() detectino method


#first get the cloudy data
cloud_window_5day <- read_csv(here('data/remote/05day_cloud_window.csv')) %>%
  select(2:4) %>%
  rename(
    lakename = 'lake',
    ice_on_water_year = 'water year',
    obs_5days = remote_near_on_date
  )


cloud_window_10day <- read_csv(here('data/remote/10days_cloud_window.csv')) %>%
  select(2:4) %>%
  rename(
    lakename = 'lake',
    ice_on_water_year = 'water year',
    obs_10days = remote_near_on_date
  )

cloud_window_15day <- read_csv(here('data/remote/15days_cloud_window.csv')) %>%
  select(2:4) %>%
  rename(
    lakename = 'lake',
    ice_on_water_year = 'water year',
    obs_15days = remote_near_on_date
  )


ice_on_w_cloud_presence <- remote_insitu_merge_iceOn_dates %>%
  select(-ice_on_insitu_yday) %>%
  full_join(cloud_window_5day) %>%
  full_join(cloud_window_10day) %>%
  full_join(cloud_window_15day) %>%
  mutate(
    yday_insitu_october = ifelse(yday(ice_on_insitu) >= 274, yday(ice_on_insitu) - 273, yday(ice_on_insitu) + 92),
    yday_remote_october = ifelse(yday(full_merge) >= 274, yday(full_merge) - 273, yday(full_merge) + 92)
  )


slice_iceOn_5day_plt <- ice_on_w_cloud_presence[ice_on_w_cloud_presence$obs_5days == TRUE,]

Metrics::mae(actual = as.numeric(slice_iceOn_5day_plt$ice_on_insitu), predicted = as.numeric(slice_iceOn_5day_plt$full_merge)) #25.6
mean(slice_iceOn_5day_plt$ice_on_insitu - slice_iceOn_5day_plt$full_merge, na.rm = TRUE) #-4.36

slice_iceOn_10day_plt <- ice_on_w_cloud_presence[ice_on_w_cloud_presence$obs_10days == TRUE,]

Metrics::mae(actual = as.numeric(slice_iceOn_10day_plt$ice_on_insitu), predicted = as.numeric(slice_iceOn_10day_plt$full_merge)) #23.81
mean(slice_iceOn_10day_plt$ice_on_insitu - slice_iceOn_10day_plt$full_merge, na.rm = TRUE) #-4.03

slice_iceOn_15day_plt <- ice_on_w_cloud_presence[ice_on_w_cloud_presence$obs_15days == TRUE,]

Metrics::mae(actual = as.numeric(slice_iceOn_15day_plt$ice_on_insitu), predicted = as.numeric(slice_iceOn_15day_plt$full_merge)) #26.67
mean(slice_iceOn_15day_plt$ice_on_insitu - slice_iceOn_15day_plt$full_merge, na.rm = TRUE) #-4.73



#Filter to eliminate points that do not have an observation within 5 days
ice_on_slice_5day <- ggplot(data = slice_iceOn_5day_plt)+
  geom_point(aes(x = yday_insitu_october, y = yday_remote_october, color = lakename), size = 4)+ #data = remote_insitu_merge_iceOff_dates_plt
  #one_to_one_iceoff <- ggplot(data = remote_insitu_merge_iceOff_dates_plt)+
  geom_abline(slope = 1, intercept = 0, size = 1.5)+
  #geom_point(aes(shape = lakename, fill = lakename), size = 10, stroke = 3)+
  #geom_point(aes(color = terra_cloud), size = 5)+ #for cloud coloring
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
  ggtitle('Ice On (Obs w/in 5 days, n = 53); MAE = 25.60 days; MBE = -4.36 days')
ice_on_slice_5day

ggplotly(ice_on_slice_5day)



#Filter to eliminate points that do not have an observation within 10 days
ice_on_slice_10day <- ggplot(data = slice_iceOn_10day_plt)+
  geom_point(aes(x = yday_insitu_october, y = yday_remote_october, color = lakename), size = 4)+ #data = remote_insitu_merge_iceOff_dates_plt
  #one_to_one_iceoff <- ggplot(data = remote_insitu_merge_iceOff_dates_plt)+
  geom_abline(slope = 1, intercept = 0, size = 1.5)+
  #geom_point(aes(shape = lakename, fill = lakename), size = 10, stroke = 3)+
  #geom_point(aes(color = terra_cloud), size = 5)+ #for cloud coloring
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
  ggtitle('Ice On (Obs w/in 10 days, n = 64); MAE = 23.81 days; MBE = -4.03 days')
ice_on_slice_10day

ggplotly(ice_on_slice_10day)


#Filter to eliminate points that do not have an observation within 15 days
ice_on_slice_15day <- ggplot(data = slice_iceOn_15day_plt)+
  geom_point(aes(x = yday_insitu_october, y = yday_remote_october, color = lakename), size = 4)+ #data = remote_insitu_merge_iceOff_dates_plt
  #one_to_one_iceoff <- ggplot(data = remote_insitu_merge_iceOff_dates_plt)+
  geom_abline(slope = 1, intercept = 0, size = 1.5)+
  #geom_point(aes(shape = lakename, fill = lakename), size = 10, stroke = 3)+
  #geom_point(aes(color = terra_cloud), size = 5)+ #for cloud coloring
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
  ggtitle('Ice On (Obs w/in 15 days, n = 96); MAE = 23.53 days; MBE = -4.29days')
ice_on_slice_15day

ggplotly(ice_on_slice_15day)




