all_remote_lunz <- all_remote_ice_data %>%
  filter(
    lakename == 'Lunz'
  ) %>%
  #pad()  %>%
  mutate(
    water_year = ifelse(month(date) >= 10, year(date)+1, year(date))
  ) %>%
  #filter(water_year > 2004 & water_year <2006) %>%
  mutate(full_merge_fill = na.approx(full_merge)) %>%
  na.omit()
  
plt_x <- ggplot(data = all_remote_lunz)+
  geom_line(aes(x = date, y = full_merge))+
  theme_classic()
ggplotly(plt_x)



all_remote_w_median_test <- all_remote_w_median %>%
  filter(lakename == 'lunz')

remote_iceOn_test <- all_remote_w_median_test %>%
  #filter(median_val == "full_merge") %>%
  #remove median_val as grouping variable when above filter is in use
  group_by(lakename, water_year, median_val) %>% #, medial_val
  filter(date > '2000-08-01') %>%
  mutate(median_iceFrac = rollapply(median_iceFrac, width = 21, min, align = "left", fill = NA, na.rm = TRUE)) %>%
  filter(median_iceFrac >= 0.8) %>%
  filter(row_number() == 1) %>%
  rename(date_ice_on = date) %>% #remove when not going through all columns
  ungroup() #%>%


remote_iceOff_test <- all_remote_w_median_test %>%
  select(-year) %>%
  left_join(., y= remote_iceOn_test) %>%
  group_by(lakename, water_year, median_val) %>% #, median_val
  #filter(median_val == "frac_31day_med") %>%
  #filter(date > date_ice_on) %>%
  #filter(date > "2000-08-01") %>%
  mutate(median_iceFrac = rollapply(median_iceFrac, width = 28, max, align = "left", fill = NA, na.rm = TRUE)) %>% #found this rollapply() solution here: https://stackoverflow.com/questions/31373256/r-selecting-first-of-n-consecutive-rows-above-a-certain-threshold-value
  filter(median_iceFrac <= 0.2 & date > date_ice_on) %>% 
  # slice(n()) %>%
  filter(row_number() == 1) %>%
  rename(date_ice_off = date) %>%
  mutate(ice_duration = date_ice_off - date_ice_on) %>%
  select(lakename, ice_off_water_year = water_year, date_ice_on, date_ice_off, ice_duration) %>%
  #select(lakename, water_year, date_ice_off) %>%
  # mutate(
  #   #ice_on_water_year = ifelse(month(date_ice_on) >= 10, year(date_ice_on)+1, year(date_ice_on)),
  #   ice_off_water_year = ifelse(month(date_ice_off) >= 10, year(date_ice_off)+1, year(date_ice_off))
  #   ) %>%
  # rename(ice_off_water_year = water_year) %>%
  ungroup

















plt_x <- ggplot(data = all_remote_w_median_test)+
  geom_line(aes(x = date, y = median_iceFrac))+
  theme_classic()
ggplotly(plt_x)
