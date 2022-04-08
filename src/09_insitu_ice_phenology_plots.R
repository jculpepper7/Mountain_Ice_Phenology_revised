## The goal of this script is to:
##
## 1. plot dates derived from script 08

# load libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(here)
library(plotly)
library(Metrics)

#load data
#ice_on_full <- read_csv(here('data/combined/remote_insitu_iceOn_dates.csv'))
#ice_off_full <- read_csv(here('data/combined/remote_insitu_iceOff_dates.csv'))
ice_on_dates <- read_csv(here('data/combined/remote_insitu_iceOn_dates_update_merge_2022.04.01.csv'))
ice_off_dates <- read_csv(here('data/combined/remote_insitu_iceOff_dates_update_merge_2022.04.01.csv'))

#add yday to in situ data
ice_on_dates <- ice_on_dates %>%
  mutate(
    ice_on_insitu_yday = yday(ice_on_insitu)
  )

ice_off_dates <- ice_off_dates %>%
  mutate(
    ice_off_insitu_yday = yday(ice_off_insitu)
  )





#ice on
ice_on_full <- ice_on_dates %>%
  filter(ice_on_15day_check == TRUE) %>%
  #select(lakename, ice_on_water_year, full_merge, ice_on_insitu, ice_on_insitu_yday) %>%
  mutate(
    #ice_on_remote_yday = yday(full_merge),
    ice_on_remote_yday = yday(date_ice_on), #for "update" data
    yday_insitu_october = ifelse(ice_on_insitu_yday >= 274, ice_on_insitu_yday - 273, ice_on_insitu_yday + 92),
    #yday_insitu_october = ifelse(year(ice_off_insitu) %% 4 == 0 & ice_off_insitu_yday > 59, yday_insitu_october + 1, yday_insitu_october),
    yday_remote_october = ifelse(ice_on_remote_yday >= 274, ice_on_remote_yday - 273, ice_on_remote_yday + 92),
    #diff_date = ice_on_insitu - full_merge,
    #diff_date = ice_on_insitu - date_ice_on, #for "update" data
    #diff_yday = yday_insitu_october - yday_remote_october
    #below for uncertainty
    ice_on_w_uncertainty_yday = yday(ice_on_w_uncertainty),
    yday_remote_w_uncert = ifelse(ice_on_w_uncertainty_yday >= 274, ice_on_w_uncertainty_yday - 273, ice_on_w_uncertainty_yday + 92)
  ) %>%
  na.omit()


#standard choosing method
mae(ice_on_full$yday_insitu_october, ice_on_full$yday_remote_october) 
mean(ice_on_full$yday_insitu_october - ice_on_full$yday_remote_october) 

#choosing middle value of cloudy days
mae(ice_on_full$yday_insitu_october, ice_on_full$yday_remote_w_uncert)
mean(ice_on_full$yday_insitu_october - ice_on_full$yday_remote_w_uncert)

#mdae(ice_on_full$yday_insitu_october, ice_on_full$yday_remote_october) #23.63912   #updated error 18
#rmse(ice_on_full$yday_insitu_october, ice_on_full$yday_remote_october) #37.13414   #updated error 31.99796
  
#ice off
ice_off_full <- ice_off_dates %>%
  filter(ice_on_15day_check == TRUE) %>%
  #select(lakename, ice_off_water_year, frac_31day_med, ice_off_insitu, ice_off_insitu_yday) %>%
  na.omit() %>%
  mutate(
    ice_off_remote_yday = yday(date_ice_off),
    #ice_off_remote_yday = yday(date_ice_off),  #for "update" data
    yday_insitu_october = ifelse(ice_off_insitu_yday >= 274, ice_off_insitu_yday - 273, ice_off_insitu_yday + 92),
    #yday_insitu_october = ifelse(year(ice_off_insitu) %% 4 == 0 & ice_off_insitu_yday > 59, yday_insitu_october + 1, yday_insitu_october),
    yday_remote_october = ifelse(ice_off_remote_yday >= 274, ice_off_remote_yday - 273, ice_off_remote_yday + 92),
    #diff_date = ice_off_insitu - frac_31day_med,
    #diff_date = ice_off_insitu - date_ice_off, #for "update" data
    #diff_yday = yday_insitu_october - yday_remote_october
    ice_off_w_uncertainty_yday = yday(ice_off_w_uncertainty),
    yday_remote_w_uncert = ifelse(ice_off_w_uncertainty_yday >= 274, ice_off_w_uncertainty_yday - 273, ice_off_w_uncertainty_yday + 92)
  ) #%>%
  #slice(-78)

#standard choosing method
mae(ice_off_full$yday_insitu_october, ice_off_full$yday_remote_october) #8.778947   #error w/ ice on in ice off algorithm: 13.89
#mae(ice_off_full$ice_off_insitu, ice_off_full$date_ice_off)
mean(ice_off_full$yday_insitu_october - ice_off_full$yday_remote_october) #5.221053  #error w/ ice on in ice off algorithm: 2.98

#choosing middle value of cloudy days
mae(ice_off_full$yday_insitu_october, ice_off_full$yday_remote_w_uncert)
mean(ice_off_full$yday_insitu_october - ice_off_full$yday_remote_w_uncert)
#mdae(ice_off_full$yday_insitu_october, ice_off_full$yday_remote_october) #6       #error w/ ice on in ice off algorithm: 7
#rmse(ice_off_full$yday_insitu_october, ice_off_full$yday_remote_october) #12.63912   #error w/ ice on in ice off algorithm: 29.98

#filter data to necessary columns derived from lowest mean error (see script 08)

# #ice on
# ice_on <- ice_on_full %>%
#   select(lakename, full_merge, ice_on_insitu, ice_on_insitu_yday) %>%
#   mutate(ice_on_remote_yday = yday(full_merge)) %>%
#   filter(lakename != "castle") %>%
#   na.omit() %>%
#   mutate(
#     diff_date = ice_on_insitu -full_merge,
#     diff_yday = ice_on_insitu_yday - ice_on_remote_yday
#   )
# 
# #ice off
# ice_off <- ice_off_full %>%
#   select(lakename, frac_31day_med, ice_off_insitu, ice_off_insitu_yday) %>%
#   mutate(ice_off_remote_yday = yday(frac_31day_med)) %>%
#   na.omit() %>%
#   mutate(
#     diff_date = ice_off_insitu - frac_31day_med,
#     diff_yday = ice_off_insitu_yday - ice_off_remote_yday
#   )


# #plot data--------------------------------------------------------------------

#one to one plots


one_to_one_iceon <- ggplot(data = ice_on_full)+
  geom_abline(slope = 1, intercept = 0, size = 1.5)+
  #geom_point(aes(x = yday_insitu_october, y = yday_remote_october, shape = lakename, fill = lakename), size = 10, stroke = 3)+
  #geom_point(aes(x = yday_insitu_october, y = yday_remote_october, color = lakename), size = 5)+
  geom_point(aes(x = yday_insitu_october, y = yday_remote_w_uncert, color = lakename), size = 5)+
  # geom_errorbar(aes(x = yday_insitu_october, 
  #                   y = yday_remote_october, 
  #                   ymin = yday_remote_october - days_since_last_image/2, 
  #                   ymax = yday_remote_october + days_since_last_image/2,), 
  #               width = 1, position = position_dodge(0.05))+
  # geom_errorbar(aes(x = yday_insitu_october,
  #                   y = yday_remote_w_uncert,
  #                   ymin = yday_remote_w_uncert - days_since_last_image/2,
  #                   ymax = yday_remote_w_uncert + days_since_last_image/2,),
  #               width = 1, position = position_dodge(0.05))+
  theme_classic()+
  ylim(c(0,200))+
  xlim(c(0,200))+
  #scale_shape_manual("Lake:", values = c(15,16,17,18))+
  #scale_shape_manual("Lake:", values = c(21,22,23,24))+
  scale_fill_viridis_d("Lake:", direction = -1)+
  #scale_color_viridis_d("Lake:", direction = 1, option = 'inferno')+
  ylab("Ice On - MODIS [day]")+
  xlab("Ice On - In Situ [day]")+
  #facet_wrap(~lakename, scales = 'free')+
  theme(
    text = element_text(size = 20),
    legend.position = "bottom"
  )+
  ggtitle('Cloud Uncertainty & 5-day Req: MAE = 28.10 days, Bias = 13.42 days, n = 67')
one_to_one_iceon
ggsave(plot = one_to_one_iceon, here("output/terra_v_merge/merge_one_to_one_iceOn_test_w_cloud_uncertainty_15day_4.1.jpeg"), dpi = 500, width = 15, height = 12, units = "in")

one_to_one_iceoff <- ggplot(data = ice_off_full)+
  geom_abline(slope = 1, intercept = 0, size = 1.3)+
  #geom_point(aes(x = yday_insitu_october, y = yday_remote_october, fill = lakename, shape = lakename), size = 10, stroke = 3)+
  #geom_point(aes(x = yday_insitu_october, y = yday_remote_october, color = lakename), size = 5)+
  geom_point(aes(x = yday_insitu_october, y = yday_remote_w_uncert, color = lakename), size = 5)+
  # geom_errorbar(aes(x = yday_insitu_october, 
  #                   y = yday_remote_october, 
  #                   ymin = yday_remote_october - days_since_last_image/2, 
  #                   ymax = yday_remote_october + days_since_last_image/2,), 
  #               width = 1, position = position_dodge(0.05))+
  # geom_errorbar(aes(x = yday_insitu_october, 
  #                   y = yday_remote_w_uncert, 
  #                   ymin = yday_remote_w_uncert - days_since_last_image/2, 
  #                   ymax = yday_remote_w_uncert + days_since_last_image/2,), 
  #               width = 1, position = position_dodge(0.05))+
  theme_classic()+
  ylim(c(90,300))+
  xlim(c(90,300))+
  #scale_shape_manual("Lake:", values = c(15,16,17,18))+
  #scale_shape_manual("Lake:", values = c(21,22,23,24,25))+
  scale_fill_viridis_d("Lake:", direction = -1)+
  #scale_color_viridis_d("Lake:", direction = 1, option = 'plasma')+
  ylab("Ice Off - MODIS [day]")+
  xlab("Ice Off - In Situ [day]")+
  theme(
    text = element_text(size = 20),
    legend.position = "bottom"
  )+
ggtitle('Cloud Uncertainty Req & 15-day: MAE = 23.20 days, Bias = 8.74 days, n = 91')
one_to_one_iceoff
ggplotly(one_to_one_iceoff)
ggsave(plot = one_to_one_iceoff, here("output/terra_v_merge/merge_one_to_one_iceOff_test_w_cloud_uncertainty_15day_4.1.jpeg"), dpi = 500, width = 15, height = 12, units = "in")

#comparison plots

#ice on
ice_on_comparison_plt <- ggplot(data = ice_on_full)+
  # geom_line(aes(x = full_merge, y = ice_on_remote_yday, color = "Remote"), size = 2)+
  # geom_line(aes(x = ice_on_insitu, y = ice_on_insitu_yday, color = "In Situ"), size = 2)+
  # geom_point(aes(x = full_merge, y = ice_on_remote_yday, color = "Remote"), size = 5)+
  # geom_point(aes(x = ice_on_insitu, y = ice_on_insitu_yday, color = "In Situ"), size = 5)+
  geom_line(aes(x = date_ice_on, y = yday_remote_w_uncert, color = "Remote"), size = 2)+
  geom_line(aes(x = ice_on_insitu, y = yday_insitu_october, color = "In Situ"), size = 2)+
  geom_point(aes(x = date_ice_on, y = yday_remote_w_uncert, color = "Remote"), size = 5)+
  geom_point(aes(x = ice_on_insitu, y = yday_insitu_october, color = "In Situ"), size = 5)+
  theme_classic()+
  facet_wrap(~lakename)+
  scale_colour_viridis_d("", begin = 0.3, end = 0.75)+
  ylab("Ice On [day]")+
  xlab("Year")+
  theme(
    text = element_text(size = 25),
    legend.position = "bottom",
    panel.spacing.x = unit(8, "mm"),
    axis.text.x = element_text(angle = 45, vjust = 0.7)
  )
ice_on_comparison_plt 

ggsave(plot = ice_on_comparison_plt, here("output/figs/ice_on_comparison_plt.jpeg"), dpi = 700, width = 15, height = 10, units = "in")

#ice_off  
ice_off_comparison_plt <- ggplot(data = ice_off_full)+
  # geom_line(aes(x = frac_31day_med, y = ice_off_remote_yday, color = "Remote"), size = 2)+
  # geom_line(aes(x = ice_off_insitu, y = ice_off_insitu_yday, color = "In Situ"), size = 2)+
  # geom_point(aes(x = frac_31day_med, y = ice_off_remote_yday, color = "Remote"), size = 5)+
  # geom_point(aes(x = ice_off_insitu, y = ice_off_insitu_yday, color = "In Situ"), size = 5)+
  geom_line(aes(x = date_ice_off, y = yday_remote_w_uncert, color = "Remote"), size = 2)+ #was ice_off_insitu
  geom_line(aes(x = ice_off_insitu, y = yday_insitu_october, color = "In Situ"), size = 2)+
  geom_point(aes(x = date_ice_off, y = yday_remote_w_uncert, color = "Remote"), size = 5)+
  geom_point(aes(x = ice_off_insitu, y = yday_insitu_october, color = "In Situ"), size = 5)+
  theme_classic()+
  facet_wrap(~lakename)+
  scale_colour_viridis_d("", begin = 0.3, end = 0.75)+
  ylab("Ice Off [day]")+
  xlab("Year")+
  theme(
    text = element_text(size = 25),
    legend.position = "bottom",
    panel.spacing.x = unit(8, "mm"),
    axis.text.x = element_text(angle = 45, vjust = 0.7)
  )
ice_off_comparison_plt

ggsave(plot = ice_off_comparison_plt, here("output/figs/ice_off_comparison_plt.jpeg"), dpi = 700, width = 15, height = 10, units = "in")

#difference plot
ice_on_diff_plt <- ggplot(data = ice_on)+
  geom_point(aes(x = ice_on_insitu, y = diff_date), size = 5)+
  geom_line(aes(x = ice_on_insitu, y = diff_date), size = 1.5)+
  facet_wrap(~lakename, ncol = 1)+
  geom_hline(yintercept = 0)+
  theme_classic()+
  ylab("Date Difference [days]")+
  xlab("Year")+
  theme(
    text = element_text(size = 25)
  )

ice_off_diff_plt <- ggplot()+
  geom_point(data = ice_off, aes(x = ice_off_insitu, y = diff_date, color = "Ice Off"), size = 5)+
  geom_line(data = ice_off, aes(x = ice_off_insitu, y = diff_date, color = "Ice Off"), size = 1.5)+
  geom_point(data = ice_on, aes(x = ice_on_insitu, y = diff_date, color = "Ice On"), size = 5)+
  geom_line(data = ice_on, aes(x = ice_on_insitu, y = diff_date, color = "Ice On"), size = 1.5)+
  facet_wrap(~lakename, ncol= 1, scales = 'free')+
  geom_hline(yintercept = 0)+
  theme_classic()+
  ylab("Date Difference [days]")+
  xlab("Year")+
  scale_color_viridis_d("", begin = 0.3, end = 0.75)+
  theme(
    text = element_text(size = 25),
    legend.position = "bottom"
  )
 
ggsave(plot = ice_off_diff_plt, here("output/figs/diff_plt_on_off.jpeg"), dpi = 700, width = 15, height = 9, units = "in") 

