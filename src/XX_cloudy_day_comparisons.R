## The purpose of this script is to determine the influence of cloudy pixels
## on the ice on and ice off 

# Import libraries--------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(plotly)
library(zoo)
library(here)
library(janitor)
library(padr)
library(viridis)

# 1. Import data----------------------------------------------------------------

# import raw remote data from GEE

all_remote <- read_csv(here('data/combined/modis_allLakes_output_Terra_aqua_merged_raw.csv'))

# 2. Merge Aqua and Terra data--------------------------------------------------

# 2a. Separate Aqua and Terra data for merge------------------------------------

all_remote <- all_remote %>%
  #improves column names from GEE output
  clean_names() %>%
  #Focusing on these lakes for now
  filter(lakename %in% c("Albion", "Castle", "Lunz", "Morskie_Oko", "Silver"))


cloudy_percent <- all_remote %>%
  filter(
    cloud_modis <= 0.4
  ) #eliminating cloud_modis <=0.1 gives ~23% usable images; cloud_modis <= 0.7 gives ~27% usable images


#Use summarise() to get average cloud_modis value per month (across 20 years of data)
cloudy_days <- all_remote %>%
  mutate(
    month = month(date),
    year = year(date)
  ) %>%
  group_by(lakename, month, source) %>%
  summarise(
    cloudy_avg = mean(cloud_modis, na.rm = TRUE)
  )


#Use summarize to get 




cloudy_days_by_sensor_plt <- ggplot(data = cloudy_days, mapping = aes(x = month, y = cloudy_avg))+
  geom_line(aes(color = source), size = 1.5)+
  theme_classic()+
  ggtitle('Mean Cloudy Days by Month')+
  ylab('Mean Cloudy Surface Area')+
  xlab('Months')+
  labs(color = 'Satellite:')+
  facet_wrap(~lakename)+
  scale_color_manual(values = c("#56B4E9", "#009E73"))+
  theme(
    legend.position = 'bottom',
    text = element_text(size = 25)
  )+
  scale_x_continuous(breaks = seq(1,12, by = 1))
cloudy_days_by_sensor_plt

ggsave(plot = cloudy_days_by_sensor_plt, filename = here('output/cloudy_days_by_sensor_plt.png'), dpi = 500, height = 10, width = 15)


cloudy_boxplt <- ggplot(all_remote, mapping = aes(x = as.factor(month(date)), y = cloud_modis, fill = month(date))) +
  geom_boxplot()+
  scale_fill_viridis( alpha=0.6) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_classic()+ 
  ggtitle('Cloudy Days by Month')+
  ylab('Mean Cloudy Surface Area')+
  xlab('')+
  labs(fill = 'Month:')+
  facet_wrap(~lakename)+
  scale_color_manual(values = c("#56B4E9", "#009E73"))+
  theme(
    legend.position = 'bottom',
    text = element_text(size = 25)
  )
cloudy_boxplt

ggsave(plot = cloudy_boxplt, filename = here('output/cloudy_boxplt.png'), dpi = 500, height = 10, width = 15)

all_remote %>%
  filter(lakename == 'Castle' & year(date) == 2020) %>%
ggplot(all_remote, mapping = aes(x = yday(date), y = cloud_modis))+
  geom_line()+
  facet_wrap(~year(date))


#use this code chunk to cross reference a particular lake and water year against Sentinel images on Sentiel Hub EO Browser
sentinel_test <- all_remote_w_median %>%
  filter(median_val == 'full_merge' & lakename == 'albion' & water_year == 2020)

