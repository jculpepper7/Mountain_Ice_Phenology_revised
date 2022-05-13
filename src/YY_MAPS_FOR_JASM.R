#MAPS FOR JASM CONFERENCE


library("ggplot2")
library("sf")       
library("rnaturalearth")
library("rnaturalearthdata")

theme_set(theme_bw())

world <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf") %>% 
  select(name, continent, geometry)


#GLRIP data

glrip <- read_csv(here('data/in_situ/glrip_all_lakes.csv')) %>% 
  select(lakename, latitude, longitude, elevation) %>% 
  filter(latitude != -999) %>% 
  mutate(
    elevation = ifelse(elevation == -999, NA, elevation)
  )

glrip_unknown_elev <- read_csv(here('data/in_situ/glrip_unknown_elevations.csv')) 

glrip_full <- glrip %>% 
  full_join(glrip_unknown_elev) %>% 
  mutate(
    elevation = ifelse(is.na(elevation), mean, elevation)
  )

mountain <- read_csv(here('data/combined/all_insitu_water_year_1.csv')) %>% 
  select(lakename, latitude, longitude, elevation)


#All glrip lakes

ggplot() +
  ggplot2::geom_sf(data = world) +
  coord_sf(ylim = c(35, 85), xlim = c(-142, 151), expand = FALSE)+  
  geom_point(data = glrip_full, aes(x = longitude, y = latitude, fill = elevation), size = 3, pch =21)+
  geom_point(data = mountain, aes(x = longitude, y = latitude), size = 3, pch = 21, fill = 'red')+
  #geom_point(data = ice_on_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), stroke = 0.5, pch = 21, size = 3)+
  #scale_fill_gradient2(midpoint = mid, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_fill_viridis()+
  theme(
    legend.position = 'bottom'
  ) +
  labs(fill = 'Elevation')

#save a copy of the ice on plot
ggsave(here("output/maps/glrip_w_mtns.png"), dpi = 700, height = 15, width = 20)


#All mountain lakes

ggplot() +
  ggplot2::geom_sf(data = world) +
  coord_sf(ylim = c(35, 85), expand = FALSE)+ #xlim = c(-154, -104), 
  geom_point(data = mountain, aes(x = longitude, y = latitude, fill = elevation), size = 3, pch =21, store = 0.5)+
  #geom_point(data = ice_on_trend_plotting_data %>% filter(p.value <= 0.05), aes(x = pour_long, y = pour_lat, fill = sen), stroke = 0.5, pch = 21, size = 3)+
  #scale_fill_gradient2(midpoint = mid, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_fill_viridis()+
  theme(
    legend.position = 'bottom'
  ) 
#labs(fill = 'Ice On Trend \nSlope Magnitude')

#save a copy of the ice on plot
ggsave(here("output/maps/glrip.png"), dpi = 700, height = 15, width = 20)
