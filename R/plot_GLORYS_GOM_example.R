
library(terra)
library(ggplot2)
library(tidyterra)
library(giscoR)

coast <- gisco_get_coastallines(resolution = 3)

# epu = vect(here::here('geometry','EPU_NOESTUARIES.shp'))
# gom = epu[c(2,3,4),]

gom = ext(-71,-67,41,45)

temp = rast(here::here('data','GLORYS','GLORYS_daily','GLORYS_daily_BottomTemp_2020.nc'))
temp = subset(temp,200)
temp.gom = crop(temp,gom)

plot(temp.gom)

ggplot()+
  geom_spatraster(data=temp.gom)+
  scale_fill_viridis_c(name = 'Temperature',na.value = 'white',option = 'A')+
  geom_sf(data = coast, fill = "grey")+
  coord_sf(
    xlim = c(-71,-67),
    ylim = c(41,44.8)
  )+
  ggtitle(time(temp))+
  theme_bw()
ggsave(here::here('Figures','GOM_GLORYS_temp.png'))  

