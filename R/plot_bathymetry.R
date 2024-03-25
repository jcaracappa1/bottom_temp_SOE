
library(terra)
library(ggplot2)
library(tidyterra)
library(giscoR)

coast <- gisco_get_coastallines(resolution = 3)

# epu = vect(here::here('geometry','EPU_NOESTUARIES.shp'))
# gom = epu[c(2,3,4),]

gom = ext(-71,-67,41,45)

bathy = rast(here::here('data','bathymetry','GLO-MFC_001_024_mask_bathy.nc'),subds = 'deptho')
bathy.gom = crop(bathy,gom)

plot(bathy.gom)

ggplot()+
  geom_spatraster(data=bathy.gom)+
  scale_fill_viridis_c(na.value = 'white', name = 'Depth')+
  geom_sf(data = coast, fill = "grey")+
  coord_sf(
    xlim = c(-71,-67),
    ylim = c(41,44.8)
  )+
  theme_bw()
ggsave(here::here('Figures','GOM_GLORYS_bathymetry.png'))  

