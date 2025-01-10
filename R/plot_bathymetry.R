
library(terra)
library(ggplot2)
library(tidyterra)
library(giscoR)

coast <- gisco_get_coastallines(resolution = 3)

# epu = vect(here::here('geometry','EPU_NOESTUARIES.shp'))
# gom = epu[c(2,3,4),]

gom = ext(-80,-67,34,42)

bathy = rast(here::here('data','bathymetry','GLO-MFC_001_024_mask_bathy.nc'),subds = 'deptho')
bathy.clamp = clamp(bathy,lower = 0, upper = 100,value = F)
bathy.gom = crop(bathy.clamp,gom)

plot(bathy.gom)

ggplot()+
  geom_spatraster(data=bathy.clamp)+
  scale_fill_viridis_c(na.value = 'white', name = 'Depth')+
  geom_sf(data = coast, fill = "grey")+
  coord_sf(
    xlim = c(-80,-67),
    ylim = c(34,42)
  )+
  theme_bw()
ggsave(here::here('Figures','MAB_GLORYS_bathymetry.png'))  

