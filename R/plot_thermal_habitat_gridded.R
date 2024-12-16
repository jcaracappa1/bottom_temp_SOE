library(terra)
library(ggplot2)
library(ggplot2)
library(mapdata)
library(dplyr)


plot_thermal_habitat_gridded = function(year, threshold, depth.min,depth.max){
  
  threshold.char =  gsub('\\.','_',threshold)
  depth.vect = rast(here::here('data','SOE','thermal_habitat_gridded_v2','GLORYS_depth_NEUS.nc'))
  depth.clamp = clamp(depth.vect,lower = depth.min, upper = depth.max)
  epu.vect = vect(here::here('geometry','EPU_NOESTUARIES.shp'))
  
  neus.map = map_data('worldHires',region = c('USA','Canada'))
  
  yt.combs = expand.grid(year=year,threshold.char=threshold.char,stringsAsFactors = F)
  
    i=1
    data.ls = list()
  for(i in 1:nrow(yt.combs)){
    
   data.rast = rast(here::here('data','SOE','thermal_habitat_gridded_v2',paste0('thermal_habitat_gridded_',yt.combs$year[i],'_SOE2025.nc')),subds = paste0('nday_',yt.combs$threshold.char[i]))
   
   data.ls[[i]] = as.data.frame(data.rast,xy = T)%>%
     rename('Value' = starts_with('nday_'))%>%
     mutate(year = yt.combs$year[i],
            threshold = yt.combs$threshold.char[i])
  }
  
  data.all = bind_rows(data.ls)

  
  ggplot()+
    # ggplot2::geom_sf(data=ecodata::coast,) +
    geom_tile(data =data.all, aes(x=x,y=y,fill = Value),width = 1/12, height = 1/12)+
    annotation_map(neus.map,fill = 'grey90',color = 'black')+
    scale_fill_viridis_c()+
    coord_equal()+
  facet_wrap(year~threshold)
  ggsave(here::here('Figures','Data_Review.png'),width =12, height = 9)
}
 plot_thermal_habitat_gridded(year = 2024,
                              threshold = c(15,18,24),
                              depth.min = 0,
                              depth.max = 100)

 