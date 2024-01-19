library(ncdf4)
library(terra)
library(tidync)
library(dplyr)

epu.shp = vect(here::here('geometry','EPU_NOESTUARIES.shp'))

depth.nc = nc_open(here::here('data','bathymetry','GLO-MFC_001_024_mask_bathy.nc'))
depth.bin = c(0,ncvar_get(depth.nc,'depth'))
depth.int = diff(depth.bin)
depth.bin.df = data.frame(depth.layer = 1:length(depth.int),depth.int)

depth.lev =rast(here::here('data','bathymetry','GLO-MFC_001_024_mask_bathy.nc'),subds = 'deptho_lev')


epu.names = c('MAB','GB','GOM')

epu.bottom.int = data.frame(epu = epu.names,depth.int = NA)
i=1
for(i in 1:length(epu.names)){
  
  epu = epu.shp[which(epu.shp$EPU == epu.names[i])]
  
  depth.lev.epu = mask(crop(depth.lev,epu),epu)
  
  epu.file = here::here('data','bathymetry',paste0(epu.names[i],'_depth_lev.nc'))
  writeCDF(depth.lev.epu,epu.file,overwrite = T,varname = 'depth_lev')
  
  depth.lev.df = tidync(epu.file)%>%
    hyper_tibble()%>%
    left_join(depth.bin.df,by = c('depth_lev' = 'depth.layer'))
    
  epu.bottom.int$depth.int[i] = mean(depth.lev.df$depth.int,na.rm=T)
}
