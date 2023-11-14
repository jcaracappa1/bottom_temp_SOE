#Crop bathymetry file to just Northeast US coast

get_GLORYS_NEUS_bathymetry = function(min.lat,max.lat,min.lon,max.lon){
 
  library(terra)
  
  bathy.full.file = here::here('data','GLO-MFC_001_024_mask_bathy.nc')
  
  bathy.full = terra::rast(bathy.full.file,subds = 'deptho')
  
  spat.ext =  terra::ext(c(min.lon,max.lon,min.lat,max.lat))
  
  bathy.neus = terra::crop(bathy.full,spat.ext)
  
  # plot(bathy.neus)
  
  writeCDF(bathy.neus,here::here('data','NEUS_bathy.nc'),varname = 'depth',overwrite =T)
  
}

