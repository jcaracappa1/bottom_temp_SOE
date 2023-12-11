#Function to create a daily file with a mask for cells over/under a threshold temperature

# data.file = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_1993-01-01_2003-12-31.nc')
# product = 'GLORYS'
# subarea = 'MABs'
# min.lat = 36.5
# max.lat = 41.1
# min.lon = -76.5
# max.lon = -71
# min.temp = 18
# max.temp = 30
# min.year = 1993
# max.year = 2003
# min.z = 30
# max.z = 90
# data.dir = here::here('data','daily_bottomT_window_gridded','/')

make_daily_threshold = function(data.file,product,subarea,min.lat,max.lat,min.lon,max.lon,min.z,max.z,min.temp, max.temp,min.year,max.year,data.dir){
  
  library(terra)
  
  area.ext = ext(min.lon,max.lon,min.lat,max.lat)
  
  bathy = rast(here::here('data','NEUS_bathy.nc'))
  
  bathy.crop = crop(bathy,area.ext)
  # plot(bathy.crop)
  bathy.window = terra::clamp(bathy.crop, lower = min.z, upper = max.z,values = F)
  
  writeCDF(bathy.window,here::here('data',paste0('bathymetry_min_',min.z,'_max_',max.z,'.nc')),overwrite =T,varname = 'depth')
  # plot(bathy.window)
  
  data = rast(data.file,subds = 'depth')
  
  data.time = time(data)
  data.year =as.numeric(format(data.time,format = '%Y'))
  
  data.crop = crop(data,area.ext)
  # plot(data.crop)
  
  time(data.crop) <- data.time
  data.time.sub = subset(data.crop,which(data.year >= min.year & data.year <= max.year))  
  
  # plot(subset(data.time.sub,1))
  
  data.temp.window = clamp(data.time.sub, lower = min.temp, upper = max.temp,values = F)
  # plot(subset(data.temp.window,250))
  
  ext(data.temp.window) = ext(bathy.window)
  data.temp.depth.window = mask(resample(data.temp.window,bathy.window),bathy.window)
  time(data.temp.depth.window) <- data.time
  
  # plot(subset(data.temp.depth.window,250))
  out.name = paste0(product,'_daily_Tmin_',min.temp,'_Tmax_',max.temp,'_',min.year,'_',max.year,'.nc')
  writeCDF(data.temp.depth.window,paste0(data.dir,out.name),varname = 'BottomT',overwrite =T,zname = 'time')

}

