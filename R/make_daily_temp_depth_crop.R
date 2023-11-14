#Function to create a daily file with a mask for cells over/under a threshold temperature

# data.file = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_1993-01-01_2003-12-31.nc')
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
# data.out = here::here('data','daily_bottomT_window_gridded','GLORYS_min18_max30_1993_2003_daily.nc')

make_daily_threshold = function(data.file,min.lat,max.lat,min.lon,max.lon,min.z,max.z,min.temp, max.temp,min.year,max.year,data.out){
  
  library(terra)
  
  area.ext = ext(min.lon,max.lon,min.lat,max.lat)
  
  bathy = rast(here::here('data','NEUS_bathy.nc'))
  
  bathy.crop = crop(bathy,area.ext)
  # plot(bathy.crop)
  bathy.window = terra::clamp(bathy.crop, lower = min.z, upper = max.z,values = F)
  
  writeCDF(bathy.window,here::here('data',paste0('bathymetry_min_',min.z,'_max_',max.z,'.nc')),varname = 'depth')
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
  writeCDF(data.temp.depth.window,data.out,varname = 'BottomT',overwrite =T,zname = 'time')

}

# area = vect(here::here('geometry','EPU_NOESTUARIES.shp'))

make_daily_threshold(data.file = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_1993-01-01_2003-12-31.nc'),
                     min.lat = 36.5,
                     max.lat = 41.1,
                     min.lon = -76.5,
                     max.lon = -71,
                     min.temp = 18,
                     max.temp = 30,
                     min.year = 1993,
                     max.year = 2003,
                     min.z = 30,
                     max.z = 90,
                      data.out = here::here('data','daily_bottomT_window_gridded','GLORYS_min18_max30_1993_2003_daily.nc')
                      )

make_daily_threshold(data.file = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_2004-01-01_2020-12-31.nc'),
                     min.lat = 36.5,
                     max.lat = 41.1,
                     min.lon = -76.5,
                     max.lon = -71,
                     min.temp = 18,
                     max.temp = 30,
                     min.year = 2004,
                     max.year = 2020,
                     min.z = 30,
                     max.z = 90,
                      data.out = here::here('data','daily_bottomT_window_gridded','GLORYS_min18_max30_2004_2020_daily.nc')
                      )

make_daily_threshold(data.file = here::here('data','PSY_daily','PSY_daily_BottomTemp_2020-11-01_2023-09-30.nc'),
                     min.lat = 36.5,
                     max.lat = 41.1,
                     min.lon = -76.5,
                     max.lon = -71,
                     min.temp = 18,
                     max.temp = 30,
                     min.year = 2020,
                     max.year = 2023,
                     min.z = 30,
                     max.z = 90,
                      data.out = here::here('data','daily_bottomT_window_gridded','PSY_min18_max30_2021_2022_daily.nc')
                      )

