#Script Crops GLORYS and PSY data onto EPUS

library(terra)

out.dir = here::here('data','ROMS','ROMS_daily_season','/')
out.prefix = 'ROMS_daily_BottomTemp_'
shp.file = here::here('geometry','EPU_NOESTUARIES.shp')

epu.area = epu.area = sf::read_sf(shp.file)

roms.file = here::here('data','ROMS','bottom_temp_debiased_roms_reg112_1959_2004.nc')

start.year = 1959
end.year = 1992

years = start.year:end.year

i=1
for(i in 1:length(years)){
  
  data.orig = raster::brick(roms.file,lvar = 4,level = i)
  
  start.date = as.POSIXct(paste0(years[i],'-01-01 00:00:00'),tz = 'UTC')
  end.date = as.POSIXct(paste0(years[i],'-12-31 00:00:00'),tz = 'UTC')
  
  new.dates = as.character(seq.Date(as.Date(start.date),as.Date(end.date),'day'))
  new.dates = as.POSIXct(new.dates,origin = '1970-01-01 00:00:00',tz = 'UTC')
  
  #Remove the extras
  data.crop = terra::crop(data.orig,epu.area)
  data.crop = terra::mask(data.crop,epu.area)
  
  data.crop = rast(data.crop)
  
  if(length(new.dates)!= 366){new.dates = c(new.dates,NA)}
  
  time(data.crop) <- as.Date(new.dates)
  
  var.time = time(data.crop)
  var.time.m = format(var.time,format = '%m') %>% as.numeric()
  
  var.time.winter = which(var.time.m %in% c(1,2,3))
  var.time.spring = which(var.time.m %in% c(4,5,6))
  var.time.summer = which(var.time.m %in% c(7,8,9))
  var.time.fall = which(var.time.m %in% c(10,11,12))
  
  var.winter = subset(data.crop,var.time.winter)
  var.spring = subset(data.crop,var.time.spring)
  var.summer = subset(data.crop,var.time.summer)
  var.fall = subset(data.crop,var.time.fall)
  
  writeCDF(var.winter,paste0(out.dir,out.prefix,'winter_',years[i],'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(var.spring,paste0(out.dir,out.prefix,'spring_',years[i],'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(var.summer,paste0(out.dir,out.prefix,'summer_',years[i],'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(var.fall,paste0(out.dir,out.prefix,'fall_',years[i],'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  
  
}
