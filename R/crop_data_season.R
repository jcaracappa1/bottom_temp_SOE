#Script Crops GLORYS and PSY data onto EPUS

crop_data_season = function(shp.file,data.file.orig,out.prefix,out.dir){
  
  # library(sf)
  # library(raster)
  library(terra)
  
  if(!dir.exists(out.dir)){dir.create(out.dir)}
  
  epu.area = vect(shp.file)
  
  data.orig = rast(data.file.orig)
  
  #Remove the extras
  data.epu = terra::crop(data.orig,epu.area)
  data.epu = terra::mask(data.epu,epu.area)
  
  time(data.epu) <- time(data.orig)
  
  var.time = time(data.epu)
  var.time.m = format(var.time,format = '%m') %>% as.numeric()
  
  var.time.winter = which(var.time.m %in% c(1,2,3))
  var.time.spring = which(var.time.m %in% c(4,5,6))
  var.time.summer = which(var.time.m %in% c(7,8,9))
  var.time.fall = which(var.time.m %in% c(10,11,12))
  
  var.winter = subset(data.epu,var.time.winter)
  var.spring = subset(data.epu,var.time.spring)
  var.summer = subset(data.epu,var.time.summer)
  var.fall = subset(data.epu,var.time.fall)
  
  writeCDF(var.winter,paste0(out.dir,out.prefix,'_winter.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(var.spring,paste0(out.dir,out.prefix,'_spring.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(var.summer,paste0(out.dir,out.prefix,'_summer.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(var.fall,paste0(out.dir,out.prefix,'_fall.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  
}

crop_data_season(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
              data.file.orig = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_1993-01-01_2003-12-31.nc'),
              out.prefix = 'GLORYS_daily_BottomTemp_1993-01-01_2003-12-31',
              out.dir = here::here('data','GLORYS_daily_season','/')
              )

crop_data_season(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
              data.file.orig = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_2004-01-01_2020-12-31.nc'),
              out.prefix = 'GLORYS_daily_BottomTemp_2004-01-01_2020-12-31',
              out.dir = here::here('data','GLORYS_daily_season','/')
              )

crop_data_season(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
              data.file.orig = here::here('data','PSY_daily','PSY_daily_BottomTemp_2020-11-01_2023-09-30.nc'),
              out.prefix = 'PSY_daily_BottomTemp_2020-11-01_2023-09-30',
              out.dir = here::here('data','PSY_daily_season','/')
              )