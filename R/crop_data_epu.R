#Script Crops GLORYS and PSY data onto EPUS

crop_data_epu = function(shp.file,data.file.orig,out.prefix,out.dir){
  
  # library(sf)
  # library(raster)
  library(terra)
  
  epu.area = vect(shp.file)
  
  mab.area = subset(epu.area,epu.area$EPU == 'MAB')
  gom.area = subset(epu.area,epu.area$EPU == 'GOM')
  ss.area = subset(epu.area,epu.area$EPU == 'SS')
  gb.area = subset(epu.area,epu.area$EPU == 'GB')
  
  data.orig = rast(data.file.orig)
  
  #Remove the extras
  data.mab = terra::crop(data.orig,mab.area)
  data.gom = terra::crop(data.orig,gom.area)
  data.gb = terra::crop(data.orig,gb.area)
  data.ss = terra::crop(data.orig,ss.area)
  
  data.mab = terra::mask(data.mab,mab.area)
  data.gom = terra::mask(data.gom, gom.area)
  data.gb = terra::mask(data.gb,gb.area)
  data.ss = terra::mask(data.ss,ss.area)
  
  time(data.mab) <- time(data.orig)
  time(data.gom) <- time(data.orig)
  time(data.gb) <- time(data.orig)
  time(data.ss) <- time(data.orig)
  
  writeCDF(data.mab,paste0(out.dir,out.prefix,'_MAB.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(data.gom,paste0(out.dir,out.prefix,'_GOM.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(data.gb,paste0(out.dir,out.prefix,'_GB.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  writeCDF(data.ss,paste0(out.dir,out.prefix,'_SS.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  
  # writeRaster(data.mab,paste0(out.dir,out.prefix,'_MAB.nc'),overwrite =T,varname = 'BottomT',format = 'CDF',xname = 'Longitude',yname = 'Latitude',zname = 'Day')
  # writeRaster(data.gom,paste0(out.dir,out.prefix,'_GOM.nc'),overwrite =T,varname = 'BottomT',format = 'CDF',xname = 'Longitude',yname = 'Latitude',zname = 'Day')
  # writeRaster(data.ss,paste0(out.dir,out.prefix,'_SS.nc'),overwrite =T,varname = 'BottomT',format = 'CDF',xname = 'Longitude',yname = 'Latitude',zname = 'Day')
  # writeRaster(data.gb,paste0(out.dir,out.prefix,'_GB.nc'),overwrite =T,varname = 'BottomT',format = 'CDF',xname = 'Longitude',yname = 'Latitude',zname = 'Day')
  
}

crop_data_epu(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
              data.file.orig = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_1993-01-01_2003-12-31.nc'),
              out.prefix = 'GLORYS_daily_BottomTemp_1993-01-01_2003-12-31',
              out.dir = here::here('data','GLORYS_daily','/')
              )

crop_data_epu(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
              data.file.orig = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_2004-01-01_2020-12-31.nc'),
              out.prefix = 'GLORYS_daily_BottomTemp_2004-01-01_2020-12-31',
              out.dir = here::here('data','GLORYS_daily','/')
              )

crop_data_epu(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
              data.file.orig = here::here('data','PSY_daily','PSY_daily_BottomTemp_2020-11-01_2023-09-30.nc'),
              out.prefix = 'PSY_daily_BottomTemp_2020-11-01_2023-09-30',
              out.dir = here::here('data','PSY_daily','/')
              )

# t = ncdf4::nc_open(here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_2004-01-01_2020-12-31.nc'))
# x = ncdf4::nc_open(paste0(out.dir,out.prefix,'_EPU.nc'))
# as.POSIXct(x$dim$time$vals,origin = '1970-01-01 00:00.00 UTC')
# ncdf4::nc_close(x)
