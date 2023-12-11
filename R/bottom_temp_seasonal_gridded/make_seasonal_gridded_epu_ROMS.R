#Script to combine multiple bottom temp files and create gridded seasonal x epu netCDF files
library(ncdf4)
library(dplyr)
library(tidync)
library(terra)
library(sf)

roms.dir = here::here('data','ROMS_daily','/')

epu.shp = read_sf(here::here('geometry','EPU_NOESTUARIES.shp'))
epus = epu.shp$EPU


i=1
for(i in 1:length(epus)){
  
    epu.files = list.files(roms.dir,paste0('*_',epus[i],'.nc'),full.names = T)
                  
    
    var.winter = list()
    var.spring = list()
    var.summer = list()
    var.fall = list()
    
    winter.time = list()
    
     j=1
    for(j in 1:length(epu.files)){
      
      var.rast = rast(epu.files[j])
      
      var.time = time(var.rast)
      var.time.m = format(var.time,format = '%m') %>% as.numeric()
      
      var.time.winter = which(var.time.m %in% c(1,2,3))
      var.time.spring = which(var.time.m %in% c(4,5,6))
      var.time.summer = which(var.time.m %in% c(7,8,9))
      var.time.fall = which(var.time.m %in% c(10,11,12))
      
      var.winter[[j]] = subset(var.rast,var.time.winter)
      var.spring[[j]] = subset(var.rast,var.time.spring)
      var.summer[[j]] = subset(var.rast,var.time.summer)
      var.fall[[j]] = subset(var.rast,var.time.fall)
    }
      
    var.winter.epu = do.call(c,var.winter)
    var.spring.epu = do.call(c,var.spring)
    var.summer.epu = do.call(c,var.summer)
    var.fall.epu = do.call(c,var.fall)
    
    writeCDF(var.winter.epu,here::here('data','gridded_seasonal_epu_ROMS',paste0('ROMS_winter_',epus[i],'.nc')),varname = 'BottomT',overwrite =T,zname = 'time')
    writeCDF(var.spring.epu,here::here('data','gridded_seasonal_epu_ROMS',paste0('ROMS_spring_',epus[i],'.nc')),varname = 'BottomT',overwrite =T,zname = 'time')
    writeCDF(var.summer.epu,here::here('data','gridded_seasonal_epu_ROMS',paste0('ROMS_summer_',epus[i],'.nc')),varname = 'BottomT',overwrite =T,zname = 'time')
    writeCDF(var.fall.epu,here::here('data','gridded_seasonal_epu_ROMS',paste0('ROMS_fall_',epus[i],'.nc')),varname = 'BottomT',overwrite =T,zname = 'time')
}
    
