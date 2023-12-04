#Use already downloaded PSY data to extract bottom salinity 
#Creates daily file
library(raster)
library(ncdf4)
library(tidync)

glorys.dir = 'D:/GLORYS/Data_PSY/'
file.prefix = 'GLORYS_REANALYSIS_PSY_'

out.dir = here::here('data','PSY_daily','/')

years = 2020:2022


bathy = raster('D:/GLORYS/Data_PSY/GLO-MFC_001_024_mask_bathy.nc',varname = 'deptho_lev')

i=1
for(i in 1:length(years)){
  
  year.file.names = list.files(paste0(glorys.dir,years[i],'/'), paste0(file.prefix,years[i]))
  dates = sapply(year.file.names, function(x) strsplit(x,paste0(file.prefix,'|.nc'))[[1]][2],USE.NAMES = F)
  
  data.bot.ls = list()
  j=1
  for(j in 1:length(dates)){
    
    data.raw = brick(paste0(glorys.dir,years[i],'/',year.file.names[j]),varname = 'so')
    # x = nc_open(paste0(glorys.dir,year.file.names[j]))
    
    if(j == 1 & i ==1){
      bathy.neus = mask(crop(bathy,extent(data.raw)),subset(data.raw,1))-1
    } 
    
    data.bot.ls[[j]] = stackSelect(data.raw,bathy.neus)
    # plot(data.bot.ls[[j]])
    
    closeAllConnections()
  }
  
  closeAllConnections()
  gc()
  
  data.year = stack(data.bot.ls)
  data.year = terra::rast(data.year)
  terra::time(data.year) = as.Date(dates)
  
  file.name = paste0('PSY_daily_BottomSalinity_',years[i],'.nc')
  terra::writeCDF(data.year,paste0(out.dir,file.name),varname = 'BottomS',overwrite =T,zname = 'time')
  
}
