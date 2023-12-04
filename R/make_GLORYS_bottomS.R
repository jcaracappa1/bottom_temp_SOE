#Use already downloaded GLORYS data to extract bottom salinity 
#Creates daily file
library(raster)
library(ncdf4)
library(tidync)

glorys.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/Data/'
file.prefix = 'GLORYS_REANALYSIS_'

out.dir = here::here('data','GLORYS_daily','/')

years = 1993:2020

# bathy = rast('C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLO-MFC_001_030_mask_bathy.nc',subds = 'deptho_lev')
bathy = raster('C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLO-MFC_001_030_mask_bathy.nc',varname = 'deptho_lev')

i=1
for(i in 21:length(years)){
  
 year.file.names = list.files(glorys.dir, paste0(file.prefix,years[i]))
 dates = sapply(year.file.names, function(x) strsplit(x,paste0(file.prefix,'|.nc'))[[1]][2],USE.NAMES = F)
 
 data.bot.ls = list()
 j=1
 for(j in 1:length(dates)){
 
  data.raw = brick(paste0(glorys.dir,year.file.names[j]),varname = 'so')
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
 
 file.name = paste0('GLORYS_daily_BottomSalinity_',years[i],'.nc')
 terra::writeCDF(data.year,paste0(out.dir,file.name),varname = 'BottomS',overwrite =T,zname = 'time')
 
}
