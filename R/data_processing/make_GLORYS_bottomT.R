#Use already downloaded GLORYS data to extract bottom salinity (This only works on R version 3.6 for some reason. Issue with terra/raster)
#Creates daily file
library(raster)
library(ncdf4)
library(tidync)

glorys.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/Daily_Bottom_Temp/'
file.prefix = 'GLORYS_REANALYSIS_'

out.dir = here::here('data','GLORYS','GLORYS_daily','/')

years = 2023

# bathy = rast('C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLO-MFC_001_030_mask_bathy.nc',subds = 'deptho_lev')
bathy = raster('C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLO-MFC_001_030_mask_bathy.nc',varname = 'deptho_lev')

is.3d =F
i=1
for(i in 1:length(years)){
  
 year.file.names = list.files(paste0(glorys.dir,years[i]), paste0(file.prefix,years[i]))
 dates = sapply(year.file.names, function(x) strsplit(x,paste0(file.prefix,'|.nc'))[[1]][2],USE.NAMES = F)
 
 data.bot.ls = list()
 j=1
 for(j in 1:length(dates)){
 
  data.raw = brick(paste0(glorys.dir,years[i],'/',year.file.names[j]),varname = 'bottomT')
  # x = nc_open(paste0(glorys.dir,year.file.names[j]))
  
  if(is.3d == T){
    if(j == 1 & i ==1){
      bathy.neus = mask(crop(bathy,extent(data.raw)),subset(data.raw,1))
    } 
    
    data.bot.ls[[j]] = stackSelect(data.raw,bathy.neus)
    # plot(data.bot.ls[[j]])
  }else{
    data.bot.ls[[j]] = data.raw
  }
  
  # plot(data.bot.ls[[j]])
  
  closeAllConnections()
  }
 
 closeAllConnections()
 gc()
 
 data.year = stack(data.bot.ls)
 data.year = terra::rast(data.year)
 terra::time(data.year) = as.Date(dates)
 
 file.name = paste0('GLORYS_daily_BottomTemp_',years[i],'.nc')
 terra::writeCDF(data.year,paste0(out.dir,file.name),varname = 'BottomT',overwrite =T,zname = 'time')
 
}
