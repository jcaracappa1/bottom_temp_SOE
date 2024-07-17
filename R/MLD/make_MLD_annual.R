#Script to combine daily files into annual files for easier processing

library(terra)

#input data directory
data.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/Daily_MLD/'

years = 1993:2023

y = 1
for(y in 1:length(years)){

  yr.dir = paste0(data.dir,years[y],'/')  
  yr.files = list.files(yr.dir)
  
  d=1
  day.ls = list()
  for(d in 1:length(yr.files)){
    
    day.ls[[d]] = rast(paste0(yr.dir,yr.files[d]))  
    
  }
  
  yr.rast = rast(day.ls)

  time(yr.rast)
  
  out.name = paste0('GLORYS_daily_MLD_',years[y],'.nc')
  terra::writeCDF(yr.rast,here::here('data','GLORYS','GLORYS_daily',out.name),varname = 'MLD',overwrite =T,zname = 'time')
  
}


  
