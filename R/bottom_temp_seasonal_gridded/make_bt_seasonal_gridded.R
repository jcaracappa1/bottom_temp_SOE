# make SOE indicator "bt_seasonal_gridded"
# output specs: netCDF with layers for seasons, year is timevar
# Run 'R/crop_data_season.R' first
library(terra)

roms.dir = here::here('data','ROMS','ROMS_daily_season','/')
glorys.dir = here::here('data','GLORYS','GLORYS_daily_season','/')
psy.dir = here::here('data','PSY','PSY_daily_season','/')

out.dir = here::here('data','SOE','bt_seasonal_gridded','/')

season.names = c('winter','spring','fall','summer')

i=1

data.ls = list()
for(i in 1:length(season.names)){
  
  if(!dir.exists(out.dir)) {dir.create(out.dir)}
  
  roms.files = list.files(roms.dir,pattern = season.names[i])
  roms.data = lapply(paste0(roms.dir,roms.files),rast)
  roms.data = do.call(c,roms.data)
  
  glorys.files = list.files(glorys.dir,pattern = season.names[i])
  glorys.data = lapply(paste0(glorys.dir,glorys.files),rast)
  glorys.data = do.call(c,glorys.data)
  
  psy.files = list.files(psy.dir,season.names[i])
  psy.data = lapply(paste0(psy.dir,psy.files),rast)
  psy.data = do.call(c,psy.data)
  
  roms.data.match = resample(roms.data,glorys.data)
  roms.data.match = mask(crop(roms.data.match,glorys.data),glorys.data)
  time(roms.data.match) = time(roms.data)
  
  data.all = c(roms.data.match,glorys.data,psy.data)
  
  time.all = time(data.all)
  time.all.y = as.numeric(format(time.all, format = '%Y'))
  
  years = min(time.all.y):max(time.all.y)

  j=1
  data.year.ls = list()
  for(j in 1:length(years)){
    
    which.year = which(time.all.y==years[j])
    
    data.year= mean(subset(data.all,which.year),na.rm=T)
    # names(data.year) <- years[j]
    time(data.year) <- as.Date(paste0(as.numeric(years[j]),'-01-01'))
    data.year.ls[[j]] = data.year
  }
  
  data.season = do.call(c,data.year.ls)
  # data.ls[[i]] = data.season  
  
  writeCDF(data.season,paste0(out.dir,'bt_seasonal_gridded_',season.names[i],'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  
}

# data.all = sds(data.ls)
# names(data.all) = season.names
# longnames(data.all) = season.names
# units(data.all) = rep('C',4)
# 
# writeCDF(data.all,paste0(out.dir,'bt_seasonal_gridded.nc'),overwrite =T)
