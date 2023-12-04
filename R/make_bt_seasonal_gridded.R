# make SOE indicator "bt_seasonal_gridded"
# output specs: netCDF with layers for seasons, year is timevar
# Run 'R/crop_data_season.R' first
library(terra)

glorys.dir = here::here('data','GLORYS_daily_season','/')
psy.dir = here::here('data','PSY_daily_season','/')

out.dir = here::here('data','gridded_seasonal_combined','/')


season.names = c('winter','spring','fall','summer')

i=1

data.ls = list()
for(i in 1:length(season.names)){
  
  if(!dir.exists(out.dir)) {dir.create(out.dir)}
  
  glorys.files = list.files(glorys.dir,pattern = season.names[i])
  glorys.data = lapply(paste0(glorys.dir,glorys.files),rast)
  glorys.data = do.call(c,glorys.data)
  
  psy.files = list.files(psy.dir,season.names[i])
  psy.data = rast(paste0(psy.dir,psy.files))
  
  data.all = c(glorys.data,psy.data)
  
  time.all = time(data.all)
  time.all.y = as.numeric(format(time.all, format = '%Y'))
  
  years = min(time.all.y):max(time.all.y)

  j=1
  data.year.ls = list()
  for(j in 1:length(years)){
    
    which.year = which(time.all.y==years[j])
    
    data.year= mean(subset(data.all,which.year))
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
