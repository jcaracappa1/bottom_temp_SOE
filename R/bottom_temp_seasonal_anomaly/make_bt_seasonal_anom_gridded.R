#creates ecodata "bt_seasonal_anom_gridded"
#requires "make_bt_seasonal_gridded.R"
library(terra)

data.dir = here::here('data','gridded_seasonal_combined','/')

anom.dir = here::here('data','gridded_seasonal_anomaly','/')
clim.dir = here::here('data','gridded_seasonal_climatology','/')

if(!dir.exists(anom.dir)){dir.create(anom.dir)}
if(!dir.exists(clim.dir)){dir.create(clim.dir)}

season.names = c('winter','spring','fall','summer')

min.year.clim = 1993
max.year.clim = 2010

i=1
for(i in 1:length(season.names)){
  
  data.season = rast(paste0(data.dir,'bt_seasonal_gridded_',season.names[i],'.nc'))
  
  data.time = time(data.season)
  data.time.y = format(data.time,format = '%Y')
  
  which.clim = which(data.time.y %in% min.year.clim:max.year.clim)
  
  data.clim = mean(subset(data.season, which.clim))
  
  writeCDF(data.clim,paste0(clim.dir,'bt_seasonal_climatology_gridded_',season.names[i],'.nc'),overwrite = T)
  
  data.anom = data.season-data.clim
  
  writeCDF(data.anom, paste0(clim.dir,'bt_seasonal_anomaly_gridded_',season.names[i],'.nc'),overwrite = T)

}