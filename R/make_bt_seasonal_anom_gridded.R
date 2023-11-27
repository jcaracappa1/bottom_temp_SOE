#creates ecodata "bt_seasonal_anom_gridded"
#requires "make_bt_seasonal_gridded.R"
library(terra)

data.dir = here::here('data','gridded_seasonal_combined','/')

out.dir = here::here('data','gridded_seasonal_anomaly','/')
if(!dir.exists(out.dir)){dir.create(out.dir)}

season.names = c('winter','spring','fall','summer')

for(i in 1:length(season.names)){
  
  data.season = 
}