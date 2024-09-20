# Script that calculates the mean, median, max proportion of an area that exceeds a temperature threshold each year

library(terra)
library(dplyr)

source(here::here('R','data_processing','make_temp_mask_funs.R'))

nes.shp = terra::vect(here::here('geometry','EPU_NOESTUARIES.shp'))

data.dir = here::here('data','multi_threshold','mask')

max.t = c(seq(5,20,1),7.5)
years = 1959:2023

combs = expand.grid(years = years, max.t = max.t)

out.df.ls = list()

i=1
for(i in 1:nrow(combs)){
  
  data = terra::rast(paste0(data.dir,'/',combs$max.t[i],'/GLORYS_NES_',combs$max.t[i],'C_',combs$years[i],'.nc'))
  
  if(i == 1){
    nes.area = make_shp_area(data,nes.shp,'km')
  }
  
  data.t = time(data)
  
  # sapp(data,make_shp_temp_area,area.shp = nes.shp, units = 'km')
  data.area = sapply(1:length(data.t),function(x) make_shp_temp_area(data[[x]],area.shp = nes.shp, units = 'km'))
  
  file.df = data.frame(max.t = combs$max.t[i], years = combs$years[i], date = data.t, area = data.area, area.pct = data.area/nes.area)
 
  out.df.ls[[i]] = file.df 
}

out.df = bind_rows(out.df.ls)

saveRDS(out.df,here::here('data','multi_threshold','daily_pct_area.rds'))
