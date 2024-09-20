#Create maps of NES thresholds by year
library(terra)
library(ggplot2)
library(mapdata)

source(here::here('R','data_processing','make_temp_mask_funs.R'))

max.t.vect = c(seq(5,20,1),7.5)
# max.t.vect = 7.5
years = 1959:2023

data.dir = here::here('data','multi_threshold','mask')
out.dir = here::here('data','multi_threshold','ndays')

remake.files = F
t=y=1
for(t in 1:length(max.t.vect)){
  
  for(y in 1:length(years)){
    
    if(remake.files == F){
      if(file.exists(paste0(out.dir,'/',max.t.vect[t],'/GLORYS_NES_',max.t.vect[t],'C_ndays_',years[y],'.nc'))){
        next()
      }
    }
    data = terra::rast(paste0(data.dir,'/',max.t.vect[t],'/','GLORYS_NES_',max.t.vect[t],'C_',years[y],'.nc'))
    
    data.binary = (data * 0)+1
    
    data.binary.n = sum(data.binary,na.rm=T)
    
    file.dir = paste0(out.dir,'/',max.t.vect[t])
    if(!dir.exists(file.dir)){dir.create(file.dir)}
    
    writeCDF(data.binary.n,paste0(out.dir,'/',max.t.vect[t],'/GLORYS_NES_',max.t.vect[t],'C_ndays_',years[y],'.nc'),overwrite =T)

  }
  
}
