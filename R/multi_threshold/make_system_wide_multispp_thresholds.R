#Script to create bottom temperature threshold maps for various temperature cutoffs
source(here::here('R','data_processing','make_temp_mask_funs.R'))

epu.file = terra::vect(here::here('geometry','EPU_NOESTUARIES.shp'))

max.t.vect = seq(5,20,1)
years = 1993:2023

glorys.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/'
glorys.prefix = 'GLORYS_daily_BottomTemp_'

out.dir = here::here('data','multi_threshold')

y=t=1
for(y in 1:length(years)){
  
  for(t in 1:length(max.t.vect)){
    
  data = make_temp_mask(file.in = paste0(glorys.dir,glorys.prefix,years[y],'.nc'),
                 file.shp = epu.file,
                 min.val = max.t.vect[t],
                 max.val = 100)
  
  file.dir = paste0(out.dir,'/',max.t.vect[t],'/')
  if(!dir.exists(file.dir)){
    dir.create(file.dir)
  }
  out.name = paste0('GLORYS_NES_',max.t.vect[t],'C_',years[y],'.nc')
  
  terra::writeCDF(data,paste0(file.dir,out.name))
  }
    
}