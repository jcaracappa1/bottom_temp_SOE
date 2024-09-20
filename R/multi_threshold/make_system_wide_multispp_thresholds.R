#Script to create bottom temperature threshold maps for various temperature cutoffs
source(here::here('R','data_processing','make_temp_mask_funs.R'))

epu.file = terra::vect(here::here('geometry','EPU_NOESTUARIES.shp'))

max.t.vect = c(seq(1,20,1),7.5)
years = 1959:2023


glorys.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/'
dupont.dir= 'E:/duPontavice_bt/bt_revised_metadata_032024/'
glorys.prefix = 'GLORYS_daily_BottomTemp_'
dupont.prefix = 'bottom_temp_'

out.dir = here::here('data','multi_threshold','mask')

y=t=1
for(y in 1:length(years)){
  
  for(t in 1:length(max.t.vect)){
  
  file.dir = paste0(out.dir,'/',max.t.vect[t],'/')
  out.name = paste0('GLORYS_NES_',max.t.vect[t],'C_',years[y],'.nc')
    
  if(file.exists(paste0(file.dir,out.name))){
    next()
  }else{
    
    if(years[y] < 1993){
      data = make_temp_mask(file.in = paste0(dupont.dir,dupont.prefix,years[y],'.nc'),
                            file.shp = epu.file,
                            min.val = max.t.vect[t],
                            max.val = 100)
      time(data) = seq.Date(from = as.Date(paste0(years[y],'-01-01')),to = as.Date(paste0(years[y],'-12-31')),by = 1)
    }else{
      data = make_temp_mask(file.in = paste0(glorys.dir,glorys.prefix,years[y],'.nc'),
                            file.shp = epu.file,
                            min.val = max.t.vect[t],
                            max.val = 100)
    }
    
    if(!dir.exists(file.dir)){
      dir.create(file.dir)
    }
    
    terra::writeCDF(data,paste0(file.dir,out.name))
  }
  }
    
}