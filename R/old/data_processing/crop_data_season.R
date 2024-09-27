#Script Crops GLORYS and PSY data onto EPUS

crop_data_season = function(shp.file,in.dir,out.dir,in.prefix,out.prefix){
  
  # library(sf)
  # library(raster)
  library(terra)
  
  if(!dir.exists(out.dir)){dir.create(out.dir)}
  
  epu.area = vect(shp.file)
  
  #Get all input files from in.dir
  data.files = list.files(in.dir,in.prefix)
  
  #Loop through each annual file and subset by EPU
  i =1
  for(i in 1:length(data.files)){
    
    data.orig = rast(paste0(in.dir,data.files[i]))
    
    #Get year from filename
    file.year = as.numeric(strsplit(data.files[i],paste0(in.prefix,'|.nc'))[[1]][2])
    
    #Remove the extras
    data.epu = terra::crop(data.orig,epu.area)
    data.epu = terra::mask(data.epu,epu.area,touches = F)
    
    time(data.epu) <- time(data.orig)
    
    var.time = time(data.epu)
    var.time.m = format(var.time,format = '%m') %>% as.numeric()
    
    var.time.winter = which(var.time.m %in% c(1,2,3))
    var.time.spring = which(var.time.m %in% c(4,5,6))
    var.time.summer = which(var.time.m %in% c(7,8,9))
    var.time.fall = which(var.time.m %in% c(10,11,12))
    
    if(length(var.time.winter)>0){
      var.winter = subset(data.epu,var.time.winter)  
      writeCDF(var.winter,paste0(out.dir,out.prefix,'winter_',file.year,'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
    }
    if(length(var.time.spring)>0){
      var.spring = subset(data.epu,var.time.spring)  
      writeCDF(var.spring,paste0(out.dir,out.prefix,'spring_',file.year,'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
    }
    if(length(var.time.summer)>0){
      var.summer = subset(data.epu,var.time.summer)  
      writeCDF(var.summer,paste0(out.dir,out.prefix,'summer_',file.year,'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
    }
    if(length(var.time.fall)>0){
      var.fall = subset(data.epu,var.time.fall)  
      writeCDF(var.fall,paste0(out.dir,out.prefix,'fall_',file.year,'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
    }
  }

}
