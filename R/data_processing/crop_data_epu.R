#Script Crops GLORYS and PSY data onto EPUS

crop_data_epu = function(shp.file,in.dir,out.dir,in.prefix,out.prefix){
  
  # library(sf)
  # library(raster)
  library(terra)
  
  epu.area = vect(shp.file)
  
  #get spatial for each epu
  mab.area = subset(epu.area,epu.area$EPU == 'MAB')
  gom.area = subset(epu.area,epu.area$EPU == 'GOM')
  ss.area = subset(epu.area,epu.area$EPU == 'SS')
  gb.area = subset(epu.area,epu.area$EPU == 'GB')
  
  data.files = list.files(in.dir,in.prefix)
  
  for(i in 1:length(data.files)){
    
    data.orig = rast(paste0(in.dir,data.files[i]))
    
    #Get year from filename
    file.year = as.numeric(strsplit(data.files[i],paste0(in.prefix,'|.nc'))[[1]][2])
    
    data.mab = terra::crop(data.orig,mab.area)
    data.gom = terra::crop(data.orig,gom.area)
    data.gb = terra::crop(data.orig,gb.area)
    data.ss = terra::crop(data.orig,ss.area)
    
    data.mab = terra::mask(data.mab,mab.area)
    data.gom = terra::mask(data.gom, gom.area)
    data.gb = terra::mask(data.gb,gb.area)
    data.ss = terra::mask(data.ss,ss.area)
    
    time(data.mab) <- time(data.orig)
    time(data.gom) <- time(data.orig)
    time(data.gb) <- time(data.orig)
    time(data.ss) <- time(data.orig)
    
    writeCDF(data.mab,paste0(out.dir,out.prefix,'MAB_',file.year,'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
    writeCDF(data.gom,paste0(out.dir,out.prefix,'GOM_',file.year,'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
    writeCDF(data.gb,paste0(out.dir,out.prefix,'GB_',file.year,'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
    writeCDF(data.ss,paste0(out.dir,out.prefix,'SS_',file.year,'.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
    
  }
}