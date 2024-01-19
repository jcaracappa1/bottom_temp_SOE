#Function to create a daily file with a mask for cells over/under a threshold temperature
# 
# data.orig = data.all
# epu = epu.names[e]
# min.temp = tz.combs$min.temp[i]
# max.temp = tz.combs$max.temp[i]
# min.z = tz.combs$min.z[i]
# max.z = tz.combs$max.z[i]
# shape.file =  here::here('geometry','EPU_NOESTUARIES.shp')
# out.name = new.file.name
# out.name.binary = new.file.name.binary
# out.name.area = new.file.name.area

make_thermal_mask_epu = function(data.orig,epu,min.z,max.z,min.temp, max.temp,shape.file,out.name,out.name.binary,out.name.area){
  
  library(terra)
  
  epu.shp = vect(shape.file)
  epu.ext =subset(epu.shp,epu.shp$EPU == epu)
  
  bathy = rast(here::here('data','bathymetry','GLO-MFC_001_024_mask_bathy.nc'),subds = 'deptho')
  
  bathy.crop = crop(bathy,epu.ext)
  bathy.mask = mask(bathy.crop,epu.ext, touches =F)
  # plot(bathy.mask)
  bathy.window = terra::clamp(bathy.mask, lower = min.z, upper = max.z,values = F)
  
  # plot(bathy.window)
  
  writeCDF(bathy.window,out.name.area,overwrite =T,varname = 'depth')
  
  
  data.time = time(data.orig)
  
  data.re = resample(data.orig,bathy.window)
  data.mask = mask(data.re,bathy.window)
  # plot(data.mask)
  
  time(data.mask) <- data.time
  
  data.temp = clamp(data.mask, lower = min.temp, upper = max.temp,values = F)
  # plot(subset(data.temp,250))
  
  time(data.temp) <- data.time
  
  # plot(subset(data.temp.depth,200))
  # plot(subset(data.temp,200))
  writeCDF(data.temp,out.name ,varname = 'BottomT',overwrite =T,zname = 'time')
  
  data.temp.depth.binary = data.temp
  values(data.temp.depth.binary)[!is.na(values(data.temp.depth.binary))] = 1
  values(data.temp.depth.binary)[is.na(values(data.temp.depth.binary))] = NA
  
  #plot(data.temp.depth.binary)
  time(data.temp.depth.binary) = data.time
  
  writeCDF(data.temp.depth.binary,out.name.binary ,varname = 'BottomT',overwrite =T,zname = 'time')
}