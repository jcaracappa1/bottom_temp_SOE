#Crop debiased ROMS data (need raw file first)
#Had different structure than GLORYS/PSY files (daily x year)

library(terra)

roms.file = here::here('data','bottom_temp_debiased_Roms_reg112_1959_2004.nc')
shp.file = here::here('geometry','EPU_NOESTUARIES.shp')

out.prefix = 'bottom_temp_debiased_Roms_reg112_1959_2004'
out.dir = here::here('data','ROMS_daily','/')

start.year = 1959
end.year = 1992

years = start.year:end.year

epu.area = sf::read_sf(shp.file)

mab.area = subset(epu.area,epu.area$EPU == 'MAB')
gom.area = subset(epu.area,epu.area$EPU == 'GOM')
ss.area = subset(epu.area,epu.area$EPU == 'SS')
gb.area = subset(epu.area,epu.area$EPU == 'GB')

data.mab.ls = list()
data.gom.ls = list()
data.gb.ls = list()
data.ss.ls = list()

i =1
for(i in 1:length(years)){
  
  data.orig = raster::brick(roms.file,lvar = 4,level = i)
  
  start.date = as.POSIXct(paste0(years[i],'-01-01 00:00:00'),tz = 'UTC')
  end.date = as.POSIXct(paste0(years[i],'-12-31 00:00:00'),tz = 'UTC')
  
  new.dates = as.character(seq.Date(as.Date(start.date),as.Date(end.date),'day'))
  new.dates = as.POSIXct(new.dates,origin = '1970-01-01 00:00:00',tz = 'UTC')
  
  #Remove the extras
  data.mab = terra::crop(data.orig,mab.area)
  data.gom = terra::crop(data.orig,gom.area)
  data.gb = terra::crop(data.orig,gb.area)
  data.ss = terra::crop(data.orig,ss.area)
  
  data.mab = terra::mask(data.mab,mab.area)
  data.gom = terra::mask(data.gom, gom.area)
  data.gb = terra::mask(data.gb,gb.area)
  data.ss = terra::mask(data.ss,ss.area)
  
  data.mab = rast(data.mab)
  data.gom = rast(data.gom)
  data.gb = rast(data.gb)
  data.ss = rast(data.ss)
  
  if(length(new.dates)!= 366){new.dates = c(new.dates,NA)}
  
  time(data.mab) <- new.dates
  time(data.gom) <- new.dates
  time(data.gb) <- new.dates
  time(data.ss) <- new.dates
  
  data.mab.ls[i] = data.mab
  data.gom.ls[i] = data.gom
  data.gb.ls[i] = data.gb
  data.ss.ls[i] = data.ss
  
}

data.mab.all = do.call(c,data.mab.ls)
data.gom.all = do.call(c,data.gom.ls)
data.gb.all = do.call(c,data.gb.ls)
data.ss.all = do.call(c,data.ss.ls)

writeCDF(data.mab.all,paste0(out.dir,out.prefix,'_MAB.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
writeCDF(data.gom.all,paste0(out.dir,out.prefix,'_GOM.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
writeCDF(data.gb.all,paste0(out.dir,out.prefix,'_GB.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
writeCDF(data.ss.all,paste0(out.dir,out.prefix,'_SS.nc'),varname = 'BottomT',overwrite =T,zname = 'time')







