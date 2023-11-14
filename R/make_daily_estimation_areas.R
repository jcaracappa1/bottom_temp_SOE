#Function to create a daily file with a mask for cells over/under a threshold temperature

data.file = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_1993-01-01_2003-12-31.nc')
temp.cat = data.frame(
  group = c('normal','stressed','danger'),
  min.temp = c(0,17,18),
  max.temp = c(17,18,30)
)
min.year = 1993
max.year = 2003
out.dir = here::here('data','gridded_daily_estimation_areas','/')

make_daily_threshold = function(data.file,min.lat,max.lat,min.lon,max.lon,min.z,max.z,min.temp, max.temp,min.year,max.year,data.out){
  
  library(terra)
  
  data = rast(data.file,subds = 'depth')
  data.time = time(data)
  data.year =as.numeric(format(data.time,format = '%Y'))
  data.time.sub = subset(data,which(data.year >= min.year & data.year <= max.year))
  
  area.shp = project(vect(here::here('geometry','MAB_ESTIMATION_AREAS_2023_UTM18_PDT_NYB.shp')),' +proj=longlat +datum=WGS84 +no_defs ')
  
  area.names = area.shp$SAMS
  
  i=1
  for(i in 1:length(area.names)){
    
    sub.area = area.shp[i,]
    
    data.subarea = mask(crop(data.time.sub,sub.area),sub.area)
    
    j =1
    for(j in 1:nrow(temp.cat)){
      data.temp.group = clamp(data.subarea,lower = temp.cat$min.temp[j], upper = temp.cat$max.temp[j], values = F)  
      
      time(data.temp.group) = time(data.time.sub)
      out.name = paste0('gridded_bottomT_',area.names[i],'_',temp.cat$group[j],'.nc')
      
      writeCDF(data.temp.group,paste0(out.dir,out.name),varname = "BottomT",overwrite = T,zname = 'time')
    }
  }
}

