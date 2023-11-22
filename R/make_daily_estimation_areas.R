#Function to create a daily file with a mask for cells over/under a threshold temperature
library(terra)

data.files = c(here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_1993-01-01_2003-12-31.nc'),
               here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_2004-01-01_2020-12-31.nc'),
               here::here('data','PSY_daily','PSY_daily_BottomTemp_2020-11-01_2023-09-30.nc'))

temp.cat = data.frame(
  group = c('normal','stressed','danger'),
  min.temp = c(0,17,19),
  max.temp = c(17,19,30)
)

out.dir = here::here('data','gridded_daily_estimation_areas','/')
out.dir.binary = here::here('data','gridded_daily_estimation_areas_binary_mask','/')

min.year = 1993
max.year = 2022

time.ls = list()
data.ls = list()


for(k in 1:length(data.files)){
  
  data.file = rast(data.files[k],subds = 'depth')
  data.time = time(data.file)
  data.year = format(data.time,format = '%Y')
  
  data.ls[[k]] = subset(data.file,which(data.year >= min.year & data.year <= max.year))
  time.ls[[k]] = time(data.ls[[k]])
  
}

data.all = do.call('c',data.ls)
time.all = unlist(time.ls)

# data = rast(data.file,subds = 'depth')
# data.time = time(data)
# data.year =as.numeric(format(data.time,format = '%Y'))
# data.time.sub = subset(data,which(data.year >= min.year & data.year <= max.year))

area.shp = project(vect(here::here('geometry','MAB_ESTIMATION_AREAS_2023_UTM18_PDT_NYB.shp')),' +proj=longlat +datum=WGS84 +no_defs ')

area.names = area.shp$NewSAMS

i=1
for(i in 1:length(area.names)){
  
  sub.area = area.shp[i,]
  
  data.subarea = mask(crop(data.all,sub.area),sub.area)
  
  writeCDF(data.subarea, paste0(out.dir,'gridded_bottomT_binary_',area.names[i],'_raw.nc'),varname = "BottomT",overwrite = T,zname = 'time')
  j =1
  for(j in 1:nrow(temp.cat)){
    data.temp.group = clamp(data.subarea,lower = temp.cat$min.temp[j], upper = temp.cat$max.temp[j], values = F)  
    # plot(subset(data.temp.group,1)) 
    
    data.temp.group.binary = data.temp.group
    values(data.temp.group.binary)[!is.na(values(data.temp.group.binary))] = 1
    # plot(subset(data.temp.group.binary,1))
    
    time(data.temp.group) = time(data.all)
    time(data.temp.group.binary) = time(data.all)
    
    out.name = paste0('gridded_bottomT_',area.names[i],'_',temp.cat$group[j],'.nc')
    out.name.binary = paste0('gridded_bottomT_binary_',area.names[i],'_',temp.cat$group[j],'.nc')
    
    writeCDF(data.temp.group,paste0(out.dir,out.name),varname = "BottomT",overwrite = T,zname = 'time')
    writeCDF(data.temp.group.binary,paste0(out.dir.binary,out.name.binary),varname = "BottomT",overwrite = T,zname = 'time')
  }
}


