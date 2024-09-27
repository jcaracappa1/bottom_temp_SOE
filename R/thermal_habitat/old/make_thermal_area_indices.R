#Make indicies for thermal habitat area
# 1) number of days per cell experiencing conditions
# 2) percent of epu area experiencing conditions
library(terra)
library(tidync)
library(dplyr)

data.dir = here::here('data','gridded_bottom_temp','gridded_thermal_mask_binary','/')
area.dir = here::here('data','gridded_bottom_temp','gridded_thermal_mask_area','/')

data.prefix = 'daily_bottomT_mask_'
area.prefix = 'daily_area_mask_'

epus = c('MAB','GB','GOM','SS')

data.file.names = list.files(data.dir)
data.file.source = sapply(data.file.names,function(x) strsplit(x,paste0(data.prefix,'|_|.nc'))[[1]][2],USE.NAMES = F)
data.file.epu = sapply(data.file.names,function(x) strsplit(x,paste0(data.prefix,'|_|.nc'))[[1]][3],USE.NAMES = F)
data.file.temp = sapply(data.file.names,function(x) strsplit(x,paste0(data.prefix,'|_|.nc'))[[1]][4],USE.NAMES = F)
data.file.z = sapply(data.file.names,function(x) strsplit(x,paste0(data.prefix,'|_|.nc'))[[1]][5],USE.NAMES = F)

area.file.names = list.files(area.dir)
area.file.source = sapply(area.file.names,function(x) strsplit(x,paste0(area.prefix,'|_|.nc'))[[1]][2],USE.NAMES = F)
area.file.epu = sapply(area.file.names,function(x) strsplit(x,paste0(area.prefix,'|_|.nc'))[[1]][3],USE.NAMES = F)
area.file.temp = sapply(area.file.names,function(x) strsplit(x,paste0(area.prefix,'|_|.nc'))[[1]][4],USE.NAMES = F)
area.file.z = sapply(area.file.names,function(x) strsplit(x,paste0(area.prefix,'|_|.nc'))[[1]][5],USE.NAMES = F)

i=1
for(i in 1:length(data.file.names)){
  
  data.file = rast(paste0(data.dir,data.file.names[i]),subds = 'bottomT')
  area.file = rast(paste0(area.dir,area.file.names[i]),subds = 'bottomT')
  
  
  #plot(data.file)
  #plot(area.file)
  #plot(subset(data.file,250))
  
  data.time = time(data.file)
  data.time.yr = format(data.time,format = '%Y')%>%as.numeric()
  data.time.jul = format(data.time,format = '%j')%>% as.numeric()
  
  years = sort(unique(data.time.yr))
  
  area.tot = expanse(area.file)$area
  area.daily = list()
  area.cell = list()
  
  y=1
  for(y in 1:length(years)){
    
    data.yr = subset(data.file,which(data.time.yr == years[y]))
    data.yr.n = sum(data.yr,na.rm=T)
    names(data.yr.n) = years[y]
    area.cell[[y]] = data.yr.n
    
    # plot(data.yr.n)
    
    area.daily[[y]] = data.frame(date = time(data.yr),
                                 area = expanse(data.yr)$area)%>%
      mutate(area.prop = area/area.tot)
  }
  
  area.daily.df = bind_rows(area.daily)%>%
    mutate(source = data.file.source[i])
  area.cell.rast = do.call('c',area.cell)
  
  area.cell.out = paste0('annual_day_count_', data.file.source[i],'_',data.file.epu[i],'_',data.file.temp[i],'_',data.file.z[i],'.nc')
  writeCDF(area.cell.rast,here::here('data','thermal_area','gridded_cell_frequency',area.cell.out),varname = 'Ndays',overwrite = T)  

  area.daily.out = paste0('daily_area_', data.file.source[i],'_',data.file.epu[i],'_',data.file.temp[i],'_',data.file.z[i],'.rds')
  saveRDS(area.daily.df,here::here('data','thermal_area','daily_area_prop',area.daily.out))
  
}
