#Create datafile to be sent to SOE for
#1) thermal_habitat_frequency
#2) thermal_habitat_area
library(tidync)

tz.combs = read.csv(here::here('data','thermal_area','tz_groups.csv'),as.is = T)

#1) thermal_habitat_frequency: the number of days per year where a grid cell experiences temperatures above a threshold
freq.dir = here::here('data','thermal_area','gridded_cell_frequency','/')
freq.files = list.files(freq.dir)
freq.prefix = 'annual_day_count_'

freq.file.names = list.files(freq.dir)
freq.file.source = sapply(freq.file.names,function(x) strsplit(x,paste0(freq.prefix,'|_|.nc'))[[1]][2],USE.NAMES = F)
freq.file.epu = sapply(freq.file.names,function(x) strsplit(x,paste0(freq.prefix,'|_|.nc'))[[1]][3],USE.NAMES = F)
freq.file.temp = sapply(freq.file.names,function(x) strsplit(x,paste0(freq.prefix,'|_|.nc'))[[1]][4],USE.NAMES = F)
freq.file.z = sapply(freq.file.names,function(x) strsplit(x,paste0(freq.prefix,'|_|.nc'))[[1]][5],USE.NAMES = F)


freq.data.ls = list()

i =1

for(i in 1:length(freq.files)){
  
  z.group = strsplit(as.character(freq.file.z[i]),'m')[[1]][1] %>% as.numeric()
  z.min = tz.combs$min.z[which(tz.combs$max.z == z.group)][1]
  z.max = tz.combs$max.z[which(tz.combs$max.z == z.group)][1]
  
  t.group = strsplit(as.character(freq.file.temp[i]),'deg')[[1]][1] %>% as.numeric()
  t.min = tz.combs$min.temp[which(tz.combs$min.temp == t.group)][1]
  
  freq.data.ls[[i]] = tidync(paste0(freq.dir,freq.files[i]))  %>%
    hyper_tibble()%>%
    mutate(source = freq.file.source[i],
           year = (time-1)+1993,
           min.depth = z.min,
           max.depth = z.max,
           temp.threshold = t.min)%>%
    select(source,year,min.depth,max.depth,temp.threshold,longitude,latitude,Ndays)
  
}
freq.data.df = bind_rows(freq.data.ls)
write.csv(freq.data.df, here::here('data','SOE','thermal_habitat_frequency_2023.csv'),row.names = F)

#2) thermal_habitat_area:  the proportion of epu x depth bin that is above temperature threshold 
area.dir = here::here('data','thermal_area','daily_area_prop','/')
area.files = list.files(area.dir)
area.prefix = 'daily_area_'

area.file.names = list.files(area.dir)
area.file.source = sapply(area.file.names,function(x) strsplit(x,paste0(area.prefix,'|_|.rds'))[[1]][2],USE.NAMES = F)
area.file.epu = sapply(area.file.names,function(x) strsplit(x,paste0(area.prefix,'|_|.rds'))[[1]][3],USE.NAMES = F)
area.file.temp = sapply(area.file.names,function(x) strsplit(x,paste0(area.prefix,'|_|.rds'))[[1]][4],USE.NAMES = F)
area.file.z = sapply(area.file.names,function(x) strsplit(x,paste0(area.prefix,'|_|.rds'))[[1]][5],USE.NAMES = F)


area.data.ls = list()
area.max.ls = list()

i =1

for(i in 1:length(area.files)){
  
  z.group = strsplit(as.character(area.file.z[i]),'m')[[1]][1] %>% as.numeric()
  z.min = tz.combs$min.z[which(tz.combs$max.z == z.group)][1]
  z.max = tz.combs$max.z[which(tz.combs$max.z == z.group)][1]
  
  t.group = strsplit(as.character(area.file.temp[i]),'deg')[[1]][1] %>% as.numeric()
  t.min = tz.combs$min.temp[which(tz.combs$min.temp == t.group)][1]
  
  area.data = readRDS(paste0(area.dir,area.files[i]))%>%
  mutate(source = area.file.source[i],
         date = as.Date(date),
         year = format(date,format = '%Y'),
         epu = area.file.epu[i],
           min.depth = z.min,
           max.depth = z.max,
           temp.threshold = t.min)%>%
    select(source,date,year,epu,min.depth,max.depth,temp.threshold,area,area.prop)
  area.data.ls[[i]] = area.data
  
  area.max.ls[[i]] = area.data %>%
    group_by(year,min.depth,max.depth,temp.threshold)%>%
    summarise(area.max = max(area.prop,na.rm=T))
  
}
area.data.df = bind_rows(area.data.ls)
area.max.df = bind_rows(area.max.ls)

write.csv(area.data.df, here::here('data','SOE','thermal_habitat_area_2023.csv'),row.names = F)
write.csv(area.max.df, here::here('data','SOE','thermal_habitat_area_max_2023.csv'),row.names = F)
