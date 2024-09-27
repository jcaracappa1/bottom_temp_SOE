#Script to make annual mean bottom temp by epu for SOE (part of bottom temp comp)
library(tidync)

roms.dir = here::here('data','ROMS','2024','ROMS_daily_epu','/')
glorys.dir = here::here('data','GLORYS','GLORYS_daily_epu','/')
psy.dir = here::here('data','PSY','PSY_daily_epu','/')

roms.prefix = 'bottom_temp_debiased_Roms_reg112_1959_2004_'
glorys.prefix = 'GLORYS_daily_BottomTemp_'
psy.prefix = 'PSY_daily_BottomTemp_'

roms.files = list.files(roms.dir)
glorys.files = list.files(glorys.dir,glorys.prefix)
psy.files = list.files(psy.dir,psy.prefix)


roms.data = list()
i =1
for(i in 1:length(roms.files)){
  
  file.epu = strsplit(roms.files[i],paste0(roms.prefix,'|_|.nc'))[[1]][2]
  
  roms.data[[i]] = tidync(paste0(roms.dir, roms.files[i]))%>%
    hyper_tibble()%>%
    mutate(date =  as.POSIXct(time,origin = '1970-01-01 00:00:00',tz = 'UTC'))%>%
    group_by(date)%>%
    summarise(BottomT = mean(BottomT,na.rm=T))%>%
    mutate(year = format(date, format = '%Y'))%>%
    group_by(year)%>%
    summarise(BottomT = mean(BottomT,na.rm=T))%>%
    mutate(subarea = file.epu,
           source = 'ROMS')%>%
    rename(bt_temp = 'BottomT')
}
roms.data = bind_rows(roms.data)

glorys.data = list()
for(i in 1:length(glorys.files)){
  
  file.epu = strsplit(glorys.files[i], paste0(glorys.prefix,"|_|.nc"))[[1]][2]
  
  glorys.data[[i]] =tidync(paste0(glorys.dir,glorys.files[i]))%>%
    hyper_tibble()%>%
    mutate(date =  as.POSIXct(time,origin = '1970-01-01 00:00:00',tz = 'UTC'))%>%
    group_by(date)%>%
    summarise(BottomT = mean(BottomT,na.rm=T))%>%
    mutate(year = format(date, format = '%Y'))%>%
    group_by(year)%>%
    summarise(BottomT = mean(BottomT,na.rm=T))%>%
    mutate(subarea = file.epu,
           source = 'GLORYS')%>%
    rename(bt_temp = 'BottomT')
}
glorys.data = bind_rows(glorys.data)

psy.data = list()
for(i in 1:length(psy.files)){
  
  file.epu = strsplit(psy.files[i], paste0(psy.prefix,"|_|.nc"))[[1]][2]
  
  psy.data[[i]] =tidync(paste0(psy.dir,psy.files[i]))%>%
    hyper_tibble()%>%
    mutate(date =  as.POSIXct(time,origin = '1970-01-01 00:00:00',tz = 'UTC'))%>%
    group_by(date)%>%
    summarise(BottomT = mean(BottomT,na.rm=T))%>%
    mutate(year = format(date, format = '%Y'))%>%
    group_by(year)%>%
    summarise(BottomT = mean(BottomT,na.rm=T))%>%
    mutate(subarea = file.epu,
           source = 'PSY')%>%
    rename(bt_temp = 'BottomT')
}
psy.data = bind_rows(psy.data)

data.all = bind_rows(roms.data,glorys.data,psy.data)

write.csv(data.all, here::here('data','SOE','bt_temp_annual.csv'),row.names = F)