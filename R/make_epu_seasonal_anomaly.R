#Script to make climatology (epu x season x year)
library(tidync)
library(sf)

glorys.epu.season.files = list.files(here::here('data','seasonal_gridded_GLORYS'))
psy.epu.season.files = list.files(here::here('data','seasonal_gridded_PSY'))
roms.epu.season.files = list.files(here::here('data','seasonal_gridded_ROMS'))
 
combs = expand.grid(epu = c('MAB','GOM','GB','SS'), season = c('winter','spring','fall','summer'))

data.climatology = list()
i =1 
for(i in 1:nrow(combs)){
  
  glorys.file = glorys.epu.season.files[grepl(combs$epu[i],glorys.epu.season.files)&grepl(combs$season[i],glorys.epu.season.files)]
  psy.file = psy.epu.season.files[grepl(combs$epu[i],psy.epu.season.files)&grepl(combs$season[i],psy.epu.season.files)]
  roms.file = roms.epu.season.files[grepl(combs$epu[i],roms.epu.season.files)&grepl(combs$season[i],roms.epu.season.files)]
  
  glorys.nc = tidync(here::here('data','seasonal_gridded_GLORYS',glorys.file))%>%
    hyper_tibble()%>%
    mutate(source = 'GLORYS')
  psy.nc = tidync(here::here('data','seasonal_gridded_PSY',psy.file))%>%
    hyper_tibble()%>%
    mutate(source = 'PSY')
  roms.nc = tidync(here::here('data','seasonal_gridded_ROMS',roms.file))%>%
    hyper_tibble()%>%
    mutate(source = 'ROMS')
  
  data.epu.season = bind_rows(glorys.nc,psy.nc)%>%
    bind_rows(roms.nc)%>%
    mutate(date = as.POSIXct(time,origin = '1970-01-01 00:00:00 UTC'),
           year = format(date,format = '%Y'))%>%
    group_by(source,year)%>%
    summarise(BottomT.mean = mean(BottomT,na.rm=T),
              BottomT.sd = sd(BottomT,na.rm=T),
              BottomT.median = median(BottomT,na.rm=T),
              BottomT.min = min(BottomT,na.rm=T),
              BottomT.max = max(BottomT,na.rm=T))%>%
    mutate(epu =combs$epu[i],
           season = combs$season[i])
  
  rm(glorys.nc,psy.nc,roms.nc)
  
  data.climatology[[i]] = data.epu.season %>%
    filter(year %in% 1993:2010)%>%
    group_by(epu,season)%>%
    summarise(BottomT.mean.ref = mean(BottomT.mean,na.rm=T))
  
  data.epu.season.anom = data.epu.season %>%
    left_join(data.climatology[[i]])%>%
    mutate(BottomT.mean.anomaly = BottomT.mean - BottomT.mean.ref)
  
  write.csv(data.epu.season.anom, paste(here::here('data','seasonal_anomaly',paste0('GLORYS_PSY_ROMS_BottomT_anomaly_1959_2022_',combs$epu[i],'_',combs$season[i],'.csv'))))
}

data.climatology = bind_rows(data.climatology)

write.csv(data.climatology, here::here('data','bottomT_climatolgy_1993_2020'))

