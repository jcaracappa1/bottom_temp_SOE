#Script to make climatology (epu x season x year)
library(tidync)
library(sf)

roms.dir = here::here('data','gridded_bottom_temp','gridded_seasonal_epu_ROMS','/')
glorys.dir =here::here('data','gridded_bottom_temp','gridded_seasonal_epu_GLORYS','/')
psy.dir = here::here('data','gridded_bottom_temp','gridded_seasonal_epu_PSY','/')

roms.epu.season.files = list.files(roms.dir)
glorys.epu.season.files = list.files(glorys.dir)
psy.epu.season.files = list.files(psy.dir)

combs = expand.grid(epu = c('MAB','GOM','GB','SS'), season = c('winter','spring','fall','summer'))

data.climatology = list()
i =1 
for(i in 1:nrow(combs)){
  
  roms.file = roms.epu.season.files[grepl(combs$epu[i],roms.epu.season.files)&grepl(combs$season[i],roms.epu.season.files)]
  glorys.file = glorys.epu.season.files[grepl(combs$epu[i],glorys.epu.season.files)&grepl(combs$season[i],glorys.epu.season.files)]
  psy.file = psy.epu.season.files[grepl(combs$epu[i],psy.epu.season.files)&grepl(combs$season[i],psy.epu.season.files)]
  
  roms.nc = tidync(paste0(roms.dir,roms.file))%>%
    hyper_tibble()%>%
    mutate(source = 'ROMS')
  glorys.nc = tidync(paste0(glorys.dir,glorys.file))%>%
    hyper_tibble()%>%
    mutate(source = 'GLORYS')
  psy.nc = tidync(paste0(psy.dir,psy.file))%>%
    hyper_tibble()%>%
    mutate(source = 'PSY')
  
  data.epu.season = bind_rows(roms.nc,glorys.nc,psy.nc)%>%
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
  
  data.climatology[[i]] = data.epu.season %>%
    filter(year %in% 1981:2010)%>%
    group_by(epu,season)%>%
    summarise(BottomT.mean.ref = mean(BottomT.mean,na.rm=T))
  
  data.epu.season.anom = data.epu.season %>%
    left_join(data.climatology[[i]])%>%
    mutate(BottomT.mean.anomaly = BottomT.mean - BottomT.mean.ref)
  
  write.csv(data.epu.season.anom, paste(here::here('data','seasonal_raw',paste0('GLORYS_PSY_',combs$epu[i],'_',combs$season[i],'.csv'))))
}

data.climatology = bind_rows(data.climatology)

write.csv(data.climatology, here::here('data','bottomT_climatolgy_1981_2020.csv'))
