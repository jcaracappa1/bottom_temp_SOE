#Make gridded daily files split by EPU for heatwave index
library(tidync)
library(dplyr)
roms.dir = here::here('data','ROMS_daily','/')
glorys.dir = here::here('data','GLORYS_daily','/')
psy.dir = here::here('data','PSY_daily','/')

epu.names = c('GOM','GB','MAB','SS')
i=1
for(i in 1:length(epu.names)){
  
  #Do GLORYS
  glorys.files = list.files(glorys.dir,paste0('*_',epu.names[i]))
  data.glorys = list()
  j=1
  for(j in 1:length(glorys.files)){
    data.glorys[[j]] = tidync(paste0(glorys.dir,glorys.files[j]))%>%
      hyper_tibble()%>%
      mutate(source = 'GLORYS')%>%
      group_by(source,time)%>%
      summarise(BottomT.mean = mean(BottomT),
                BottomT.sd = sd(BottomT))
  }
  data.glorys = bind_rows(data.glorys)
  
  #Do PSY
  psy.file = list.files(psy.dir,paste0('*_',epu.names[i]))
  data.psy = tidync(paste0(psy.dir,psy.file))%>%
    hyper_tibble()%>%
    mutate(source = 'PSY')%>%
    group_by(source,time)%>%
    summarise(BottomT.mean = mean(BottomT),
              BottomT.sd = sd(BottomT))
  
  #Do ROMS
  roms.file = list.files(roms.dir,paste0('*_',epu.names[i]))
  data.roms = tidync(paste0(roms.dir,roms.file))%>%
    hyper_tibble()%>%
    mutate(source = 'ROMS')%>%
    group_by(source,time)%>%
    summarise(BottomT.mean = mean(BottomT),
              BottomT.sd = sd(BottomT))
  
  data.combined = data.glorys %>%
    bind_rows(data.psy)%>%
    bind_rows(data.roms)%>%
    arrange(time)%>%
    mutate(EPU = epu.names[i],
           date = as.Date(as.POSIXct(time,origin = '1970-01-01 00:00:00',tz = 'UTC')))%>%
    select(EPU,date,source,BottomT.mean,BottomT.sd)
  
  write.csv(data.combined,here::here('data','daily_epu_combined',paste0('daily_bottomT_',epu.names[i],'_1959_2022.csv')),row.names = F)
}