#Script to format the bottom_temp_seasonal_gridded for the soe as a flat file

library(dplyr)
library(tidync)

data.dir = here::here('data','SOE','bt_seasonal_gridded','/')

season.names  = c('winter','spring','summer','fall')
 
i=1
data.season = list()
for(i in 1:length(season.names)){
  
  season.file = list.files(data.dir,season.names[i])    
  
  data.season[[i]] = tidync(paste0(data.dir,season.file))%>%
    hyper_tibble()%>%
    mutate(date = as.POSIXct(time,origin = '1970-01-01 00:00:00',tz = 'UTC'),
           year = format(date,format = '%Y'),
           season = season.names[i])%>%
    select(year,latitude,longitude,season,BottomT)%>%
    rename(Time = 'year',
           Lat = 'latitude',
           Lon = 'longitude',
           Variable = 'season',
           Value = 'BottomT')
}

data.all = bind_rows(data.season)

write.csv(data.all,here::here('data','SOE','bt_seasonal_gridded.csv'),row.names =F)
