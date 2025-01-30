#Script that generates SOE indicators:
# bottom_temp_seasonal_gridded
# bottom_temp_comp

library(dplyr)
library(ggplot2)
#Define Season Names
season.match = data.frame(season.id = 1:4, season.name = c('Winter','Spring','Summer','Fall'))

#Define GLORYS and ROMS input files

glorys.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/'
glorys.prefix = 'GLORYS_daily_BottomTemp_'
glorys.files = list.files(glorys.dir,glorys.prefix,full.names = T)
glorys.years = 1993:2024
glorys.out.dir = here::here('data','GLORYS','2025','GLORYS_daily_seasonal_EPU','/')

roms.dir = 'E:/duPontavice_bt/bt_revised_metadata_032024/'
roms.files.long = list.files(roms.dir,'^bottom_temp_',full.names = T)
roms.files.short = list.files(roms.dir,'^bottom_temp_',full.names = F)
roms.yr = sapply(roms.files.short,function(x) as.numeric(strsplit(x,'bottom_temp_|.nc')[[1]][2]),USE.NAMES = F)
roms.files.long = roms.files.long[which(roms.yr<1993)]
roms.years = 1959:1992
roms.out.dir = here::here('data','ROMS','2025','ROMS_daily_seasonal_EPU','/')

#Create seasonal-annual means

##GLORYS
glorys.season = EDABUtilities::make_2d_summary_gridded(data.in = glorys.files,
                                       write.out = F,
                                       output.files = paste0(glorys.out.dir,'GLORYS_daily_BottomTemp_',glorys.years,'.nc'),
                                       shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                                       var.name = 'BottomT',
                                       agg.time = 'season',
                                       statistic = 'mean',
                                       area.names = c('MAB','GB','GOM','SS')
)

glorys.ls = list()
i=1
for(i in 1:length(glorys.season)){
  
  glorys.yr =terra::as.data.frame(glorys.season[[i]],xy =T)
  colnames(glorys.yr) = c('Longitude','Latitude','winter','spring','summer','fall')[1:ncol(glorys.yr)]
  glorys.ls[[i]] =glorys.yr %>%
    tidyr::gather(Var,Value,-Longitude,-Latitude)%>%
    mutate(Time = glorys.years[i] )%>%
    select(Time,Latitude,Longitude,Var,Value)
}
glorys.df = bind_rows(glorys.ls)

##ROMS
roms.season =EDABUtilities::make_2d_summary_gridded(data.in = roms.files.long,
                                       write.out = F,
                                       output.files = paste0(roms.out.dir,'ROMS_daily_BottomTemp_', roms.years,'.nc'),
                                       shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                                       var.name = 'sea_water_temperature_at_sea_floor',
                                       agg.time = 'season',
                                       statistic = 'mean',
                                       area.names = c('MAB','GB','GOM','SS')
)

roms.ls = list()
i=1
for(i in 1:length(roms.season)){
  
  roms.yr =terra::as.data.frame(roms.season[[i]],xy =T)
  colnames(roms.yr) = c('Longitude','Latitude','winter','spring','summer','fall')[1:ncol(roms.yr)]
  roms.ls[[i]] =roms.yr %>%
    tidyr::gather(Var,Value,-Longitude,-Latitude)%>%
    mutate(Time = roms.years[i] )%>%
    select(Time,Latitude,Longitude,Var,Value)
}
roms.df = bind_rows(roms.ls)

# Write Season Gridded Means
bt.season = bind_rows(roms.df,glorys.df)
write.csv(bt.season, here::here('data','SOE','bottom_temp_seasonal_gridded_2025.csv'),row.names =F)

# Create Seasonal Bottom Temp Anomalies
##GLORYS
glorys.season.epu = EDABUtilities::make_2d_summary_ts(data.in = glorys.files,
                                  write.out =F,
                                  shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                                  var.name = 'bottomT',
                                  agg.time = 'season',
                                  statistic = 'mean',
                                  touches =F,
                                  area.names = c('MAB','GB','GOM','SS')
                                  )
for(i in 1:length(glorys.season.epu)){glorys.season.epu[[i]] = mutate(glorys.season.epu[[i]], year = glorys.years[i], Var = 'GLORYS')}
##ROMS
roms.season.epu = EDABUtilities::make_2d_summary_ts(data.in = roms.files.long,
                                                      write.out =F,
                                                      shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                                                      var.name = 'sea_water_temperature_at_sea_floor',
                                                      agg.time = 'season',
                                                      statistic = 'mean',
                                                    touches =F,
                                                      area.names = c('MAB','GB','GOM','SS')
)
for(i in 1:length(roms.season.epu)){roms.season.epu[[i]] = mutate(roms.season.epu[[i]], year = roms.years[i], Var = 'ROMS')}
##Combine to output dataframe
data.all.anom.ls = c(roms.season.epu,glorys.season.epu)
data.all.anom.df = bind_rows(data.all.anom.ls)

data.all.anom.clim = data.all.anom.df %>%
  filter(year %in% 1990:2020) %>%
  group_by(time,area)%>%
  summarise(value.clim = mean(value,na.rm=T))

data.all.anom = data.all.anom.df %>%
  left_join(data.all.anom.clim)%>%
  rename(Source = 'Var')%>%
  mutate(value.anom = value-value.clim)%>%
  left_join(season.match,by = c('time'= 'season.id')) %>%
  mutate(dum = '_Bottom Temp Anomaly')%>%
  tidyr::unite(Var,c('season.name','dum'),sep='')%>%
  rename(Time = 'year',
         EPU = 'area',
         Value = 'value.anom')%>%
  mutate(Units = 'degree C')%>%
  select(Time, Value, EPU, Source, Var, Units)

#Create Annual mean bottom temperature
##GLORYS
glorys.epu.day = EDABUtilities::make_2d_summary_ts(data.in = glorys.files,
                                               write.out =F,
                                               shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                                               var.name = 'bottomT',
                                               agg.time = 'days',
                                               statistic = 'mean',
                                               touches =F,
                                               area.names = c('MAB','GB','GOM','SS')
)

ggplot(glorys.epu.day[[1]],aes(x=time, y= value, color = area))+
  geom_line()

glorys.epu.year.ls = list()
for(i in 1:length(glorys.epu.day)){
  glorys.epu.year.ls[[i]] =glorys.epu.day[[i]] %>%
    mutate(year = format(as.Date(time),format = '%Y'))%>%
    group_by(year,area)%>%
    summarise(Value = mean(value,na.rm=T))%>%
    mutate(source = 'GLORYS')
}
glorys.epu.year = bind_rows(glorys.epu.year.ls)
##ROMS
roms.epu.day = EDABUtilities::make_2d_summary_ts(data.in = roms.files.long,
                                                    write.out =F,
                                                    shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                                                    var.name = 'sea_water_temperature_at_sea_floor',
                                                    agg.time = 'days',
                                                    tz = 'EST',
                                                    statistic = 'mean',
                                                    touches =F,
                                                    area.names = c('MAB','GB','GOM','SS')
)
roms.epu.year.ls = list()
for(i in 1:length(roms.epu.day)){
  roms.epu.year.ls[[i]] =roms.epu.day[[i]] %>%
    mutate(year = format(as.Date(time),format = '%Y'))%>%
    group_by(year,area)%>%
    summarise(Value = mean(value,na.rm=T))%>%
    mutate(source = 'ROMS')
}
roms.epu.year = bind_rows(roms.epu.year.ls)
##Combine
  
data.all.mean = bind_rows(roms.epu.year,glorys.epu.year)
data.all.mean =data.all.mean%>%
  rename(Time = 'year',
         EPU = 'area',
         Source = 'source')%>%
  mutate(Var = 'Annual_Bottom Temp',
         Units = 'degree C',
         Time = as.numeric(Time))%>%
  select(Time,Value,EPU,Source,Var,Units)

#Join Dataframes and write out  
data.comp.out = bind_rows(data.all.mean,data.all.anom)
write.csv(data.comp.out, here::here('data','SOE','bottom_temp_anomaly_2025_V3.csv'),row.names =F)

#Plots compared to last year
bt24 = ecodata::bottom_temp_model_anom%>%
  mutate(report.yr = 2024)
bt25 = read.csv(here::here('data','SOE','bottom_temp_anomaly_2025_V3.csv'))%>%
  mutate(report.yr = 2025)
data.all = bind_rows(bt24,bt25) %>%
  filter(Source != 'PSY')

ggplot(data.all, aes(x = Time, y = Value, color = Source,lty = factor(report.yr)))+
  geom_line()+
  facet_grid(Var~EPU,scale = 'free_y')+
  geom_vline(xintercept = 2024,lty = 2)
ggsave(here::here('Figures','bottom_temp_anomoly_2024_2025.png'),width = 12, height = 9, units = 'in')
