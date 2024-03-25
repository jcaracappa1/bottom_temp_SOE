#match emolt data to GLORYS and plot
library(dplyr)
library(ggplot2)
library(terra)
library(maps)
library(mapdata)

#Reads in EMOLT data, adds time variables, and aggregates by date
emolt = read.csv(here::here('data','emolt','emolt_QCed_telemetry_and_wified.csv'))%>%
  mutate(datet = as.Date(datet),
         date = format(datet,format = '%Y-%m-%d'),
         year = as.numeric(format(datet,format = '%Y')),
         month = as.numeric(format(datet,format = '%m')))%>%
  select(year,month,date,lat,lon,mean_temp)%>%
  group_by(year,month,date,lon,lat)%>%
  summarise(emolt_temp = mean(mean_temp,na.rm=T))

years = sort(unique(emolt$year))

#Specifies directory with GLORYS annual temperature and salinity netCDF files
glorys.dir = here::here('data','gridded_raw_bottom_TS_GOM','/')

#Loops through each year, pulls relevant data from EMOLT and GLORYS, does a nearest match basedo on lat/lon and exports difference to dataframe
i=1
emolt.df.ls = list()
for(i in 1:length(years)){
  
  #Subset EMOLT data for current year
  emolt.year = filter(emolt, year == years[i])%>%
    mutate(date = as.Date(date))%>%
    ungroup()%>%
    select(date,lat,lon,emolt_temp)
    
  
  #read in this year's GLORYS file
  glorys.name = paste0('daily_GOM_bottom_TS_',years[i],'.nc')
  glorys.file = paste0(glorys.dir,glorys.name)
  if(!file.exists(glorys.file)){
    next()
  }
  
  #Converts glorys' data to spatial raster (change to "bottomS" for salinity)
  glorys.rast = rast(glorys.file,subds = 'bottomT')
  
  emolt.dates = sort(unique(emolt.year$date))
  glorys.dates = as.Date(time(glorys.rast))
  
  #Loops through each date in EMOLT data for this year and extracts lat/lon of each obs. from GLORYS raster
  j=1
  emolt.year.ls = list()
  for(j in 1:length(emolt.dates)){
    
    #Identifies position of current day in GLORYS dates
    date.match = which(emolt.dates[j] == glorys.dates)
    
    if(length(date.match) == 0){
      next()
    }
    glorys.date = subset(glorys.rast, date.match)
    
    #Performs extraction
    emolt.year.ls[[j]] = filter(emolt.year, date == emolt.dates[j]) %>%
      mutate(glorys_temp = extract(glorys.date,data.frame(lon,lat))[,2])
  }
  
  #combines all dates for this year
  emolt.df.ls[[i]] = bind_rows(emolt.year.ls)

}

#Combines all years, formats and filters out missing values
emolt.df = bind_rows(emolt.df.ls)%>%
  mutate(temp_delta = glorys_temp - emolt_temp,
         year = as.numeric(format(date,format = '%Y')),
         month = as.numeric(format(date,format = '%m')))%>%
  select(date,year,month,lat,lon,glorys_temp,emolt_temp,temp_delta)%>%
  filter(lon < -10 & !is.na(temp_delta))
  

write.csv(emolt.df,here::here('data','emolt_glorys_bottomT_match.csv'),row.names = F)

## Plots annual comparisons on a map with coastline
fig.dir = here::here('figures','lobster','/')

neus.map = map_data('worldHires',region = c('USA','Canada'))

y=1
for( y in 1:length(years)){
  
  emolt.yr = filter(emolt.df,year == years[y])
  
  ggplot(emolt.yr, aes(x = lon, y = lat, color = temp_delta))+
    annotation_map(neus.map,fill = 'grey70')+
    geom_point(size = 3,alpha = 0.7)+
    facet_wrap(~month,labeller = 'label_both')+
    scale_color_gradient2(low = 'blue',high = 'red',mid = 'grey80', midpoint = 0, name = 'GLORYS - EMOLT')+
    theme_bw()
  
  ggsave(paste0(fig.dir,'GLORYS-EMOLT_delta_',years[y],'.png'))
}

ggplot(emolt.df, aes(x = lon, y = lat, color = temp_delta))+
  annotation_map(neus.map,fill = 'grey70')+
  geom_point(size = 1,alpha = 0.7)+
  facet_grid(year~month,labeller = 'label_both')+
  scale_color_gradient2(low = 'blue',high = 'red',mid = 'grey80', midpoint = 0, name = 'GLORYS - EMOLT')+
  theme_bw()
ggsave(paste0(fig.dir,'GLORYS-EMOLT_delta_2015_2023.png'))
