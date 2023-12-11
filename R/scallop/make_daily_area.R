#Script to calculate daily area withing region that is within threshold window
library(tidync)
library(dplyr)
library(ncdf4)
library(ggplot2)

bathy = rast(here::here('data','bathymetry_min_30_max_90.nc'),subds = 'depth')
area.tot = area(bathy)


#get temp data
data.dir = here::here('data','daily_bottomT_window_gridded','/')

glorys.files =list.files(data.dir,'GLORYS*')
psy.files = list.files(data.dir,'PSY*')

i=1
# data.glorys.ls = list()
area.glorys  = list()
time.glorys = list()
for(i in 1:length(glorys.files)){
  
  # data.glorys.ls[[i]] = tidync(paste0(data.dir,glorys.files[i]),'BottomT')%>%
  #   hyper_tibble()
  
  glorys.rast = rast(paste0(data.dir,glorys.files[i]))
  area.glorys[[i]] = area(glorys.rast)
  time.glorys[[i]] = time(glorys.rast)
}

area.glorys = unlist(area.glorys)
time.glorys = as.POSIXct(unlist(time.glorys),origin = '1970-01-01 00:00:00 UTC')
data.glorys = data.frame(time = time.glorys, source = "GLORYS",area = area.glorys)

psy.rast = rast(paste0(data.dir,psy.files))
area.psy = area(psy.rast)
time.psy = time(psy.rast)
data.psy = data.frame(time = time.psy, source = 'PSY',area = area.psy)

data.all = data.glorys %>%
  bind_rows(data.psy)%>%
  mutate(area.km2 = area/1E6,
         area.prop = area/area.tot,
         year = as.numeric(format(time,format = '%Y')),
         month.name = format(time,format = '%B'),
         month.num = as.numeric(format(time,format = '%m')),
         jul = as.numeric(format(time,format = '%j')))

write.csv(data.all,here::here('data','threshold_area_daily.csv'),row.names = T)

ggplot(data.all,aes(x = time, y = area.km2))+
  geom_line()+
  theme_bw()+
  ylab('Area (km2)')+
  xlab('')+
  ggtitle('Lat: 36.5 - 41.1 Lon: -76.5  -71 Z: 30 - 90m T: 18 - 30 ')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(here::here('Figures','scallop','daily_area_over_18deg.png'))

ggplot(data.all,aes(x = as.Date(time), y = area.prop))+
  geom_line()+
  theme_bw()+
  ylab('Area Proportion')+
  xlab('')+
  scale_x_date(date_breaks = '2 year',date_labels = '%Y')+
  ggtitle('Lat: 36.5 - 41.1 Lon: -76.5  -71 Z: 30 - 90m T: 18 - 30 ')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(here::here('Figures','scallop','daily_prop_area_over_18deg.png'))

plot.col = colorRampPalette(c('blue3','red3'))
ggplot(data.all,aes(x = jul, y = area.prop, color = factor(year)))+
  geom_line(size = 1)+
  scale_color_viridis_d()+
  theme_bw()+
  ylab('Area Proportion')+
  xlab('')
ggsave(here::here('Figures','scallop','daily_prop_area_annual_lines.png'))

data.month = data.all %>%
  group_by(year,month.num,month.name)%>%
  arrange(year,month.num)%>%
  summarise(area.prop =mean(area.prop,na.rm=T))%>%
  mutate(month.name = as.factor(month.name))

ggplot(data.month,aes(x = year, y = area.prop))+
  geom_line()+
  facet_wrap(~month.name)+
  scale_x_continuous(breaks = seq(1990,2024,5))
ggsave(here::here('Figures','scallop','monthly_prop_area.png'),width =12, height = 8, units = 'in', dpi =300)
  
  