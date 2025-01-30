library(terra)
library(mapdata)
library(dplyr)
library(ggplot2)

file.shp = terra::project(terra::vect('C:/Users/joseph.caracappa/Documents/GitHub/scallop_habitat/geometry/Scallop_2024_MAB_Est_Areas_SAMS_CASA_UTM18_EDAB_80meters.shp',crs = '+proj=longlat'),'+proj=longlat +datum=WGS84 +no_defs ')

plot(file.shp)
data = rast('C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_2022.nc')
data =subset(data,244:365)

data.all.sub = subset(data,1)
data.all.df = as.data.frame(data.all.sub,xy =T)
ggplot(data.all.df,aes(x=x,y=y,fill = BottomT_244))+
  geom_tile()+
  # facet_wrap(~year)+
  coord_equal()+
  xlab('')+
  ylab('')+
  scale_fill_viridis_c(option = 'inferno',name = 'Bottom Temperature (C)')+
  annotation_map(neus.map,fill = 'grey70',color = 'black')+
  ggtitle(time(data.all.sub)[1])+
  theme_bw()+
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))
ggsave(here::here('Figures','GLORYS_NEUS_example.png'))


data.crop = crop(mask(data,file.shp),file.shp)

data.time = time(data.crop)
names(data.crop) = data.time
data.df = as.data.frame(data.crop,xy =T)%>%
  tidyr::gather(date,bottom.temp,-x,-y)%>%
  mutate(date = as.Date(date),
          month =format(date,format = '%B'))%>%
  group_by(month,x,y)%>%
  summarise(bottom.temp = mean(bottom.temp,na.rm=T))

data.df$month = factor(data.df$month,levels = month.name[9:12])
neus.map = map_data('worldHires',region = c('USA','Canada'))


ggplot(data.df,aes(x=x,y=y,fill = bottom.temp))+
  geom_tile()+
  # facet_wrap(~year)+
  coord_equal()+
  xlim(-76,-70)+
  ylim(35,41)+
  xlab('')+
  ylab('')+
  scale_fill_viridis_c(option = 'inferno')+
  annotation_map(neus.map,fill = 'grey70',color = 'black')+
  facet_wrap(~month,nrow =1)+
  ggtitle('2022 Monthly Mean')+
  theme_bw()+
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))

ggsave(here::here('Figures','GLORYS_MAB_2022_example.png'))
