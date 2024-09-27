library(ggplot2)
library(mapdata)

data = read.csv(here::here('data','SOE','bt_seasonal_gridded.csv'))%>%
  # filter(Time == max(Time))
  filter(Time == 2022)

data$Variable = factor(data$Variable, levels = c('winter','spring','summer','fall'))
  

neus_map = map_data('worldHires',region = c('USA','Canada'))

ggplot(data = data, aes(x= Lon,y = Lat,fill = Value))+
  geom_tile()+
  facet_wrap(~Variable)+
  scale_fill_viridis_c(option = 'A',name = 'Bottom \n Temp')+
  ggtitle('Seasonal Mean Bottom Temperature')+
  theme_bw()
ggsave(here::here('Figures','SOE','bt_seasonal_gridded_2022.png'))
