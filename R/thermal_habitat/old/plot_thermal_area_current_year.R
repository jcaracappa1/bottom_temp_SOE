library(dplyr)
library(tidync)
library(ggplot2)
library(mapdata)
library(terra)

data = read.csv(here::here('data','SOE','thermal_habitat_persistence_2023.csv'))%>%
  filter(year == 2022 & source == 'GLORYS')

neus.map = map_data('worldHires',region = 'USA')

head(data)

ggplot()+
  geom_tile(data = data, aes(x=longitude,y = latitude,fill = Ndays))+
  facet_wrap(~temp.threshold)+
  annotation_map(neus.map,fill = 'grey70')+
  scale_fill_viridis_c()+
  xlab('')+
  ylab('')+
  theme_bw()+
  ggtitle('2022')+
  theme(legend.position = 'bottom',
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(here::here('Figures','SOE','thermal_habitat_frequency_2022.png'))



