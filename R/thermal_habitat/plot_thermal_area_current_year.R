library(dplyr)
library(tidync)
library(ggplot2)
library(mapdata)
library(terra)

data = read.csv(here::here('data','SOE','thermal_habitat_frequency_2023.csv'))%>%
  filter(year == 2023)

neus.map = map_data('worldHires',region = 'USA')

head(data)

ggplot()+
  geom_tile(data = data, aes(x=longitude,y = latitude,color = Ndays),size = 2)+
  facet_wrap(max.depth~temp.threshold,labeller = 'label_both')+
  annotation_map(neus.map,fill = 'grey70')+
  scale_color_viridis_c()+
  xlab('')+
  ylab('')+
  theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(here::here('Figures','SOE','thermal_habitat_frequency_2023.png'))


