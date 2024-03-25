
library(dplyr)
library(ggplot2)
library(tidync)
library(terra)
library(maps)
library(mapdata)


emolt.df = read.csv(here::here('data','emolt_glorys_bottomT_match.csv'))%>%
  filter(year == 2020)
neus.map = map_data('worldHires',region = c('USA','Canada'))

emolt.range = emolt.df %>%
  mutate(temp_delta = ifelse(temp_delta<(-3),-3,temp_delta))%>%
  mutate(temp_delta = ifelse(temp_delta>3,3,temp_delta))
  
ggplot(emolt.range, aes(x = lon, y = lat, color = temp_delta))+
  annotation_map(neus.map,fill = 'grey70')+
  geom_point(size = 2,alpha = 0.7)+
  facet_wrap(~month,labeller = 'label_both')+
  scale_color_gradient2(low = 'blue',high = 'red',mid = 'grey80', midpoint = 0, name = 'GLORYS - EMOLT',labels = c('≤-3', -2,-1,0,1,2,'≥3'))+
  ggtitle('Bottom Temp Delta - 2020')+
  coord_sf(
    xlim = c(-71,-67),
    ylim = c(41,44.8)
  )+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(here::here('Figures','lobster','2020_emolt_comp.png'),width = 12, height = 8, units = 'in', dpi =300)


emolt.df = read.csv(here::here('data','emolt_glorys_bottomT_match.csv'))%>%
  filter(year == 2022)

emolt.range = emolt.df %>%
  mutate(temp_delta = ifelse(temp_delta<(-3),-3,temp_delta))%>%
  mutate(temp_delta = ifelse(temp_delta>3,3,temp_delta))

ggplot(emolt.range, aes(x = lon, y = lat, color = temp_delta))+
  annotation_map(neus.map,fill = 'grey70')+
  geom_point(size = 2,alpha = 0.7)+
  facet_wrap(~month,labeller = 'label_both')+
  scale_color_gradient2(low = 'blue',high = 'red',mid = 'grey80', midpoint = 0, name = 'GLORYS - EMOLT',labels = c('≤-3', -2,-1,0,1,2,'≥3'))+
  ggtitle('Bottom Temp Delta - 2022')+
  coord_sf(
    xlim = c(-71,-67),
    ylim = c(41,44.8)
  )+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(here::here('Figures','lobster','2022_emolt_comp.png'),width = 12, height = 8, units = 'in', dpi =300)
