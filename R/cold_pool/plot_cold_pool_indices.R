library(ggplot2)
library(dplyr)
data = read.csv(here::here('data','SOE','cold_pool_indices_1959_2023.csv'))%>%
  tidyr::gather(variable,value,-year, -source)

head(data)

ggplot(data,aes(x= year, y = value,color = source))+
  geom_line()+
  facet_wrap(~variable,scales = 'free_y')+
  theme_bw()

ggsave(here::here('Figures','SOE','cold_pool_indices.png'))
