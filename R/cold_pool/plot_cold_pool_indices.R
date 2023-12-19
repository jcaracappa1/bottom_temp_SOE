data = read.csv(here::here('data','SOE','cold_pool_indices_1959_2022.csv'))%>%
  tidyr::gather(variable,value,-year)

head(data)

ggplot(data,aes(x= year, y = value))+
  geom_line()+
  facet_wrap(~variable,scales = 'free_y')+
  theme_bw()

ggsave(here::here('Figures','SOE','cold_pool_indices.png'))
