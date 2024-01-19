library(dplyr)
library(ggplot2)

old = read.csv('C:/Users/joseph.caracappa/Downloads/bt_temp_times_series_anomaly_epu.csv')%>%
  mutate(source = 'SOE_2023')
head(old)

new = read.csv(here::here('data','SOE','bt_temp_time_series_anomaly_epu.csv'))%>%
  filter(source %in% c('ROMS','GLORYS'))%>%
    mutate(source = 'SOE_2024')


data = bind_rows(old,new)

ggplot(data,aes(x = year,y = anomaly,color = source))+
  geom_line()+
  facet_wrap(subarea~season)+
  geom_hline(yintercept = 0)
ggsave(here::here('Figures','bottom_temp_anomaly_2023_2024_comparison.png'),width = 10,height = 10, units = 'in')


old = ecodata::cold_pool%>%
  mutate(soe.year = 'SOE_2023',
         source = 'ecodata')

new = read.csv(here::here('data','SOE','cold_pool_indices_1959_2023.csv'))%>%
  tidyr::gather(Var,Value,-year,-source)%>%
  # filter(source != 'PSY')%>%
  rename(Time = 'year')%>%
  mutate(soe.year = 'SOE_2024',
         EPU = 'MAB')


ggplot(new,aes(x=Time,y=Value,color = source,lty = soe.year))+
  geom_line(size = 1)+
  facet_wrap(~Var,scale = 'free_y')

data = bind_rows(old,new)

ggplot(data,aes(x=Time,y=Value,color = soe.year))+
  geom_line(size = 1)+
  facet_wrap(~Var,scale = 'free_y')
ggsave(here::here('Figures','cold_pool_2023_2024_comparison.png'))

filter(new,source == 'GLORYS' & Time == 2021)
