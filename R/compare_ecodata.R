old = read.csv('C:/Users/joseph.caracappa/Downloads/bt_temp_times_series_anomaly_epu.csv')%>%
  mutate(source = 'SOE_2023')
head(old)

new = read.csv(here::here('data','SOE','bt_temp_time_series_anomaly_epu.csv'))%>%
    mutate(source = 'SOE_2024')


data = bind_rows(old,new)

ggplot(data,aes(x = year,y = anomaly,color = source))+
  geom_line()+
  facet_wrap(subarea~season)+
  geom_hline(yintercept = 0)
ggsave(here::here('Figures','bottom_temp_anomaly_2023_2024_comparison.png'))


old = ecodata::cold_pool%>%
  mutate(source = 'SOE_2023')

new = read.csv(here::here('data','SOE','cold_pool_indices_1959_2022.csv'))%>%
  tidyr::gather(Var,Value,-year)%>%
  rename(Time = 'year')%>%
  mutate(source = 'SOE_2024',
         EPU = 'MAB')


data = bind_rows(old,new)

ggplot(data,aes(x=Time,y=Value,color = source))+
  geom_line()+
  facet_wrap(~Var,scale = 'free_y')
ggsave(here::here('Figures','cold_pool_2023_2024_comparison.png'))

