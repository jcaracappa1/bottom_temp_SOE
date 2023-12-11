#Format anomaly data to SOE format
library(dplyr)
library(ggplot2)

data.dir = here::here('data','seasonal_raw','/')
file.names = list.files(data.dir)

data.ls = list()

season.df = data.frame(season.names = c('winter','spring','summer','fall'),
                       season.nums = 1:4)

i=1
for(i in 1:length(file.names)){
  
  season.match = season.df$season.num[season.df$season.names %in% strsplit(file.names[i],'_|\\.')[[1]]]
  
  data.ls[[i]] = read.csv(paste0(data.dir,file.names[i]))  %>%
    mutate(season = season.match)%>%
    dplyr::select(season,year,epu,source,BottomT.mean,BottomT.mean.ref,BottomT.mean.anomaly)%>%
    rename(subarea = 'epu',
           bt_temp = 'BottomT.mean',
           ref_bt = 'BottomT.mean.ref',
           anomaly = 'BottomT.mean.anomaly')
  
}

data.all = bind_rows(data.ls)

write.csv(data.all,here::here('data','SOE','bt_temp_time_series_anomaly_epu.csv'))

ggplot(data.all, aes(x= year, y = anomaly,color = source,color = source))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0,lty = 2)+
  facet_wrap(subarea~season)+
  theme_bw()+
  ylab('Bottom Temperature Anomaly (degC)')+
  xlab('')+
  theme(panel.grid= element_blank())
ggsave(here::here('Figures','bt_temp_time_series_anomaly_epu.png'))
