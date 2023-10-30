#Plots seasonal anomaly by EPU
library(ggplot2)

data.dir = here::here('data','seasonal_anomaly')
figure.dir = here::here('Figures','/')

data.files = list.files(data.dir)
data.files.long = list.files(data.dir,full.names = T)

for(i in 1:length(data.files)){
  
  data = read.csv(data.files.long[i])
  
  plot.name = strsplit(data.files[i],'.csv')[[1]][1]
  
  ggplot(data,aes(x= year, y = BottomT.mean.anomaly,color = source))+
    geom_line()+
    geom_point()+
    theme_bw()+
    xlab('')+
    ylab('Bottom Temperature Anomaly')+
    ggtitle(data.files[i])+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0(figure.dir,plot.name,'.png'))
  
}
