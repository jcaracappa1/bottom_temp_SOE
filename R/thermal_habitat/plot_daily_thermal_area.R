#Script to plot the daily proportion of epu in each temperature category
library(ggplot2)

data.dir = here::here('data','SOE','thermal_area','/')
data.prefix = 'thermal_area_'
data.files = list.files(data.dir)

fig.dir = here::here('Figures','thermal_area','area_timeseries','/')

tz.combs = read.csv(here::here('data','thermal_area','tz_groups.csv'),as.is = T)

i=1
for(i in 1:length(data.files)){
  
  data.file = read.csv(paste0(data.dir,data.files[i]))
    
  data.year = data.file%>%
    mutate(date = as.Date(date),
           year = as.numeric(format(date,format = '%Y')))%>%
    group_by(source,epu,year)%>%
    summarise(max.area = max(area.prop,na.rm=T))
  
  z.group = strsplit(as.character(data.file$z.group[1]),'m')[[1]][1] %>% as.numeric()
  z.max = tz.combs$max.z[which(tz.combs$max.z == z.group)][1]
  z.min = tz.combs$min.z[which(tz.combs$max.z == z.group)][1]
  
  ggplot(data.year,aes(x=year,y=max.area,color = source))+
    geom_line()+
    facet_wrap(~epu)+
    ylab(paste0('% of EPU above ',data.file$temp.group))+
    ggtitle(paste0(z.min,'m - ',z.max,'m'))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste(fig.dir,'thermal_area_prop_',data.file$temp.group[1],data.file$z.group[1],'.png'),width = 8,height =6, units = 'in', dpi =300)
  
  ggplot(data.file,aes(x=as.Date(date),y=area.prop,color =source))+
    geom_line()+
    facet_wrap(~epu)+
    ylab(paste0('% of EPU above ',data.file$temp.group))+
    ggtitle(paste0(z.min,'m - ',z.max,'m'))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste(fig.dir,'thermal_area_prop_daily',data.file$temp.group[1],data.file$z.group[1],'.png'),width = 8,height =6, units = 'in', dpi =300)
  
}