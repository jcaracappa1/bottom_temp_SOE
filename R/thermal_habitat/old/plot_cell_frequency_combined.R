#Script to plot total cell frequency across all depth groups

library(mapdata)
library(ggplot2)
library(maps)
library(terra)
library(dplyr)
library(tidync)

data.dir = here::here('data','thermal_area','gridded_cell_frequency','/')
area.shp = project(vect(here::here('geometry','EPU_NOESTUARIES.shp')),' +proj=longlat +datum=WGS84 +no_defs ')

###!!!!fix epu so shape plot proper
epu.df = as.data.frame(geom(area.shp))
# which.epu = which(epu.df$EPU == file.epu)
# subarea.df = as.data.frame(geom(area.shp[which.epu,]))

file.names = list.files(data.dir)

neus.map = map_data('worldHires',region = c('USA','Canada'))

# tz.combs = read.csv(here::here('data','thermal_area','tz_groups.csv'),as.is = T)

fig.dir = here::here('Figures','SOE','/')

file.prefix = 'annual_day_count_'

file.temp = sapply(file.names,function(x) strsplit(x,paste0(file.prefix,'|_|.nc'))[[1]][4],USE.NAMES = F)
temp.groups = sort(unique(file.temp))

i=1
for(i in 1:length(temp.groups)){
  
  file.match = grep(temp.groups[i],file.names,value = T)  
  
  file.source = sapply(file.match,function(x) strsplit(x,paste0(file.prefix,'|_|.nc'))[[1]][2],USE.NAMES = F)
  data.temp = list()
  j=1
  for(j in 1:length(file.match)){
  
      data.temp[[j]]=tidync(paste0(data.dir,file.match[j]))%>%
      hyper_tibble()%>%
      mutate(source = file.source[j])
  }
  data.temp = bind_rows(data.temp)%>%
    filter(time == max(time))%>%
    group_by(longitude,latitude)%>%
    summarise(Ndays = mean(Ndays,na.rm=T))
  
  plot.title = paste0('Days above ',temp.groups[i],' C')
  
  ggplot()+
    geom_tile(data = data.temp,aes(x=longitude,y=latitude,fill = Ndays),width = 1/12,height = 1/12)+
    coord_equal()+
    scale_fill_viridis_c(name = 'Days')+
    annotation_map(neus.map,fill = 'grey70')+
    xlab('')+
    ylab('')+
    geom_polygon(data =epu.df,aes(x=x,y=y,group= geom),fill = 'transparent',color = 'black')+
    ggtitle(plot.title)+
    theme_bw()+
    theme(
      legend.position = 'bottom',
      plot.title = element_text(hjust = 0.5)
    )
  
  ggsave(here::here('Figures','SOE',paste0('Thermal habitat ',temp.groups[i],'.png')),width = 10,height =10,units = 'in',dpi = 300)
    
    
  
    
}
