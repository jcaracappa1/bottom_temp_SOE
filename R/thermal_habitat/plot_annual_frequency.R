#Scripts that make annual maps of cell counts in each temp category 
library(dplyr)
library(tidync)
library(ggplot2)
library(mapdata)
library(terra)

data.dir = here::here('data','thermal_area','gridded_cell_frequency','/')
area.shp = project(vect(here::here('geometry','EPU_NOESTUARIES.shp')),' +proj=longlat +datum=WGS84 +no_defs ')
epu.df = as.data.frame(area.shp)

file.names = list.files(data.dir)

neus.map = map_data('worldHires',region = 'USA')

tz.combs = read.csv(here::here('data','thermal_area','tz_groups.csv'),as.is = T)

fig.dir = here::here('Figures','thermal_area','annual_frequency','/')
fig.dir2 = here::here('Figures','thermal_area','annual_frequency_current_year','/')

i=1
for(i in 1:length(file.names)){
 
  file.source =strsplit(file.names[i],paste0('annual_day_count_|_|.nc'))[[1]][2]
  file.epu =strsplit(file.names[i],paste0('annual_day_count_|_|.nc'))[[1]][3]
  file.temp =strsplit(file.names[i],paste0('annual_day_count_|_|.nc'))[[1]][4]
  file.z =strsplit(file.names[i],paste0('annual_day_count_|_|.nc'))[[1]][5]
  z.group = strsplit(file.z,'m')[[1]][1] %>% as.numeric()
  z.max = tz.combs$max.z[which(tz.combs$max.z == z.group)][1]
  z.min = tz.combs$min.z[which(tz.combs$max.z == z.group)][1]
  
  data = tidync(paste0(data.dir,file.names[i]))%>%
    hyper_tibble()%>%
    mutate(year = (time-1) + 1993)
  
  which.epu = which(epu.df$EPU == file.epu)
  subarea.df = as.data.frame(geom(area.shp[which.epu,]))
  
  region.lat = c(min(subarea.df$y)-0.25,max(subarea.df$y)+0.25)
  region.lon = c(min(subarea.df$x)-0.25,max(subarea.df$x)+0.25)
  
  if(nrow(data)==0){
    next()
  }else{
    ggplot()+
      geom_tile(data = data,aes(x=longitude,y=latitude,fill = Ndays))+
      facet_wrap(~year)+
      coord_equal(xlim = region.lon,ylim = region.lat)+
      annotation_map(neus.map,fill = 'grey70')+
      annotate('polygon',x = subarea.df$x,y = subarea.df$y,fill = 'transparent',color = 'black')+
      scale_fill_viridis_c(name = 'Days')+
      ggtitle(paste0(file.source,' ',file.epu,', >',file.temp,',',z.min,'m - ',z.max,'m'))+
      xlab('')+
      ylab('')+
      theme_bw()+
      theme(legend.position = 'bottom',
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5))
    
    ggsave(paste0(fig.dir,'annual_frequency_',file.epu,'_',file.temp,'_',file.z,'.png'),width  =10, height = 10, units = 'in', dpi = 300)
    
    data.this.year = filter(data,year == max(year))
    
    ggplot()+
      geom_tile(data=data.this.year,aes(x=longitude,y=latitude,fill = Ndays))+
      coord_equal(xlim = region.lon,ylim = region.lat)+
      annotation_map(neus.map,fill = 'grey70')+
      annotate('polygon',x = subarea.df$x,y = subarea.df$y,fill = 'transparent',color = 'black')+
      scale_fill_viridis_c(name = 'Days')+
      ggtitle(paste0(file.source, ' ',file.epu,', >',file.temp,',',z.min,'m - ',z.max,'m'))+
      xlab('')+
      ylab('')+
      theme_bw()+
      theme(legend.position = 'bottom',
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
  ggsave(paste0(fig.dir2,'annual_frequency_',file.epu,'_',file.temp,'_',file.z,'.png'),width  =10, height = 10, units = 'in', dpi = 300)
  

}