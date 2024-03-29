#For each estimation area, calculate the proportional area that is within each temperature category and plot
library(dplyr)
library(tidync)
library(ggplot2)
library(mapdata)
# library(rnaturalearthdata)
# library(rnaturalearth)
# library(sf)

data.dir = here::here('data','estimation_areas','gridded_daily_estimation_areas_binary_mask','/')

temp.cat = data.frame(
  group = c('normal','stressed','danger'),
  min.temp = c(0,17,19),
  max.temp = c(17,19,30)
)

area.shp = project(vect(here::here('geometry','MAB_ESTIMATION_AREAS_2023_UTM18_PDT_NYB.shp')),' +proj=longlat +datum=WGS84 +no_defs ')

area.df = as.data.frame(area.shp)

combs = expand.grid(temp.group = temp.cat$group,subarea = area.shp$NewSAMS)

figure.dir = here::here('Figures','scallop','estimation_areas','/')

neus.map = map_data('worldHires',region = 'USA')

data.subarea.ls = list()
i=1
for(i in 1:nrow(combs)){
  
  data.file = paste0(data.dir,'gridded_bottomT_binary_',combs$subarea[i],'_',combs$temp.group[i],'.nc')
  
  data = tidync(data.file)%>%
    hyper_tibble()%>%
    mutate(date = as.POSIXct(time,origin = '1970-01-01 00:00:00 UTC'),
           year = format(date,format = '%Y'),
           jul = format(date,format = '%j'))%>%
    group_by(year,jul,longitude,latitude)%>%
    summarise(BottomT = mean(BottomT,na.rm=T))%>%
    group_by(year,longitude,latitude)%>%
    summarise(temp.N = sum(BottomT,na.rm=T))
  
  if(nrow(data) == 0){next()}
  
  which.area = which(area.df$NewSAMS == combs$subarea[i])
  subarea.df = as.data.frame(geom(area.shp[which.area,]))
  
  area.fig.dir = paste0(figure.dir,combs$subarea[i],'/')
  if(!dir.exists(area.fig.dir)){dir.create(area.fig.dir,recursive = T)}
  
  area.fig.name = paste0(area.fig.dir,'frequency_',combs$temp.group[i],'_temp_',combs$subarea[i],'.png')
  
  region.lat = c(min(data$latitude)-0.25,max(data$latitude)+0.25)
  region.lon = c(min(data$longitude)-0.25,max(data$longitude)+0.25)
  
  min.temp = temp.cat$min.temp[which(temp.cat$group == combs$temp.group[i])]
  max.temp = temp.cat$max.temp[which(temp.cat$group == combs$temp.group[i])]
  
  ggplot()+
    geom_tile(data = data,aes(x=longitude,y=latitude,fill = temp.N))+
    facet_wrap(~year)+
    coord_equal(xlim = region.lon,ylim = region.lat)+
    annotation_map(neus.map,fill = 'grey70')+
    annotate('polygon',x = subarea.df$x,y = subarea.df$y,fill = 'transparent',color = 'black')+
    scale_fill_viridis_c(name = 'Days')+
    ggtitle(paste0(combs$temp.group[i],' temp ',min.temp,'-',max.temp,' degrees: ',combs$subarea[i]))+
    theme_bw()+
    theme(legend.position = 'bottom',
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  ggsave(area.fig.name,width = 8,height = 12,units = 'in',dpi = 300)
    
  data.subarea.ls[[i]] = data%>%
    mutate(subarea = combs$subarea[i], temp.group = combs$temp.group[i])
  
}

data.all = bind_rows(data.subarea.ls)%>%
  filter(year == 2022)

area.all.df = as.data.frame(geom(area.shp))
area.et.df = as.data.frame(geom(area.shp[grep('ET',area.shp$SAMS)]))
for(i in 1:nrow(temp.cat)){
  
  min.temp = temp.cat$min.temp[i]
  max.temp = temp.cat$max.temp[i]
  
  data.temp = data.all %>%
    filter(temp.group == temp.cat$group[i])%>%
    group_by(year,longitude,latitude)%>%
    summarise(temp.N = mean(temp.N,na.rm=T))
  
  temp.fig.name = paste0(figure.dir,temp.cat$group[i],'_temp_NEUS_2022.png')
  
  ggplot()+
    geom_tile(data = data.temp,aes(x=longitude,y=latitude,fill = temp.N),width = 1/12,height = 1/12)+
    coord_equal()+
    annotation_map(neus.map,fill = 'grey70')+
    geom_polygon(data = area.all.df,aes(x= x, y = y, group = geom),fill = 'transparent',color = 'grey50')+
    geom_polygon(data = area.et.df,aes(x= x, y = y, group = geom),fill = 'transparent',color = 'black',size =2)+
    scale_fill_viridis_c(name = 'Days')+
    # ggtitle(paste0(combs$temp.group[i],' temp ',min.temp,'-',max.temp,' degrees'))+
    theme_bw()+
    xlab('Longitude')+
    ylab('Latitude')+
    theme(legend.position = 'bottom',
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 18),
          legend.key.size = unit(1.1,'cm'))
  ggsave(temp.fig.name,width = 8,height = 12,units = 'in',dpi = 300)
}
