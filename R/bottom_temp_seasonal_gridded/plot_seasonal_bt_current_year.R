library(terra)
library(ggplot2)
library(mapdata)
library(ggmap)
library(maps)

data.dir = here::here('data','SOE','bt_seasonal_gridded','/')

file.prefix = 'bt_seasonal_gridded_'

file.names = list.files(data.dir)

file.season = sapply(file.names,function(x) strsplit(x,paste0(file.prefix,'|_|.nc'))[[1]][2],USE.NAMES = F)

season.names = c('winter','spring','summer','fall')

current.yr.ls = list()
i=1
for(i in 1:length(season.names)){
  
  which.file = grep(season.names[i],file.names)
  
  data.season = rast(paste0(data.dir,file.names[which.file]))    
  
  data.time = time(data.season)
  data.time.y = as.numeric(format(data.time,format = '%Y'))
  
  this.year = which(data.time.y == max(data.time.y))
  
  current.yr.ls[[i]] = subset(data.season, this.year-1)%>%
    as.data.frame(xy = T)%>%
    mutate(season = season.names[i])
  
}

current.yr.df = bind_rows(current.yr.ls)
current.yr.df$season = factor(current.yr.df$season,levels = season.names)

ggplot(current.yr.df,aes(x=x,y=y,fill = BottomT_64))+
  geom_tile()+
  annotation_map(map_data('worldHires'),fill = 'grey70',alpha=0.7)+
  facet_wrap(~season)+
  scale_fill_viridis_c()+
  theme_bw()

ggsave(here::here('Figures','SOE','NEUS_bottom_temp_seasonal.png'))
