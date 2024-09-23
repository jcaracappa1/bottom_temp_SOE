#Do BSB minimum bottom temp data 
library(ggplot2)
library(mapdata)
library(dplyr)

figure.dir = here::here('Figures','BlackSeabass')

epu.shp =system.file('data','EPU_NOESTUARIES.shp',package = 'EDABUtilities')

glorys.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/'
glorys.files = list.files(glorys.dir,'GLORYS_daily_BottomTemp_',full.names = T)

roms.dir= 'E:/duPontavice_bt/bt_revised_metadata_032024/'
roms.files.short = list.files(roms.dir,'^bottom_temp_',full.names = T)
roms.files.long = list.files(roms.dir,'^bottom_temp_',full.names = T)
roms.yr = sapply(roms.files,function(x) as.numeric(strsplit(x,'bottom_temp_|.nc')[[1]][2]),USE.NAMES = F)
roms.files.long = roms.files.long[which(roms.yr<1993)]

glorys.crop = EDABUtilities::crop_nc_2d(
  input.files = glorys.files,
  shp.file = epu.shp,
  var.name = 'BottomT',
  write.out = F
)
roms.crop = EDABUtilities::crop_nc_2d(
  input.files = roms.files.long,
  shp.file = epu.shp,
  var.name = 'sea_water_temperature_at_sea_floor ',
  write.out = F
)


roms.4 = EDABUtilities::make_2d_deg_day_gridded_nc(
  data.in = roms.crop,
  write.out = F,
  shp.file = epu.shp,
  var.name = 'sea_water_temperature_at_sea_floor',
  statistic = 'nd',
  ref.value = 4,
  type = 'below',
  area.names = c('MAB','GB','GOM')
)


glorys.4 = EDABUtilities::make_2d_deg_day_gridded_nc(
  data.in = glorys.crop,
  write.out = F,
  shp.file = epu.shp,
  var.name = 'BottomT',
  statistic = 'nd',
  ref.value = 4,
  type = 'below',
  area.names = c('MAB','GB','GOM')
)

data.all = c(roms.4,glorys.4)
years = numeric()
for(k in 1:length(roms.crop)){ years = c(years,format(terra::time(roms.crop[[k]]),format = '%Y')[1])}
for(k in 1:length(glorys.crop)){ years = c(years,format(terra::time(glorys.crop[[k]]),format = '%Y')[1])}

#Plot maps
neus.map = map_data('worldHires',region = c('USA','Canada'))


i=1
pdf(paste0(figure.dir,'/Black_Seabass_Days_Below_4C.pdf'))
for(i in 1:length(data.all)){
  
  data = data.all[[i]]
  data.df = as.data.frame(data,xy =T)
  
  p = ggplot(data.df,aes(x=x,y=y,fill = sum))+
    geom_tile()+
    coord_equal()+
    xlim(-76,-65)+
    ylim(35,45)+
    xlab('')+
    ylab('')+
    scale_fill_viridis_c(name = paste('Days Below 4C'))+
    annotation_map(neus.map,fill = 'grey70',color = 'black')+
    ggtitle(years[i])+
    theme_bw()+
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5))
  gridExtra::grid.arrange(p)
  
  print(i)
  
}
dev.off()

#Area time series
day.bins = data.frame( day.min = c(0,30,60,90),
                       day.max = c(30,60,90,366),
                       day.group = 1:4)

out.df = expand.grid(years = as.numeric(years), max.t = 4, day.group = day.bins$day.group )%>%
  left_join(day.bins)%>%
  mutate(area = NA,
         pct.area = NA)

area.tot = sum(terra::expanse(terra::vect(epu.shp), unit = 'km'))

i=1
for(i in 1:nrow(out.df)){
  
  data = data.all[[which( out.df$years[i] == years)]]
  
  data.bin = terra::clamp(data,lower = out.df$day.min[i], upper = out.df$day.max[i],values =F)
  
  data.area = terra::expanse(data.bin, unit = 'km')$area
  
  out.df$area[i] = data.area
  out.df$pct.area[i] = data.area/area.tot
  
}

saveRDS(out.df,here::here('data','BSB_below_4_area.rds'))

out.df =out.df %>%
  tidyr::unite(plot.group, day.min, day.max, sep = '-')
out.df$plot.group[which(out.df$plot.group == '90-366')] = '90+'

ggplot(out.df,aes(x= years, y = pct.area, color = plot.group))+
  geom_line(size =1.1)+
  scale_color_manual(name = 'Duration of Exposure (days)', values = RColorBrewer::brewer.pal(4,'Set2'))+
  ylab("Proportion of NE shelf")+
  ggtitle('Days Below 4C')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,'/Black_Seabass_Proportion_Below_4C.png'),width = 10,height = 8, units = 'in',dpi =250)
