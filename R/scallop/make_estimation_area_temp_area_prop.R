#Make timeseries plots for each estimation area (and combined areas) that shows prop area in each temp cat
library(terra)
library(dplyr)
library(ggplot2)

data.dir = here::here('data','estimation_areas','gridded_daily_estimation_areas_binary_mask','/')

temp.cat = data.frame(
  group = c('normal','stressed','danger'),
  min.temp = c(0,17,19),
  max.temp = c(17,19,30)
)

area.shp = project(vect(here::here('geometry','MAB_ESTIMATION_AREAS_2023_UTM18_PDT_NYB.shp')),' +proj=longlat +datum=WGS84 +no_defs ')
area.df = as.data.frame(area.shp)

combs = expand.grid(temp.group = temp.cat$group,subarea = area.shp$NewSAMS)

fig.dir = here::here('Figures','scallop','estimation_areas','/')

i=1
data.all.ls = list()
max.t = numeric()
for(i in 1:nrow(combs)){
  
  
  data.file = paste0(data.dir,'gridded_bottomT_binary_',combs$subarea[i],'_',combs$temp.group[i],'.nc')
  
  data = rast(data.file)
  
  this.area = area.shp[which(area.df$NewSAMS == combs$subarea[i]),]
  this.area.res = resample(rast(this.area),subset(data,1))
  values(this.area.res) = 1
  this.area.mask = mask(this.area.res,this.area)
  area.tot = area(this.area.mask)*1E-6
  # plot(this.area.mask)

  # area.tot = area(subset(data,1),mask = F,na.rm=F)*1E-6
  
  data.comb= data.frame(temp.group = combs$temp.group[i],
             subarea = combs$subarea[i],
             time = time(data),
             area = area(data)*1E-6,
             area.tot = area.tot)%>%
    mutate(area.prop = area/area.tot,
           date = as.Date(time))
  
  min.temp = temp.cat$min.temp[which(temp.cat$group == combs$temp.group[i])]
  max.temp = temp.cat$max.temp[which(temp.cat$group == combs$temp.group[i])]
  
  max.t[i] = max.temp
  
  area.fig.dir = paste0(fig.dir,combs$subarea[i],'/')
  if(!dir.exists(area.fig.dir)){dir.create(area.fig.dir,recursive = T)}
  
  fig.name = paste0(area.fig.dir,'prop_area_',combs$temp.group[i],'_temp_',combs$subarea[i],'.png')
  
  
  # ggplot(data.comb, aes(x = date, y = area.prop))+
  #   geom_line()+
  #   theme_bw()+
  #   ylab('Area (km2)')+
  #   xlab('')+
  #   ggtitle(paste0(combs$temp.group[i],' temp ',min.temp,'-',max.temp,' degrees: ',combs$subarea[i]))+
  #   theme(plot.title = element_text(hjust = 0.5))
  # ggsave(fig.name,width = 10,height = 6,units = 'in',dpi = 300)
  # 
  data.all.ls[[i]] = data.comb
  
}

data.all = bind_rows(data.all.ls)%>%
  filter(temp.group != 'normal')%>%
  group_by(temp.group,date)%>%
  summarise(area = sum(area,na.rm=T),
            area.tot = sum(area.tot,na.rm=T))%>%
  mutate(area.prop = area/area.tot)

ggplot(data.all,aes(x=date,y=area.prop))+
  geom_line()+
  facet_wrap(~temp.group,nrow = 2)+
  theme_bw()+
  ylab('Area Proportion')+
  xlab('')+
  scale_x_date(date_breaks = '2 year',date_labels = '%Y')
ggsave(paste0(fig.dir,'temperature_groups_proportional_area.png'),width = 12,height = 8,units = 'in', dpi =300)
