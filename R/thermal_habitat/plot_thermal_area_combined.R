#Script to plot timeseries of proportion of EPU area above temperature threshold
library(dplyr)
library(tidyr)
library(ggplot2)

# tz.combs = read.csv(here::here('data','thermal_area','tz_groups.csv'),as.is = T)%>%
#   rename(min.depth = 'min.z',
#          max.depth = 'max.z',
#          temp.threshold = 'min.temp')

data = read.csv(here::here('data','SOE','thermal_habitat_area_2023.csv'))%>%
  mutate(current.year = year == max(year),
         date = as.Date(date),
          min.depth.name = paste0(min.depth,'m'),
         max.depth.name = paste0(max.depth,'m'),
         temp.threshold.name = paste0('>',temp.threshold, '\u00B0C'))%>%
  # left_join(tz.combs)%>%
  unite(depth.name, c('min.depth.name','max.depth.name'),sep = '-')

data$depth.name = factor(data$depth.name, levels = c('0m-25m','25m-100m','100m-3000m'))


  
epu.names = sort(unique(data$epu))
years = sort(unique(data$year))

i=1
for(i in 1:length(epu.names)){
  
  data.epu = data%>%
    filter(epu == epu.names[i])

  
  data.epu.single.source= data.epu %>%
    select(source,date,depth.name,temp.threshold.name,area.prop)%>%
    tidyr::spread(source,area.prop,fill = NA)%>%
    mutate(present.both = !is.na(GLORYS + PSY),
           PSY.mask = ifelse(!present.both,1,NA))%>%
    mutate(PSY = PSY * PSY.mask)%>%
    gather(source,area.prop, GLORYS:PSY)%>%
    select(-present.both,-PSY.mask)%>%
    filter(!is.na(area.prop))%>%
    mutate(j.day = as.numeric(format(date,format = '%j')),
           year = as.numeric(format(date,format = '%Y')),
           current.year = year == max(year))
  
  data.epu.range = data.epu.single.source %>%
    group_by(depth.name,temp.threshold.name,j.day)%>%
    summarise(area.prop.max = max(area.prop),
              area.prop.min = min(area.prop))
  
  data.epu.plot = data.epu.single.source%>%
    filter(current.year == T)%>%
    left_join(data.epu.range)
  
  ggplot()+
    # geom_ribbon(data=data.epu.plot,aes(x = j.day, ymin = area.prop.min, ymax = area.prop.max),fill = 'grey50')+
    geom_line(data = data.epu.single.source,aes(x = j.day,y = area.prop,color = year))+
    scale_color_gradient(low = 'grey10',high = 'grey90')+
    geom_line(data=data.epu.plot,aes(x= j.day, y= area.prop),color = 'red',alpha = 0.7,size =1)+
    
    facet_grid(temp.threshold.name~depth.name)+
    theme_bw()+
    xlab('Calendar Day')+
    ylab('Proportion of EPU Area above threshold')+
    ggtitle(epu.names[i])+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(here::here('Figures','SOE',paste0('thermal_area_',epu.names[i],'.png')),width =12,height = 8, units = 'in',dpi = 300)
}

# x = data %>%
#   filter(epu == 'GB'& depth.name == '25m-100m' & temp.threshold == 15 & year == 2023)
