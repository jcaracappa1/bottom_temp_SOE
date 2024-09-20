#Plot the daily, monthly, annual habitable area for the NE shelf based on various temp maxima
library(dplyr)
library(ggplot2)
figure.dir = here::here('Figures','multi_threshold','habitat_proportion')

data.daily = readRDS(here::here('data','multi_threshold','daily_pct_area.rds'))%>%
  mutate(date = as.Date(date),
         month = format(date, format = '%m'),
         area.pct = area/255212.8)
data.daily$area.pct[which(data.daily$area.pct > 1)] = 1

data.monthly = data.daily %>%
  group_by(years,month,max.t)%>%
  summarise(area.pct.mean = mean(area.pct,na.rm=T),
            area.pct.median = median(area.pct,na.rm=T),
            area.pct.min = min(area.pct,na.rm=T),
            area.pct.max = max(area.pct,na.rm=T))%>%
  tidyr::unite(time,'years','month',sep = '-01-',remove =F)%>%
  mutate(time = as.Date(time, format = '%Y-%d-%m'))
saveRDS(data.monthly,here::here('data','multi_threshold','GLORYS_proportion_exceeding_NES_monthly.rds'))

#Plot Mean
plot.month = function(data,var.name,fig.dir,var.name.full){
  max.t.val = sort(unique(data$max.t))
  pdf(paste0(fig.dir,'/GLORYS_habitat_proportion_monthly_',var.name.full,'.pdf'),width = 12, height =5)
  for(i in 1:length(max.t.val)){
    plot.data = select(data,time,max.t, all_of(var.name))%>%
      rename(Value = var.name)%>%
      filter(max.t == max.t.val[i])
    
    p = ggplot(plot.data,aes(x = time, y = Value))+
      geom_line()+
      ylab(var.name.full)+
      xlab('Year-Month')+
      # ylim(0,1)+
      ggtitle(paste0('Proportion of Habitat >= ',max.t.val[i]))+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
    gridExtra::grid.arrange(p)
  }
  dev.off()
}

plot.month(data.monthly, 'area.pct.mean',figure.dir,'percent_area_mean')
plot.month(data.monthly, 'area.pct.median',figure.dir,'percent_area_median')
plot.month(data.monthly, 'area.pct.min',figure.dir,'percent_area_min')
plot.month(data.monthly, 'area.pct.max',figure.dir,'percent_area_max')

#calculated on Monthly data
data.annual = data.monthly %>%
  group_by(years,max.t)%>%
  summarise(area.pct.mean = mean(area.pct.mean,na.rm=T),
            area.pct.median = median(area.pct.mean,na.rm=T),
            area.pct.min = min(area.pct.mean,na.rm=T),
            area.pct.max = max(area.pct.mean,na.rm=T))

saveRDS(data.annual,here::here('data','multi_threshold','GLORYS_proportion_exceeding_NES_annual.rds'))
  
plot.annual = function(data,var.name,fig.dir,var.name.full){
  max.t.val = sort(unique(data$max.t))
  pdf(paste0(fig.dir,'/GLORYS_habitat_proportion_annual_',var.name.full,'.pdf'),width = 12, height =5)
  for(i in 1:length(max.t.val)){
    plot.data = select(data,years,max.t, all_of(var.name))%>%
      rename(Value = var.name)%>%
      filter(max.t == max.t.val[i])
    
    p = ggplot(plot.data,aes(x = years, y = Value))+
      geom_line()+
      # ylim(0,1)+
      ylab(var.name.full)+
      xlab('Year')+
      ggtitle(paste0('Proportion of Habitat >= ',max.t.val[i]))+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
    gridExtra::grid.arrange(p)
  }
  dev.off()
}

plot.annual(data.annual, 'area.pct.mean',figure.dir,'percent_area_mean')
plot.annual(data.annual, 'area.pct.median',figure.dir,'percent_area_median')
plot.annual(data.annual, 'area.pct.min',figure.dir,'percent_area_min')
plot.annual(data.annual, 'area.pct.max',figure.dir,'percent_area_max')
