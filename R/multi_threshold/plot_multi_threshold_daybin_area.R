#Script to generate time series plots showing the area of cells that exceed temperature thresholds for certain durations
library(dplyr)
library(ggplot2)

figure.dir = here::here('Figures','multi_threshold','habitat_area_daybins')

data = read.csv(here::here('data','multi_threshold','area','multi_threshold_area.csv'))%>%
  tidyr::unite(plot.group, day.min, day.max, sep = '-')
data$pct.area[which(data$pct.area > 1)] = 1
data$plot.group[which(data$plot.group == '90-366')] = '90+'

max.t.val = sort(unique(data$max.t))

pdf(paste0(figure.dir,'/GLORYS_habitat_area_daybins.pdf'))
for(i in 1:length(max.t.val)){
  plot.data = filter(data, max.t == max.t.val[i])
  
  p = ggplot(plot.data,aes(x = years, y = area, color = plot.group))+
    geom_line()+
    labs(color = 'Duration Range (days)')+
    xlab('Year')+
    ylab('Area')+
    ggtitle(paste0('Area Exceeding ',max.t.val[i],'C'))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom')
  
  gridExtra::grid.arrange(p)
}
dev.off()

pdf(paste0(figure.dir,'/GLORYS_habitat_area_proportion_daybins.pdf'))
for(i in 1:length(max.t.val)){
  plot.data = filter(data, max.t == max.t.val[i])
  
  p = ggplot(plot.data,aes(x = years, y = pct.area, color = plot.group))+
    geom_line()+
    labs(color = 'Duration Range (days)')+
    ylim(0,1)+
    xlab('Year')+
    ylab('Proportion of Area')+
    ggtitle(paste0('Proportion of Area Exceeding ',max.t.val[i],'C'))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom')
  
  gridExtra::grid.arrange(p)
}
dev.off()
