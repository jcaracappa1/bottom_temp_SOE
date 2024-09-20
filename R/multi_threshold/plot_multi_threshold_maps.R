#Create maps of NES thresholds by year
library(terra)
library(ggplot2)
library(mapdata)

max.t.vect = c(seq(5,20,1),7.5)
years = 1959:2023

data.dir = here::here('data','multi_threshold','ndays')
figure.dir = here::here('Figures','multi_threshold')

fig.prefix = 'GLORYS_days_exceeding_'

neus.map = map_data('worldHires',region = c('USA','Canada'))

# 
# t=y=1
# 
# for(y in 1:length(years)){
#   
#   data.yr.ls = list()
#   # png(paste0(figure.dir,'/max_t/GLORYS_days_above_threshold_',years[y],'.png'),width = 12,height =12, units = 'in',res =300 ) 
#   for(t in 1:length(max.t.vect)){
#     
#     data = terra::rast(paste0(data.dir,'/',max.t.vect[t],'/GLORYS_NES_',max.t.vect[t],'C_ndays_',years[y],'.nc'))
# 
#     data.n.df = as.data.frame(data,xy =T)
#     colnames(data.n.df) = c('x','y','sum')
#     
#     data.n.df$sum[which(data.n.df$sum == 0)]= NA
#     
#     data.n.df$Threshold = max.t.vect[t]
#     
#     data.yr.ls[[t]] = data.n.df
#     
#   }
#   
#   data.yr = dplyr::bind_rows(data.yr.ls)
#   
#   ggplot(data.yr,aes(x=x,y=y,fill = sum))+
#     geom_tile()+
#     facet_wrap(~Threshold,labeller = label_both)+
#     xlim(-76,-65)+
#     ylim(35,45)+
#     coord_equal()+
#     scale_fill_viridis_c(name = 'Days Above Threshold')+
#     annotation_map(neus.map,fill = 'grey70',color = 'black')+
#     ggtitle(years[y])+
#     theme(legend.position = 'bottom')+
#     theme_bw()
#   
#   ggsave(paste0(figure.dir,'/year/GLORYS_days_above_thresholds_',years[y],'.png'),width = 18, height =18, units = 'in',dpi = 300)
#   
# }


t=y=1
for(t in 1:length(max.t.vect)){
  
  data.t.ls = list()
  
  pdf(paste0(figure.dir,'/max_t/GLORYS_days_above_thresholds_',max.t.vect[t],'C.pdf'))
  for(y in 1:length(years)){
    
    data = terra::rast(paste0(data.dir,'/',max.t.vect[t],'/GLORYS_NES_',max.t.vect[t],'C_ndays_',years[y],'.nc'))
    
    data.n.df = as.data.frame(data,xy =T)
    colnames(data.n.df) = c('x','y','sum')
    
    data.n.df$sum[which(data.n.df$sum == 0)]= NA  
    
    data.n.df$year = years[y]
    
    # data.t.ls[[y]] = data.n.df
    
    p = ggplot(data.n.df,aes(x=x,y=y,fill = sum))+
      geom_tile()+
      # facet_wrap(~year)+
      coord_equal()+
      xlim(-76,-65)+
      ylim(35,45)+
      xlab('')+
      ylab('')+
      scale_fill_viridis_c(name = paste('Days Above ',max.t.vect[t],'C'))+
      annotation_map(neus.map,fill = 'grey70',color = 'black')+
      ggtitle(years[y])+
      theme_bw()+
      theme(legend.position = 'bottom',
            plot.title = element_text(hjust = 0.5))
    
    gridExtra::grid.arrange(p)
      
  }
  dev.off()
  # data.t = dplyr::bind_rows(data.t.ls)
  

  
  # ggsave(,width = 18, height =18, units = 'in',dpi = 300)
  
}
