#Plot thermal habiat indices
library(dplyr)
library(ggplot2)

spp.names = read.csv(url('https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/refs/heads/master/currentVersion/neus_groups.csv')) %>% select(Code,LongName)

thresh = read.csv(url('https://raw.githubusercontent.com/NEFSC/READ-EDAB-enviroThresholds/refs/heads/main/thresholds/atlantis_group_thresholds_survdat.csv')) %>%
  left_join(spp.names)%>%
  arrange(-max_bottom_temp)%>%
  mutate(ID = 1:n())


data = readRDS(here::here('data','SOE','thermal_habitat_2025_V2.rds'))%>%
  filter(year != 2024)%>%
  tidyr::gather(Varname, Value, -year,-t.max,-epu)%>%
  mutate(Value = as.numeric(Value),
         current.year = ifelse(year == max(year),T,F))

var.groups = c('nd.con.','^nd.(?!con)','dd.','pct.area.')
plot.names = c('Number_Consecutive_Days','Number_Days','Degree_Days','Percent_Area')
plot.units = c('Consecutive Days','Days','Degree Days','Proportion Area')

i=1
pdf(here::here('Figures','Thermal_Habitat_Metrics.pdf'))
for(i in 1:length(var.groups)){
  
  data.var = data %>%
    filter(grepl(var.groups[i],Varname,perl = T) & year == 2023)
    
  p =ggplot(data.var, aes( x= t.max, y = Value))+
    geom_line()+
    facet_grid(Varname~epu,scales = 'free_y')+
    ggtitle(plot.names[i])+
    ylab(plot.units[i])+
    xlab('Temperature Max (degC)')+
    theme_bw()
  
  gridExtra::grid.arrange(p)
  
}

dev.off()


data.nd.con = data %>%
  filter(Varname == 'nd.con.mean' & current.year == F)

data.nd.con.this.year = data %>%
  filter(Varname == 'nd.con.mean' & current.year == T)

epu.names = c('all','MAB','GB','GOM','SS')
pdf(here::here('Figures','Consecutive Days All Years.pdf'),width = 18,height = 8)
for(i in 1:length(epu.names)){
  x = filter(data.nd.con.this.year, epu == epu.names[i])
  p =ggplot()+
    geom_text(data = thresh, aes(x = max_bottom_temp, y = (365*ID)/nrow(thresh),label = LongName ),hjust = 0)+
    geom_segment(data = thresh, aes(x = max_bottom_temp, y = 0,yend = (365*ID)/nrow(thresh)), linewidth =0.5,linetype =2, color = 'grey')+
    # geom_line(data=filter(data.nd.con, epu == epu.names[i]),aes(x= t.max, y = Value, color = current.year,group = factor(year)))+
    # scale_color_manual(values = c('grey80','red'))+
    geom_line(data=filter(data.nd.con, epu == epu.names[i]),aes(x= t.max, y = Value, color = year,group = year))+
    annotate('line',x = x$t.max, y = x$Value, color = 'red')+
    ylab('Consecutive Days')+
    xlab('Max Bottom Temperature')+
    xlim(0,35)+
    facet_wrap(~epu,ncol =2)+
    guides(color = F)+
    theme_bw()
  gridExtra::grid.arrange(p)
}
dev.off()

data.pct.area = data %>%
  filter(Varname == 'pct.area.mean' & current.year == F)

data.pct.area.this.year = data %>%
  filter(Varname == 'pct.area.mean' & current.year == T)

epu.names = c('all','MAB','GB','GOM','SS')
pdf(here::here('Figures','Percent Area All Years.pdf'),width = 18,height = 8)
for(i in 1:length(epu.names)){
  x = filter(data.pct.area.this.year, epu == epu.names[i])
  p =ggplot()+
    geom_text(data = thresh, aes(x = max_bottom_temp, y = (ID)/nrow(thresh),label = LongName ),hjust = 0)+
    geom_segment(data = thresh, aes(x = max_bottom_temp, y = 0,yend = (ID)/nrow(thresh)), linewidth =0.5,linetype =2, color = 'grey')+
    # geom_line(data=filter(data.pct.area, epu == epu.names[i]),aes(x= t.max, y = Value, color = current.year,group = factor(year)))+
    # scale_color_manual(values = c('grey80','red'))+
    geom_line(data=filter(data.pct.area, epu == epu.names[i]),aes(x= t.max, y = Value, color = year,group = year))+
    annotate('line',x = x$t.max, y = x$Value, color = 'red')+
    ylab('Consecutive Days')+
    xlab('Max Bottom Temperature')+
    xlim(0,35)+
    facet_wrap(~epu,ncol =2)+
    guides(color = F)+
    theme_bw()
  gridExtra::grid.arrange(p)
}
dev.off()

data.dd = data %>%
  filter(grepl('dd.',Varname,perl = T) & current.year == F)%>%
  tidyr::spread(Varname,Value)

data.dd.this.year = data %>%
  filter(grepl('dd.',Varname,perl = T) & current.year == T)%>%
  tidyr::spread(Varname,Value)

epu.names = c('all','MAB','GB','GOM','SS')
pdf(here::here('Figures','Degree Days All Years.pdf'),width = 18,height = 8)
for(i in 1:length(epu.names)){
  x = filter(data.dd.this.year, epu == epu.names[i])
  ymax = max(data.dd$dd.max,na.rm=T)
  p =ggplot()+
    geom_text(data = thresh, aes(x = max_bottom_temp, y = (ymax*ID)/nrow(thresh),label = LongName ),hjust = 0)+
    geom_segment(data = thresh, aes(x = max_bottom_temp, y = 0,yend = (ymax*ID)/nrow(thresh)), linewidth =0.5,linetype =2, color = 'grey')+
    # geom_line(data=filter(data.nd.con, epu == epu.names[i]),aes(x= t.max, y = Value, color = current.year,group = factor(year)))+
    # scale_color_manual(values = c('grey80','red'))+
    # geom_ribbon(data=filter(data.dd, epu == epu.names[i]),aes(x= t.max, y = dd.mean, fill = year,group = year))+
    geom_line(data=filter(data.dd, epu == epu.names[i]),aes(x= t.max, y =dd.min, color = year,group = year),lty =2)+
    geom_line(data=filter(data.dd, epu == epu.names[i]),aes(x= t.max, y =dd.mean, color = year,group = year),lty =1)+
    geom_line(data=filter(data.dd, epu == epu.names[i]),aes(x= t.max, y =dd.max, color = year,group = year),lty =2)+
    annotate('line',x = x$t.max, y = x$dd.min, color = 'red',lty =2)+
    annotate('line',x = x$t.max, y = x$dd.mean, color = 'red')+
    annotate('line',x = x$t.max, y = x$dd.max, color = 'red',lty =2)+
    ylab('Consecutive Days')+
    xlab('Max Bottom Temperature')+
    xlim(0,35)+
    ylim(0,ymax)+
    facet_wrap(~epu,ncol =2)+
    guides(color = F)+
    theme_bw()
  gridExtra::grid.arrange(p)
}
dev.off()

