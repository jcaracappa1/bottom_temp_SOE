# -------------------------------------------
rm(list=ls())
# ------------------------------------------------------------------
library(tidyverse)
library(ggpubr)
# -------------------------------------------
dt_cp<-read.csv(file="cold_pool_indices/cold_pool_indices_1959_2022.csv")
# ----------------------------------------------------
# ----------------------------------------------------
# Cold Pool Index
# ----------------------------------------------------
# ----------------------------------------------------
limit <- aes(x = year, ymax = ((-cold_pool_index)+(qnorm(0.975)*se_cold_pool_index)) , 
             ymin = ((-cold_pool_index)-(qnorm(0.975)*se_cold_pool_index)))
#
cpi_plot <- ggplot() +
  geom_hline(yintercept = 0, size=0.5,colour="grey60") + 
  geom_line(data=dt_cp, aes(x=year,y=(-cold_pool_index)),size=0.4)+ 
  geom_point(data=dt_cp, aes(x=year,y=(-cold_pool_index)),size=0.8)+ 
  geom_ribbon(data=dt_cp,limit,alpha=0.1,linetype="dotted", color="black",size=0.3) +
  annotate(geom="text",label="Warmer", y=(-2.85), x=2022.9,size=3,fontface="bold",color="red",hjust=0.8)+
  annotate(geom="text",label="Colder", y=(2.25), x=2022.9,size=3,fontface="bold",color= "blue",hjust=0.8)+
  geom_segment(mapping=aes(x=2022.9, y=(-0.1), xend=2022.9, yend=(-2.75)), size=0.8,arrow = arrow(length = unit(0.2, "cm")),color="red")+
  geom_segment(mapping=aes(x=2022.9, y=(0.1), xend=2022.9, yend=(2.15)), size=0.8,arrow = arrow(length = unit(0.2, "cm")),color= "blue") + 
  scale_y_continuous(name="Cold Pool Index  (x(-1))") + scale_x_continuous(name="Year",breaks=seq(1950,2022,10)) + 
  coord_cartesian(ylim=c(-2.8,2.2),c(1959,2022.9))  +
  theme_bw() + theme(axis.title = element_text(size=11), axis.text = element_text(size=11), legend.position = "none")
# ----------------------------------------------------
# ----------------------------------------------------
# Persistence Index
# ----------------------------------------------------
# ----------------------------------------------------
limit <- aes(x = year, ymax = (persistence_index+(qnorm(0.975)*se_persistence_index)) , 
             ymin = (persistence_index-(qnorm(0.975)*se_persistence_index)))
# ----------------------------------------------------
persistence_plot <- ggplot() +
  geom_hline(yintercept = 0, size=0.5,colour="grey60") + 
  geom_line(data=dt_cp, aes(x=year,y=persistence_index),size=0.4)+
  geom_point(data=dt_cp, aes(x=year,y=persistence_index),size=0.8)+
  geom_ribbon(data=dt_cp,limit,alpha=0.2,linetype="dashed", size=0.3,color="black")+
  annotate(geom="text",label="Longer", y=(0.8), x=2022.9,size=3,fontface="bold",color="blue",hjust=0.8)+
  annotate(geom="text",label="Shorter", y=(-1.7), x=2022.9,size=3,fontface="bold",color= "red",hjust=0.8)+
  geom_segment(mapping=aes(x=2022.9, y=(0.1), xend=2022.9, yend=(0.75)), size=0.8,arrow = arrow(length = unit(0.2, "cm")),color="blue")+
  geom_segment(mapping=aes(x=2022.9, y=(-0.1), xend=2022.9, yend=(-1.65)), size=0.8,arrow = arrow(length = unit(0.2, "cm")),color= "red") + 
  scale_y_continuous(name="Persistence Index",breaks=seq(-4,4,0.5)) + scale_x_continuous(name="Year",breaks=seq(1950,2022,10)) + 
  coord_cartesian(ylim=c(-1.65,0.8),c(1958,2022.9)) +
  theme_bw()+
  theme(axis.title = element_text(size=11), axis.text = element_text(size=11))
# ----------------------------------------------------
# ----------------------------------------------------
#  Spatial Extent  Index
# ----------------------------------------------------
# ----------------------------------------------------
limit <- aes(x = year, ymax = (extent_index+(qnorm(0.975)*se_extent_index)) , 
             ymin = (extent_index-(qnorm(0.975)*se_extent_index)))
# ----------------------------------------------------
sp_extent_plot <- ggplot() +
  geom_hline(yintercept = 0, size=0.5,colour="grey60") + 
  geom_line(data=dt_cp, aes(x=year,y=extent_index),size=0.4)+
  geom_point(data=dt_cp, aes(x=year,y=extent_index),size=0.8)+
  geom_ribbon(data=dt_cp,limit,alpha=0.2,linetype="dashed", size=0.3,color="black")+
  annotate(geom="text",label="Larger", y=(95), x=2023.2,size=3,fontface="bold",color="blue",hjust=0.8)+
  annotate(geom="text",label="Smaller", y=(-445), x=2023.2,size=3,fontface="bold",color= "red",hjust=0.8)+
  geom_segment(mapping=aes(x=2023.2, y=(10), xend=2023.2, yend=(85)), size=0.8,arrow = arrow(length = unit(0.2, "cm")),color="blue")+
  geom_segment(mapping=aes(x=2023.2, y=(-10), xend=2023.2, yend=(-435)), size=0.8,arrow = arrow(length = unit(0.2, "cm")),color= "red") + 
  scale_y_continuous(name="Spatial Extent Index",breaks=seq(-400,400,50)) + scale_x_continuous(name="Year",breaks=seq(1950,2022,10)) + 
  coord_cartesian(ylim=c(-435,80),c(1958,2023)) +
  theme_bw()+ theme(axis.title = element_text(size=11), axis.text = element_text(size=11))
  # ----------------------------------------------------------------
  # download plot
  jpeg(file = "cold_pool_indices_soe2023.jpeg", width = 33, height = 12, units = "cm", res = 500, type="quartz")
  ggarrange(cpi_plot,persistence_plot,sp_extent_plot,
            labels=c("(a)","(b)","(c)"),font.label = list(size = 12),ncol=3,nrow=1)
  dev.off()
  getwd()
