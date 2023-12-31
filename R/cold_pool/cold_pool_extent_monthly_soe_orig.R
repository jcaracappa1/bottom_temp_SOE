# -------------------------------------------
rm(list=ls())
# -------------------------------------------
library(tidyverse)
library(sf)
library(colorRamps)
# -------------------------------------------
# LOAD DATA - BOTTPM TEMPERATURE
load(here::here('data','cold_pool',"bt_temp_1959_2022_april2023.Rda"))
bt_temp_time_series = bt_temp_time_series
  
# load month, day, year data
load(here::here('data','cold_pool',"y_m_cd.Rda"))
# -------------------------------------------
# Load grid cell within the cold area: 
# within the SNEMA management area + depth between 20 and 200m
load(here::here('data','cold_pool','grid_cell_cp.Rda'))
# -------------------------------------------
# Computing monthly temperature
bt_temp_time_series_month <- filter(bt_temp_time_series,cell_no %in% grid_cell_cp[,"cell_no"]) %>%
  inner_join(y_m_cd,by=c("year","calendar_day")) %>%
  group_by(cell_no,year,month) %>%
  summarise(bt_temp=mean(bt_temp)) %>%
  as.data.frame()
head(bt_temp_time_series_month)
dim(bt_temp_time_series_month)
length(unique(bt_temp_time_series_month$year))
length(unique(bt_temp_time_series_month$month))
length(unique(bt_temp_time_series_month$cell_no))
table(bt_temp_time_series_month[,"year"])
table(bt_temp_time_series_month[,"month"])
table(bt_temp_time_series_month[,c("year","month")])
hist(bt_temp_time_series_month$bt_temp)
# ----------------------------------------------------------------------
# MAP
# -------------------------------------------
# 1/12° grid
load(here::here('data','cold_pool',"grid_cell_no.Rda"))
# extract land border
load(here::here('data','cold_pool',"geo_epu.Rda"))
# EPU delineation
load(here::here('data','cold_pool',"geo_borders.Rda"))
# ---------------------------------------------------------------------------
ymax<-42
ymin<-34.5
xmax<-(-68.5)
xmin<-(-76.5)
xlim1<-c(xmin,xmax)
ylim1<-c(ymin,ymax)
# ----------------------------------------------------------------------
dt_cp_extent<- filter(bt_temp_time_series_month, year%in%(1959:2022) & month%in%(6:9)) %>%
  group_by(cell_no)  %>%
  summarise(avg_bottom_t=mean(bt_temp)) %>%
  as.data.frame() %>%
  filter(avg_bottom_t<10) 
head(dt_cp_extent)
dim(dt_cp_extent)
# 
dt_cp_extent_map<-inner_join(grid_cell_no,dt_cp_extent, by="cell_no")
map_cp_extent <- ggplot()+ 
  geom_sf(data=dt_cp_extent_map, aes(fill=avg_bottom_t,color=avg_bottom_t),size=0.005) +
  geom_sf(data=geo_borders, colour=NA, fill="lightgoldenrod2")+ # geo_border loaded in the map code see source(...)
  scale_x_continuous(limits=xlim1) +  scale_y_continuous(limits=ylim1) # limit in the map R code
# ------------------------------------------------------
# create shapefile with the grid cell within the cold pool extent
# Open it in QGIS or ArcGIS or others - report isolated grid cell numbers 
# st_write(dt_cp_extent_map, dsn = "dt_cp_extent_map.shp", layer = "dt_cp_extent_map.shp", driver = "ESRI Shapefile")
# ------------------------------------------------------
# /!\ preliminary analysis showed some isolated grid cells ==> We removed this cell
# should be adjust each year
dt_cp_extent <-  filter(dt_cp_extent,!cell_no%in%c(64515,58938,58541,58542,58145,47748))
head(dt_cp_extent)
dim(dt_cp_extent)
# 
dt_cp_extent_map<-inner_join(grid_cell_no,dt_cp_extent, by="cell_no")
# DOWNLOAD MAP
map_cp_extent <- ggplot()+ 
  geom_sf(data=dt_cp_extent_map, aes(fill=avg_bottom_t,color=avg_bottom_t),size=0.005) +
  scale_fill_gradientn(colours=matlab.like(50),name="Mean bottom\ntemperature")+
  scale_color_gradientn(colours=matlab.like(50),name="Mean bottom\ntemperature")+
  geom_sf(data=geo_borders, colour=NA, fill="lightgoldenrod2")+ # geo_border loaded in the map code see source(...)
  scale_x_continuous(limits=xlim1) + # limit in the map R code
  scale_y_continuous(limits=ylim1) + # limit in the map R code
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        panel.grid.major = element_line(colour = "grey50",size=0.1,linetype = "dotted")) +
  ggtitle("Cold pool extent") 
# ------------------------------------------------------
jpeg(file = "cp_extent_soe.jpeg", width = 14, height = 14, units = "cm", res = 500)
map_cp_extent
dev.off()
