# -------------------------------------------
rm(list=ls())
# -------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(colorRamps)
library(tidync)
library(terra)
library(mapdata)
library(ggmap)
library(maps)
# -------------------------------------------
# LOAD DATA - BOTTPM TEMPERATURE
cell.index = readRDS(here::here('data','cold_pool','cold_pool_cell_index.rds'))
cp.rast = rast(here::here('data','cold_pool','cold_pool_rast.nc'))

roms.dat = readRDS(here::here('data','cold_pool','cold_pool_input_ROMS.rds'))

glorys.dat = readRDS(here::here('data','cold_pool','cold_pool_input_GLORYS.rds'))

psy.dat = readRDS(here::here('data','cold_pool','cold_pool_input_ROMS.rds'))

bt_temp_time_series = bind_rows(roms.dat,glorys.dat,psy.dat)%>%
  left_join(cell.index)

saveRDS(bt_temp_time_series,here::here('data','cold_pool','bt_temp_time_series.rds'))

rm(glorys.dat,psy.dat,roms.dat)
gc()

# #Optional = Run with last year
bt_temp_time_series = readRDS(here::here('data','cold_pool','bt_temp_time_series.rds'))
# bt_temp_time_series = filter(bt_temp_time_series,year == 2010)
# saveRDS(bt_temp_time_series,here::here('data','cold_pool','bt_temp_time_series_2022.rds'))
# bt_temp_time_series = readRDS(here::here('data','cold_pool','bt_temp_time_series_2022.rds'))

# load('C:/Users/joseph.caracappa/Downloads/bt_temp_1959_2022_april2023.Rda')
# load("temperature_data/bt_temp_1959_2022_april2023.Rda")
# load month, day, year data

load(here::here('data','cold_pool','y_m_cd.Rda'))

# -------------------------------------------
# Load grid cell within the cold area: 
# within the SNEMA management area + depth between 20 and 200m
#load("data_to_run_scripts/grid_cell_cp.Rda")
# -------------------------------------------
# Computing monthly temperature
bt_temp_time_series_month <- filter(bt_temp_time_series,cell_no %in% cell.index$cell_no) %>%
  inner_join(y_m_cd,by=c("year","calendar_day")) %>%
  group_by(cell_no,year,month) %>%
  summarise(bt_temp=mean(bt_temp)) %>%
  as.data.frame()

saveRDS(bt_temp_time_series_month,here::here('data','cold_pool','bt_temp_time_series_month.rds'))

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
# load(here::here('data','grid_cell_no.Rda'))
# extract land border
load(here::here('data','cold_pool','geo_epu.Rda'))
# EPU delineation
load(here::here('data','cold_pool', 'geo_borders.Rda'))
# ---------------------------------------------------------------------------
ymax<-42
ymin<-34.5
xmax<-(-68.5)
xmin<-(-76.5)
xlim1<-c(xmin,xmax)
ylim1<-c(ymin,ymax)
# ----------------------------------------------------------------------
dt_cp_extent<- filter(bt_temp_time_series_month, year%in%(1959:2023) & month%in%(6:9)) %>%
  group_by(cell_no)  %>%
  summarise(avg_bottom_t=mean(bt_temp)) %>%
  as.data.frame() %>%
  filter(avg_bottom_t<10) 

saveRDS(dt_cp_extent,here::here('data','cold_pool','dt_cp_extent.rds'))

head(dt_cp_extent)
dim(dt_cp_extent)
# 
# dt_cp_extent_map<-inner_join(cell.index,dt_cp_extent, by="cell_no")
dt_cp_extent_map<-left_join(dt_cp_extent,cell.index, by="cell_no")%>%st_as_sf()

#Do to identify missing points instead of gis
# cp.rast.map = cp.rast
# values(cp.rast.map)[cells(cp.rast.map)[which(!(cells(cp.rast.map) %in% dt_cp_extent_map$cell_no))]] = NA
# values(cp.rast.map)[cells(cp.rast.map)[which((cells(cp.rast.map) %in% dt_cp_extent_map$cell_no))]] = 1
# plot(cp.rast.map)
# click(cp.rast.map,cell = T)

map_cp_extent <- ggplot()+ 
  geom_sf(data=dt_cp_extent_map,aes(fill=avg_bottom_t,color=avg_bottom_t),size=2, pch = 22) +
  annotation_map(map_data('worldHires'),fill = 'grey70',alpha=0.3)+
  # geom_sf(data=geo_borders, colour=NA, fill="lightgoldenrod2")+ # geo_border loaded in the map code see source(...)
  scale_x_continuous(limits=xlim1) +  scale_y_continuous(limits=ylim1) # limit in the map R code
# ------------------------------------------------------
# create shapefile with the grid cell within the cold pool extent
# Open it in QGIS or ArcGIS or others - report isolated grid cell numbers 
# st_write(dt_cp_extent_map, dsn = "dt_cp_extent_map.shp", layer = "dt_cp_extent_map.shp", driver = "ESRI Shapefile")
# ------------------------------------------------------
# /!\ preliminary analysis showed some isolated grid cells ==> We removed this cell
# should be adjust each year
dt_cp_extent <-  filter(dt_cp_extent,!cell_no%in%c(1399,2850,3259,3341,2605))
head(dt_cp_extent)
dim(dt_cp_extent)
# 
dt_cp_extent_map<-inner_join(cell.index,dt_cp_extent, by="cell_no")
# DOWNLOAD MAP
map_cp_extent <- ggplot()+ 
  geom_sf(data=dt_cp_extent_map, aes(fill=avg_bottom_t,color=avg_bottom_t),pch = 15,size=2) +
  scale_fill_gradientn(colours=matlab.like(50),name="Mean bottom\ntemperature")+
  scale_color_gradientn(colours=matlab.like(50),name="Mean bottom\ntemperature")+
  annotation_map(map_data('worldHires'),fill = 'grey70',alpha=0.3)+
  # geom_sf(data=geo_borders, colour=NA, fill="lightgoldenrod2")+ # geo_border loaded in the map code see source(...)
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
jpeg(file = here::here('Figures','cold_pool','cp_extent_soe.jpeg'), width = 14, height = 14, units = "cm", res = 500)
map_cp_extent
dev.off()
