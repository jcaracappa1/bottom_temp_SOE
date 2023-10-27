#Script to make climatology (epu x season x year)
library(tidync)
library(sf)

data.all = readRDS(here::here('Data','GLORYS_PSY_BottomT_EPU_1993_2022.rds'))
 
shp = read_sf(here::here('geometry','EPU_NOESTUARIES.shp'))

data.epu = data.all %>%
  st_as_sf(coords = c('longitude','latitude'),crs = sf::st_crs(epu_shp))

  
  pnts.shp = sf::st_as_sf(cell.index, coords = c('lon','lat'),crs = sf::st_crs(epu_shp))
  
  epu.match = pnts.shp %>% 
    mutate(intersection = as.integer(sf::st_intersects(geometry,epu_shp)),
           area = if_else(is.na(intersection),'',epu_shp$EPU[intersection])
    ) %>% select(cell,area)
  epu.match = as.data.frame(epu.match) %>% select(cell,area) %>% filter(area != '')
  
  
  
  
  time(var.winter.epu) <- unlist(winter.time)
  time(var.winter.epu)
  # if(j == 1){
  # 
  #   cell.index = data.frame(cell = 1:ncell(var.rast),rowColFromCell(var.rast,1:ncell(var.rast)))
  #   cell.index$longitude = round(xFromCell(var.rast,cell = cell.index$cell),2)
  #   cell.index$latitude = round(yFromCell(var.rast,cell = cell.index$cell),2)
  #   
  #   rm(var.rast)
  # }
  
  data = tidync(epu.files[j])%>%
    hyper_tibble()
  # mutate(longitude = round(longitude,2),
  #        latitude = round(latitude,2))%>%
  # left_join(cell.index)%>%
  # select(time,cell,BottomT)
  
  data.epu = data %>%
    group_by(time)%>%
    summarise(BottomT.mean = mean(BottomT,na.rm=T),
              BottomT.sd = sd(BottomT,na.rm=T),
              BottomT.median = median(BottomT,na.rm=T),
              BottomT.min = min(BottomT,na.rm=T),
              BottomT.max = max(BottomT,na.rm=T))
  
  
  # x = data %>% filter(time == min(time))
  # ggplot(x,aes(x=longitude,y = latitude,color = BottomT))+geom_point()