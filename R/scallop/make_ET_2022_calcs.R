library(tidync)
library(dplyr)

ET.open = tidync(here::here('data','estimation_areas','gridded_daily_estimation_areas_binary_mask','gridded_bottomT_binary_ET-Open_stressed.nc'))%>%
  hyper_tibble()%>%
  mutate(time = as.POSIXct(time,origin = '1970-01-01 00:00:00 UTC'),
         year = format(time,format = '%Y'))%>%
  filter(year == 2022)%>%
  group_by(longitude,latitude)%>%
  summarise(Ndays = sum(BottomT))

ET.flex = tidync(here::here('data','estimation_areas','gridded_daily_estimation_areas_binary_mask','gridded_bottomT_binary_ET-Flex_stressed.nc'))%>%
  hyper_tibble()%>%
  mutate(time = as.POSIXct(time,origin = '1970-01-01 00:00:00 UTC'),
         year = format(time,format = '%Y'))%>%
  filter(year == 2022)%>%
  group_by(longitude,latitude)%>%
  summarise(Ndays = sum(BottomT))
  
x = bind_rows(ET.open,ET.flex)
mean(x$Ndays)

