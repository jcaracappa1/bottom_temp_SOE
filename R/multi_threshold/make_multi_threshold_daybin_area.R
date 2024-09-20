#Creates metrics relating to the proportion of areas that exceed a temperature threshold for some duration (day bins)
library(terra)
library(dplyr)

source(here::here('R','data_processing','make_temp_mask_funs.R'))

nes.shp = terra::vect(here::here('geometry','EPU_NOESTUARIES.shp'))

data.dir = here::here('data','multi_threshold','ndays')

max.t = c(seq(1,20,1),7.5)
years = 1959:2023

day.bins = data.frame( day.min = c(0,30,60,90),
                       day.max = c(30,60,90,366),
                       day.group = 1:4)

out.df = expand.grid(years = years, max.t = max.t, day.group = day.bins$day.group )%>%
  left_join(day.bins)%>%
  mutate(area = NA,
         pct.area = NA)

remake.files =F

if(remake.files == F){
  out.df.old = read.csv(here::here('data','multi_threshold','area','multi_threshold_area.csv'))
}

i =1
for(i in 1:nrow(out.df)){

  data = terra::rast(paste0(data.dir,'/',out.df$max.t[i],'/GLORYS_NES_',out.df$max.t[i],'C_ndays_',out.df$years[i],'.nc'))
  
  if(i == 1){
      nes.area = make_shp_area(data = data, file.shp = nes.shp,units = 'km')
  }
  
  if(remake.files == F){
    match = filter(out.df.old, years == out.df$years[i] & max.t == out.df$max.t[i])
    
    if(nrow(match) > 0 & !is.na(match$area[1])){
      next()
    }
  }

  data.nday.bin = make_temp_mask(data,file.shp= nes.shp, min.val = out.df$day.min[i],max.val =out.df$day.max[i])  
  
  data.nday.area = make_shp_temp_area(data.nday.bin,nes.shp,'km')
  
  data.area.pct = data.nday.area/nes.area
  
  out.df$area[i] = data.nday.area
  out.df$pct.area[i] = data.area.pct
    
  print(i/nrow(out.df))
}

if(remake.files == F){
  
  out.df2 = filter(out.df, !is.na(area))
  
  out.df = dplyr::bind_rows(out.df2, out.df.old)%>%
    arrange(years,max.t)
}

write.csv(out.df,here::here('data','multi_threshold','area','multi_threshold_area.csv'),row.names =F)
