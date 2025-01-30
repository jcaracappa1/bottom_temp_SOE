library(dplyr)

shp.file =here::here('geometry','EPU_NOESTUARIES.shp')

glorys.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/'
glorys.prefix = 'GLORYS_daily_BottomTemp_'
glorys.files = list.files(glorys.dir,glorys.prefix,full.names = T)
glorys.years = 1993:2024

t.max.seq = seq(0.5,30,by =  0.5)

EPU.names = c('MAB','GB','GOM','SS','all')
depth.df = data.frame(depth.min = c(0,25,100, 0),
                      depth.max = c(25,100,300, 2000),
                      depth.name = c('0-25m','25-100m','100-300m', 'AllDepths'))

combs = expand.grid(year = glorys.years,t.max = t.max.seq,depth.name = depth.df$depth.name,EPU = EPU.names,stringsAsFactors = F)%>%
  left_join(depth.df)
  
bathy.shp = terra::rast(here::here('data','bathymetry','GLO-MFC_001_024_mask_bathy.nc'),subds = 'deptho')

out.area.ls = list()
out.gridded.ls = list()
i=22717
for(i in 1:nrow(combs)){
  
  if(combs$EPU[i] == 'all'){
    area.names = c('MAB','GB','GOM','SS')
  }else{
    area.names = combs$EPU[i]
  }
  
  this.file = glorys.files[[which(glorys.years == combs$year[i])]]
  
  if(i ==1){
    neus.shp = terra::crop(bathy.shp,terra::rast(this.file))
  }
  
  depth.rast = terra::clamp(neus.shp, lower = combs$depth.min[i], upper = combs$depth.max[i],values =F)
  EPU.vect = terra::vect(shp.file)
  area.vect = EPU.vect[which(EPU.vect$EPU %in% area.names)]
  
  area.mask = terra::mask(depth.rast,area.vect)
  
  #Mask of area over t.max
  area.i = EDABUtilities::mask_nc_2d(data.in = this.file,
                                     write.out =F,
                                     shp.file = area.mask,
                                     var.name = 'BottomT',
                                     min.value =  combs$t.max[i],
                                     max.value = Inf,
                                     binary = F,
                                     area.names =NA
  )
  
  this.rast = terra::subset(terra::rast(this.file),1)
    
  nd.i = EDABUtilities::make_2d_deg_day_gridded_nc(data.in = area.i,
                                                shp.file = area.mask,
                                                var.name = 'BottomT',
                                                statistic = 'nd',
                                                type = 'above',
                                                ref.value = combs$t.max[i],
                                                area.names = NA
                                                )
  
  # data.area = terra::mask(this.rast,area.vect)
  # terra::values(data.area)[!is.na(terra::values(data.area))] <- 1
  shp.area = terra::expanse(area.mask)$area
  
  area.df = terra::expanse(area.i[[1]]) %>%
    as.data.frame()%>%
    mutate(Time = terra::time(area.i[[1]]),
           EPU = combs$EPU[i],
           Depth = combs$depth.name[i],
           Var = paste0('>',combs$t.max[i],'\u00B0C'),
           Value = area/ shp.area,
           Source = 'GLORYS',
           year = combs$year[i],
           temp.threshold = combs$t.max[i],
           Units = 'Proportion'
     )
  
  out.area.ls[[i]] = area.df
  
  out.gridded.ls[[i]] = as.data.frame(nd.i[[1]],cells =T, xy = T) %>%
    mutate(Time = combs$year[i], EPU = combs$EPU[i], Depth = combs$depth.name[i], Var = combs$t.max[i], Source = 'GLORYS',Units = 'Number of Days')%>%
    rename(Latitude = 'y', Longitude = 'x', Value = 'sum')%>%
    select(Time,EPU, Depth, Var,Value,Latitude,Longitude,Source,Units)
  
}

out.area.df = bind_rows(out.area.ls)%>%
  select(Time, EPU, Depth, Var, Value, Source, year, temp.threshold, Units)%>%
  mutate(Year = format(as.Date(Time),format = '%Y'))%>%
  group_by(Year,EPU, Depth, Var, temp.threshold, Units,Source)%>%
  summarise(Value = mean(Value))%>%
  rename(Time = Year)

write.csv(out.area.df, here::here('data','SOE','thermal_habitat_area_2025.csv'),row.names = F)

out.gridded.df = bind_rows(out.gridded.ls)

write.csv(out.gridded.df, here::here('data','SOE','thermal_habitat_gridded_2025.csv'),row.names = F)

this.year = data.table::fread( here::here('data','SOE','thermal_habitat_gridded_2025.csv')) %>%
  filter(Time == max(Time))
write.csv(this.year, here::here('data','SOE','thermal_habitat_gridded_2024_only_SOE2025.csv'),row.names = F)
  


