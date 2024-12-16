#Script to calculate the habitable area by year within each SAMS

out.dir = here::here('data','scallop','habitat_area')

file.shp = terra::project(terra::vect(here::here('geometry','MAB_ESTIMATION_AREAS_2023_UTM18_PDT_NYB.shp')),' +proj=longlat +datum=WGS84 +no_defs ')

source(here::here('R','old','data_processing','make_temp_mask_funs.R'))

years = 1993:2024

max.vals = seq(16,19,0.5)
SAM.names = file.shp$NewSAMS

SAM.combs = expand.grid(max.vals =max.vals,SAM.names =SAM.names)

SAMs.area.ls = list()
i=1
for(i in 1:length(years)){
  
  file.in = paste0('C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_',years[i],'.nc')
  # file.in = here::here('data','GLORYS','GLORYS_daily',paste0('GLORYS_daily_BottomTemp_',years[i],'.nc'))
  
  year.df = SAM.combs
  year.df$year = years[i]
  year.df$area.prop = NA
  j=1
  for(j in 1:nrow(SAM.combs)){
    
    SAM.shp = file.shp[which(file.shp$NewSAMS == SAM.combs$SAM.names[j]),]
    
    SAM.mask =make_temp_mask(file.in = file.in,
                  file.shp =SAM.shp,
                   min.val = 0,
                   max.val = SAM.combs$max.vals[j])  
    
    SAM.area = make_shp_area(data = file.in,
                             file.shp = SAM.shp,
                             units = 'km')
    
    SAM.temp.area = make_shp_temp_area(data = SAM.mask,
                       area.shp = SAM.shp,
                       units = 'km')
    
    SAM.temp.prop = SAM.temp.area/SAM.area
    
    year.df$area.prop[j] = SAM.temp.prop
  }
  
  
  SAMs.area.ls[[i]] = year.df
  
}

SAMS.area = dplyr::bind_rows(SAMs.area.ls) %>%
  dplyr::select(SAM.names,max.vals,year,area.prop)%>%
  dplyr::rename(SAM = 'SAM.names',
                max.temp = 'max.vals',
                prop.habitable = 'area.prop')%>%
  arrange(SAM,max.temp,year)

write.csv(SAMS.area,here::here('data','scallop','habitat_area','Annual_SAMS_thermal_habitat_area.csv'),row.names = F)
