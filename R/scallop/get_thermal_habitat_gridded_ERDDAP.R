### Thermal Habitat Persistence - Joe Caracappa

# install.packages('rerddap')


get_thermal_habitat_gridded_ERDDAP <- function(threshold, year, min.depth,max.depth,min.lat,min.lon,max.lat,max.lon,depth.mask.file, plot,write.table,plot.file,table.file,save_clean = F){
  library(dplyr)
  library(usethis)
  library(rerddap)
  library(terra)
  library(mapdata)
  
  yt.combs = expand.grid(year = year, threshold = threshold)%>%
    dplyr::mutate(threshold.char = gsub('\\.','_',threshold))
  
  #Get spatial extent
  region.ext = terra::ext(min.lon,max.lon,min.lat,max.lat)
 
  #Read Bathy Shp
  depth.mask = terra::rast(depth.mask.file)
  
  df.ind = 1
  output.ls = list()
  
  #Loop through year x threshold combinations
  for(i in 1:nrow(yt.combs)){
    
    #Download from ERDDAP
    # http://nefsctest.nmfs.local:8080/erddap/griddap/annual_thermal_habitat_glorys.htmlTable?nday_1%5B(2024-01-01T00:00:00Z):1:(2024-12-01T00:00:00Z)%5D%5B(44.5):1:(35.83333333333333)%5D%5B(-75.91666666666667):1:(-65.66666666666669)%5D
    file.url = URLencode(paste0('http://nefsctest.nmfs.local:8080/erddap/griddap/annual_thermal_habitat_glorys.nc?nday_',yt.combs$threshold.char[i],'%5B(',yt.combs$year[i],'-01-01):1:(',yt.combs$year[i],'-01-01)%5D%5B(44.5):1:(35.83333333333333)%5D%5B(-75.91666666666667):1:(-65.66666666666669)%5D'),repeated = F,reserved = T)
    data.file.name = here::here(paste0('thermal_habitat_gridded_',yt.combs$threshold.char[i],'_',yt.combs$year[i],'.nc'))
    system.download = paste('curl --compressed -g "',file.url,'" -o ',data.file.name,sep = '')
    system(system.download)
    
    #Convert to raster object
    comb.data = terra::rast(data.file.name)
    
    # print(mean(values(comb.data),na.rm=T))
    
    # Create mask depth range & coordinate range
    depth.range = terra::clamp(depth.mask,lower = min.depth, upper = max.depth, values = F)
    depth.region.rast = terra::crop(terra::mask(depth.range,region.ext),region.ext)
      
    # terra::crop(comb.data,depth.epu.rast)
    data.depth.region = terra::crop(comb.data,depth.region.rast)
    
    # convert to dataframe
    output.ls[[df.ind]] = as.data.frame(data.depth.region,xy =T) %>%
      dplyr::rename(Value = dplyr::starts_with('nday_'),
                    Longitude = 'x',
                    Latitude = 'y')%>%
      dplyr::mutate(Time = yt.combs$year[i],
                    min.depth = min.depth,
                    max.depth = max.depth,
                    Var = yt.combs$threshold[i],
                    Source = 'GLORYS',
                    Units = 'Number of Days above threshold')%>%
      dplyr::select(Time, min.depth,max.depth, Var, Value, Latitude, Longitude, Source, Units)
    
    df.ind = df.ind +1
    
    
    #Delete temp file
    file.remove(data.file.name)
    
  }
  
  thermal_habitat_gridded = dplyr::bind_rows(output.ls)

  if(plot==T){
    neus.map = map_data('worldHires',region = c('USA','Canada'))
    
    ggplot()+
      # ggplot2::geom_sf(data=ecodata::coast,) +
      geom_tile(data =thermal_habitat_gridded, aes(x=Longitude,y=Latitude,fill = Value),width = 1/12, height = 1/12)+
      annotation_map(neus.map,fill = 'grey90',color = 'black')+
      scale_fill_viridis_c()+
      coord_equal()+
      facet_grid(Var~Time)+
      theme_bw()+
      theme(legend.position = 'bottom')
    ggsave(plot.file,width =4 * length(year), height =4*length(threshold))
  }
  if(write.table == T){
    write.csv(thermal_habitat_gridded,table.file)
  }
  
  
}
get_thermal_habitat_gridded_ERDDAP(threshold = c(17,18.5),
                            year = 2020:2024,
                            min.depth = 25,
                            max.depth = 100,
                            min.lat = 34,
                            max.lat = 42,
                            min.lon = -78,
                            max.lon = -68,
                            depth.mask.file = here::here('geometry','GLORYS_depth_NEUS.nc'),
                            plot =T,
                            plot.file = here::here('Figures','scallop_18_5_2022_2024.png'),
                            write.table =T,
                            table.file = here::here('Figures','scallop_18_5_2022_2024.csv') )



