#Full workflow for creating scallop cropped bottom Temp data from GLORYS

#Parameter bounds
subarea.df = data.frame(
  name = c('MABs','MABn'),
  temp.group = c('normal','normal'),
  min.lat = c(36.5,40),
  max.lat = c(40,41.1),
  min.lon = c(-76.5,-76.5),
  max.lon = c(-71,-71),
  min.z = c(35,0),
  max.z = c(80,80),
  min.temp = c(18,18),
  max.temp = c(30,30)
)

#create bathymetry file
source(here::here('R','get_GLORYS_NEUS_bathymetry.R'))
get_GLORYS_NEUS_bathymetry(min.lat,max.lat,min.lon,max.lon)

#make cropped gridded data
source(here::here('R','make_daily_temp_depth_crop.R'))

i=1
for(i in 1:nrow(subarea.df)){
  make_daily_threshold(data.file = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_1993-01-01_2003-12-31.nc'),
                       product = 'GLORYS',
                       subarea = subarea.df$name[i],
                       min.lat = subarea.df$min.lat[i],
                       max.lat = subarea.df$max.lat[i],
                       min.lon = subarea.df$min.lon[i],
                       max.lon = subarea.df$max.lon[i],
                       min.temp = subarea.df$min.temp[i],
                       max.temp = subarea.df$max.temp[i],
                       min.z = subarea.df$min.z[i],
                       max.z = subarea.df$max.z[i],
                       min.year = 1993,
                       max.year = 2003,
                       data.dir = here::here('data','daily_bottomT_window_gridded','/')
  )
  
  make_daily_threshold(data.file = here::here('data','GLORYS_daily','GLORYS_daily_BottomTemp_2004-01-01_2020-12-31.nc'),
                       product = 'GLORYS',
                       subarea = subarea.df$name[i],
                       min.lat = subarea.df$min.lat[i],
                       max.lat = subarea.df$max.lat[i],
                       min.lon = subarea.df$min.lon[i],
                       max.lon = subarea.df$max.lon[i],
                       min.temp = subarea.df$min.temp[i],
                       max.temp = subarea.df$max.temp[i],
                       min.z = subarea.df$min.z[i],
                       max.z = subarea.df$max.z[i],
                       min.year = 2004,
                       max.year = 2020,
                       data.dir = here::here('data','daily_bottomT_window_gridded','/')
  )
  
  make_daily_threshold(data.file = here::here('data','PSY_daily','PSY_daily_BottomTemp_2020-11-01_2023-09-30.nc'),
                       product = 'PSY',
                       subarea = subarea.df$name[i],
                       min.lat = subarea.df$min.lat[i],
                       max.lat = subarea.df$max.lat[i],
                       min.lon = subarea.df$min.lon[i],
                       max.lon = subarea.df$max.lon[i],
                       min.temp = subarea.df$min.temp[i],
                       max.temp = subarea.df$max.temp[i],
                       min.z = subarea.df$min.z[i],
                       max.z = subarea.df$max.z[i],
                       min.year = 2020,
                       max.year = 2023,
                       data.dir = here::here('data','daily_bottomT_window_gridded','/')
  )
}


