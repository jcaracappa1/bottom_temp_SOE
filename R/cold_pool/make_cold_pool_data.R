#Makes data inputs for cold pool index scripts
library(terra)
library(dplyr)
library(ncdf4)
library(sf)

glorys.dir = here::here('data','GLORYS','GLORYS_daily','/')
psy.dir =  here::here('data','PSY','PSY_daily','/')

glorys.files = list.files(glorys.dir,'GLORYS_daily_BottomTemp_*')
psy.files = list.files(psy.dir,'PSY_daily_BottomTemp_*')

mab.ext = ext(-75.5,-71,36,42)

bathy =  rast(here::here('data','bathymetry','GLO-MFC_001_024_mask_bathy.nc'),subds = 'deptho')

load(here::here('data','cold_pool',"grid_cell_no.Rda"))
load(here::here('data','cold_pool','grid_cell_cp.Rda'))

cell.combined = grid_cell_cp %>%
  left_join(grid_cell_no)%>%
  filter(!st_is_empty(geom))%>%
  mutate(Value = 1)%>%
  select(Value,geom)

cp.ext =vect(cell.combined$geom)

glorys.sample = subset(rast(paste0(glorys.dir,glorys.files[1])),1)
glorys.grid = crop(mask(glorys.sample,cp.ext),cp.ext)
# plot(glorys.grid)

bathy.cp = mask(crop(bathy,glorys.grid),glorys.grid)
bathy.cp = clamp(bathy.cp, lower = 20, upper = 200,values = F)
# plot(bathy.cp)

glorys.grid = mask(crop(glorys.grid,bathy.cp),bathy.cp)
glorys.grid.rast = raster::raster(glorys.grid)

glorys.grid.out = glorys.grid %>%
  as.data.frame(glorys.grid,cells = T,xy =T) %>%
  select(-BottomT_1)%>%
  st_as_sf(coords = c('x','y'))%>%
  rename(cell_no = 'cell')

writeCDF(glorys.grid,here::here('data','cold_pool','cold_pool_rast.nc'),overwrite =T)
saveRDS(glorys.grid.out,here::here('data','cold_pool','cold_pool_cell_index.rds'))


dat.glorys = list()
f=1
for(f in 1:length(glorys.files)){
  # dat.glorys = tidync(paste0(glorys.dir,glorys.files[f]))%>%
  #   hyper_array(force =T,select_var = c('time','bottomT'))
  dat.file = rast(paste0(glorys.dir,glorys.files[f]))
  dat.time = time(dat.file)
  dat.file = mask(crop(dat.file,glorys.grid),glorys.grid)
  dat.time.y = format(dat.time,format = '%Y') %>% as.numeric()
  dat.time.j = format(dat.time,format = '%j') %>% as.numeric()
  dat.out.ls = list()
  for(t in 1:length(dat.time)){
    dat.t = subset(dat.file,t)

    dat.t = as.data.frame(dat.t,cell = T,na.rm=T)
    colnames(dat.t) = c('cell_no','bt_temp')
    
    dat.out.ls[[t]]= dat.t%>%
      mutate(year = dat.time.y[t],
             calendar_day = dat.time.j[t])%>%
      select(cell_no,year,calendar_day,bt_temp)
    
    # test = dat.out.ls[[t]] %>%
    #   left_join(glorys.grid.out)
    # ggplot()+
    #   geom_sf(data=test, aes(color=bt_temp,geometry = geometry),size=3,pch =15)
  }
  dat.glorys[[f]] = bind_rows(dat.out.ls)
  rm(dat.out.ls)
}
dat.glorys = bind_rows(dat.glorys)%>%
  mutate(source = 'GLORYS')
saveRDS(dat.glorys,here::here('data','cold_pool','cold_pool_input_GLORYS.rds'))

dat.psy = list()
for(f in 1:length(psy.files)){
  dat.file = rast(paste0(psy.dir,psy.files[f]))
  dat.time = time(dat.file)
  dat.file = mask(crop(dat.file,glorys.grid),glorys.grid)
  dat.time.y = format(dat.time,format = '%Y') %>% as.numeric()
  dat.time.j = format(dat.time,format = '%j') %>% as.numeric()
  dat.out.ls = list()
  for(t in 1:length(dat.time)){
    dat.t = subset(dat.file,t)
    dat.t = as.data.frame(dat.t,cell = T,na.rm=T)
    colnames(dat.t) = c('cell_no','bt_temp')

    dat.out.ls[[t]]= dat.t%>%
      mutate(year = dat.time.y[t],
             calendar_day = dat.time.j[t])%>%
      select(cell_no,year,calendar_day,bt_temp)
    
    # test = dat.out.ls[[t]] %>%
    #   left_join(glorys.grid.out)
    # ggplot()+
    #   geom_sf(data=test, aes(color=bt_temp,geometry = geometry),size=3,pch =15)
  }
  dat.psy[[f]] = bind_rows(dat.out.ls)
  rm(dat.out.ls)
}
dat.psy = bind_rows(dat.psy)%>%
  mutate(source = 'PSY')
saveRDS(dat.psy,here::here('Data','cold_pool','cold_pool_input_PSY.rds'))

#ROMS-NWA bias corrected inputs
roms.file = here::here('data','ROMS','bottom_temp_debiased_roms_reg112_1959_2004.nc')
roms.years = 1959:1992

dat.roms = list()
for(i in 1:length(roms.years)){
  
  roms.yr = raster::brick(roms.file,lvar = 4,level = i)
  
  start.date = as.POSIXct(paste0(roms.years[i],'-01-01 00:00:00'),tz = 'UTC')
  end.date = as.POSIXct(paste0(roms.years[i],'-12-31 00:00:00'),tz = 'UTC')
  
  new.dates = as.character(seq.Date(as.Date(start.date),as.Date(end.date),'day'))
  new.dates = as.POSIXct(new.dates,origin = '1970-01-01 00:00:00',tz = 'UTC')
  
  roms.crop =terra::crop(roms.yr,glorys.grid.rast)
  roms.crop = raster::projectRaster(roms.crop,glorys.grid.rast)
  
  roms.mask = terra::mask(roms.crop,glorys.grid.rast)
  
  roms.dat = rast(roms.mask)
  if(length(new.dates)!= 366){new.dates = c(new.dates,NA)}
  time(roms.dat) = new.dates
  
  dat.out.ls = list()
  for(t in 1:length(new.dates)){
    dat.t = subset(roms.dat,t)
    dat.t = as.data.frame(dat.t,cell = T,na.rm=T)
    colnames(dat.t) = c('cell_no','bt_temp')
    
    
    dat.out.ls[[t]]= dat.t%>%
      mutate(year = roms.years[i],
             calendar_day = as.numeric(format(new.dates[t],format = '%j')))%>%
      select(cell_no,year,calendar_day,bt_temp)
  }
  dat.roms[[i]] = bind_rows(dat.out.ls)
}
dat.roms = bind_rows(dat.roms)%>%
  mutate(source = 'ROMS')
saveRDS(dat.roms,here::here('Data','cold_pool','cold_pool_input_ROMS.rds'))


