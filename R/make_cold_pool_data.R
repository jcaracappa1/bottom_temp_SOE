#Makes data inputs for cold pool index scripts
library(terra)

glorys.dir = here::here('data','GLORYS_daily','/')
psy.dir =  here::here('data','PSY_daily','/')

glorys.files = list.files(glorys.dir,'GLORYS_daily_BottomTemp_*')
psy.files = list.files(psy.dir,'PSY_daily_BottomTemp_*')

mab.ext = ext(-75.5,-71,36,42)

bathy =  rast(here::here('data','NEUS_bathy.nc'))

glorys.grid = crop(subset(rast(paste0(glorys.dir,glorys.files[1])),1),mab.ext)

bathy.cp = crop(bathy,glorys.grid)
bathy.cp = clamp(bathy.cp, lower = 20, upper = 200,values = F)
plot(bathy.cp)

glorys.grid = mask(crop(glorys.grid,bathy.cp),bathy.cp)
plot(glorys.grid)

glorys.grid.out = glorys.grid %>%
  as.data.frame(glorys.grid,cells = T,xy =T) %>%
  select(-bottomT_1)%>%
  st_as_sf(coords = c('x','y'))%>%
  rename(cell_no = 'cell')

saveRDS(glorys.grid.out,here::here('data','cold_pool','cold_pool_cell_index.rds'))


dat.glorys = list()
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
dat.glorys = bind_rows(dat.glorys)
saveRDS(dat.glorys,here::here('data','cold_pool','GLORYS_cold_pool_input.rds'))

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
dat.psy = bind_rows(dat.psy)
saveRDS(dat.psy,here::here('Data','cold_pool','PSY_cold_pool_input.rds'))

