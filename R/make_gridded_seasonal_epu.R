# Concatenate seasonal gridded data between GLORSY, PSY, and ROMS
library(terra)

glorys.dir = here::here('data','gridded_seasonal_epu_GLORYS','/')
psy.dir = here::here('data','gridded_seasonal_epu_PSY','/')
roms.dir = here::here('data','gridded_seasonal_epu_ROMS','/')

glorys.files = list.files(glorys.dir)
psy.files = list.files(psy.dir)
roms.files = list.files(roms.dir)

out.dir = here::here('data','gridded_seasonal_epu_combined','/')
if(!dir.exists(out.dir)){dir.create(out.dir)}

combs = expand.grid(epu = c('MAB','GOM','GB','SS'), season = c('winter','spring','fall','summer'))
i=1
for(i in 1:nrow(combs)){
  
  glorys.file = glorys.files[grepl(combs$epu[i],glorys.files)&grepl(combs$season[i],glorys.files)]
  psy.file = psy.files[grepl(combs$epu[i],psy.files)&grepl(combs$season[i],psy.files)]
  roms.file = roms.files[grepl(combs$epu[i],roms.files)&grepl(combs$season[i],roms.files)]
  
  glorys.data = rast(paste0(glorys.dir,glorys.file))
  psy.data = rast(paste0(psy.dir,psy.file))
  roms.data = rast(paste0(roms.dir,roms.file))
  
  glorys.time = time(glorys.data)
  psy.time = time(psy.data)
  roms.time = time(roms.data)
  
  all.time = c(roms.time,glorys.time,psy.time)
  
  glorys.data = crop(glorys.data,roms.data)
  psy.data = crop(psy.data,roms.data)
  
  data.ls = list(roms.data,glorys.data,psy.data)
  
  data.all = do.call(c,data.ls)
  time(data.all) <- all.time
  
  writeCDF(data.all,paste0(out.dir,'bot_temp_gridded_',combs$epu[i],'_',combs$season[i],'1959_2022.nc'),varname = 'BottomT',overwrite =T,zname = 'time')
  
}