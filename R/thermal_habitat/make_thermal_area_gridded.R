#Script to generate thermal area masks per EPU
library(terra)
library(dplyr)

source(here::here('R','thermal_habitat','make_thermal_mask_epu.R'))

out.dir = here::here('data','gridded_bottom_temp','/')

glorys.prefix = 'GLORYS_daily_BottomTemp_'
psy.prefix = 'PSY_daily_BottomTemp_'

glorys.dir = here::here('data','GLORYS','GLORYS_daily_epu','/')
psy.dir = here::here('data','PSY','PSY_daily_epu','/')

temp.bins = data.frame(temp.group = 1:3,
                       min.temp = c(10,14,18),
                       max.temp = c(30,30,30))
z.bins = data.frame(z.group = 1:2,
                    min.z = c(0,25),
                    max.z = c(25,100))
epu.names = c('MAB','GB','GOM','SS')

tz.combs = expand.grid(temp.group = temp.bins$temp.group,z.group = z.bins$z.group)%>%
  left_join(temp.bins)%>%
  left_join(z.bins)

i=e=g=p=1

for(e in 1:length(epu.names)){
  
  glorys.files = list.files(glorys.dir,paste0(glorys.prefix,epu.names[e]))
  # file.year = sapply(glorys.files,function(x) strsplit(x,paste0(glorys.prefix,'|_|.nc'))[[1]][3],USE.NAMES = F)
  
  glorys.time = list()
  glorys.dat = list()
  for(g in 1:length(glorys.files)){
    glorys.yr = rast(paste0(glorys.dir,glorys.files[g]))
    glorys.dat[[g]] = glorys.yr
    glorys.time[[g]] = as.Date(time(glorys.yr))
  }
  glorys.dat = do.call('c',glorys.dat)
  time(glorys.dat) = do.call('c',glorys.time)
  
  psy.files = list.files(psy.dir,paste0(psy.prefix,epu.names[e]))
  
  psy.time =list()
  psy.dat = list()
  for(p in 1:length(psy.files)){
    psy.yr = rast(paste0(psy.dir,psy.files[p]),subds = 'BottomT')
    # plot(subset(psy.yr,250))
    psy.dat[[p]] = psy.yr
    psy.time[[p]] = as.Date(time(psy.yr))
  }
  psy.dat = do.call('c',psy.dat)
  time(psy.dat) = do.call('c',psy.time)
  
  data.all = c(glorys.dat,psy.dat)
  rm(glorys.dat,psy.dat)
  
  data.all.file = here::here('data','gridded_bottom_temp','gridded_epu_combined',paste0('daily_bottomT_',epu.names[e],'.nc'))
  writeCDF(data.all,data.all.file,overwrite =T)
  
  # data.all.yr = format(time(data.all),format = '%Y')%>% as.numeric()
  # data.2022 = subset(data.all, which(data.all.yr == 2022))
  # plot(data.2022)
  for(i in 1:length(tz.combs)){
    
    new.file.name = here::here('data','gridded_bottom_temp','gridded_thermal_mask',paste0('daily_bottomT_mask_',epu.names[e],'_',tz.combs$min.temp[i],'deg_',tz.combs$max.z[i],'m.nc'))
    new.file.name.binary = here::here('data','gridded_bottom_temp','gridded_thermal_mask_binary',paste0('daily_bottomT_mask_',epu.names[e],'_',tz.combs$min.temp[i],'deg_',tz.combs$max.z[i],'m_binary.nc'))
    new.file.name.area = here::here('data','gridded_bottom_temp','gridded_thermal_mask_area',paste0('daily_area_mask_',epu.names[e],'_',tz.combs$min.temp[i],'deg_',tz.combs$max.z[i],'m.nc'))
    make_thermal_mask_epu(data.orig = data.all,
                      epu = epu.names[e],
                      min.temp = tz.combs$min.temp[i],
                      max.temp = tz.combs$max.temp[i],
                      min.z = tz.combs$min.z[i],
                      max.z = tz.combs$max.z[i],
                      shape.file =  here::here('geometry','EPU_NOESTUARIES.shp'),
                      out.name = new.file.name,
                      out.name.binary = new.file.name.binary,
                      out.name.area = new.file.name.area)  
    
    print(i)
  }
  
  
}
