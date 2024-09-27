# out.dir = 'C:/Users/joseph.caracappa/Documents/Data/duPontavice/bt_revised_metadata_032024/'
library(terra)
library(dplyr)
library(ncdf4)
library(ggplot2)
out.dir = 'C:/Users/joseph.caracappa/Documents/Data/duPontavice/original_KH/'
out.f =list.files(out.dir)

for(i in 1:length(out.f)){
  
  # in.nc = rast(paste0(out.dir,out.f[i]))
  in.nc = nc_open(paste0(out.dir,out.f[i]))
  yr = strsplit(out.f[i],'bottom_temp_|.nc')[[1]][2]
  
  true.date =seq.Date(as.Date(paste0(yr,'-01-01')),as.Date(paste0(yr,'-12-31')),by =1) %>% as.character()
  in.time = in.nc$dim$time$vals
  in.time = as.POSIXct(in.time*86400,origin = '1950-01-01')
  in.time = as.Date(in.time,origin = '1950-01-01',tz = 'EST') %>% as.character()
  
  date.match = all(in.time == true.date)
  # print(paste0(yr,'_',length(time(in.nc))))
  print(paste0(yr,'_',date.match))
  
  nc_close(in.nc)
}

epu.shp = vect(here::here('geometry','EPU_NOESTUARIES.shp'))[1,]

dat.06 = rast(paste0(out.dir,'bottom_temp_2006.nc'))
dat.07 = rast(paste0(out.dir,'bottom_temp_2007.nc'))
dat.08 = rast(paste0(out.dir,'bottom_temp_2008.nc'))

t.06 = time(dat.06)
t.07 = time(dat.07)
t.08 = time(dat.08)

mab.06 = crop(mask(dat.06,epu.shp),epu.shp)
mab.07 = crop(mask(dat.07,epu.shp),epu.shp)
mab.08 = crop(mask(dat.08,epu.shp),epu.shp)

mean.06  = global(mab.06,mean,na.rm=T)[,1]
mean.07  = global(mab.07,mean,na.rm=T)[,1]
mean.08  = global(mab.08,mean,na.rm=T)[,1]

out.df = data.frame(date = c(t.06,t.07,t.08),
                    year = c(rep(2006, length(t.06)), rep(2007,length(t.07)), rep(2008,length(t.08))),
                             temp = c(mean.06,mean.07,mean.08))

ggplot(out.df,aes(x= date,y = temp,color = factor(year)))+
  geom_line()
