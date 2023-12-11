#merge bottom temp, salinity, and bathymetry for Han
library(terra)

out.dir = here::here('data','gridded_raw_bottom_TS','/')

glorys.dir = here::here('data','GLORYS_daily','/')
psy.dir = here::here('data','PSY_daily','/')

years = 1993:2023

isDate <- function(mydate, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

ymax<-42
ymin<-34.5
xmax<-(-68.5)
xmin<-(-76.5)

temp.files.df = data.frame(
  file.name.full = c(list.files(glorys.dir,'GLORYS_daily_BottomTemp_',full.names = T),
                     list.files(psy.dir, 'PSY_daily_BottomTemp_',full.names = T)),
  file.name = c(list.files(glorys.dir,'GLORYS_daily_BottomTemp_'),
                list.files(psy.dir, 'PSY_daily_BottomTemp_')),
  start.year = NA,
  end.year = NA,
  stringsAsFactors = F
)
for(f in 1:nrow(temp.files.df)){
  file.str = strsplit(temp.files.df$file.name[f],'_|.nc')[[1]]
  file.dates = as.Date(file.str[isDate(file.str)])
  file.years = as.numeric(format(file.dates,format = '%Y'))
  temp.files.df$start.year[f] = file.years[1]
  temp.files.df$end.year[f] = file.years[2]
}
  
salt.files.df = data.frame(
  file.name.full = c(list.files(glorys.dir,'GLORYS_daily_BottomSalinity_',full.names =T),
                     list.files(psy.dir,'PSY_daily_BottomSalinity_',full.names = T)),
  file.name = c(list.files(glorys.dir,'GLORYS_daily_BottomSalinity_',full.names =F),
                     list.files(psy.dir,'PSY_daily_BottomSalinity_',full.names = F)),
  year = NA,
  stringsAsFactors = F
)

for(f in 1:nrow(salt.files.df)){
  
  file.str = strsplit(salt.files.df$file.name[f],'_|.nc')[[1]]
  salt.files.df$year[f] = as.numeric(file.str[length(file.str)])
}

i=1
for(i in 28:length(years)){
  
  # temp.file.year = temp.files.df$file.name.full[which(years[i] >= temp.files.df$start.year & years[i]<=temp.files.df$end.year )][1]
  temp.file.year = temp.files.df$file.name.full[grep(paste0('_',years[i],'.nc'),temp.files.df$file.name)][1]
  
  temp.rast = rast(temp.file.year)
  
  temp.time = time(temp.rast)
  temp.time.yr = as.numeric(format(as.Date(temp.time),format = '%Y'))
  
  temp.rast = subset(temp.rast,which(temp.time.yr == years[i]))
  
  time.dim= temp.time[which(temp.time.yr == years[i])]
  
  # salt.file.year = salt.files.df$file.name.full[which(salt.files.df$year == years[i])]
  salt.file.year = salt.files.df$file.name.full[grep(paste0('_',years[i],'.nc'),salt.files.df$file.name)][1]
  
  salt.rast = rast(salt.file.year)
  
  if(i == 1){
    bathy = rast('C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLO-MFC_001_030_mask_bathy.nc',subds = 'deptho')
    bathy =  crop(bathy,ext(xmin,xmax,ymin,ymax))
  }
  
  temp.rast = mask(crop(temp.rast,ext(bathy)),bathy)
  salt.rast = mask(crop(salt.rast,ext(bathy)),bathy)
  
  min.time = min(nlyr(temp.rast),nlyr(salt.rast))
  
  temp.rast = subset(temp.rast,1:min.time)
  salt.rast = subset(salt.rast,1:min.time)
  
  time(temp.rast) = time.dim[1:min.time]
  time(salt.rast) = time.dim[1:min.time]
  
  rast.out = sds(temp.rast,salt.rast,bathy)
  
  names(rast.out) = c('bottomT','bottomS','depth')
  
  writeCDF(rast.out,paste0(out.dir,'daily_MAB_bottom_TS_',years[i],'.nc'),overwrite =T)
  
}
