#Script to create a daily mean mixed layer depth for each SAMs areas
library(terra)

#input data directory
data.dir = here::here('Data','GLORYS','GLORYS_daily')

#SAMS areas
sams.shp = project(vect(here::here('geometry','MAB_ESTIMATION_AREAS_2023_UTM18_PDT_NYB.shp')),' +proj=longlat +datum=WGS84 +no_defs ')

sams.names = sams.shp$NewSAMS

not.na = function(x){ return( length(x[!is.na(x)]))}

years = 1993:2023

mld.yr.ls =list()
y =1
for(y in 1:length(years)){
  
  yr.rast = rast(paste0(data.dir,'/GLORYS_daily_MLD_',years[y],'.nc'))
  
  s=1
  sams.yr.ls = list()
  for(s in 1:length(sams.names)){
    
    this.sam = sams.shp[which(sams.shp$NewSAMS == sams.names[s])]
    
    yr.sam = crop(mask(yr.rast,this.sam),this.sam)
    
    yr.dates = time(yr.sam)
    
    mld.mean = global(yr.sam,mean,na.rm=T)[,1]
    mld.sd =  global(yr.sam,sd,na.rm=T)[,1]
    mld.n =  global(yr.sam,not.na)[,1]
    
   sams.yr.ls[[s]]= data.frame(date = yr.dates, SAM = sams.names[s], mld.mean, mld.sd, mld.n)
  }
  mld.yr.ls[[y]] = dplyr::bind_rows(sams.yr.ls)

} 

mld.df = dplyr::bind_rows(mld.yr.ls)

write.csv(mld.df,here::here('data','MLD','daily_MLD_sams.csv'),row.names =F)
