library(dplyr)
library(terra)

shp.file =here::here('geometry','EPU_NOESTUARIES.shp')

glorys.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/'
glorys.prefix = 'GLORYS_daily_BottomTemp_'
glorys.files = list.files(glorys.dir,glorys.prefix,full.names = T)
glorys.years = 1993:2024

t.max.seq = seq(0.5,30,by =  0.5)
# t.max.seq = c(10,10.5)

EPU.names = c('MAB','GB','GOM','SS')
depth.df = data.frame(id = 1:4,
                      depth.min = c(0,25,100,300),
                      depth.max = c(25,100,300,Inf),
                      depth.name = c('0-25m','25-100m','100-300m','300+'))

# combs = expand.grid(t.max = t.max.seq,depth.name = depth.df$depth.name,EPU = EPU.names,stringsAsFactors = F)%>%
#   left_join(depth.df)%>%
#   tidyr::unite(var.name,t.max,depth.name,EPU,sep = '_',remove = F)
combs = expand.grid(depth.name = depth.df$depth.name,EPU = EPU.names,stringsAsFactors = F)%>%
  left_join(depth.df) 
    # tidyr::unite(var.name,EPU,depth.name,t.max,sep = '_',remove = F)%>%
    # mutate(var.name.long= paste0('ndays ',t.max,'C in GLORYS cells ',depth.name,' in the ',EPU))

bathy.shp = terra::rast(here::here('data','bathymetry','GLO-MFC_001_024_mask_bathy.nc'),subds = 'deptho')

var.atts = read.csv(here::here('data','SOE','thermal_habitat_gridded_variable_attributes.csv')) %>%
  filter(!is.na(Value) & Attribute.Name != '_FillValue')
global.atts = read.csv(here::here('data','SOE','thermal_habitat_gridded_global_attributes.csv'))%>%
  filter(!is.na(Value))

i=1
y=1

for(y in 1:length(glorys.years)){
# for(y in 1:2){
  
  # if(y == 1){
  # 
  #   combs2 = combs[which(combs$EPU == 'NES' & combs$depth.name == 'alldepths' & combs$t.max == 1),]
  #   
  #   if(combs$EPU[i] == 'all'){
  #     area.names = c('MAB','GB','GOM','SS')
  #   }else{
  #     area.names = combs$EPU[i]
  #   }
  #     
  #   # this.file = glorys.files[[which(glorys.years == glorys.years[i])]]
  #   this.file = glorys.files[[y]]
  #   this.year = glorys.years[y]
  #   
  #   if(i ==1){
  #     neus.shp = terra::crop(bathy.shp,terra::rast(this.file))
  #   }
  #   
  #   depth.rast = terra::clamp(neus.shp, lower = combs$depth.min[i], upper = combs$depth.max[i],values =F)
  #   EPU.vect = terra::vect(shp.file)
  #   area.vect = EPU.vect[which(EPU.vect$EPU %in% area.names)]
  #   
  #   # area.crop = terra::crop(depth.rast,area.vect)
  #   area.mask = terra::mask(depth.rast,area.vect)
  #   
  #   #Mask of area over t.max
  #   area.test = EDABUtilities::mask_nc_2d(data.in = this.file,
  #                                      write.out =F,
  #                                      shp.file = area.mask,
  #                                      var.name = 'BottomT',
  #                                      min.value =  combs$t.max[i],
  #                                      max.value = Inf,
  #                                      binary = F,
  #                                      area.names =NA
  #   )
  #   out.test = EDABUtilities::make_2d_deg_day_gridded_nc(data.in = area.test,
  #                                                           shp.file = area.mask,
  #                                                           var.name = 'BottomT',
  #                                                           statistic = 'nd',
  #                                                           type = 'above',
  #                                                           ref.value = combs$t.max[i],
  #                                                           area.names = NA
  #   )[[1]]
  #   
  #   
  #   
  #   out.var.test = terra::sds(out.test)
  #   
  #   #concatenate names
  #   names(out.var.test) = combs$var.name
  #   longnames(out.var.test) = combs$var.name.long
  #   units(out.var.test) = 'days'
  #   
  #   out.filename = here::here('data','SOE','thermal_habitat_gridded',paste0('thermal_habitat_gridded_test_SOE2025.nc'))
  #   writeCDF(out.var.test,filename = out.filename,overwrite =T)
  #   
  #   file.nc = ncdf4::nc_open(out.filename,write =T)
  #   file.var = names(file.nc$var)
  #   
  #   for(g in 1:nrow(global.atts)){
  #     
  #     ncdf4::ncatt_put(file.nc,0,global.atts$Attribute.Name[g],global.atts$Value[g])
  #   }
  #   # new.nc = nc_open(new.file,write =T)
  #   # new.var.names = names(new.nc$var)
  #   # new.var.names = new.var.names[which(new.var.names!='crs')]
  #   # v=va=1
  #   for(v in 1:length(file.var)){
  #     
  #     for(va in 1:nrow(var.atts)){
  #       
  #       ncdf4::ncatt_put(file.nc,file.var[v],var.atts$Attribute.Name[va],var.atts$Value[va],prec ='text' )
  #       
  #     }
  #   }
  #   
  #   ncdf4::nc_close(file.nc)
  #   
  # }
  

  
  t=i=1
  out.ind =1
  out.var.names = character()
  year.out.ls = list()
  out.df.ls = list()
  for(t in 1:length(t.max.seq)){
    
    # this.file = glorys.files[[which(glorys.years == glorys.years[i])]]
    this.file = glorys.files[[y]]
    this.year = glorys.years[y]
    
    depth.rast.ls = list()
    epu.rast.ls = list()
    
    out.ls = list()
    
    for(i in 1:nrow(combs)){
      # for(i in 1:length(glorys.)){
      
      if(combs$EPU[i] == 'NES'){
        area.names = c('MAB','GB','GOM','SS')
      }else{
        area.names = combs$EPU[i]
      }
      
      EPU.vect = terra::vect(shp.file)
      area.vect = EPU.vect[which(EPU.vect$EPU %in% area.names)]
      
      if(i ==1 & t == 1){
        neus.shp = terra::crop(bathy.shp,terra::rast(this.file))
        #Create depth mask layer
        depth.ls = lapply(1:nrow(depth.df),function(x){
          d = terra::clamp(neus.shp,lower = depth.df$depth.min[x],upper = depth.df$depth.max[x],values = F)
          values(d)[!is.na(values(d))] = x
          return(d)
        })
        depth.mask = terra::merge(terra::sprc(depth.ls))
        depth.mask = as.factor(depth.mask)
        levels(depth.mask) = dplyr::select(depth.df,id,depth.name)
        terra::time(depth.mask) = glorys.years[y]
        writeCDF(depth.mask,here::here('data','SOE','thermal_habitat_gridded','depth_mask.nc'),overwrite =T)
        #create EPU mask layer
        epu.ls = lapply(1:length(EPU.names), function(x){
          e.vect = EPU.vect[which(EPU.vect$EPU %in% EPU.names[x])]
          e = terra::mask(neus.shp,e.vect)
          values(e)[!is.na(values(e))] = x
          return(e)
        })
        epu.mask = terra::merge(terra::sprc(epu.ls))
        epu.mask = as.factor(epu.mask)
        levels(epu.mask) = data.frame(1:4, EPU.names)
        terra::time(epu.mask) = glorys.years[y]
        
        epu.mask.binary = epu.mask
        values(epu.mask.binary)[!is.na(values(epu.mask.binary))] = 1
        writeCDF(epu.mask,here::here('data','SOE','thermal_habitat_gridded','EPU_mask.nc'),overwrite =T)
      }
      
      depth.rast = terra::clamp(neus.shp, lower = combs$depth.min[i], upper = combs$depth.max[i],values =F)
   
      # area.crop = terra::crop(depth.rast,area.vect)
      area.mask = terra::mask(depth.rast,area.vect)
      
      #Mask of area over t.max
      area.i = EDABUtilities::mask_nc_2d(data.in = this.file,
                                         write.out =F,
                                         shp.file = area.mask,
                                         var.name = 'BottomT',
                                         min.value =  t.max.seq[t],
                                         max.value = Inf,
                                         binary = F,
                                         area.names =NA
      )
      
      out.ls[[i]] = EDABUtilities::make_2d_deg_day_gridded_nc(data.in = area.i,
                                                              shp.file = area.mask,
                                                              var.name = 'BottomT',
                                                              statistic = 'nd',
                                                              type = 'above',
                                                              ref.value = t.max.seq[t],
                                                              area.names = NA
      )[[1]]
      
      year.time = as.POSIXct(paste0(glorys.years[y], '-01-01 00:00:00UTC'),origin = '1970-01-01 00:00:00',tz = 'UTC')
      terra::time(out.ls[[i]]) = as.numeric(year.time)
      # terra::time(out.ls[[i]]) = as.Date(paste0(glorys.years[y], '-01-01'))
      
      
      out.df.ls[[out.ind]] = as.data.frame(out.ls[[i]],cells =T, xy = T) %>%
        mutate(Time = this.year, EPU = combs$EPU[i], Depth = combs$depth.name[i], Var =t.max.seq[t], Source = 'GLORYS',Units = 'ndays')%>%
        rename(Latitude = 'y', Longitude = 'x', Value = 'sum')%>%
        select(Time,EPU, Depth, Var,Value,Latitude,Longitude,Source,Units)
      
      
      
      print(signif(i/nrow(combs) ,2))
    
      out.ind = out.ind + 1
    }
    
    #Combine epu x depth combs to single layer
    out.sds = terra::merge(terra::sprc(out.ls))* epu.mask.binary
    out.var.names[t] = paste0('Number of Days per Year Above ',t.max.seq[t],' degrees C')
    year.out.ls[[t]]  = out.sds
  }
  
  # saveRDS(out.ls,here::here('data','SOE','thermal_habitat_gridded_rast_neus_2025.rds'))
  saveRDS(out.df.ls,here::here('data','SOE','thermal_habitat_gridded',paste0('thermal_habitat_gridded_df_neus_',this.year,'_SOE2025.rds')))
  
  
  # for(i in 1:length(out.ls)){
  #   if(ext(out.ls[[i]])!=ext(out.ls[[1]])){
  #     print(i)
  #   }
  # }
  # out.ls = readRDS(here::here('data','SOE','thermal_habitat_gridded_rast_neus_2025.rds'))
  
  # year.out.ls[[length(t.max.seq)+1]] = depth.mask
  # year.out.ls[[length(t.max.seq)+2]] = epu.mask
  
  out.var = terra::sds(year.out.ls)
  
  #concatenate names
  # out.names = c(paste0('nday_',t.max.seq),'depth_bin','EPU'
  out.names = c(paste0('nday_',t.max.seq))
  out.names = gsub('\\.','_',out.names)
  names(out.var) = out.names
  # longnames(out.var) = c(out.var.names,'Depth Bin','Ecosystem Production Unit')
  terra::longnames(out.var) = out.var.names
  units(out.var) = c(rep('n days',length(t.max.seq)))
  # terra::time(out.var) = as.Date(paste0(glorys.years[y],'-01-01'))
  
  out.filename = here::here('data','SOE','thermal_habitat_gridded',paste0('thermal_habitat_gridded_',this.year,'_SOE2025.nc'))
  # saveRDS(out.var,here::here('test.rds'))
  # out.var =readRDS(here::here('test.rds'))
  
  terra::writeCDF(out.var,filename = out.filename,overwrite =T,missval = 0)
  
  file.nc = ncdf4::nc_open(out.filename,write =T)
  file.var = names(file.nc$var)
  
  for(g in 1:nrow(global.atts)){
    
    ncdf4::ncatt_put(file.nc,0,global.atts$Attribute.Name[g],global.atts$Value[g])
    
  }
  ncdf4::ncatt_put(file.nc,0,'time_coverage_start',paste0(glorys.years[y],'-01-01'))
  ncdf4::ncatt_put(file.nc,0,'time_coverage_end',paste0(glorys.years[y],'-12-31'))
  ncdf4::ncatt_put(file.nc,'time','units','seconds since 1970-01-01T00:00:00Z')
  ncdf4::ncatt_put(file.nc,'latitude','standard_name','latitude')
  ncdf4::ncatt_put(file.nc,'latitude','coverage_content_type','coordinates')
  
  ncdf4::ncatt_put(file.nc,'longitude','standard_name','longitude')
  ncdf4::ncatt_put(file.nc,'longitude','coverage_content_type','coordinates')
  
  # new.nc = nc_open(new.file,write =T)
  # new.var.names = names(new.nc$var)
  # new.var.names = new.var.names[which(new.var.names!='crs')]
  # v=va=1
  for(v in 1:length(file.var)){
    
    
    ncdf4::ncatt_put(file.nc,file.var[v],'standard_name','number_of_days_with_bottom_temperature_above_threshold',prec ='text' )
    
    for(va in 1:nrow(var.atts)){
      
      ncdf4::ncatt_put(file.nc,file.var[v],var.atts$Attribute.Name[va],var.atts$Value[va],prec ='text' )
      
    }
    
  }
  
  # ncdf4::ncdim_def('t','year',glorys.years[y],create_dimvar = T)
  
  ncdf4::nc_close(file.nc)
}


file.names = list.files(here::here('data','SOE','thermal_habitat_gridded'),'thermal_', full.names = T)
test.nc = ncdf4::nc_open(file.names[2])
# test.nc = ncdf4::nc_open('C:/Users/joseph.caracappa/Downloads/thermal_habitat_gridded_1993_SOE2025.nc')
ncdf4::ncatt_get(test.nc,'nday_10')
ncdf4::nc_close(test.nc)
plot(terra::rast(file.names[1]))
# test.sds = terra::sds(file.names[1],1)
# writeCDF(test.sds,here::here('data','SOE','thermal_habitat_checker.nc'))


# 
# f=1
# for( f in 1:length(file.names)){
#   
#   this.year = strsplit(file.names[f],'_|.nc')[[1]][4]
#   
#   file.name.long = here::here('data','SOE','thermal_habitat_gridded', file.names[f])
#   file.nc = ncdf4::nc_open(file.name.long, write = T)
#   file.var = names(file.nc$var)
#   t.var = sapply(file.var,function(x) return(strsplit(x,'_')[[1]][1]),USE.NAMES = F)
#   z.var = sapply(file.var,function(x) return(strsplit(x,'_')[[1]][2]),USE.NAMES = F)
#   z.var[which(z.var== 'all')] = 'alldepths'
#   epu.var = sapply(file.var,function(x) return(strsplit(x,'_')[[1]][3]),USE.NAMES = F)
#   epu.var[which(epu.var == 'all')] = 'NES'
#   new.file.var = paste0(epu.var,'_',z.var,'_',t.var,'C')
#   new.file.var.long = paste0('Days Above ',t.var,'C in GLORYS cells ',z.var,' in the ',epu.var)
# 
#   ncdf4::nc_close(file.nc)
#   
#   new.out =  terra::sds(file.name.long)
#   
#   names(new.out) = new.file.var
#   terra::longnames(new.out) = new.file.var.long
#   terra::units(new.out) = 'degree C'
# 
#   terra::writeCDF(new.out,file.name.long,overwrite =T,missval = NA)
#   
#   file.nc2 =  ncdf4::nc_open(file.name.long, write = T)
#   for(g in 1:nrow(global.atts)){
#     
#     ncdf4::ncatt_put(file.nc2,0,global.atts$Attribute.Name[g],global.atts$Value[g])
#   }
#     # new.nc = nc_open(new.file,write =T)
#     # new.var.names = names(new.nc$var)
#     # new.var.names = new.var.names[which(new.var.names!='crs')]
#     # v=va=1
#   for(v in 1:length(file.var)){
#     
#     ncdf4::ncatt_put(file.nc2,file.var[v],'Long_Name',new.file.var.long[v])
#     
#     for(va in 1:nrow(var.atts)){
#       
#       ncdf4::ncatt_put(file.nc2,file.var[v],var.atts$Attribute.Name[va],var.atts$Value[va],prec ='text' )
#       
#     }
#   }
#    
#   ncdf4::nc_close(file.nc2)
# }
