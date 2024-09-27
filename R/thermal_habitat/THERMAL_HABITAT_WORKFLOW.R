library(dplyr)
# #make_thermal_area_gridded
# source(here::here('R','thermal_habitat','make_thermal_area_gridded.R'))
# 
# #make_thermal_area_indices
# source(here::here('R','thermal_habitat','make_thermal_area_indices.R'))
# 
# #format_thermal_area_indices
# source(here::here('R','thermal_habitat','format_thermal_area_indices.R'))
# 
# #plot_daily_thermal_area
# source(here::here('R','thermal_habitat','plot_daily_thermal_area.R'))
# 
# #plot_annual_frequency
# source(here::here('R','thermal_habitat','plot_annual_frequency.R'))
# 
# #format_thermal_habitat_soe
# source(here::here('R','thermal_habitat','format_thermal_habitat_soe.R'))
# 
# #plot_cell_frequency_combined
# source(here::here('R','thermal_habitat','plot_cell_frequency_combined.R'))
# 
# #plot_thermal_area_combined
# source(here::here('R','thermal_habitat','plot_thermal_area_combined.R'))

shp.file =here::here('geometry','EPU_NOESTUARIES.shp')

glorys.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/'
glorys.prefix = 'GLORYS_daily_BottomTemp_'
glorys.files = list.files(glorys.dir,glorys.prefix,full.names = T)
glorys.years = 1993:2024

t.max.seq = seq(0.5,30,by =  0.5)
epu.names = c('MAB','GB','GOM','SS','all')
out.df = expand.grid(year = glorys.years,t.max = t.max.seq,epu = epu.names)%>%
  mutate(nd.min = NA,
         nd.max = NA,
         nd.mean = NA,
         nd.med = NA,
         nd.sd = NA,
         nd.con.min = NA,
         nd.con.max = NA,
         nd.con.mean = NA,
         nd.con.med = NA,
         nd.con.sd = NA,
         dd.min = NA,
         dd.max = NA,
         dd.med = NA,
         dd.mean = NA,
         dd.sd = NA,
         pct.area.min = NA,
         pct.area.med = NA,
         pct.area.mean = NA,
         pct.area.max = NA,
         pct.area.sd = NA)

i=1
for(i in 1:nrow(out.df)){
  #Number of days exceeding t.max
  nd.i =EDABUtilities::make_2d_deg_day_gridded_nc(data.in = glorys.files[[which(glorys.years == out.df$year[i])]],
                                                  write.out =F,
                                                  shp.file = shp.file,
                                                  var.name = 'BottomT',
                                                  statistic = 'nd',
                                                  ref.value = out.df$t.max[i],
                                                  type = 'above',
                                                  area.names = ifelse(out.df$epu[i] == 'all',c('MAB','GB','GOM','SS'),out.df$epu[i]) 
  )
  out.df$nd.min[i]=terra::global(nd.i[[1]],min,na.rm=T)
  out.df$nd.max[i]=terra::global(nd.i[[1]],max,na.rm=T)
  out.df$nd.med[i]=terra::global(nd.i[[1]],mean,na.rm=T)
  out.df$nd.mean[i]=terra::global(nd.i[[1]],median,na.rm=T)
  out.df$nd.sd[i]=terra::global(nd.i[[1]],sd,na.rm=T)
  
  #Number of consecutive days exceeding t.max
  nd.con.i =EDABUtilities::make_2d_deg_day_gridded_nc(data.in = glorys.files[[which(glorys.years == out.df$year[i])]],
                                                  write.out =F,
                                                  shp.file = shp.file,
                                                  var.name = 'BottomT',
                                                  statistic = 'nd.con',
                                                  ref.value = out.df$t.max[i],
                                                  type = 'above',
                                                  area.names = ifelse(out.df$epu[i] == 'all',c('MAB','GB','GOM','SS'),out.df$epu[i]) 
  )
  
  out.df$nd.con.min[i]=terra::global(nd.con.i[[1]],min,na.rm=T)
  out.df$nd.con.max[i]=terra::global(nd.con.i[[1]],max,na.rm=T)
  out.df$nd.con.med[i]=terra::global(nd.con.i[[1]],mean,na.rm=T)
  out.df$nd.con.mean[i]=terra::global(nd.con.i[[1]],median,na.rm=T)
  out.df$nd.con.sd[i]=terra::global(nd.con.i[[1]],sd,na.rm=T)
  
  #Degree days exceeding t.max
  dd.i =EDABUtilities::make_2d_deg_day_gridded_nc(data.in = glorys.files[[which(glorys.years == out.df$year[i])]],
                                                  write.out =F,
                                                  shp.file = shp.file,
                                                  var.name = 'BottomT',
                                                  statistic = 'dd',
                                                  ref.value = out.df$t.max[i],
                                                  type = 'above',
                                                  area.names = ifelse(out.df$epu[i] == 'all',c('MAB','GB','GOM','SS'),out.df$epu[i]) 
  )
  
  out.df$dd.min[i]=terra::global(dd.i[[1]],min,na.rm=T)
  out.df$dd.max[i]=terra::global(dd.i[[1]],max,na.rm=T)
  out.df$dd.med[i]=terra::global(dd.i[[1]],mean,na.rm=T)
  out.df$dd.mean[i]=terra::global(dd.i[[1]],median,na.rm=T)
  out.df$dd.sd[i]=terra::global(dd.i[[1]],sd,na.rm=T)
  
  #Calculate EPU area
  shp.vect = terra::vect(shp.file)
  shp.area.epu = terra::expanse(shp.vect)
  if(out.df$epu[i] == 'all'){
    shp.area = sum(shp.area.epu)
  }else{
    shp.str = as.data.frame(shp.vect)
    which.att = which(apply(shp.str,2,function(x) all(out.df$epu[i] %in% x)))
    which.area =  match(out.df$epu[i],shp.str[,which.att])
    shp.area = shp.area.epu[which.area]
  }
  
  #Mask of area over t.max
  area.i = EDABUtilities::mask_nc_2d(data.in = glorys.files[[which(glorys.years == out.df$year[i])]],
                                     write.out =F,
                                     shp.file = shp.file,
                                     var.name = 'BottomT',
                                     min.value =  out.df$t.max[i],
                                     max.value = Inf,
                                     binary = T
  )
  area.df = lapply(area.i,function(x) return(terra::expanse(x))) %>%
    as.data.frame()%>%
    mutate(area.pct = area/ shp.area)
  
  out.df$pct.area.min[i] = min(area.df$area.pct,na.rm=T)
  out.df$pct.area.med[i] = median(area.df$area.pct,na.rm=T)
  out.df$pct.area.max[i] = max(area.df$area.pct,na.rm=T)
  out.df$pct.area.mean[i] = mean(area.df$area.pct,na.rm=T)
  out.df$pct.area.sd[i] = sd(area.df$area.pct,na.rm=T)
  
  print(i/nrow(out.df))
}

saveRDS(out.df,here::here('data','SOE','thermal_habitat_2025.rds'))

                                          

