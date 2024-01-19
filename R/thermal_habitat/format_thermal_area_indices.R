#Script to reformat thermal area indicies for SOE
library(dplyr)

data.dir = here::here('data','thermal_area','daily_area_prop','/')
out.dir = here::here('data','SOE','thermal_area','/')

data.files = list.files(data.dir)
data.prefix = 'daily_area_'

file.source = sapply(data.files,function(x) strsplit(x,paste0(data.prefix,'|_|.rds'))[[1]][2],USE.NAMES = F)
file.epu = sapply(data.files,function(x) strsplit(x,paste0(data.prefix,'|_|.rds'))[[1]][3],USE.NAMES = F)
file.temp= sapply(data.files,function(x) strsplit(x,paste0(data.prefix,'|_|.rds'))[[1]][4],USE.NAMES = F)
file.z= sapply(data.files,function(x) strsplit(x,paste0(data.prefix,'|_|.rds'))[[1]][5],USE.NAMES = F)

epu.names = sort(unique(file.epu))
temp.groups = sort(unique(file.temp))
z.groups = sort(unique(file.z))

combs = expand.grid(temp.group = temp.groups,z.group = z.groups)

i=1
for( i in 1:nrow(combs)){
  
  which.files = which(file.temp == combs$temp.group[i] & file.z == combs$z.group[i])
  comb.source = file.source[which.files]
  comb.files = data.files[which.files]
  comb.epu = file.epu[which.files]
  comb.temp = file.temp[which.files]
  comb.z = file.z[which.files]
  
  j=1
  data.comb.ls = list()
  for(j in 1:length(comb.files)){
    
    data.comb.ls[[j]] = readRDS(paste0(data.dir,comb.files[j]))%>%
      mutate(source = comb.source[j],
             epu = comb.epu[j],
             temp.group = comb.temp[j],
             z.group = comb.z[j])
  }
  data.comb.df = bind_rows(data.comb.ls)
  
  out.name = paste0(out.dir,'thermal_area_',combs$temp.group[i],'_',combs$z.group[i],'.csv')
  
  write.csv(data.comb.df,out.name,row.names = F)
  
}
