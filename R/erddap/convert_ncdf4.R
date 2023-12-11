#converts files in directory to netcdf 4 versions

in.dir = here::here('data','du_Pontavice','ncdf3','/')
in.files = list.files(in.dir,full.names = T)
in.files.short = list.files(in.dir,full.names = F)

out.dir = here::here('data','du_Pontavice','ncdf4','/')

i=1
for(i in 1:length(in.files)){
  new.file = paste0(out.dir,in.files.short[i])
  file.copy(in.files[i],new.file)
  
  in.nc = nc_open(new.file,write = T)
  ncatt_put(in.nc,0,'title','Bottom temperature product for the northeast U.S continental shelf obtained from a combination of observations and models, daily, 1/12Â°, 1959 - 2020')
  nc_close(in.nc)
  
}