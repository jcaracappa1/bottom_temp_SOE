#Script to make adjustments to duPontavice bottom temperature data includeing
#1) convert netcdf3 to netcdf4
#2) fix typo in metadata, duplicate "combination"
#3) Check proper DOI and CMEMS credit in metadata

library(ncdf4)

in.dir = 'C:/Users/joseph.caracappa/Documents/Data/duPontavice/original_KH/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Data/duPontavice/bt_revised_metadata_032024/'

in.files = list.files(in.dir)

i=1
for(i in 1:length(in.files)){
  
  file.copy(paste0(in.dir,in.files[i]),paste0(out.dir,in.files[i]),overwrite = T)
  
  in.nc = nc_open(paste0(out.dir,in.files[i]),write = T)  
  
  # year = strsplit(in.files[i],'bottom_temp_|.nc')[[1]][2]
  # var.t = in.nc$dim$time$vals
  # as.POSIXct(var.t*86400,origin = '1950-01-01 00:00:00')
  
  ncatt_put(in.nc,0,'title','Bottom temperature product for the northeast U.S continental shelf obtained from a combination of observations and models, daily, 1/12°, 1959 - 2020')
  
  nc_close(in.nc)

}


# x = nc_open(paste0(out.dir,in.files[i]))
# x$dim$time$vals
