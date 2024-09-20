#Function to extract value range from GLORYS data (or any gridded data)


# in.file = here::here('data','GLORYS','GLORYS_daily','GLORYS_daily_BottomTemp_2022.nc')
# min.val = 18
# max.val = 30
# file.shp = terra::project(terra::vect(here::here('geometry','MAB_ESTIMATION_AREAS_2023_UTM18_PDT_NYB.shp')),' +proj=longlat +datum=WGS84 +no_defs ')


## Function to create a mask of a gridded file based on a min and max value
# file.in: full file name for desired netcdf file
# file.shp: shape file read in by terra as a spatvector. Try terra::vect("file.name.shp")
# min.val/max.val: min and max value from gridded variable you want to crop
# Returns a Terra spatRaster of just the cells that are in that data range. 
make_temp_mask = function(file.in, file.shp, min.val, max.val){
  
  if(is.character(file.in)){
    data.rast = terra::rast(file.in)  
  }else{
    data.rast = file.in
  }
  
  data.crop = terra::crop(terra::mask(data.rast,file.shp, touches = T),file.shp)
  
  data.window =  terra::clamp(data.crop, lower = min.val, upper = max.val,values = F)
  
  # plot(subset(data.window,300))
  
  return(data.window)

}

## Function to calculate sum of days within temperature range
# data: a Terra SpatRaster object (returned from make_temp_mask())
# out.df: T/F flag. Turn on true if you want an X,Y,sum data frame
# Returns a SpatRaster of the sum of days in the input file that are within the min/max range above
make_temp_ndays = function(data, out.df = T){
  
  data.binary = (data * 0)+1
  
  data.binary.n = sum(data.binary,na.rm=T)
  
  if(out.df == T){
    data.n.df = as.data.frame(data.binary.n,xy =T)
    
    data.n.df$sum[which(data.n.df$sum == 0)]= NA  
  }else{
    return(data.binary.n)
  }
  
}

#Function to calculate the total area for input SpatVector shapefile
# data: Input spatRaster from make_temp_mask
# file.shp: Input SpatVect object for area of interest
# units: "m" or "km" 
make_shp_area = function(data,file.shp,units){
  
  data.rast = terra::subset(data,1)
  
  data.crop = terra::crop(terra::mask(data.rast,file.shp, touches = T),file.shp)
  
  shp.area = expanse(data.crop,unit = units)$area
  
  return(shp.area)
}

#Function to the area that is satisfying the min/max conditions from make_temp_mask()
# data: make_temp_mask(file.in,file.shp,min.val,max.val)
# area.shp: SpatVector of region of interest
# units: "m" or "km" 
make_shp_temp_area = function(data, area.shp,units){
  
  data.binary = (data *0)+1
  # plot(subset(data.binary,300))
  
  ntime = length(terra::time(data.binary))
  
  data.sum =app(data.binary,sum,na.rm=T)
  data.less = (data.sum== ntime)
  data.all = data.sum * data.less
  # plot(data.all)
  
  values(data.all)[values(data.all) == 0] = NA
  # plot(data.all)
  # plot(area.shp,add =T)
  # values(data.mask)
  
  area.mask = expanse(data.all, unit = units)$area
  
  return(area.mask)
  
}
