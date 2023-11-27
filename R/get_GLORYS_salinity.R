#' Retrieves GLORYS data from (present day - 3 years) to present day
#' Updates annually

out.dir = here::here('data','GLORYS_daily','/')
REGION = c(-76,35,-70,42)
VERT_RANGE =  c(0.49402499198913574, 120) 
username = ''
password = ''
start.date = '1994-01-01'
end.date = '1994-12-31'

get_GLORYS_data = function(out.dir,REGION, VERT_RANGE, username,password,start.date,end.date){
  library(CopernicusMarine)
  library(tidyverse)
  library(tidync)  
  
  if(!dir.exists(out.dir)){dir.create(out.dir)}
  
  # copernicus_product_details(product  = "GLOBAL_MULTIYEAR_PHY_001_030",
  #                            layer    = "cmems_mod_glo_phy_my_0.083_P1D-m",
  #                            variable = 'so')
  
  new.file.name = paste0(out.dir,'GLORYS_daily_Salinity_',start.date,'_',end.date,'.nc')
  
  # while(file.)
  tic()
  copernicus_download_motu(
    username = username,
    password = password,
    destination   = new.file.name,
    product       = "GLOBAL_MULTIYEAR_PHY_001_030",
    layer         = "cmems_mod_glo_phy_my_0.083_P1D-m",
    variable      = "so",
    output        = "netcdf",
    region        = REGION,
    timerange     = c(start.date, end.date),
    sub_variables = 'so',
    verticalrange = VERT_RANGE,
    overwrite=TRUE
  )
  toc()
}