#' Retrieves PSY data from (present day - 3 years) to present day
#' Updates annually

get_GLORYS_data = function(out.dir,REGION, VERT_RANGE,username,password,start.date,end.date,
                           layer= 'cmems_mod_glo_phy_my_0.083_P1D-m'){
  library(CopernicusMarine)
  library(tidyverse)
  library(tidync)  
  
  if(!dir.exists(out.dir)){dir.create(out.dir)}
  
  copernicus_product_details(product  = "GLOBAL_MULTIYEAR_PHY_001_030",
                             layer    = "cmems_mod_glo_phy_my_0.083_P1D-m",
                             variable = 'bottomT')
  
  copernicus_download_motu(
    username = username,
    password = password,
    destination   = paste0(out.dir,'GLORYS_daily_BottomTemp_',start.date,'_',end.date,'.nc'),
    product       = "GLOBAL_MULTIYEAR_PHY_001_030",
    layer         = "cmems_mod_glo_phy_my_0.083_P1D-m",
    variable      = "bottomT",
    output        = "netcdf",
    region        = REGION,
    timerange     = c(start.date, end.date),
    sub_variables = 'bottomT',
    verticalrange = VERT_RANGE,
    overwrite=TRUE
  )
}