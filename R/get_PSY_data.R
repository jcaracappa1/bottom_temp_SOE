#' Retrieves PSY data from (present day - 3 years) to present day
#' Updates annually

get_PSY_data = function(out.dir,REGION, VERT_RANGE, username,password,start.date,end.date){
  library(CopernicusMarine)
  library(tidyverse)
  library(tidync)  

  # copernicus_product_details(product  = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
  #                            layer    = "cmems_mod_glo_phy_anfc_0.083deg_P1D-m",
  #                            variable = 'tob')
  
  copernicus_download_motu(
    username = username,
    password = password,
    destination   = paste0(out.dir,'PSY_daily_BottomTemp_',start.date,'_',end.date,'.nc'),
    product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
    layer         = "cmems_mod_glo_phy_anfc_0.083deg_P1D-m",
    variable      = "tob",
    output        = "netcdf",
    region        = REGION,
    timerange     = c(start.date, end.date),
    sub_variables = 'tob',
    verticalrange = VERT_RANGE,
    overwrite=TRUE
  )
}
  

