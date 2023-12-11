#Script that calls on all other processes in order to generate seasonal bottom temperature anomaly by EPU

#1) Run /R/data_processing/DATA_PROCESSING_WORKFLOW.R first

#2) Create a seasonal gridded netCDF of GLORYS data
source(here::here('R','bottom_temp_seasonal_gridded','make_gridded_seasonal_epu_GLORYS.R'))

#3) Create a seasonal gridded netCDF of PSY data
source(here::here('R','bottom_temp_seasonal_gridded','make_gridded_seasonal_epu_PSY.R'))

#4) Create a seasonal gridded netCDF of ROMS data
source(here::here('R','make_gridded_seasonal_epu_ROMS.R'))

#5) Create seasonal anomaly CSV and climatology
source(here::here('R','make_epu_seasonal_anomaly.R'))

#6) Plot all seasonal anomalies to check
source(here::here('R','plot_epu_seasonal_anomaly.R'))
