#Script that calls on all other processes in order to generate seasonal bottom temperature anomaly by EPU

#1) Pull data from GLORYS. Make sure to fill out CMEMS username and password (DO NOT COMMIT THEM THOUGH!)
source(here::here('R','get_GLORYS_data.R'))
get_GLORYS_data(out.dir = here::here('data','GLORYS_daily','/'),
                REGION = c(-81,29,-43,56),
                VERT_RANGE =  c(0.49402499198913574, 5727.9169921875) ,
                username = '',
                password = '',
                start.date = '1993-01-01',
                end.date = '2003-12-31')
get_GLORYS_data(out.dir = here::here('data','GLORYS_daily','/'),
                REGION = c(-81,29,-43,56),
                VERT_RANGE =  c(0.49402499198913574, 5727.9169921875) ,
                username = '',
                password = '',
                start.date = '2004-01-01',
                end.date = '2020-12-31')

#2) Pull data from PSY. Make sure to fill out CMEMS username and password (DO NOT COMMIT THEM THOUGH!)
source(here::here('R','get_PSYU_data.R'))
get_PSY_data(out.dir = here::here('data','PSY_daily','/'),
             REGION = c(-81,29,-43,56),
             VERT_RANGE =  c(0.49402499198913574, 5727.9169921875) ,
             username = '',
             password = '',
             start.date = '2020-11-01',
             end.date = '2023-09-30')

#3) Crop the data by epu and for GLORYS and PSY years
source(here::here('R','crop_data_epu.R'))

#4) Crop the data by epu and for ROMS
source(here::here('R','crop_data_epu_ROMS.R'))

#5) Create a seasonal gridded netCDF of GLORYS data
source(here::here('R','make_seasonal_gridded_epu_GLORYS.R'))

#6) Create a seasonal gridded netCDF of PSY data
source(here::here('R','make_seasonal_gridded_epu_PSY.R'))

#7) Create a seasonal gridded netCDF of ROMS data
source(here::here('R','make_seasonal_gridded_epu_ROMS.R'))

#8) Create seasonal anomaly CSV and climatology
source(here::here('R','make_epu_seasonal_anomaly.R'))

#9) Plot all seasonal anomalies to check
source(here::here('R','plot_epu_seasonal_anomaly.R'))
