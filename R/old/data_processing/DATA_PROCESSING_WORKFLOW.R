#Workflow for generating bottom temperature date used in SOE

#1) CopernicusMarine Package Depricated - if fixed the following functions can do batch downloads
#source(here::here('R','get_GLORYS_bottomT.R'))
#source(here::here('R','get_PSY_bottomT.R'))

#1b) If pulled from 3D run scripts that extract bottom temperature
source(here::here('R','data_processing','make_GLORYS_bottomT.R'))
source(here::here('R','data_processing','make_PSY_bottomT.R'))

#salinity
source(here::here('R','data_processing','make_GLORYS_bottomS.R'))
source(here::here('R','data_processing','make_PSY_bottomS.R'))

#3) Crop the data by epu and for GLORYS and PSY years
source(here::here('R','old','data_processing','crop_data_epu.R'))
crop_data_epu(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                 in.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/GLORYS_daily/',
                 in.prefix = 'GLORYS_daily_BottomTemp_',
                 out.prefix = 'GLORYS_daily_BottomTemp_',
                 out.dir = here::here('data','GLORYS','2025','GLORYS_daily_epu','/')
)
# crop_data_epu(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
#                  in.dir = here::here('data','PSY','PSY_daily','/'),
#                  in.prefix = 'PSY_daily_BottomTemp_',
#                  out.prefix = 'PSY_daily_BottomTemp_',
#                  out.dir = here::here('data','PSY','PSY_daily_epu','/')
# )
crop_data_epu(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
              in.dir = here::here('data','PSY','PSY_daily','/'),
              in.prefix = 'PSY_daily_BottomTemp_',
              out.prefix = 'PSY_daily_BottomTemp_',
              out.dir = here::here('data','PSY','PSY_daily_epu','/')
)
#3b) crop data by epu for ROMS years
source(here::here('R','data_processing','crop_data_epu_ROMS.R'))

#4) crop data by season for GLORYS and PSY
source(here::here('R','data_processing','crop_data_season.R'))
crop_data_season(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                 in.dir = here::here('data','GLORYS','GLORYS_daily','/'),
                 in.prefix = 'GLORYS_daily_BottomTemp_',
                 out.prefix = 'GLORYS_daily_BottomTemp_',
                 out.dir = here::here('data','GLORYS','GLORYS_daily_season','/')
)
crop_data_season(shp.file = here::here('geometry','EPU_NOESTUARIES.shp'),
                 in.dir = here::here('data','PSY','PSY_daily','/'),
                 in.prefix = 'PSY_daily_BottomTemp_',
                 out.prefix = 'PSY_daily_BottomTemp_',
                 out.dir = here::here('data','PSY','PSY_daily_season','/')
)
source(here::here('R','data_processing','crop_data_season_ROMS.R'))

