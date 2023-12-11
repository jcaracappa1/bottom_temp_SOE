#Full workflow to generate the cold_pool SOE indices

#1) Pull daily data for GLORYS and PSY
# here::here('R','data_access','make_GLORYS_bottomT.R')
# here::here('R','data_access','make_GLORYS_bottomS.R')

#2) Generate long-format data for cold pool analysis
source(here::here('R','cold_pool','make_cold_pool_data.R'))

#3) Generate max cold pool extent and data
source(here::here('R','cold_pool','cold_pool_extent_monthly_soe.R'))

#4) Generate cold pool indices
source(here::here('R','cold_pool','cold_pool_indicies_monthly_soe.R'))

# Output files are located in 
# her::here('data','SOE','cold_pool_indicies_1959_2023.csv')