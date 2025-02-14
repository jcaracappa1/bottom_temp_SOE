#Full workflow to generate the cold_pool SOE indices

#1) Pull daily data for GLORYS and PSY
# here::here('R','data_access','make_GLORYS_bottomT.R')
# here::here('R','data_access','make_GLORYS_bottomS.R')

#2) Generate long-format data for cold pool analysis
source(here::here('R','cold_pool','make_cold_pool_data.R'))

#3) Generate max cold pool extent and data
source(here::here('R','cold_pool','cold_pool_extent_monthly_soe.R'))

#4) Generate cold pool indices
source(here::here('R','cold_pool','cold_pool_indices_monthly_soe.R'))

#Compare to last year
cp24 = ecodata::cold_pool%>%
  mutate(report.year = 2024)
cp25 = read.csv(here::here('data','SOE','cold_pool_indices_1959_2024_SOE2025.csv'))%>%
  rename(Time = 'year')%>%
  tidyr::gather(Var, Value,-Source,-Time)%>%
  mutate(EPU = 'MAB',
         report.year = 2025)

cp.all = bind_rows(cp24,cp25) %>%
  filter(Source != 'PSY')

ggplot(cp.all, aes(x = Time, y = Value, color = Source,lty = factor(report.year)))+
  geom_line()+
  facet_wrap(~Var,scale = 'free_y')

#### Need to update for 2024 Fall