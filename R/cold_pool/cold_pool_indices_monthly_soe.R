# --------------------------------------------------------------------
# Cold Pool indices
# --------------------------------------------------------------------

y_m_cd = readRDS(here::here('data','cold_pool','y_m_cd.rds'))


bt_temp_time_series_month = readRDS(here::here('data','cold_pool','bt_temp_time_series_month.rds'))
dt_cp_extent = readRDS(here::here('data','cold_pool','dt_cp_extent.rds'))

bt_temp_time_series_month_cpi<-filter(bt_temp_time_series_month, cell_no %in% unique(dt_cp_extent$cell_no))
dim(bt_temp_time_series_month_cpi)
length(unique(bt_temp_time_series_month_cpi$year))
length(unique(bt_temp_time_series_month_cpi$month))
length(unique(bt_temp_time_series_month_cpi$cell_no))
table(bt_temp_time_series_month_cpi[,"year"])
table(bt_temp_time_series_month_cpi[,"month"])
table(bt_temp_time_series_month_cpi[,c("year","month")])
hist(bt_temp_time_series_month_cpi$bt_temp)
# ------------------------------------------------------
# ------------------------------------------------------
# Cold Pool index calculation
# ------------------------------------------------------
# ------------------------------------------------------
# mean over the period 1972-2018 between June and September
dt_avg_bt<-bt_temp_time_series_month_cpi %>%
  filter(month %in% (6:9)) %>%
  group_by(cell_no) %>%
  summarise(mean_bt_temp=mean(bt_temp)) %>% #  mean temp over the time period
  as.data.frame()
head(dt_avg_bt)
dim(dt_avg_bt)
max(dt_avg_bt$mean_bt_temp)
min(dt_avg_bt$mean_bt_temp)
# -----------------------------------------------------
dt_cpi_soe <- bt_temp_time_series_month_cpi %>%
  filter(month %in% (6:9)) %>%
  group_by(year,source,cell_no) %>%
  summarise(bt_temp=mean(bt_temp)) %>%
  as.data.frame() %>%
  inner_join(dt_avg_bt, by="cell_no") %>%
  mutate(res_bottom_t=bt_temp-mean_bt_temp) %>%
  group_by(year,source) %>%
  summarise(cold_pool_index=mean(res_bottom_t),
            sd_cold_pool_index=sd(res_bottom_t),
            mean_bottom_temp=mean(bt_temp),
            nbr_cell=length(res_bottom_t)) %>%
  as.data.frame() %>%
  mutate(se_cold_pool_index=sd_cold_pool_index/sqrt(nbr_cell)) %>%
  select(year,source,cold_pool_index,sd_cold_pool_index,se_cold_pool_index,mean_bottom_temp) %>%
  mutate(type= "Model_CPI")
head(dt_cpi_soe)
dim(dt_cpi_soe)
# -----------------------------------------------------------
# -----------------------------------------------------------
# Persistence index calculation
# -----------------------------------------------------------
# -----------------------------------------------------------
dt_end_month<-data.frame()
dt_bt_sep_oct<-filter(bt_temp_time_series_month_cpi,month %in% (6:10))
for (y in sort(unique(dt_bt_sep_oct$year))) { # for each year
  dt_pre_year<-filter(dt_bt_sep_oct,year==y) %>%
    mutate(below = ifelse(bt_temp<10, 1, 0))
  # cell where temperature below 10˚ at least once betweem June and October
  dt_cell_cold <- dt_pre_year %>% group_by(source,cell_no) %>%
    summarise(length_below=sum(below)) %>%
    filter(length_below!=0) %>%
    as.data.frame()  %>%
    select(source,cell_no) 
  # First month when the temperature is below 10˚C 
  dt_start_below <- filter(dt_pre_year,cell_no%in%dt_cell_cold$cell_no & below==1) %>%
    group_by(source,cell_no) %>%
    summarise(start_below=min(month)) %>% as.data.frame() 
  # First month when the temperature is below 10˚C 
  dt_end_month_year <- filter(dt_pre_year,cell_no%in%dt_cell_cold$cell_no) %>%
    inner_join(dt_start_below,by=c('source',"cell_no")) %>%
    filter((month-start_below)>0) # we keep only the cell where temperature falls below 10˚
  # if temperature raise again above 10˚C
  dt_end_month_year_during <- filter(dt_end_month_year,below==0) %>% # selection of days where temp > 10˚C
    group_by(source,cell_no) %>% 
    summarise(end_month=min(month))  # selection the first month where temp > 10˚C
  # if temperature remain below 10˚C
  dt_end_month_year_above <- anti_join(dt_end_month_year, dt_end_month_year_during,by=c('source',"cell_no")) %>%
    filter(below==1)  
  #
  if (nrow(dt_end_month_year_above)>0) { # selection the first month where temp > 10˚C
    dt_end_month_year_above <- dt_end_month_year_above %>% 
      group_by(source,cell_no) %>%
      summarise(end_month=max(month))
  } 
  dt_end_month_year<-bind_rows(dt_end_month_year_during,dt_end_month_year_above)%>%
    mutate(year=y) %>%
    as.data.frame()
  dt_end_month<-bind_rows(dt_end_month,dt_end_month_year)
}
# Mean persistence
mean_end_month<-dt_end_month %>%
  group_by(source,cell_no) %>%
  summarise(mean_end_month=mean(end_month)) %>%
  as.data.frame()
# residual persistence and index calculation
dt_persistence_index_soe<-inner_join(mean_end_month,dt_end_month,by=c('source',"cell_no")) %>%
  mutate(res_month=end_month-mean_end_month) %>%
  group_by(source,year) %>%
  summarise(persistence_index=mean(res_month),
            sd_persistence_index=sd(res_month),
            mean_end_month=mean(end_month),
            count_cell=length(res_month)) %>%
  as.data.frame() %>%
  mutate(se_persistence_index=sd_persistence_index/sqrt(count_cell)) %>%
  select(source,year,persistence_index,sd_persistence_index,se_persistence_index,mean_end_month) %>%
  mutate(type= "Model_PI")
head(dt_persistence_index_soe)
dim(dt_persistence_index_soe)
# ------------------------------------------------------
# ------------------------------------------------------
# Spatial extent index calculation
# ------------------------------------------------------
# -----------------------------------------------------
# Bottom temperature per year
# -----------------------------------------------------
bt_temp_time_series_month_extent <- filter(bt_temp_time_series_month_cpi, month %in% (6:9))
dt_bt_ts_mean<-bt_temp_time_series_month_extent %>%
  group_by(source,year) %>%
  summarise(bt_temp=mean(bt_temp)) %>%
  as.data.frame()
# -----------------------------------------------------------
dt_extent_year<-data.frame()
for (y in sort(unique(bt_temp_time_series_month_extent$year))) { # for each year
  dt_pre_year<-filter(bt_temp_time_series_month_extent,year==y) %>%
    mutate(below = ifelse(bt_temp<10, 1, 0))
  # cell where monthly temperature below 10˚ at least once between June and September
  dt_cell_cold <- dt_pre_year %>%
    group_by(source,cell_no) %>%
    summarise(length_below=sum(below)) %>%
    filter(length_below>=2) %>%
    mutate(year=y) %>% as.data.frame() %>%
    select(source,year, cell_no)  
  dt_extent_year<-rbind(dt_extent_year,dt_cell_cold)
}
dt_extent_year<-dt_extent_year%>%
  group_by(source,year) %>%
  summarise(nbr_cell=length(cell_no)) %>%
  as.data.frame()
# Mean extent
mean_nbr_cell<-dt_extent_year %>%
  summarise(mean_nbr_cell=mean(nbr_cell)) %>%
  as.data.frame()
# spatial extent index calculation
dt_extent_index<-dt_extent_year %>%
  mutate(extent_index=nbr_cell-mean_nbr_cell$mean_nbr_cell) 
# sd of the index
dt_sd_extent_index<-dt_extent_index %>%
  summarise(sd_extent_index=sd(extent_index),
            se_extent_index=sd(extent_index)/nrow(dt_extent_index))
# bind sd and index
dt_extent_index_soe<-dt_extent_index%>%
  mutate(sd_extent_index=dt_sd_extent_index$sd_extent_index,
                            se_extent_index=dt_sd_extent_index$se_extent_index) %>%
  select(source,year,extent_index,sd_extent_index,se_extent_index) %>%
  mutate(type= "Model_SEI")
head(dt_extent_index_soe)
dim(dt_extent_index_soe)
# -----------------------------------------------------
# ----------------------------------------------------
dt_cp<-dt_cpi_soe%>%
  select(source,year,cold_pool_index,se_cold_pool_index)  %>%
  inner_join(select(dt_persistence_index_soe,source,year,persistence_index,se_persistence_index), by=c('source',"year")) %>%
  inner_join(select(dt_extent_index_soe,source,year,extent_index,se_extent_index), by=c('source',"year")) %>%
  select(source,year,cold_pool_index,se_cold_pool_index,persistence_index,se_persistence_index,extent_index,se_extent_index)
  
write.csv(file=here::here('data','SOE',"cold_pool_indices_1959_2023.csv"),x=dt_cp,row.names = F)

#Optional: Compare to ecodata
# soe = ecodata::cold_pool%>%
#   tidyr::spread(Var,Value)
# 
# plot(cold_pool_index~year,dt_cp,type='l',ylim =c(-2.5,3))
# lines(cold_pool_index~Time,soe,col =2)
# abline(h =0)
# 
# plot(persistence_index~year,dt_cp,type='l',ylim = c(-2,1))
# lines(persistence_index~Time,soe,col =2)
# abline(h=0)
# 
# plot(extent_index~year,dt_cp,type='l')
# lines(extent_index~Time,soe,col =2)
# abline(h=0)

