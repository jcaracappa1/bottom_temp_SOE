start.year = 1950
stop.year = 2050

dates = seq.Date(as.Date(paste0(start.year,'-01-01')),as.Date(paste0(stop.year,'-12-31')),by= 1)

y_m_cd = data.frame(
  year = as.numeric(format(dates,format = '%Y')),
  month = as.numeric(format(dates,format = '%m')),
  calendar_day = as.numeric(format(dates,format = '%j')),
  day = as.numeric(format(dates,format = '%e'))
)

saveRDS(y_m_cd,here::here('data','cold_pool','y_m_cd.rds'))
