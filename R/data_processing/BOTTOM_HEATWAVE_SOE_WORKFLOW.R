#Workflow for generating the data required to calculate bottom marine heatwaves
#Forwarded to Vince Saba when completed

#Required inputs
source(here::here('R','data_processing','DATA_PROCESSING_WORKFLOW.R'))

#combine epu-level data to single long-format file

source(here::here('R','data_processing','make_daily_epu_combined.R'))

