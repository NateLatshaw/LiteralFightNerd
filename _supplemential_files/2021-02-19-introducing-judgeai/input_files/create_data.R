rm(list = ls())
gc()

library(data.table)
library(lubridate)

data_path <- 'E:/UFC-Analytics/MMA Decisions/Processed Decisions/'

# read in data
df <- readRDS(paste0(data_path, 'round_decisions_and_stats.RDS'))

# subset data
df <- df[grepl('UFC', Event)]
df <- df[year(EventDate) %in% 2011:2020]

# save data
current_dt <- gsub('-', '', Sys.Date())
saveRDS(df, paste0('round_decisions_and_stats_', current_dt, '.RDS'))