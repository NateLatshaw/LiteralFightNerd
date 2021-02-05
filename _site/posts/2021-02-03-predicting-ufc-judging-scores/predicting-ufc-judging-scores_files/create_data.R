rm(list = ls())
gc()

library(data.table)
library(lubridate)

data_path <- 'E:/UFC-Analytics/MMA Decisions/Processed Decisions/'
output_path <- './_posts/2021-02-03-predicting-ufc-judging-scores/predicting-ufc-judging-scores_files/'

# read in data
df <- readRDS(paste0(data_path, 'round_decisions_and_stats.RDS'))

# subset data
df <- df[grepl('UFC', Event)]
df <- df[year(EventDate) %in% 2010:2020]

# save data
current_dt <- gsub('-', '', Sys.Date())
saveRDS(df, paste0(output_path, 'round_decisions_and_stats_', current_dt, '.RDS'))