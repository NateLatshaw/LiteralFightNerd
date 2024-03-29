rm(list = ls())
gc()

library(data.table)
library(lubridate)

data_path <- 'E:/UFC-Analytics/MMA Decisions/Processed Decisions/'
output_path <- './_supplemential_files/2021-02-19-introducing-judgeai/input_files/'

# read in data
df <- readRDS(paste0(data_path, 'round_decisions_and_stats.RDS'))

# subset data
df <- df[grepl('UFC', Event) | (grepl('The Ultimate Fighter', Event) & grepl('Finale', Event))]
df <- df[year(EventDate) %in% 2011:2020]

# save data
current_dt <- gsub('-', '', Sys.Date())
saveRDS(df, paste0(output_path, 'round_decisions_and_stats_', current_dt, '.RDS'))