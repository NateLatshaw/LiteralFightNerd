rm(list = ls())
gc()

library(data.table)
library(lubridate)

#setwd('D:/LiteralFightNerd/')
data_path <- 'E:/UFC-Analytics/Processed Data/'
output_path <- './_supplemential_files/2025-03-02-introducing-strike-accuracy-over-expected/input_files/'

# read in data
careers <- fread(paste0(data_path, 'career_stats.csv'))
fights <- fread(paste0(data_path, 'fight_level_stats.csv'))

# format data
fights[, EventDate := as.Date(EventDate)]

fights <- fights[grepl('UFC', Event) | (grepl('The Ultimate Fighter', Event) & grepl('Finale', Event))]
fights <- fights[EventDate >= '2020-01-01']

# save data
current_dt <- gsub('-', '', Sys.Date())
saveRDS(fights, paste0(output_path, 'fight_stats_', current_dt, '.RDS'))
saveRDS(careers, paste0(output_path, 'career_stats_', current_dt, '.RDS'))
