rm(list = ls())
gc()

library(data.table)
library(lubridate)

data_path <- 'E:/UFC-Analytics/Processed Data/'
output_path <- './_supplemential_files/2021-02-28-introducing-expected-rounds-xr/input_files/'

# read in data
rounds <- fread(paste0(data_path, 'round_level_stats.csv'))
fights <- fread(paste0(data_path, 'fight_level_stats.csv'))

# set data types
rounds[, EventDate := as.Date(EventDate)]
fights[, EventDate := as.Date(EventDate)]

# subset data
rounds <- rounds[grepl('UFC', Event) | (grepl('The Ultimate Fighter', Event) & grepl('Finale', Event))]
fights <- fights[grepl('UFC', Event) | (grepl('The Ultimate Fighter', Event) & grepl('Finale', Event))]

rounds <- rounds[year(EventDate) %in% 2011:2020]
fights <- fights[year(EventDate) %in% 2011:2020]

# save data
current_dt <- gsub('-', '', Sys.Date())
saveRDS(rounds, paste0(output_path, 'round_stats_', current_dt, '.RDS'))
saveRDS(fights, paste0(output_path, 'fight_stats_', current_dt, '.RDS'))