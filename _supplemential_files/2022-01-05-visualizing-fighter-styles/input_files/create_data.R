rm(list = ls())
gc()

library(data.table)
library(lubridate)

data_path <- 'E:/UFC-Analytics/Processed Data/'
output_path <- './_supplemential_files/2022-01-05-visualizing-fighter-styles/input_files/'

# read in data
rounds <- fread(paste0(data_path, 'round_level_stats.csv'))
fights <- fread(paste0(data_path, 'fight_level_stats.csv'))

# set data types
rounds[, EventDate := as.Date(EventDate)]
fights[, EventDate := as.Date(EventDate)]

# subset data
rounds <- rounds[grepl('UFC', Event) | (grepl('The Ultimate Fighter', Event) & grepl('Finale', Event))]
fights <- fights[grepl('UFC', Event) | (grepl('The Ultimate Fighter', Event) & grepl('Finale', Event))]

all_fights <- copy(fights)
all_fights <- all_fights[EventDate <= '2021-12-31']
rounds <- rounds[EventDate <= '2021-12-31']
fights <- fights[EventDate <= '2021-12-31']

# save data
current_dt <- gsub('-', '', Sys.Date())
saveRDS(rounds, paste0(output_path, 'round_stats_', current_dt, '.RDS'))
saveRDS(fights, paste0(output_path, 'fight_stats_', current_dt, '.RDS'))
#saveRDS(all_fights, paste0(output_path, 'all_fight_stats_', current_dt, '.RDS'))